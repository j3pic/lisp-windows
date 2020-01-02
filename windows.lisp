(defpackage :lisp-windows
  (:use :cffi :common-lisp)
  (:documentation "Macros to define bindings for Win32 functions.")
  (:export message-box open-file get-c-string raise-windows-error with-converted-object with-converted-objects
	   windows-error my-name memset with-foreign-objects* foreign-slot-value* c-buffer->byte-vector make-foreign-object free
	   with-foreign-types load-library close-libraries malloc with-byte-buffer wincall start-program run-buffer create-exe suicide
	   with-byte-buffers mem-aref* getenv closehandle defwinfun))

(in-package :lisp-windows)

;; Extend CFFI with a :wstring type that uses Windows wchar_t characters.
;;
;; The default :string type is either UTF-8 or wchar (UTF-16), or something
;; else entirely, depending on the value of CFFI:*DEFAULT-FOREIGN-ENCODING*
;; at the time that the string is translated.

(defclass wstring (cffi::foreign-string-type) ())

(defmethod initialize-instance :after ((instance wstring) &rest ignored &key &allow-other-keys)
  (declare (ignore ignored))
  (setf (slot-value instance 'cffi::encoding) :utf-16))

(let ((*package* (find-package :cffi)))
  (define-parse-method :wstring (&rest args)
    (apply #'make-instance 'wstring args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-foreign-library "user32.dll")
  (load-foreign-library "ws2_32.dll"))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant +invalid-handle-value+ (with-foreign-pointer (ptr (foreign-type-size :pointer))
                                        ;; This is the CFFI equivalent to ((size_t)-1) in C.
                                        ;; First we have to find the integer that is equivalent
                                        ;; in size to a pointer.
                                        (let ((equivalent-integer (find-if (lambda (types)
                                                                             (= (foreign-type-size (car types))
                                                                                (foreign-type-size :pointer)))
                                                                           '((:long :ulong) (:long-long :unsigned-long-long)))))
                                          (destructuring-bind (signed unsigned) equivalent-integer
                                            (setf (mem-ref ptr signed) -1)
                                            (mem-ref ptr unsigned)))))

  (defun insert-error-handler (name retval error-convention getlasterror)
    "Returns an error handling form for use in the macroexpansion of
DEFWINFUN. Which form is returned depends on the ERROR-CONVENTION specified. Each ERROR-CONVENTION
corresponds to one of the several error-reporting methods used by the Windows API."
    (ecase error-convention
      ((:invalid-handle-value)
       `(if (= ,retval +invalid-handle-value+)
	    (raise-windows-error ,name (,getlasterror))))
      ;; Returns 0 on success and -1 on failure.
      ((:unix)
       `(if (= ,retval -1)
	    (raise-windows-error ,name (,getlasterror))))
      ((:nonzero)
       `(if (/= ,retval 0)
	    (raise-windows-error ,name (,getlasterror))))
      ;; The function returns a BOOL that equals TRUE
      ;; for success or FALSE for failure.
      ((:boolean) 
       `(if (= ,retval 0)
	    (raise-windows-error ,name (,getlasterror))))
      ;; The function returns a BOOL indicating success, but sometimes
      ;; the FALSE return value can indicate success, too.
      ((:boolean/check-last-error) 
       (let ((last-error (gensym)))
         `(let ((,last-error (,getlasterror)))
	    (if (and (= ,retval 0)
		     (/= ,last-error 0))
	        (raise-windows-error ,name ,last-error)))))
      ;; The function returns a NULL pointer on failure, but sometimes
      ;; also returns NULL on success.
      ((:null-pointer/check-last-error)
       (let ((last-error (gensym)))
         `(let ((,last-error (,getlasterror)))
	    (if (and 
	         (not (eql ,last-error 0))
	         (or (not ,retval)
		     (and (not (stringp ,retval))
			  (not (numberp ,retval))
			  (null-pointer-p ,retval))))
	        (raise-windows-error ,name ,last-error)))))
      ;; A NULL pointer indicates success.
      ((:non-null-pointer)
       `(unless (or (not ,retval)
		    (eql ,retval 0)
		    (and (not (stringp ,retval))
		         (not (numberp ,retval))
		         (cffi:null-pointer-p ,retval)))
	  (raise-windows-error ,name (,getlasterror))))
      ;; A NULL pointer indicates failure.
      ((:null-pointer)
       `(if (or (not ,retval)
	        (eql ,retval 0)
	        (and (not (stringp ,retval))
		     (not (numberp ,retval))
		     (cffi:null-pointer-p ,retval)))
	    (raise-windows-error ,name (,getlasterror))))
      ;; Returns a Windows error code directly instead of relying on GetLastError.
      ((:errcode)
       `(if (/= ,retval 0)
	    (raise-windows-error ,name ,retval))))))

(defmacro defwinfun ((name &optional lisp-name &key (getlasterror 'getlasterror))
			   return-type error-convention &rest args)
  "Like CFFI's defcfun, except it adds automatic error handling. The ERROR-CONVENTION determines
which of Win32's many error-handling conventions to use to convert the function's return code into
an exception of type WINDOWS:WINDOWS-ERROR. See the source code to INSERT-ERROR-HANDLER to see
which error conventions are supported.

The :GETLASTERROR key allows you to specify a function to use instead of GetLastError. For example,
if you're defining a Winsock function, you would specify WSAGetLastError, and then WSAGetLastError would
be used instead of GetLastError to get the error code."
  (let ((lisp-name (or lisp-name
		       (car (multiple-value-list (cffi::parse-name-and-options name))))))
    `(defun ,lisp-name ,(mapcar #'car args)
       ,(let ((retval (gensym)))
	     `(let ((,retval (cffi:foreign-funcall ,name ,@(apply #'append (mapcar #'reverse args)) ,return-type)))
		,(insert-error-handler name retval error-convention getlasterror)
		,retval)))))

(defmacro with-cstring ((var lisp-string) &body body)
  `(let ((,var (convert-to-cstring ,lisp-string)))
    (unwind-protect
	 (progn ,@body)
      (free-cstring ,var))))

(defmacro with-foreign-string ((var lisp-string) &body body)
  `(let ((,var (convert-to-foreign-string ,lisp-string)))
    (unwind-protect
	 (progn ,@body)
      (free-foreign-object ,var))))

(defmacro with-cstring* (var-defs &body body)
  (let ((result (cons 'progn body)))
    (loop for def in (reverse var-defs) do
	 (setf result `(with-cstring ,def ,result)))
    result))

(defmacro with-foreign-string* (var-defs &body body)
  (let ((result (cons 'progn body)))
    (loop for def in (reverse var-defs) do
	 (setf result `(with-foreign-string ,def ,result)))
    result))

(defcfun "GetCurrentProcessId" :ulong)
(defwinfun ("GetCurrentProcess") :ulong :null-pointer)

(defwinfun ("TerminateProcess") :int :boolean
  (hprocess :ulong)
  (exit-code :uint))

(defun suicide ()
  ;; For some strange reason, (terminateprocess (getcurrentprocess)) doesn't
  ;; always result in the current process terminating. Sometimes you have to
  ;; do it twice.
  (loop do (terminateprocess (getcurrentprocess) 0)))

(defwinfun ("MessageBoxA") :int :boolean
  (wnd :unsigned-int)
  (msg :string)
  (title :string)
  (buttons :unsigned-int))

(defwinfun ("MessageBoxW") :int :boolean
  (wnd :unsigned-int)
  (msg :wstring)
  (title :wstring)
  (buttons :unsigned-int))

(defwinfun ("shutdown" nil :getlasterror wsagetlasterror) :int :unix
  (sock :int)
  (how :int))

(defcfun "free" :void
  (addr :pointer))

(defconstant +file-read-data+ #x1)
(defconstant +file-list-directory+ #x1)
(defconstant +file-write-data+ #x2)
(defconstant +file-add-file+ #x2)
(defconstant +file-read-ea+ #x8)
(defconstant +file-write-ea+ #x10)
(defconstant +file-read-attributes+ #x80)
(defconstant +file-write-attributes+ #x100)
(defconstant +standard-rights-required+ #xf0000)
(defconstant +synchronize+ #x100000)
(defconstant +file-all-access+ (logior +standard-rights-required+ +synchronize+ #x1ff))
(defconstant +file-share-read+ #x1)
(defconstant +file-share-write+ #x2)
(defconstant +file-share-delete+ #x4)

(defconstant +create-new+ 1)
(defconstant +create-always+ 2)
(defconstant +open-existing+ 3)
(defconstant +open-always+ 4)
(defconstant +truncate-existing+ 5)


(defwinfun ("CreateFileA") :ulong :invalid-handle-value 
  (file-name :string)
  (desired-access :ulong)
  (share-mode :ulong)
  (security-attributes :pointer)
  (creation-disposition :ulong)
  (flags-and-attributes :ulong)
  (template-file-handle :ulong))

(defwinfun ("CreateFileW") :ulong :invalid-handle-value 
  (file-name :wstring)
  (desired-access :ulong)
  (share-mode :ulong)
  (security-attributes :pointer)
  (creation-disposition :ulong)
  (flags-and-attributes :ulong)
  (template-file-handle :ulong))

(defconstant +sd-receive+ 0)
(defconstant +sd-send+ 1)
(defconstant +sd-both+ 2)

(defun message-box (message &key (title "Error"))
  (messageboxw 0 message title 0))


(defconstant +test-cant-open-run-key+ 1)
(defconstant +test-no-value-exists+ 2)
(defconstant +test-value-exists+ 0)

(defcfun "ShellExecuteA" :int
  (hwnd :pointer)
  (action :string)
  (file :string)
  (parameters :string)
  (directory :string)
  (show-cmd :int))

(defun open-file (name)
  (shellexecutea (cffi-sys:null-pointer) "open" name "" (cffi-sys:null-pointer) 1))

(defun get-c-string (ptr)
  (let ((temp (loop for ix from 0
		 for ch = (mem-ref ptr :char ix)
		 until (= ch 0)
		 collect (code-char ch))))
    (make-array (length temp) :element-type 'character :initial-contents temp)))

(defcfun "GetLastError" :ulong)
(defcfun "WSAGetLastError" :ulong)

(defcfun ("LocalFree") (:pointer :char)
  (hmem (:pointer :char)))

;; It's much more difficult to use :wstring
;; for output string arguments.

(defcfun "FormatMessageA" :ulong
  (flags :ulong)
  (source (:pointer :char))
  (message-id :ulong)
  (language-id :ulong)
  (buffer (:pointer (:pointer :char)))
  (size :ulong)
  (varargs (:pointer :char)))

(defwinfun ("RegisterClassW") :pointer :null-pointer
  (window-class (:pointer window-class)))

;; FIXME! :ULONG is not pointer-sized on 64-bit hardware! That
;;        means we can't use integers as handles. We'll need to
;;        use pointers!
(defwinfun ("CreateWindowExW" createwindow) :ulong :null-pointer
  (ex-style :ulong)
  (class-name :wstring)
  (window-name :wstring)
  (style :ulong)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (parent :ulong)
  (menu :ulong)
  (instance :ulong)
  (param :pointer))


(defconstant +format-message-allocate-buffer+ #x100)
(defconstant +format-message-from-system+ #x1000)

(define-condition windows-error (simple-error) ((errcode :initarg :errcode)))

(defvar *variable-types* nil)

(defmacro with-foreign-objects* (bindings &body body)
  (let ((gensyms (loop for b in bindings collect (gensym))))
    `(let ,(loop for (name type) in bindings
		 for sym in gensyms collect (list sym type))
       (with-foreign-objects ,(loop for (name type) in bindings
				   for sym in gensyms collect (list name sym))
	 (let* ,(loop for value in (mapcar #'car bindings)
		       for type in gensyms
		   collect `(*variable-types* (cons (cons ,value ,type) *variable-types*)))
	   ,@body)))))

(defmacro with-foreign-types (bindings &body body)
  (let ((gensyms (loop for b in bindings collect (gensym))))
    `(let ,(loop for (name type) in bindings
		 for sym in gensyms collect (list sym type))
       (let* ,(loop for value in (mapcar #'car bindings)
		 for type in gensyms
		 collect `(*variable-types* (cons (cons ,value ,type) *variable-types*)))
	 ,@body))))

(defun get-value-type (c-struct)
  (or (cdr (assoc c-struct *variable-types*))
      (error "No type associated with ~a using a WITH-FOREIGN-OBJECTS* form.
WITH-FOREIGN-OBJECTS* objects are only valid within the dynamic extent of the WITH-FOREIGN-OBJECTS* form." c-struct)))

(defun get-foreign-slot-aref (c-struct slot ix)
  (getf (mem-aref c-struct (get-value-type c-struct) ix) slot))

(defun foreign-slot-value* (c-struct slot &optional (index 0))
  (if (= index 0)
      (foreign-slot-value c-struct (get-value-type c-struct) slot)
      (get-foreign-slot-aref c-struct slot index)))

(defun set-foreign-slot-aref (new-value c-struct slot ix)
  (let ((plist (mem-aref c-struct (get-value-type c-struct) ix)))
    (setf (getf plist slot) new-value)
    (setf (mem-aref c-struct (get-value-type c-struct) ix) plist)))

(defun (setf foreign-slot-value*) (new-value c-struct slot &optional (index 0))
  (if (= index 0)
      (setf (foreign-slot-value c-struct (get-value-type c-struct) slot) new-value)
      (set-foreign-slot-aref new-value c-struct slot index)))

(defun mem-aref* (c-pointer &optional (index 0))
    (mem-aref c-pointer (get-value-type c-pointer) index))

(defun (setf mem-aref*) (new-value c-pointer &optional (index 0))
  (setf (mem-aref c-pointer (get-value-type c-pointer) index) new-value))

(defun raise-windows-error (wincall &optional (errcode (getlasterror)))
  (with-foreign-objects ((buffer '(:pointer (:pointer :char))))
    (formatmessagea (logior +format-message-allocate-buffer+ +format-message-from-system+)
		    (null-pointer) errcode 0 buffer 1 (null-pointer))
    (unwind-protect
	 (error 'windows-error :errcode errcode :format-control "~a: ~a (Windows error code: 0x~x)"
		:format-arguments `(,wincall ,(if (null-pointer-p (mem-ref buffer '(:pointer :char)))
						  (format nil "Failed to look up error code 0x~x~%" errcode)
						  (get-c-string (mem-ref buffer '(:pointer :char)))) ,errcode))
      (localfree buffer))))

(defmacro with-converted-object (name type lisp-value &body body)
  (let ((alloc-params (gensym)))
    `(multiple-value-bind (,name ,alloc-params)
	 (convert-to-foreign ,lisp-value ,type)
       (unwind-protect
	    (progn ,@body)
	 (free-converted-object ,name ,type ,alloc-params)))))

(defmacro with-converted-objects (bindings &body body)
  (loop with result = (cons 'progn body)
       for (name type lisp-value) in (reverse bindings) do
       (setf result `(with-converted-object ,name ,type ,lisp-value ,result))
       finally (return result)))

(defcfun "memset" :pointer
    (dest :pointer)
    (value :int)
    (bytes :ulong))

(defcfun "getenv" :string
  (name :string))

(defwinfun ("GetModuleFileNameA") :ulong :null-pointer
  (hmodule (:pointer :ulong))
  (buffer (:string))
  (size (:ulong)))

(defun my-name (&optional (size 128))
  (with-converted-objects ((buffer :string (make-string size :initial-element #\Nul)))
    (let ((code (getmodulefilenamea (null-pointer) buffer size)))
      (cond ((= code size)
	     (my-name (* size 2)))
	    ((not (= (getlasterror) 0))
	     (raise-windows-error "GetModuleFileNameA"))
	    (t (convert-from-foreign buffer :string))))))

(defun c-buffer->byte-vector (buffer length)
  (let ((retval (make-array length :element-type '(unsigned-byte 8))))
    (loop for ix from 0 below length
	 do (setf (aref retval ix)
	       (mem-ref buffer :uint8 ix)))
    retval))

(defun make-foreign-object (type)
  "Creates a foreign object using the C function malloc(). Must be freed using free()."
  (let ((pointer (eval `(foreign-funcall "malloc" :ulong ,(foreign-type-size type) (:pointer ,type)))))
    (memset pointer 0 (foreign-type-size type))
    pointer))

(defun malloc (bytes)
  (let ((ptr (foreign-funcall "malloc" :long  bytes :pointer)))
    (if (null-pointer-p ptr)
	(raise-windows-error "malloc")
	ptr)))

(defmacro with-byte-buffer ((var &key source-vector length) &body body)
  "Allocates a byte buffer using malloc(), binds it to VAR, evaluates BODY, and
then frees the buffer when control leaves the BODY.

&KEY parameters:

  :SOURCE-VECTOR - If non-NIL, the bytes from it will be copied into the
                   malloc'd buffer before continuing.
  
  :LENGTH        - If provided and :SOURCE-BINDING is not provided,
                   then the buffer will be :LENGTH bytes long.
"
  (let ((source-binding (gensym))
	(length-binding (gensym)))
    `(let* ((,source-binding ,source-vector)
	    (,length-binding ,length)
	    (,var (malloc (if ,source-binding 
			      (length ,source-binding)
			      ,length-binding))))
       (unwind-protect
	    (progn
	      (when ,source-binding
		(loop for byte across ,source-binding
		   for ix from 0 do
		     (setf (mem-aref ,var :uchar ix) byte)))
	      ,@body)
	 (free ,var)))))

(defmacro with-byte-buffers (bindings &body body)
  "Like WITH-BYTE-BUFFERS, but each form in BINDINGS can
have the form (VAR &KEY SOURCE-VECTOR LENGTH), allowing
multiple bindings."
  (setf body `(progn ,@body))
  (loop for binding in bindings do
       (setf body `(with-byte-buffer ,binding ,body)))
  body)

(defcstruct STARTUPINFO-A
  "STARTUPINFOA"
  (cb :ulong)
  (reserved (:pointer :char))
  (desktop (:pointer :char))
  (title (:pointer :char))
  (x :ulong)
  (y :ulong)
  (x-size :ulong)
  (y-size :ulong)
  (x-count-chars :ulong)
  (y-count-chars :ulong)
  (fill-attribute :ulong)
  (flags :ulong)
  (show-window :ushort)
  (len-reserved-2 :ushort)
  (reserved-2 (:pointer :uchar))
  (h-std-input :pointer)
  (h-std-output :pointer)
  (h-std-error :pointer))


(defcstruct process-information
  "Windows struct PROCESS_INFORMATION"
  (h-process :ulong)
  (h-thread :ulong)
  (process-id :ulong)
  (thread-id :ulong))

(defwinfun ("CreateProcessA") :int :boolean
  (filename :string)
  (command-line :string)
  (process-attributes :pointer)
  (thread-attributes :pointer)
  (inherit-handles :int)
  (creation-flags :ulong)
  (environment :pointer)
  (current-directory :string)
  (startup-info (:pointer (:struct startupinfo-a)))
  (process-information (:pointer (:struct process-information))))

(defun create-process (name startup-info process-info &key (command-line name) (process-attributes (null-pointer)) (thread-attributes (null-pointer))
			 (inherit-handles 0) (creation-flags 0) (environment (null-pointer)) (current-directory (null-pointer)))
  (when (= 0
	 (createprocessa name command-line process-attributes thread-attributes inherit-handles creation-flags environment current-directory
			 startup-info process-info))
    (raise-windows-error 'createprocessa)))

(defwinfun ("CloseHandle") :int :boolean (handle :ulong))

(defun start-program (name &rest args)
  (with-foreign-objects* ((startup-info '(:struct startupinfo-a))
			  (process-info '(:struct process-information)))
    (memset startup-info 0 (foreign-type-size '(:struct startupinfo-a)))
    (memset process-info 0 (foreign-type-size '(:struct process-information)))
    (create-process name startup-info process-info
		    :command-line (if args
					   (format nil "~{~a~^ ~}" (cons name args))
					   name))
    (wincall (closehandle (foreign-slot-value* process-info 'h-process)))
    (wincall (closehandle (foreign-slot-value* process-info 'h-thread)))))

