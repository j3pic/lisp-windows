(defpackage :winprocess (:use :windows :common-lisp :utils :cffi)
	    (:export get-process-list))

(in-package :winprocess)

(defconstant +th32cs-snapprocess+ #x2)
(defconstant +invalid-handle-value+ -1)
(defconstant +max-path+ 260)
(defconstant +error-no-more-files+ 18)

(defstruct process
  (pid 0 :type fixnum)
  (ppid 0 :type fixnum)
  (name "" :type string))


(defcstruct processentry32
  (dw-size :int)
  (cnt-usage :int)
  (th32-processid :int)
  (th32-defaultheapid :int)
  (th32-moduleid :int)
  (cnt-threads :int)
  (th32-parentprocessid :int)
  (pc-priclassbase :int)
  (dw-flags :int)
  (sz-exe-file :char :count 260))

(defwinfun ("CreateToolhelp32Snapshot") :int :invalid-handle-value
  (flags :int)
  (th32pid :int))

(defwinfun ("Process32First") :int :boolean
  (snapshot :int)
  (process-entry (:pointer (:struct processentry32))))

(defcfun "Process32Next" :int
  (snapshot :int)
  (process-entry (:pointer (:struct processentry32))))

(add-breakpoint-check "Process32Next")

(defun make-result (entry)
  (make-process :pid (foreign-slot-value* entry 'th32-processid)
		:ppid (foreign-slot-value* entry 'th32-parentprocessid)
		:name (get-c-string (foreign-slot-value* entry 'sz-exe-file))))

(defun get-process-list ()
  (let ((snapshot (createtoolhelp32snapshot +th32cs-snapprocess+ 0))
	(result nil))
    (when (= snapshot +invalid-handle-value+)
      (raise-windows-error "CreateToolhelp32Snapshot"))
    (unwind-protect
	 (with-foreign-objects* ((a-process '(:struct processentry32)))
	   (setf (foreign-slot-value* a-process 'dw-size)
		 (foreign-type-size '(:struct processentry32)))
	   (when (= (process32first snapshot a-process) 0)
	     (raise-windows-error "Process32First"))
	   (push (make-result a-process) result)
	   (loop until (= (process32next snapshot a-process) 0)
		do (push (make-result a-process) result))
	   result)
      (closehandle snapshot))))

(defwinfun ("OpenProcess") :ulong :null-pointer
  (desired-access :ulong)
  (inherit-handle :int)
  (process-id :ulong))

(defun kill-process-by-pid (pid)
  (let ((handle (openprocess 1 0 pid)))
    (when (= handle 0)
      (raise-windows-error "OpenProcess"))
    (unwind-protect
	 (windows::terminateprocess handle 0)
      (closehandle handle))))

(defun kill-process (&key pid name)
  (when pid (kill-process-by-pid pid))
  (when name
    (loop for pid in (mapcar #'process-pid (remove-if-not
					    (lambda (item)
					      (string= item name))
					    (get-process-list) :key #'process-name))
	 do (kill-process-by-pid pid))))
