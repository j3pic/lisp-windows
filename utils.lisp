(cl:defpackage utils
  (:use :common-lisp :windows :sb-thread :closer-mop)
  (:export showf enum 
	   starts-with ends-with 
	   string->list list->string rcase maybe-subseq handler-case* symbol->string package-symbols list-packages split
	   package-ls encode-ip-addr do-restart let*/debug defvalues defvalues* delete-metadata suck-stream suck-file subst* recursive-map recursive-find
	   remove-plist-keys slot-path struct-like-defclass class-slot-names bind-class-slots list->vector html->lhtml lhtml->html it aif
	   awhen aunless acond date/time-as-string with-gensyms unread *rcase-test* foreach vector-nconcat with-try-again try-again
	   array->simple-vector debug-if ignore-warnings :replace-string :slice))

(in-package :utils)

(defun list->vector (list &key (element-type t) adjustable fill-pointer displaced-to
			    displaced-index-offset)
  (make-array (length list) :element-type element-type :adjustable adjustable
	      :initial-contents list
	      :fill-pointer fill-pointer :displaced-to displaced-to
	      :displaced-index-offset displaced-index-offset))

(defmacro defvalues ((&rest vars) value-form)
  (let ((gensyms (loop for v in vars collect (gensym))))
    `(multiple-value-bind ,gensyms ,value-form
       ,@(loop for v in vars
	    for g in gensyms
	    collect `(defparameter ,v ,g)))))

(defmacro defvalues* ((&rest vars) (values &rest forms))
  (unless (eq values 'values)
    (error "Second form in DEFVALUES* must be a VALUES form"))
  `(progn
     ,@(loop for v in vars
	  for f in forms
	  collect `(defparameter ,v ,f))))

(defun encode-ip-addr (ip-address)
  "Encodes an IP address in binary."
  (let ((result (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for part in (split ip-address ".")
       for ix from 0 do
	 (setf (aref result ix) (parse-integer part)))
    result))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun mapapply (func list)
    "Maps over a list, treating each element in the list as an argument list for FUNC."
    (mapcar (lambda (arglist)
	      (apply func arglist)) list))
  
  (defun expand-handler-bind (type lambda-list &key before-unwind after-unwind)
    "Helper for the HANDLER-CASE* macro. This one creates the HANDLER-BIND lambdas out of the :BEFORE-UNWIND form."
    (declare (ignorable after-unwind))
    (when before-unwind
      (when (null lambda-list)
	(error ":BEFORE-UNWIND must bind the condtion to a variable."))
      `(,type (lambda ,lambda-list (declare (ignorable ,@lambda-list)) ,before-unwind))))
  
  (defun expand-handler-case (type lambda-list &key before-unwind after-unwind)
    "Helper for the HANDLER-CASE* macro. This one creates the HANDLER-CASE handler-clauses out of the :AFTER-UNWIND form"
    (declare (ignorable before-unwind))
    (if after-unwind
	`(,type ,lambda-list (declare (ignorable ,@lambda-list)) ,after-unwind))))

(defmacro handler-case* (form &rest cases)
  "Like HANDLER-CASE and HANDLER-BIND rolled into one. Example usage:
 
      (handler-case* (restart-case (error \"ZOMG! ERROR!\")
                         (fuck-it () 'ignored))
         (t (condition)
            :before-unwind
               (progn (format t \"An error occurred Ignore it (y/N)? \")
                      (if (eq (read) 'y)
                          (invoke-restart 'fuck-it)))
            :after-unwind
               (format t \"You just couldn't fucking ignore it, could you?~%\")))
     
:before-unwind is, of course, executed before the stack unrolls, so you can
invoke restarts from there. If no restart is invoked, the error falls through
to the :after-unwind case, where you can handle it like a regular handler-case.

If no :after-unwind form is provided and no restart is invoked, the condition is not trapped."
  `(handler-case
       (handler-bind ,(remove nil (mapapply #'expand-handler-bind cases))
	 ,form)
     ,@(remove nil (mapapply #'expand-handler-case cases))))

(defun showf (format &rest args)
  "Format the arguments and then show it in a Windows dialog."
  (message-box (apply #'format
		      (list* nil format args))))

(defmacro enum (name &rest identifiers)
  "Like C/C++ enums. Defines a set of constants with successively higher integer values.
You can specify the value of a constant, which affects incrementation. Example:

   (enum *numbers* +one+ +two+ (+twenty+ 20) +twenty-one+)

The NAME argument will be defined as a constant containing an alist mapping the names of
the constants to their values.

"
  `(progn
	,@(loop for x from 0
	   for id in identifiers
	   if (consp id)
	   do (setf x (second id))
	   and collect `(defconstant ,(first id) ,x)
	   else collect `(defconstant ,id ,x))
	(defconstant ,name (list ,@(loop for id-or-pair in identifiers collect
					(let ((id (if (consp id-or-pair) (car id-or-pair) id-or-pair)))
					  `(list ',id ,id)))))))

(defun maybe-aref (arr ix)
  "Dereferences an array if it's within bounds, otherwise returns NIL"
  (if (>= ix (length arr))
      nil
      (aref arr ix)))

(defmacro foreach (defs &body body)
  "Special FOREACH designed to iterate over arbitrary sequences. Mainly for implementing ELEMENTS="
  (let ((indices (loop for d in defs collect (list (gensym) 0)))
	(lists (loop for (var list) in defs
		  collect (list (gensym) list))))
    `(let ,(append indices lists)
       (do ,(loop for (var list*) in defs
	       for (list-name list) in lists
	       for (ix zero) in indices
	       collect `(,var (if (listp ,list-name)
				  (car ,list-name)
				  (maybe-aref ,list-name ,ix))
			      (progn
				(if (listp ,list-name)
				    (setf ,list-name (cdr ,list-name))
				    (setf ,ix (1+ ,ix)))
				(if (listp ,list-name)
				    (car ,list-name)
				    (maybe-aref ,list-name ,ix)))))
	   ((and . ,(loop for (var list-val) in defs
		       for (list-name l) in lists
		       for (ix zero) in indices
		       collect `(if (listp ,list-name)
				    (not ,list-name)
				    (>= ,ix (length ,list-name))))) nil)
	 ,@body))))

(defun elements= (seq1 seq2)
  "Compare the elements of two sequences, while ignoring the types of each sequence."
  (block elements-block
    (foreach ((e1 seq1)
	      (e2 seq2))
      (unless (equalp e1 e2)
	(return-from elements-block nil)))
    t))

(defun delete-metadata (filename)
  (handler-case*
   (if (eq (type-of filename) 'pathname)
       (delete-metadata (format nil "~a" filename))
       (progn
	 (delete-file (concatenate 'string filename ":Zone.Identifier"))
	 :deleted))
   (t ()
      :after-unwind nil)))

(defun starts-with (start whole &optional (offset 0))
  "Returns T if WHOLE begins with START. The two sequences need not be the same type; only the elements are important."
  (declare (type sequence start whole))
  (and (>= (length whole) (length start))
       (elements= start (subseq whole offset (length start)))))

(defun ends-with (end whole)
  "Returns T if WHOLE ends with END. The two sequences need not be the same type; only the elements are important."
  (declare (type sequence end whole))
  (let ((wlength (length whole))
	(elength (length end)))
    (and (>= wlength elength)
	 (elements= (subseq whole (- wlength elength))
		    end))))

(defun maybe-subseq (seq start end)
  "Returns (subseq seq start end) if it can be done without raising an error, otherwise returns NIL."
  (let ((l (length seq)))
    (and (<= 0 start end l)
	 (subseq seq start end))))

(defun string->list (string)
  (loop for ch across string collect ch))

(defun list->string (list)
  (with-output-to-string (*standard-output*)
    (loop for ch in list do (write-char ch))))

(defun split (string delim)
  "PERL-style split function. DELIM is a sequence."
  (declare (type string string)
	   (type string delim))
  (let ((token nil))
    (append
     (loop for rest on (string->list string)
	when (starts-with (string->list delim) rest)
	collect (list->string (reverse token)) and
	do (loop repeat (- (length delim) 1)
	      do (setf rest (cdr rest)))
	  (setf token nil)
	else 
	do (push (car rest) token)) (list (list->string (reverse token))))))

(defvar *rcase-test* #'eql)

(defmacro rcase (keyform &rest cases)
  "Runtime case. The case labels are evaluated at runtime, so they can be variables."
  `(let ((it ,keyform))
     (cond
       ,@(loop for (value . body) in cases
	    collect (cond ((or (equalp value t)
			       (eq value 'otherwise))
			   (cons t body))
			  ((consp value)
			   (cons `(or ,@(loop for v in value collect
					     `(funcall *rcase-test* ,v it))) body))
			  (t (cons `(funcall *rcase-test* ,value it) body)))))))

(defun symbol->string (symbol)
  "Convert a symbol to a string. Includes the package name with the correct number of
   colons if PRINT would include it.
   Does not include pipe escapes for symbols that are not otherwise readable."
  (let ((printed (with-output-to-string (*standard-output*)
		   (print symbol))))
    (if (find #\: printed)
	(format nil "~a~a~a" (package-name (symbol-package symbol))
		(make-array (loop for ch across printed count
				 (eql ch #\:)) :element-type 'character
				 :initial-element #\:)
		(symbol-name symbol))
	(symbol-name symbol))))

(defun package-symbols (package)
  "Returns a list of all symbols in the specified PACKAGE."
  (with-package-iterator (get-next (list package) :internal :external)
    (loop for next = (multiple-value-bind (fuck name three four) (get-next) name)
       while next collect next)))

(defun package-ls (package)
  "Prints all the symbols in PACKAGE."
  (mapcar #'print (package-symbols package))
  t)

(defun list-packages ()
  "Returns a list of all package names as keywords."
  (loop for pkg in (list-all-packages)
     collect (intern (package-name pkg) :keyword)))

(defun do-restart (n)
  "Invoke the Nth restart in the list generated by (compute-restarts), counting from 1."
  (invoke-restart (nth (- n 1) (compute-restarts))))

(defmacro let*/debug (defs &body body)
  `(let* ,(loop for (var val) in defs
	     collect `(,var (progn
			      (message "Receiving ~a = " ',var)
			      (print ,val))))
     ,@body))

(defun suck-stream (stream &key (reader 'read-char))
  (let ((result nil))
    (handler-case*
     (loop do (push (restart-case
			(ecase reader
			  ((read-char) (read-char stream))
			  ((read-byte) (read-byte stream)))
		      (switch-reader ()
			:report "Switch between binary and text readers."
			(setf reader (ecase reader
				       ((read-char) 'read-byte)
				       ((read-byte) 'read-char)))
			(ecase reader
			  ((read-char) (read-char stream))
			  ((read-byte) (read-byte stream)))))
		      result))
     (end-of-file ()
		  :after-unwind
		  (list->vector (nreverse result) :element-type (ecase reader
						       ((read-char) 'character)
						       ((read-byte) '(unsigned-byte 8))))))))


(defun suck-file (path &key (element-type '(unsigned-byte 8)))
  (with-open-file (in path :direction :input :element-type element-type)
    (suck-stream in :reader 'read-byte)))

(defun recursive-map (proc tree &key walk-quasiunquote)
  (labels ((do-process (processed item)
	   (cond ((not (equalp processed item))
		  processed)
		 ((consp item)
		  (recursive-map proc item :walk-quasiunquote walk-quasiunquote))
		 ((and walk-quasiunquote (sb-impl::comma-p item))
		  (sb-int:unquote (do-process (funcall proc (sb-int:comma-expr item)) (sb-int:comma-expr item))))
		 (t processed))))
  (loop for item in tree
     collect (do-process (funcall proc item) item))))

(defun recursive-find/collect (proc tree)
  (loop for item in tree
       if (funcall proc item) collect item
       else if (consp tree) append (recursive-find/collect proc tree)))

(defun recursive-find (item tree &key walk-quasiunquote (test #'eq))
  (recursive-map (lambda (node)
		   (if (funcall test node item)
		       (return-from recursive-find t)))
		 tree
		 :walk-quasiunquote walk-quasiunquote))
		       

(defun subst* (bindings form &key (test #'eql))
  (recursive-map
   (lambda (node)
     (block mapper
       (loop for (var val) in bindings
	  when (funcall test node var) do (return-from mapper val))
       node)) form :walk-quasiunquote t))

(defun remove-plist-keys (plist &rest keys)
  (loop for (key value) on plist by #'cddr
       unless (member key keys)
         collect key
         and collect value))

(defun slot-path (obj &rest members)
  (rcase (length members)
    (0 (error "SLOT-PATH requires at least one MEMBER argument."))
    (1 (slot-value obj (car members)))
    (otherwise
      (apply #'slot-path (cons (slot-value obj (car members)) (cdr members))))))

(defun expand-struct-like-defclass-slot (class-name name &optional default-value &key type)
  `(,name :accessor ,(intern (format nil "~a-~a" class-name name))
	  :initform ,default-value
	  :initarg ,(intern (symbol-name name) :keyword)
	  ,@(if type `(:type ,type))))

(defmacro struct-like-defclass (name superclasses &rest slot-defs)
  "An extremely simplified version of DEFCLASS. Written because I needed to be able
to list the slots of a certain struct."
  `(progn (defparameter ,name 
	    (defclass ,name ,superclasses 
	      ,(loop for def in slot-defs collect
		    (if (listp def)
			(apply #'expand-struct-like-defclass-slot (cons name def))
			(expand-struct-like-defclass-slot name def)))))
	  (finalize-inheritance ,name)))

(defun class-slot-names (class)
    (mapcar #'slot-definition-name (class-slots class)))

(defmacro bind-class-slots (class instance &body body)
  (let ((instance-value (gensym)))
    `(let* ,(cons `(,instance-value ,instance)
		 (loop for name in (class-slot-names (eval class)) collect
		      `(,name (slot-value ,instance ',name))))
       (declare (ignorable ,instance-value))
       ,@body)))

(defun html->lhtml (string)
  (closure-html:parse string (chtml:make-lhtml-builder)))


(defun lhtml->html (lhtml)
  (closure-html:serialize-lhtml lhtml (chtml:make-string-sink)))


(defmacro aif (cond-form true-form &optional false-form)
  `(let ((it ,cond-form))
     (if it ,true-form ,false-form)))

(defmacro awhen (cond-form &body body)
  `(aif ,cond-form
	(progn ,@body)))

(defmacro aunless (cond-form &body body)
  `(let ((it ,cond-form))
     (unless it
       ,@body)))

(defmacro acond (&body forms)
  (let ((result nil))
    (loop for (condition . body) in (reverse forms) do
	 (setf result `(aif ,condition
			    (progn ,@body)
			    ,result))
	 finally (return result))))

(defmacro invoking-available-restarts (restarts &body body)
  `(handler-case*
    (progn ,@body)
    (t (exn)
       :before-unwind
       (loop for r in ',restarts do
	    (aif (find-restart r)
		 (invoke-restart it))))))

(defmacro do-restarts (restarts &body body)
  `(invoking-available-restarts ,restarts ,@body))

(defun date/time-as-string ()
  "From the Common Lisp Cookbook."
  (let ((day-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
	(get-decoded-time)
      (format nil "~2,'0d:~2,'0d:~2,'0d on ~a, ~d/~2,'0d/~d (GMT~@d)"
	      hour
	      minute
	      second
	      (nth day-of-week day-names)
	      month
	      date
	      year
	      (- tz)))))

(defmacro with-gensyms (names &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro foreach (var list &body body)
  "Loop across either a list or an array without having to know which ahead of time."
  (let ((list* (gensym)))
    `(let ((,list* ,list))
       (if (arrayp ,list*)
	   (loop for ,var across ,list* do
		(progn  ,@body))
	   (loop for ,var in ,list* do
		(progn ,@body))))))

(defun vector-nconcat (vector &rest lists-and-vectors)
  "Mutatively append to a vector using VECTOR-PUSH-EXTEND in a loop. Arguments can be a mixture of strings, lists, and arrays."
  (declare (type array vector))
  (loop for new-data in lists-and-vectors
       do (foreach elem new-data
	    (vector-push-extend elem vector)))
  vector)

(defmacro with-try-again (&body body)
  (let ((block-name (gensym))
	(retry (gensym)))
    `(block ,block-name
       (tagbody
	  ,retry
	  (restart-case
	      (return-from ,block-name
		(progn ,@body))
	    (try-again () :report "Attempt to evaluate the WITH-TRY-AGAIN form again."
	       (go ,retry)))))))

(defun array->simple-vector (arr)
  (concatenate 'vector arr))

(defmacro debug-if (condition  true-expr false-expr)
  `(if (progn 
	 (print '(:testing ,condition))
	 ,condition)
       (progn
	 (print '(:evaluating ,true-expr))
	 ,true-expr)
	 (print '(:evaluating ,false-expr)
		,false-expr)))

(defun hash->alist (hash-table)
  (let ((result nil))
    (maphash (lambda (k v)
	       (push (cons k v) result)) hash-table)
    result))

(defmacro ignore-warnings (&body body)
  `(handler-case*
    (progn ,@body)
    (t (exn)
       :before-unwind (muffle-warning))))

(defun slice (seq start &optional (end (length seq)))
  (let ((seq-type (type-of seq)))
    (when (and (listp seq-type)
	     (eq (car seq-type) 'simple-array))
      (setf seq-type (list (car seq-type) (cadr seq-type) '(*))))
    (cond ((> start (length seq))
	   (return-from slice
	     (coerce nil seq-type)))
	  ((> end (length seq))
	   (setf end (length seq))))
    (subseq seq start end)))

(defun replace-string (what with string)
  (with-output-to-string (out)
    (loop for ix from 0 below (length string)
	 with with-ix = 0
	 with replacing = nil
	 do (cond ((string= what (slice string ix (+ ix (length what))))
		   (write-string with out)
		   (incf ix (- (length what) 1)))
		  (t (write-char (aref string ix) out))))))
