(defpackage :clipboard
  (:use :common-lisp :windows :utils)
  (:export :set-clip-string :get-clip-string :windows)
  (:shadow :memcpy))

(in-package :clipboard)

(defwinfun ("GetClipboardData" get-clip) :pointer :null-pointer
  (uformat  :unsigned-int))

(defwinfun ("OpenClipboard" open-clip) :int :boolean
  (hOwner  :unsigned-int))

(defwinfun ("CloseClipboard" close-clip) :int :boolean
  (hOwner  :unsigned-int))

(defwinfun ("EmptyClipboard" empty-clip) :int :boolean)


(defwinfun ("SetClipboardData" set-clip) :pointer :null-pointer
  (data  :unsigned-int)
  (format :pointer))

(defwinfun ("GlobalAlloc" global-alloc) :pointer :null-pointer
  (flags  :unsigned-int)
  (numbytes :unsigned-int))


(defwinfun ("GlobalLock" global-lock) :pointer :null-pointer
  (typ  :pointer))

(defwinfun ("GlobalLock" global-lock-string) :string :null-pointer
  (typ  :pointer))

(defwinfun ("GlobalUnlock" global-unlock) :int :boolean/check-last-error
  (typ  :pointer))

(defwinfun ("GlobalFree" global-free) :pointer :non-null-pointer
  (hmem :pointer))

(cffi:defcfun ("memcpy" memcpy) :int
  (dest  :pointer)
  (src :string) 
  (coun :unsigned-int))

(defun get-clip-string ()
  (open-clip 0)
  (let* ((h (get-clip 1))
	 (s (global-lock-string h)))
    (global-unlock h)
    (close-clip 0)
    s))

(defun set-clip-string (s)
  "Seems to work locally, but can't paste the data to other programs with Ctrl+V."
  (let* ((slen (+ 1 (length s)))
	 (newh (global-alloc 8194 slen))
	 (newp (global-lock newh)))
    
    (memcpy newp s (+ 1 slen))
    (global-unlock newh)
    (open-clip 0)
    (unwind-protect
	 (progn
	   (empty-clip)
	   (handler-case*
	    (set-clip 1 newh)
	    (windows-error (err)
	       :before-unwind (global-free newh))))
      (close-clip 0))))

(defun empty-clipboard ()
  (open-clip 0)
  (unwind-protect
       (empty-clip)
    (close-clip 0)))
