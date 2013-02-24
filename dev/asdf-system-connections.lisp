(in-package #:asdf)

(eval-when (:compile-toplevel :load-toplevel)
  (export '(map-system-connections defsystem-connection))

  (when (find-package '#:asdf/interface)
    (export '(map-system-connections defsystem-connection) '#:asdf/interface)))

;;; ---------------------------------------------------------------------------
;;; not particularly rich person's system interconnection facility
;;; ---------------------------------------------------------------------------

(defclass system-connection (system)
  ((systems-required :initarg :systems-required :reader systems-required)))

;;; ---------------------------------------------------------------------------

(defun map-system-connections (fn)
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (typep (cdr v) 'system-connection)
               (funcall fn (cdr v))))
           *defined-systems*))

;;; ---------------------------------------------------------------------------

(defmacro defsystem-connection (name &body options)
  (let ((requires (getf options :requires))
        (class (getf options :class 'system-connection)))
    (remf options :requires)
    (remf options :class)
    `(progn
       (defsystem ,name
         :class ,class
         :depends-on ,requires
         :systems-required ,requires
         ,@options)
       (values ',name))))

;;; ---------------------------------------------------------------------------

(defun load-connected-systems ()
  (map-system-connections
   (lambda (connection)
     (when (and (required-systems-loaded-p connection)
                (not (system-loaded-p (component-name connection))))
       (asdf:load-system (component-name connection))))))

(defun required-systems-loaded-p (connection)
  (every #'system-loaded-p (systems-required connection)))

;;; ---------------------------------------------------------------------------

(defun system-loaded-p (system-name)
  (let ((it (cdr (system-registered-p system-name))))
    (and it
	 (component-operation-time (make-instance 'load-op) it)))) 

;;; ---------------------------------------------------------------------------

(defmethod operate :after (operation-class system &key &allow-other-keys)
  (declare (ignorable operation-class system))
  (load-connected-systems))

;;; ---------------------------------------------------------------------------

(pushnew :asdf-system-connections *features*)
