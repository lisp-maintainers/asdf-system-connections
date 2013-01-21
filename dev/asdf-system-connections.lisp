(in-package #:asdf)

(eval-when (:compile-toplevel :load-toplevel)
  (export '(map-system-connections
	    defsystem-connection)))

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
  (aif (cdr (system-registered-p system-name))
       (component-operation-time (make-instance 'load-op) it)))

;;; ---------------------------------------------------------------------------

(defmethod operate :after (operation-class system &key &allow-other-keys)
  (declare (ignore operation-class system))
  (load-connected-systems))

;;; ---------------------------------------------------------------------------

(pushnew :asdf-system-connections *features*)
