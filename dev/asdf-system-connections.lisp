(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(map-system-connections defsystem-connection)))

;;; ---------------------------------------------------------------------------
;;; not particularly rich person's system interconnection facility
;;; ---------------------------------------------------------------------------

(defclass system-connection (system)
  ((systems-required :initarg :systems-required :reader systems-required)))

;;; ---------------------------------------------------------------------------

(defun map-system-connections (fn)
  (map-systems
   (lambda (s) (when (typep s 'system-connection) (funcall fn s)))))

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
       (load-system (component-name connection))))))

(defun required-systems-loaded-p (connection)
  (every #'system-loaded-p (systems-required connection)))

;;; ---------------------------------------------------------------------------
(unless (fboundp 'registered-system)
  (defun registered-system (system-name)
    (cdr (system-registered-p system-name))))

(defun system-loaded-p (system-name)
  (if-let (it (registered-system system-name))
    (component-operation-time (make-operation 'load-op) it)))

;;; ---------------------------------------------------------------------------

(defvar *asdf-after-operate-hook* nil
  "List of functions that are called by the ((ASDF:OPERATE :AFTER) T T)  method.
Each function should take two arguments.")

(defun load-connected-systems/2 (operation component)
  (declare (ignore operation component))
  (load-connected-systems))

(pushnew 'load-connected-systems/2 *asdf-after-operate-hook*)

(defmethod operate :after ((operation t) (component t) &key &allow-other-keys)
  (dolist (function *asdf-after-operate-hook*)
    (funcall function operation component)))

;;; ---------------------------------------------------------------------------

(pushnew :asdf-system-connections *features*)
