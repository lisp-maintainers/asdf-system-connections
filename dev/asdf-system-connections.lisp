(in-package asdf)

(export '(map-system-connections
          defsystem-connection))

;;; ---------------------------------------------------------------------------
;;; not particularly rich person's system interconnection facility
;;; ---------------------------------------------------------------------------

(defclass system-connection (system)
  ((systems-required :initarg :systems-required :reader systems-required)
   (been-loaded? :accessor been-loaded? :initform nil)))

;;; ---------------------------------------------------------------------------

(defun map-system-connections (fn)
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (typep (cdr v) 'system-connection)
               (funcall fn (cdr v))))
           *defined-systems*))

;;; ---------------------------------------------------------------------------

(defmacro defsystem-connection (name &body options)
  (let ((requires (getf options :requires)))
    (remf options :requires)
    `(progn
       (defsystem ,name
         :class system-connection
         :systems-required ,requires
         ,@options) 
       (values ',name))))

;;; ---------------------------------------------------------------------------

(defun load-connected-systems ()
  (map-system-connections 
   (lambda (connection)
     (when (and (required-systems-loaded-p connection)
                (not (system-loaded-p (component-name connection))))
       (setf (been-loaded? connection) t)
       (asdf:oos 'asdf:load-op (component-name connection) :force t)))))

#+Test
(load-connected-systems)

(defun required-systems-loaded-p (connection)
  (every (lambda (system)
           (system-loaded-p system))
         (systems-required connection)))

;;; ---------------------------------------------------------------------------

(defun system-loaded-p (system-name)
  (let ((system (find-system system-name nil)))
    (when system
      (gethash 'load-op (asdf::component-operation-times system)))))

;;; ---------------------------------------------------------------------------

(defmethod perform :around ((operation load-op) (system system))
  (call-next-method)
  (load-connected-systems))

;;; ---------------------------------------------------------------------------

(defmethod operation-done-p :around ((o load-op) (c system))
  (let ((it (find-system c)))
    (if (typep it 'system-connection)
      (been-loaded? it)
      (call-next-method))))

