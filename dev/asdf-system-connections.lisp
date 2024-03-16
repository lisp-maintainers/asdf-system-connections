(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(map-system-connections defsystem-connection)))

;;; ---------------------------------------------------------------------------
;;; not particularly rich person's system interconnection facility
;;; ---------------------------------------------------------------------------

(defclass system-connection (system)
  ((systems-required :initarg :systems-required :reader systems-required)))

;;; ---------------------------------------------------------------------------

(defun find-system-from-dep-spec (depends-on)
  (etypecase depends-on
    (system
     depends-on)
    ((or string symbol)
     (find-system depends-on nil))
    (list
     (find-system
      (let ((spec (ecase (first depends-on)
                    (:version (second depends-on))
                    (:feature (third depends-on)))))
        (etypecase spec
          ((or string symbol) spec)
          (list
           (ecase (first spec)
             (:require (second spec))))))
      nil))))

(defun system-depends-on-p (system depends-on)
  "Returns non-NIL if DEPENDS-ON is a dependency of SYSTEM."
  (let* ((system (find-system-from-dep-spec system))
         (depends-on (find-system-from-dep-spec depends-on))
         (depends-on-name (when depends-on
                            (component-name depends-on))))
    (when (and system depends-on)
      (some (lambda (dep)
              (let* ((dep (find-system-from-dep-spec dep))
                     (dep-name (component-name dep)))
                (or (string= dep-name depends-on-name)
                    (loop :for dep-dep :in (system-depends-on dep)
                            :thereis (system-depends-on-p dep-dep depends-on)))))
            (system-depends-on system)))))

;;; ---------------------------------------------------------------------------

(defvar *system-connections* (make-hash-table :test 'equal))

(defmacro defsystem-connection (name &body options)
  (let ((requires (getf options :requires))
        (depends-on (getf options :depends-on))
        (class (getf options :class 'system-connection))
        (connections (gensym "CONNECTIONS"))
        (prerequisites (gensym "PREREQUISITES")))
    (remf options :requires)
    (remf options :class)
    `(progn
       (defsystem ,name
         :class ,class
         :depends-on ,(append requires
                              depends-on)
         :systems-required ,requires
         ,@options)
       ,@(mapcar (lambda (r)
                   (flet ((system-name (s)
                            (etypecase s
                              (string s)
                              (symbol (string-downcase (symbol-name s)))
                              (asdf:component (asdf:component-name s)))))
                     (let* ((r    (system-name r))
                            (req  (gensym "REQ"))
                            (name (system-name name))
                            (requires
                              (sort (mapcar #'system-name requires) #'string<)))
                       `(let* ((,prerequisites
                                 (remove-if
                                  (lambda (,req)
                                    (system-depends-on-p ,req ',r))
                                  (remove ',r ',requires
                                          :test #'string=)))
                               (,connections
                                 (append (gethash ',r *system-connections*)
                                         (list (cons ,prerequisites
                                                ',name)))))
                          (when ,prerequisites
                            (setf (gethash ',r *system-connections*)
                                  ,connections))))))
                 requires)
       (values ',name))))

;;; ---------------------------------------------------------------------------

(defun load-connected-systems (operation component)
  (let* ((component (find-system-from-dep-spec component))
         (deps (system-depends-on component))
         (connections (gethash (component-name component)
                               *system-connections*)))
    (loop :for dep :in deps
          :do (load-connected-systems 'asdf:load-op dep))
    (loop :for (prerequisites . connection) :in connections
          :do (when (and (not (component-loaded-p connection))
                         (every #'component-loaded-p prerequisites))
                (dolist (prereq prerequisites)
                  (dolist (dep (system-depends-on (find-system prereq)))
                    (load-connected-systems operation dep)))
                (asdf:oos operation connection)))))

;;; ---------------------------------------------------------------------------

(defmethod operate :after ((operation t) (component t) &key &allow-other-keys)
  (when (or (eq 'asdf:load-op operation)
            (typep operation 'asdf:load-op))
    (load-connected-systems 'asdf:load-op component)))

;;; ---------------------------------------------------------------------------

(pushnew :asdf-system-connections *features*)
