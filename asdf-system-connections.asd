;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defpackage #:asdf-system-connections-system (:use #:cl #:asdf))
(in-package #:asdf-system-connections-system)

(defsystem asdf-system-connections
  :version "0.8.4"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Allows for ASDF system to be connected so that auto-loading may occur."
  :components 
  ((:module 
    "dev"
    :components ((:file "asdf-system-connections")))   
   (:module 
    "website"
    :components ((:module
		  "source"
		  :components ((:static-file "index.md")))))))







