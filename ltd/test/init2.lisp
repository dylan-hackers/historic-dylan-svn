;; for non-allegro CL systems.
(defmacro declaim (x) `(proclaim ', x))
(or (find-package 'excl)
    (make-package 'excl))
(load "mma")
(use-package :mma)
(in-package :mma)
(shadow 'user::set)
(shadow 'Exp)
(shadow 'Log)
(shadow 'Sin)
(shadow 'Cos)
(shadow 'Tan)
(shadow 'Sinh)
(shadow 'Cosh)
(import 'user::declaim)
(require "ucons1")  ; should really be uconsalt copied to this file
(require 'math-parser "parser")
(require "stack1")
(require "disp1")
(require 'math-eval "eval")
(require "poly")
(require "rat1")
(require "simp1")
(require "pf")
(require "match")
(require "diffrat")




