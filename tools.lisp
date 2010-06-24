(in-package :rpg)

(defun list-of-lists-p (a)
  "Is a a list of lists?"
  (and (listp a) (> (length a) 0) (listp (first a))))