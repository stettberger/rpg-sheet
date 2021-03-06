(in-package :rpg)

(setf pdf:*compress-streams* nil)

(defclass character-sheet ()
  ((name    :initarg :name :accessor character-name)
   (rang    :initarg :rang :accessor character-rang)
   (race    :initarg :race :accessor character-race)
   (age     :initarg :age  :accessor character-age)
   (size    :initarg :size :accessor character-size)
   (player  :initarg :player :accessor character-player)
   (setting :initarg :setting :accessor character-setting)
   (concept :initarg :concept :accessor character-concept)
   (sex     :initarg :sex :accessor character-sex)
   (handicaps :initarg :handicaps :accessor character-handicaps)
   (talents  :initarg :talents :accessor character-talents)
   (weight  :initarg :weight :accessor character-weight)))

(defclass character-sheet-savage-world (character-sheet)
  ((attributes :initarg :attributes :accessor character-attributes)
   (skills     :initarg :skills     :accessor character-skills)
   (upgrades   :initarg :upgrades   :accessor character-upgrades)))

(defparameter *default-font* (pdf:get-font "Courier"))
(defparameter *point-stroke-color* '(0   0   0))
(defparameter *point-fill-color*   '(0.2 0.3 0.4))
(defparameter *text-color*         '(0.0 0.0 0.0))

(defgeneric draw-character-sheet (character template destination)
  (:documentation "Uses the data in the character object to draw upon a template"))

(defun set-stroke-color-list (color)
  (pdf:set-rgb-stroke (first color) (second color) (third color)))

(defun set-fill-color-list (color)
  (pdf:set-rgb-fill (first color) (second color) (third color)))


(defun draw-point (x y &optional (radius 5))
  "Draw a point at the position x/y"
  (set-fill-color-list   *point-fill-color*)
  (set-stroke-color-list *point-stroke-color*)
  (pdf:circle x y radius)
  (pdf:close-fill-and-stroke))

(defun put-text (x y text &optional (size 8.0))
  "Draw a text at position"
  (pdf:in-text-mode
    (pdf:set-font *default-font* size)
    (set-fill-color-list *text-color*)
    (pdf:move-to x y)
    (pdf:draw-text text)))

(defun put-list (base-x base-y first-offset delta-y line-width elements)
  (flet ((size (x) (text-width x *default-font* 8.0)))
  (let* ((line 0) (offset-x first-offset)
         ;; Sort the list the way, that the shortest text (text-width) is at the first place
         (elements (sort (copy-list elements) #'(lambda (a b) (< (size a) (size b)))))
         (elements  (mapcon #'(lambda (x)
                                (if (> (length x) 1)
                                    (list (format nil "~a, " (first x)))
                                    x)) 
                            elements)))
    
    (dolist (elem elements)
      ;; Go to next line, if text would'nt fit
      (when (< line-width (+ offset-x (size elem)))
          (incf line)
          (setf offset-x 0))
      (put-text (+ base-x offset-x) (+ base-y (* delta-y line)) elem)
      (incf offset-x (size elem))))))


(defun testit ()
  (let ((character (make-instance 
                    'character-sheet-savage-world 
                    :name "Talor Ordo"
                    :rang "Anfänger"
                    :race "Mandolorianer"
                    :age 23
                    :size "178 cm"
                    :player "Christian"
                    :setting "Star Wars"
                    :concept "Krieger"
                    :sex "männlich"
                    :weight "72 kg"
                    :attributes '(:dexterity 1
                                  :constitution 3 
                                  :strength 1 
                                  :inteligence 2 
                                  :backbone 5)
                    :skills '(("Schießen" 4)
                              ("Reiten"   1))
                    :upgrades '(nil 
                                ("scdsc") 
                                ("Schwimmen" 4)
                                (("Reiten" 3) ("Fliegen" 2)))
                    :handicaps '("Ehrencodex (Mando)" "Loyal" "Gier")
                    :talents   '("Beidhändig" "Kräftig"))))
    (draw-character-sheet character #P"/home/stettberger/u/rpg/savageworld.pdf" #P"/tmp/test.pdf")))
