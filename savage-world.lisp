(in-package :rpg)

(defparameter *savage-magic-numbers*
  '(:small-line-offset 15.6 ;; y-delta for skills
    :big-line-offset 17.0   ;; y-delta for attributes
    ;; Skills
    :skills-base-x 109.8 ;; Left down point
    :skills-base-y 131.4 ;;
    :skills-point-delta  20.45 
    :skills-text-delta   -80 ;; The from O to first _: ___________ O
    :skills-count 30

    ;; Attributes
    :attributes-base-x 109.8 ;; Left down point
    :attributes-base-y 626.95 
    :attributes-point-delta 20.45

    ;; Base-info
    :base-info-first-column 59
    :base-info-second-column 260
    :base-info-base-y 802

    ;; Upgrades
    :upgrades-base-x 245
    :upgrades-base-y 470
    :upgrades-count  23

    ;; Handicaps
    :handicaps-base-x 211.5
    :handicaps-base-y 578
    :handicaps-first-line-offset 50
    :handicaps-line-width 129.3

    ;; Talents
    :talents-base-x 211.5
    :talents-base-y 532
    :talents-first-line-offset 40
    :talents-line-width 129.3
))
  

(defun savage-size (size)
  "Get the defined size in *savage-magic-numbers*"
  (getf *savage-magic-numbers* size))

(defun savage-dice (number)
  (if (not number) ""
      (ecase number
        (0 "")
        (1 "W4")
        (2 "W6")
        (3 "W8")
        (4 "W10")
        (5 "W12"))))

(defun savage-draw-skills (skills)
  "Draws a list of skills within an savage world pdf template"
  (let ((base-x  (savage-size :skills-base-x))  
        (base-y  (savage-size :skills-base-y))
        (delta-x (savage-size :skills-point-delta)) 
        (delta-y (savage-size :small-line-offset)))
    (loop for y from (- (savage-size :skills-count) 1) downto 0
         for skill in skills
         do (progn
              (put-text (+ base-x (savage-size :skills-text-delta))
                        (+ base-y (- 6) (* y delta-y))
                        (first skill))
              (dotimes (x (min 5 (second skill)))
                (draw-point (+ base-x (* x delta-x))
                            (+ base-y (* y delta-y))))))))

(defun savage-draw-attributes (attributes)
  "Draws a plist of attributes within an savage world pdf template"
  (let ((base-x (savage-size  :attributes-base-x))  
        (base-y (savage-size  :attributes-base-y)) 
        (delta-x (savage-size :attributes-point-delta)) 
        (delta-y (savage-size :big-line-offset)))
    (flet ((draw-attribute (slot attribute)
             (if attribute
                 (dotimes (x (min 5 attribute))
                   (draw-point (+ base-x (* x delta-x))
                               (+ base-y (* slot delta-y)))))))
      ;; The attributes are counted from bottom up
      (draw-attribute 4 (getf attributes :dexterity))
      (draw-attribute 3 (getf attributes :constitution))
      (draw-attribute 2 (getf attributes :strength))
      (draw-attribute 1 (getf attributes :inteligence))
      (draw-attribute 0 (getf attributes :backbone)))))

(defun savage-draw-base-info (character)
  (let ((base-x-1   (savage-size :base-info-first-column)) 
        (base-x-2   (savage-size :base-info-second-column)) 
        (base-y     (savage-size :base-info-base-y)) 
        (delta-y (- (savage-size :small-line-offset))))
    (flet ((put-slot (x y slot)
             (when (slot-boundp character slot)
               (put-text x y (slot-value character slot)))))
      ;; Draw first column
      (loop for y from 0 to 5
           for slot in '(name rang race age size)
           do (put-slot base-x-1 (+ base-y (* y delta-y)) slot))
      ;; Draw second column
      (loop for y from 0 to 5
         for slot in '(player setting concept sex weight)
         do (put-slot base-x-2 (+ base-y (* y delta-y)) slot)))))

(defun savage-do-upgrades (character)
  "This does really update the skills of the character according to the updates"
  (labels ((skill (upgrade) (assoc (first upgrade) (character-skills character) :test #'equal))
           (do-upgrade (upgrade)
             "Does an upgrade in the form of '(\"attribute\" level), where level is a number"
             (if (and (> (length upgrade) 1) (> (second upgrade) 0))
                 (if (skill upgrade)
                     (setf (cdr (skill upgrade))
                           (list (max (second upgrade)
                                      (second (skill upgrade)))))
                     (setf (character-skills character)
                           (append (character-skills character) (list upgrade)))))))
           
    (dolist (upgrade (character-upgrades character))
      (cond ((list-of-lists-p upgrade) ;; List of upgrades
             (dolist (up upgrade)
               (do-upgrade up)))
            ((and (listp upgrade) (not (null upgrade))) ;; just a single upgrade
             (do-upgrade upgrade))))))

(defun savage-draw-upgrades (upgrades)
  (let ((base-x (savage-size :upgrades-base-x)) 
        (base-y (savage-size :upgrades-base-y)) 
        (delta-y (- (savage-size :small-line-offset))))
    (flet ((format-upgrade (upgrade)
             (format nil "~a ~a" (first upgrade) (savage-dice (second upgrade)))))
      (loop for y from 0 to (savage-size :upgrades-count)
         for upgrade in upgrades
         do
           (cond ((list-of-lists-p upgrade) ;; List of upgrades
                  (put-text base-x (+ base-y (* y delta-y)) 
                            (format nil "~{~a~^, ~}" (mapcar #'format-upgrade upgrade))
                            7.0))
                 ((and (listp upgrade) (not (null upgrade)))
                  (put-text base-x (+ base-y (* y delta-y)) (format-upgrade upgrade) 7.0)))))))

(defun savage-draw-handicaps (handicaps)
  (let ((base-x       (savage-size :handicaps-base-x)) 
        (base-y       (savage-size :handicaps-base-y)) 
        (first-offset (savage-size :handicaps-first-line-offset))
        (line-width   (savage-size :handicaps-line-width))
        (delta-y (-   (savage-size :small-line-offset))))
    (put-list base-x base-y first-offset delta-y line-width handicaps)))

(defun savage-draw-talents (talents)
  (let ((base-x       (savage-size :talents-base-x)) 
        (base-y       (savage-size :talents-base-y)) 
        (first-offset (savage-size :talents-first-line-offset))
        (line-width   (savage-size :talents-line-width))
        (delta-y (-   (savage-size :small-line-offset))))
    (put-list base-x base-y first-offset delta-y line-width talents)))
  
(defmethod draw-character-sheet  ((character character-sheet-savage-world) template destination)
  (pdf:with-existing-document (template)
    (pdf:with-existing-page (0)
      (pdf:insert-original-page-content)
      (savage-do-upgrades     character)
      (savage-draw-base-info  character)
      (savage-draw-skills     (character-skills character))
      (savage-draw-attributes (character-attributes character))
      (savage-draw-upgrades   (character-upgrades character))
      (savage-draw-handicaps  (character-handicaps character))
      (savage-draw-talents    (character-talents character)))
    (pdf:write-document destination)))
