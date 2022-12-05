(defparameter *score-rock* 1)
(defparameter *score-paper* 2)
(defparameter *score-scissors* 3)
(defparameter *score-loss* 0)
(defparameter *score-draw* 3)
(defparameter *score-win* 6)

;; silently return nil on invalid input :)
(defun to-rps (val)
  (cond ((or (equal #\A val)
             (equal #\X val))
         :rock)
        ((or (equal #\B val)
             (equal #\Y val))
         :paper)
        ((or (equal #\C val)
             (equal #\Z val))
         :scissors)))

(defun style-points (choice)
  (cond ((equal :rock choice) *score-rock*)
        ((equal :paper choice) *score-paper*)
        ((equal :scissors choice) *score-scissors*)))
        
(defun match-points (opp me)
  (cond ((equal opp me)
         *score-draw*)
        ((equal opp :rock)
         (if (equal me :scissors)
             *score-loss*
             *score-win*))
        ((equal opp :paper)
         (if (equal me :rock)
             *score-loss*
             *score-win*))
        ((equal opp :scissors)
         (if (equal me :paper)
             *score-loss*
             *score-win*))))
             
(defun calc-score (in total)
  (let ((line (read-line in nil)))
    (if line
        (let ((opp (to-rps (char line 0)))
              (me  (to-rps (char line 2))))
          (calc-score in (+ total
                            (style-points me)
                            (match-points opp me))))
        total)))

(with-open-file (in "input")
  (calc-score in 0))

(defun choose (opp result)
  (cond ((equal #\Y result)
         opp)
        ((equal opp :rock)
         (if (equal #\X result)
             :scissors
             :paper))
        ((equal opp :paper)
         (if (equal #\X result)
             :rock
             :scissors))
        ((equal opp :scissors)
         (if (equal #\X result)
             :paper
             :rock))))
    
(defun to-rps-p2 (line)
  (let* ((opp (to-rps (char line 0)))
        (result (char line 2))
        (me (choose opp result)))
    (list opp me)))

(defun calc-score-p2 (in total)
  (let ((line (read-line in nil)))
    (if line
        (let* ((choices (to-rps-p2 line))
                 (opp (car choices))
                 (me (cadr choices)))
          (print line)
          (print opp)
          (print me)
          (print (+ (style-points me)
                    (match-points opp me)))
          (calc-score-p2 in (+ total
                            (style-points me)
                            (match-points opp me))))
        total)))

(with-open-file (in "input")
  (calc-score-p2 in 0))
           
