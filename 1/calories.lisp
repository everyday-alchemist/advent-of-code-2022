;; Part 1
;;
;; This is pretty bad.
;; There is no error handling for opening the file or parse-integer.
;; My function is not tail recursive.
(defun rf (in curr big)
  (let ((x (read-line in nil)))
    (cond ((equal nil x)
           (max curr big))
          ((equal "" x)
           (if (> curr big)
               (rf in 0 curr)
               (rf in 0 big)))
          (t
           (rf in (+ curr (parse-integer x)) big)))))

(with-open-file (in "input")
  (print (rf in 0 0)))

;; Part 2
;;
;; We aren't going to over-engineer this...
(defun top3 (in curr big)
  (let ((x (read-line in nil)))
    (cond ((equal nil x)
           (reduce #'+ big))
          ((equal "" x)
           (let ((sorted (sort big #'<)))
             (if (> curr (car sorted))
                 (top3 in 0 (cons curr (cdr big)))
                 (top3 in 0 big))))
           (t
            (top3 in (+ curr (parse-integer x)) big)))))

(with-open-file (in "input")
  (print (top3 in 0 (list 0 0 0))))
