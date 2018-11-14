;;; Exercise 31 of "Exercises for programmers"
;;; Karvonen Heart Rate

(defconst karvonen--buffer-name "* Karvonen Output *")
(defconst karvonen--left-column-size 15)

(defun karvonen-formula (restingHR age intensity)
  (+ (* (- 220 age restingHR)
        intensity)
     restingHR))

(defun karvonen-create--loop (min max)
  (let ((value min)
        (output (list))) 
    (while (< value max)
      (setq output (append (list value) output))
      (setq value (+ value 5)))
    (reverse output)))

(defun karvonen--header (restingHR age)
  (concat (format "Resting Pulse: %d  Age: %d\n" restingHR age)
          "\n"
          (format (concat "%-" (number-to-string karvonen--left-column-size) "s|%s") "Intensity" "Rate\n")
          (make-string karvonen--left-column-size ?-) "+------\n"))



(defun karvonen--show-buffer ()
  (when (get-buffer karvonen--buffer-name)
    (kill-buffer karvonen--buffer-name))
  (switch-to-buffer (get-buffer-create karvonen--buffer-name))
  (insert "foobar"))
