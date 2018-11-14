;;; Exercise 31 of "Exercises for programmers"
;;; Karvonen Heart Rate

(defconst karvonen--buffer-name "* Karvonen Output *")
(defconst karvonen--left-column-size 15)
(defconst karvonen--min-percent 55)
(defconst karvonen--max-percent 95)

(defun karvonen-formula (restingHR age intensity)
  (+ (* (- 220 age restingHR)
        intensity)
     restingHR))

(defun karvonen--create-loop (min max)
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

(defun karvonen--format-entry (percentage bpm)
  (format (concat "%-" (number-to-string karvonen--left-column-size) "s| %sbpm\n")
          percentage
          bpm))

(defun karvonen--insert-entry (percentage bpm)
  (insert (karvonen--format-entry percentage bpm))
  (redisplay))

(defun karvonen--show-buffer (restingHR age)
  (interactive)
  (pop-to-buffer (get-buffer-create karvonen--buffer-name))
  (erase-buffer)
  (insert (karvonen--header restingHR age))
  (seq-map (lambda (intensity)
             (karvonen--insert-entry intensity
                                     (karvonen-formula restingHR age intensity)
                                     ))
           (karvonen--create-loop karvonen--min-percent karvonen--max-percent)))
