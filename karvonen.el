;;; Exercise 31 of "Exercises for programmers"
;;; Karvonen Heart Rate

(defconst karvonen--buffer-name "* Karvonen Output *")
(defconst karvonen--maximum-heart-rate-per-gender '(("M" . 206.9) ("W" . 206)))
(defconst karvonen--left-column-size 15)
(defconst karvonen--min-percent 55)
(defconst karvonen--max-percent 95)

(defun karvonen-formula (maximumHR restingHR age intensity)
  (+ (* (- maximumHR age restingHR)
        (/ intensity 100.0))
     restingHR))

(defun karvonen--create-loop (min max)
  (let ((value min)
        (output (list))) 
    (while (<= value max)
      (setq output (append (list value) output))
      (setq value (+ value 5)))
    (reverse output)))

(defun karvonen--header (restingHR age)
  (concat (format "Resting Pulse: %d  Age: %d\n" restingHR age)
          "\n"
          (format (concat "%-" (number-to-string karvonen--left-column-size) "s|%s") "Intensity" "Rate\n")
          (make-string karvonen--left-column-size ?-) "+------\n"))

(defun karvonen--format-entry (percentage bpm)
  (format (concat "%-" (number-to-string karvonen--left-column-size) "s| %dbpm\n")
          (concat (number-to-string percentage) "%")
          (round bpm 1)))

(defun karvonen--insert-entry (percentage bpm)
  (insert (karvonen--format-entry percentage bpm))
  (sit-for 0.05))

(defun karvonen--read-input (prompt)
  (let ((raw-value (read-from-minibuffer prompt)))
    (if (string-match "^[0-9]+$" raw-value)
        (string-to-int raw-value)
      (message "Invalid input '%s'" raw-value)
      (karvonen--read-input prompt))))

(defun karvonen--show-buffer (maxHR restingHR age)
  (interactive)
  (pop-to-buffer (get-buffer-create karvonen--buffer-name))
  (erase-buffer)
  (insert (karvonen--header restingHR age))
  (sit-for 0.05)
  (seq-map (lambda (intensity)
             (karvonen--insert-entry intensity
                                     (karvonen-formula maxHR restingHR age intensity)))
           (karvonen--create-loop karvonen--min-percent karvonen--max-percent)))

(defun karvonen--read-gender (prompt)
  (let ((raw-value (read-from-minibuffer prompt)))
    (if (string-match "^[mMwW].*$" raw-value)
        (upcase (substring raw-value 0 1))
      (message "Invalid input '%s'" raw-value)
      (karvonen--read-gender prompt))))

(defun karvonen--get-maximum-heart-rate ()
  (cdr
   (assoc (karvonen--read-gender "Are you biologically a man or a woman? ")
          karvonen--maximum-heart-rate-per-gender)))

(defun karvonen ()
  (interactive)
  (karvonen--show-buffer
   (karvonen--get-maximum-heart-rate)
   (karvonen--read-input "Resting heart rate: ")
   (karvonen--read-input "Age: ")))
