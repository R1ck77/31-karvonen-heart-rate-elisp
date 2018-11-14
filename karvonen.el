;;; Exercise 31 of "Exercises for programmers"
;;; Karvonen Heart Rate

(defconst karvonen--buffer-name "* Karvonen Output *")

(defun karvonen-formula (age restingHR intensity)
  (+ (* (- 220 age restingHR)
        intensity)
     restingHR))


(defun karvonen--show-buffer ()
  (when (get-buffer karvonen--buffer-name)
    (kill-buffer karvonen--buffer-name))
  (switch-to-buffer (get-buffer-create karvonen--buffer-name))
  (insert "foobar"))
