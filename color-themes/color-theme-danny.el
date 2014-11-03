(eval-when-compile
  (require 'color-theme))

(defun color-theme-danny ()
  "Yet another color theme. I like it a bit."
  (interactive)
  (color-theme-install
   '(color-theme-danny
     ((foreground-color . "brightgreen")
      (background-color . "black")
      (background-mode . light))
     (underline ((t (:foreground "brightyellow"))))
     (italic ((t (:foreground "brightcyan" :italic t))))
     (bold ((t (:foreground "brightblue" :bold t))))
     (bold-italic ((t (:foreground "dark magenta" :bold t :italic t))))
     (font-lock-comment-face
      ((t (:foreground "white" :background "dark magenta"))))
     (modeline-buffer-id ((t (:background nil
                                          :foreground "#870000"
                                          :underline nil))))
     (modeline-mousable ((t (:background "brightgreen" :foreground "white"))))
     (mode-line
      ((t (:background "#870"
                       :foreground "black"
                       :overline nil
                       :underline nil))))
     (mode-line-inactive
      ((t (:background "black"
                       :foreground "#870"
                       :underline nil
                       :overline nil
                       :weight light)))))))


(provide 'color-theme-danny)
