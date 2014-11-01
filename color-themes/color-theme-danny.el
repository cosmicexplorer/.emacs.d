(eval-when-compile
  (require 'color-theme))

(defun color-theme-danny ()
  "Just another color theme. I like it a bit.
Danny McClanahan, 2014."
  (interactive)
  (color-theme-install
   '(color-theme-danny
     nil                                ; no fore/background changes
     (underline ((t (:foreground "dark yellow" :underline t))))
     (italic ((t (:foreground "dark blue" :italic t))))
     (bold ((t (:foreground "dark purple" :bold t))))
     (bold-italic ((t (:foreground "dark green" :bold t :italic t))))
     (font-lock-comment-face ((t (:foreground "FireBrick")))))))

(provide 'color-theme-danny)
