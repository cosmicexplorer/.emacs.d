;;; modifications to ui only (called when frame made)

;;; remove toolbars
(menu-bar-mode 0)                       ;  remove menu bar for a line of space
(tool-bar-mode 0)                       ; and tool bar for graphical

;;; configure scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(scroll-bar-mode 0)
(setq scroll-preserve-screen-position t)

;;; set font size and type
(defun best-text-size ()
  (interactive)
  (set-face-attribute 'default nil :height 90))
(when (member "Telegrama" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Telegrama 9"))
  (set-face-attribute 'default t :font "Telegrama 9")
  (set-frame-font "Telegrama 9"))

;;; have normal delete/selection (type over selected text to delete)
(delete-selection-mode 1)
