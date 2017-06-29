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
(set-face-attribute 'default nil :height default-text-height)
(when (member "Telegrama" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Telegrama 10"))
  (set-face-attribute 'default t :font "Telegrama 10")
  (set-frame-font "Telegrama 10"))

;;; have normal delete/selection (type over selected text to delete)
(delete-selection-mode 1)
