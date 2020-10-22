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
(defcustom best-text-size-height 40
  "This scales the size of text on the screen.

NB: It appears to be necessary to change for no conceivable reason -- it would be good to keep an
eye on when and why this occurs in the future."
  :type 'integer
  :group 'display)

(defun best-text-size ()
  (interactive)
  (set-face-attribute 'default nil :height best-text-size-height))

(defcustom best-font-face "Telegrama"
  "The default font face to use in emacs!"
  :type '(string symbol)
  :group 'display)

(defun extract-font-family (font-spec-like)
  (pcase-exhaustive font-spec-like
    ((and (cl-type string) family) family)
    ((and (cl-type symbol) (app (symbol-name) family)) family)
    ((and (pred fontp) (lit (symbol-name (font-get lit :family)) family)) family)))

(defun select-font (font-spec-like)
  (interactive (list (x-select-font)))
  (let ((family (extract-font-family font-spec-like)))
    (check-whatever-type-it-may-be family string)
    (add-to-list 'default-frame-alist `(font . ,family))
    (set-face-attribute 'default t :font family)
    (set-frame-font family)
    family))

(add-hook 'window-setup-hook (z (select-font best-font-face)))
(add-hook 'window-setup-hook #'best-text-size)

;;; have normal delete/selection (type over selected text to delete)
(delete-selection-mode 1)
