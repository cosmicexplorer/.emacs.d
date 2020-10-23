;;; modifications to ui only

(require 'cl-lib)

;;; set font size and type
(defcustom best-text-sizes-alist
  `((best . 1.0)
    (massive . 10.0)
    (big . 5.0)
    (medium . 2.0)
    (little . 1.0)
    (even-littler . 0.5)
    (tiny . 0.2))
  "This alist generates methods to scale the size of text on the screen."
  :type '(alist :key-type (symbol :tag "Method name prefix.")
                :value-type (float :tag "The font size as a multiple of the default height."))
  :group 'display)

(defun generate-specific-text-size-method (prefix size)
  (check-whatever-type-it-may-be prefix symbol)
  (check-whatever-type-it-may-be size integer)
  `(defun ,(->> prefix (symbol-name) (format "%s-text-size") (intern)) ()
     ,(format "Set the `default' font size to %d points." size)
     (interactive)
     (set-face-attribute ','default nil :height ,size)))

;;; Once the first window has been created, generate the window sizing methods.
(add-hook
 'window-setup-hook
 (z (cl-loop
     with default-size = (face-attribute 'default :height)
     for (prefix . size) in best-text-sizes-alist
     for dilated-points = (round (* default-size size))
     do (eval (generate-specific-text-size-method prefix dilated-points)))))

(defun increase-font-size ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ attr 10))))

(defun increase-font-size-a-little ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ attr 5))))

(defun decrease-font-size ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- attr 10))))

(defun decrease-font-size-a-little ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- attr 5))))

(define-minor-mode change-font-size-mode
  "Change font size interactively!" nil "FontSize"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'increase-font-size)
    (define-key map (kbd "<down>") #'decrease-font-size)
    (define-key map (kbd "<left>") #'decrease-font-size-a-little)
    (define-key map (kbd "<right>") #'increase-font-size-a-little)
    (define-key map (kbd "q") #'change-font-size-mode)
    map))
