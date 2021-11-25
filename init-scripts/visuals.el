;;; modifications to ui only

(require 'cl-lib)

;;; set font size and type
(defcustom best-text-sizes-alist
  `((best . 2.5)
    (massive . 10)
    (big . 5)
    (alright-its-a-little-a-big . 4)
    (not-that-big . 3)
    (somewhat-bigger . 2.5)
    (medium . 2)                      ; good for a small 1920x1080 screen
    (little . 1)                      ; good for a large 1360x768 screen
    (just-a-tad-tinier . 0.9)
    (smaller . 0.8)
    (pretty-small . 0.7)
    (pixelated-small . 0.6)
    (even-littler . 0.5)
    (tiny . 0.2)
    (miniscule . 0.1))
  "This alist generates methods to scale the size of text on the screen."
  :type '(alist :key-type (symbol :tag "Method name prefix.")
                :value-type (float :tag "The font size as a multiple of the default height."))
  :group 'display)

(defmacro generate-specific-text-size-method (default-size spec)
  (pcase-exhaustive `(,default-size . ,spec)
    ((and `(,(helm-rg-cl-typep integer)
            .
            (,(and (helm-rg-cl-typep symbol) prefix)
             .
             ,(and (helm-rg-cl-typep integer float) size)))
          (let dilated-points (-> (* default-size size) (round))))
     `(defun ,(->> prefix (symbol-name) (format "%s-text-size") (intern)) ()
          ,(format "Set the `default' font size to use a %dx multiplier." size)
        (interactive)
        (set-face-attribute ', 'default nil :height ,dilated-points)))))

(defconst emacs-startup-font-size (face-attribute 'default :height)
  "The font size that emacs starts up with.")

(defmacro generate-all-text-sizes ()
  `(progn
     ,@(--map
        `(generate-specific-text-size-method ,emacs-startup-font-size ,it)
        best-text-sizes-alist)))

;;; Once the first window has been created, generate the window sizing methods.
(add-hook 'window-setup-hook (z (generate-all-text-sizes)))

(defun increase-font-size ()
  (interactive)
  (-> (face-attribute 'default :height) (+ 10)))

(defun increase-font-size-a-little ()
  (interactive)
  (-> (face-attribute 'default :height) (+ 5)))

(defun decrease-font-size ()
  (interactive)
  (-> (face-attribute 'default :height) (- 10)))

(defun decrease-font-size-a-little ()
  (interactive)
  (-> (face-attribute 'default :height) (- 5)))

(define-minor-mode change-font-size-mode
  "Change font size interactively!" nil "FontSize"
  (let ((map (make-sparse-keymap)))
    ;;; TODO: don't make these the arrow keys -- make this a persistent minor mode, and make it
    ;;; really easy to get back to where you were at the start!
    (define-key map (kbd "<up>") #'increase-font-size)
    (define-key map (kbd "<down>") #'decrease-font-size)
    (define-key map (kbd "<left>") #'decrease-font-size-a-little)
    (define-key map (kbd "<right>") #'increase-font-size-a-little)
    (define-key map (kbd "q") #'change-font-size-mode)
    map))
