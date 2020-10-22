;;; highlight-80+.el --- highlight characters beyond column 80
;;
;; Copyright (C) 2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0
;; Keywords: faces
;; URL: http://nschum.de/src/emacs/highlight-tabs/
;; Compatibility: GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This mode highlights all characters that cross the 80 character line limit.
;;
;;; Change Log:
;;
;; 2008-08-11 (1.0)
;;    Initial release.
;;
;;; Code:

(defgroup highlight-80+ nil
  "Highlight characters beyond column 80."
  :group 'faces)

(defcustom highlight-80+-columns 80
  "*Number of columns to allow in lines."
  :group 'highlight-80+
  :safe t
  :type 'integer)

(defface highlight-80+-line
  nil
  "*Face for showing lines with over `highlight-80+-columns'."
  :group 'highlight-80+-line)

(defface highlight-80+
  '((((background dark)) (:background "blue"))
    (((background light)) (:background "dark gray")))
  "*Face for showing characters beyond column `highlight-80+-columns'."
  :group 'highlight-80+-line)

(defconst highlight-80+-keywords
  `((highlight-80+-matcher (0 'highlight-80+-line prepend)
                           (1 'highlight-80+ prepend))))

(defun highlight-80+-format ()
  (format "^[^\n]\\{%d\\}\\([^\n]*\\)$" highlight-80+-columns))

(defvar highlight-80+-last-width 0)
(make-variable-buffer-local 'highlight-80+-last-width)

(defvar highlight-80+-last-keywords "")
(make-variable-buffer-local 'highlight-80+-last-keywords)

(defun highlight-80+-matcher (limit)
  ;; Update search when `tab-width' has changed.
  (unless (equal highlight-80+-last-width tab-width)
    (highlight-80+-reset)
    ;; The rest of the buffer can't be right, either.
    (let ((font-lock-keywords))
      (font-lock-fontify-buffer)))
  ;; re-search-forward is C and much faster checking columns ourselves
  (re-search-forward highlight-80+-last-keywords nil t))

(defun highlight-80+-reset ()
  (setq highlight-80+-last-keywords (highlight-80+-format)
        highlight-80+-last-width tab-width))

;;;###autoload
(define-minor-mode highlight-80+-mode
  "Highlight the portions of lines longer than 80 characters."
  nil " 80+" nil
  (if highlight-80+-mode
      (progn
        (font-lock-add-keywords nil highlight-80+-keywords t)
        (add-hook 'hack-local-variables-hook #'highlight-80+-reset t t))
    (font-lock-remove-keywords nil highlight-80+-keywords)
    (kill-local-variable 'highlight-80+-last-keywords)
    (kill-local-variable 'highlight-80+-last-width))
  (font-lock-fontify-buffer))

(provide 'highlight-80+)
;;; highlight-80+.el ends here
