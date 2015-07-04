;;; ob-coffee.el --- org-babel functions for coffeescript evaluation

;; Authors: Danny McClanahan
;; Keywords: literate programming, reproducible research

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating coffeescript source code.

;;; Code:
(require 'ob)
(require 'cl-lib)

(declare-function run-coffee "ext:coffee" (cmd &optional dedicated show))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("coffee" . "coffee"))

(defvar org-babel-default-header-args:coffee nil)

(defcustom org-babel-coffee-command "coffee -s"
  "Name of command to execute coffeescript code."
  :group 'org-babel
  :type 'string)

;;; sessions are annoying
(defun org-babel-execute:coffee (body params)
    "Execute a block of coffeescript code with org-babel.
This function is called by `org-babel-execute-src-block'"
    (let ((org-babel-coffee-cmd (or (cdr (assoc :cmd params))
                                    org-babel-coffee-command))
          (result-type (cdr (assoc :result-type params)))
          (full-body (org-babel-expand-body:generic
                      body params
                      (org-babel-variable-assignments:coffee params))))
        (format "%S|%S" full-body params)))

(defun org-babel-coffee-var-to-coffee (var)
  "Convert VAR into a coffeescript variable. Convert an elisp value into a
string of coffeescript source code specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-coffee-var-to-coffee var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-variable-assignments:coffee (params)
  "Return list of coffeescript statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s = %s") (car pair) (org-babel-coffee-var-to-coffee (cdr pair)))
   (mapcar #'cdr (org-babel-get-header params :var))))

(provide 'ob-coffee)
