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
(eval-when-compile (require 'cl-lib))

(declare-function org-remove-indentation "org")
(declare-function run-coffee "ext:coffee" (cmd &optional dedicated show))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("coffee" . "coffee"))

(defvar org-babel-default-header-args:coffee nil)

(defcustom org-babel-coffee-command "coffee"
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:coffee (body params)
  )

(provide 'ob-coffee)
