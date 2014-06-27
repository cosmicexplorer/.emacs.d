;;; dired-xattr.el --- Handle MacOSX Finder color label in dired

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-09
;; Last changed: 2013-01-10 15:58:16
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  To use dired-xattr:
;;
;;  (require 'dired-xattr)
;;  (add-hook 'dired-after-readin-hook 'dired-xattr-add-overlay)

;;; Code:


(eval-when-compile
  (require 'cl)
  (require 'tramp)
  (require 'dired)
  (require 'ucs-normalize))

(defgroup dired-xattr nil
  "Manipulate MacOSX extanded attributes in Dired."
  :group 'dired)

(defcustom dired-xattr-attribute-6 "#fe7e70"
  "Color to use for red label."
  :group 'dired-xattr
  :type 'color)

(defcustom dired-xattr-attribute-7 "#f9b854"
  "Color to use for orange label."
  :group 'dired-xattr
  :type 'color)

(defcustom dired-xattr-attribute-5 "#f3df58"
  "Color to use for yellow label."
  :group 'dired-xattr
  :type 'color)


(defcustom dired-xattr-attribute-2 "#c1db59"
  "Color to use for green label."
  :group 'dired-xattr
  :type 'color)

(defcustom dired-xattr-attribute-4 "#6bb5ff"
  "Color to use for blue label."
  :group 'dired-xattr
  :type 'color)

(defcustom dired-xattr-attribute-3 "#cda3e0"
  "Color to use for purple label."
  :group 'dired-xattr
  :type 'color)

(defcustom dired-xattr-attribute-1 "#b8b8b8"
  "Color to use for gray label."
  :group 'dired-xattr
  :type 'color)

(defcustom dired-xattr-mdls-bin (executable-find "mdls")
  "Path to mdls(1) executable."
  :type 'string
  :group 'dired-xattr)

(defcustom dired-xattr-max-buffer-lines 300
 "Buffer lines limit before `dired-xattr-add-overlay' is skipped."
  :type 'interger
  :group 'dired-xattr)
  

(defcustom dired-xattr-mdls-attributes
  '("kMDItemContentCreationDate"
    "kMDItemContentModificationDate"
    "kMDItemContentType"
    "kMDItemContentTypeTree"
    "kMDItemDateAdded"
    "kMDItemDisplayName"
    "kMDItemFSContentChangeDate"
    "kMDItemFSCreationDate"
    "kMDItemFSCreatorCode"
    "kMDItemFSFinderFlags"
    "kMDItemFSHasCustomIcon"
    "kMDItemFSInvisible"
    "kMDItemFSIsExtensionHidden"
    "kMDItemFSIsStationery"
    "kMDItemFSLabel"
    "kMDItemFSName"
    "kMDItemFSNodeCount"
    "kMDItemFSOwnerGroupID"
    "kMDItemFSOwnerUserID"
    "kMDItemFSSize"
    "kMDItemFSTypeCode"
    "kMDItemKind"
    "kMDItemLogicalSize"
    "kMDItemPhysicalSize")
  "List of properties to retrieve with `dired-xattr-mdls-bin'."
  :type 'list
  :group 'dired-xattr)

(defconst dired-xattr-labels
  '(:1 "Grey"
    :2 "Green"
    :3 "Purple"
    :4 "Blue"
    :5 "Yellow"
    :6 "Red"
    :7 "Orange")
  "Use to convert label number to name")

(defstruct dired-xattr
  kMDItemContentCreationDate
  kMDItemContentModificationDate
  kMDItemContentType
  kMDItemContentTypeTree
  kMDItemDateAdded
  kMDItemDisplayName
  kMDItemFSContentChangeDate
  kMDItemFSCreationDate
  kMDItemFSCreatorCode
  kMDItemFSFinderFlags
  kMDItemFSHasCustomIcon
  kMDItemFSInvisible
  kMDItemFSIsExtensionHidden
  kMDItemFSIsStationery
  kMDItemFSLabel
  kMDItemFSName
  kMDItemFSNodeCount
  kMDItemFSOwnerGroupID
  kMDItemFSOwnerUserID
  kMDItemFSSize
  kMDItemFSTypeCode
  kMDItemKind
  kMDItemLogicalSize
  kMDItemPhysicalSize)


(defun dired-xattr-get-from-dir (dir &optional attrs)
  "Read attributes for all files in DIR. Return an ALIST having
the filename as key ans a `dired-xattr' structure as value."
  (let* ((attrs (or attrs dired-xattr-mdls-attributes))
	 (size (length attrs))
	 (attrs-func (loop for x in attrs
			   collect (intern (concat "dired-xattr-" x))))
	 (attrs-int (loop for x in attrs
			  collect (intern (concat ":" x))))
	 (data (split-string
		(ucs-normalize-NFC-string
		 (shell-command-to-string
		  (format "%s -name %s -raw %s/*"
			  dired-xattr-mdls-bin
			  (mapconcat #'identity attrs " -name ")
			  (shell-quote-argument (expand-file-name dir))))
		 "\0")))
	 (ret (make-hash-table :test 'equal)))
    (loop for items on data by (lambda (x) (subseq x size))
	  for details-plist = (loop for i below size
				   nconc (list (nth i attrs-int)
					       (nth i items)))
	  for details = (apply 'make-dired-xattr details-plist)
	  do  (puthash (dired-xattr-kMDItemFSName details) details ret))
    ret))

;;;###autoload
(defun dired-xattr-add-overlay (&optional force)
  "Colorize the perm column from current `dired' buffer according
MacOSX Finder label colors.

Does nothing if `dired-default-directory' is not a local
directory of if buffer has more that
`dired-xattr-max-buffer-lines' lines."
  (interactive)
  (let ((lines (count-lines (point-min) (point-max)))
	(force (or force current-prefix-arg)))
    (when (and
	   (not
	    (tramp-file-name-real-host
	     (or (ignore-errors (tramp-dissect-file-name
				 (dired-default-directory)))
		 (tramp-dissect-file-name
		  (concat "/:" (dired-default-directory)) 1))))
	   (or force
	       (< lines dired-xattr-max-buffer-lines)))

      (let ((xattrs (dired-xattr-get-from-dir
		     (dired-default-directory)
		     '("kMDItemFSLabel" "kMDItemFSName")))
	    (inhibit-read-only t))
	(save-excursion
	  (goto-char (point-min))
	  (dired-initial-position (dired-default-directory))
	  (while (< (point) (point-max))
	    (let* ((fnap (dired-file-name-at-point))
		   (filename (file-name-nondirectory
			      (if (file-directory-p fnap)
				  (substring fnap 0 -1)
				(dired-file-name-at-point))))
		   (attr (gethash filename xattrs))
		   (label (if attr
			      (dired-xattr-kMDItemFSLabel attr)
			    "0"))
		   (face (intern
			  (format "dired-xattr-attribute-%s"
				 label )))
		   (beg (+ 1 (point-at-bol)))
		   (end (+ 2 (point-at-bol)))
		   (ov (make-overlay beg end)))
	      (when (boundp face)
		(overlay-put ov 'face (cons 'background-color (eval face)))
		(add-text-properties
		 beg end
		 `(help-echo
		   ,(format "Label: %s"
			    (plist-get dired-xattr-labels
				       (intern (concat ":" label))))))))
	    (forward-line 1)))))))

(provide 'dired-xattr)

;; dired-xattr.el ends here
