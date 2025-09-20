;;; -*- lexical-binding: t -*-


;;;;; Set up which package repos we connect to, and make all third-party packages visible to
;;;;; e.g. `require'.

;;; Make the concept of packages available!
(package-initialize)

(defgroup package-connections nil
  "Package listing and internet connection settings."
  :group 'my-customizations)

(defcustom internet-check-url "https://codeberg.org/cosmicexplorer/helm-rg"
  "Defaults to my own project URL on Codeberg."
  :type 'string
  :group 'package-connections)

(defun internet-connected-p ()
  "Ping `internet-check-url' to quickly gauge whether we have any internet.

This is expected to fail much faster than trying to ping the package server itself, so it's a good
early exit mechanism e.g. when traveling without wifi."
  (zerop
   (call-process "ping" nil nil nil
                 internet-check-url "-c1")))

;;; Add ELPA and MELPA.
(defcustom ping-melpa t
  "Whether to add MELPA to `package-archives' when searching for new packages."
  :type 'boolean
  :group 'package-connections)

(when ping-melpa
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(defcustom ping-org-elpa t
  "Whether to add org elpa to `package-archives' when searching for new packages."
  :type 'boolean
  :group 'package-connections)

(when ping-org-elpa
  ;; This archive will occasionally time out! >='[
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))


;;;;; Persist the installed packages data to a customizable file path.
;;;;; Do a package refresh, if possible!

(defcustom package-sync-on-init t
  "Whether to try to refresh packages when starting up emacs."
  :type 'boolean
  :group 'package-connections)

;;; When we initialize, ensure we have all the packages from this file installed.
(when (and package-sync-on-init (internet-connected-p))
  (package-refresh-contents))
