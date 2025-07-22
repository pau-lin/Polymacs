;;; polymacs.el --- Incremental learning for self-study -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025 P.M

;; Author: P.M (contact via github)
;; Version: 0.1
;; URL: https://github.com/pau-lin/polymacs
;; Package-Requires: ((emacs "26.1") (org "9.6"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Polymacs is a SuperMemo-inspired Emacs package intended for
;; self-learners. SuperMemo (https://www.super-memory.com/) is a
;; learning method and software developed by Piotr Woźniak since 1985,
;; which has introduced and implemented many techniques for lifelong
;; and autodidactic learners, such as incremental learning and spaced
;; repetition. We have adapted some of these techniques into the Emacs
;; environment, taking advantage of its rich ecosystem.
;;
;; Polymacs aims to be fast, scalable, future-proof, and modular,
;; providing a robust open-source solution for self-taught learners,
;; adaptable to their individual needs.

;;; Code:

(require 'polymacs-resource)
(require 'polymacs-mode)

;;; Options
(defgroup polymacs nil
  "Incremental learning for self-study"
  :group 'applications
  :prefix "polymacs-"
  :link '(url-link :tag "Github" "https://github.com/pau-lin/polymacs"))

(defcustom polymacs-resources-directory (expand-file-name "~/polymacs-resources/")
  "Path to directory containing polymacs resource files."
  :type 'directory
  :group 'polymacs)

;;; Variables
(defconst polymacs-pkg-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Path to Polymacs source code.")

;;; Installation
(defun polymacs-install ()
  "Install dependencies of polymacs when installed from version control."
  (interactive)
  (let* ((default-directory (file-name-directory (or load-file-name buffer-file-name)))
         (script-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "../scripts/install.ps1"
                         "../scripts/install.sh")
                       default-directory)))
    (if (file-exists-p script-path)
        (if (eq system-type 'windows-nt)
            ;; Windows: use PowerShell to run install.ps1
            (shell-command (concat "powershell -ExecutionPolicy Bypass -File "
                                   (shell-quote-argument script-path)))
          ;; Unix: run the shell script
          (shell-command (concat "bash " (shell-quote-argument script-path))))
      (message "Install script not found: %s" script-path))))

;;; Parsing functions
(defun polymacs-html-to-org (url)
  "Parse a html page to org format."
  (interactive "sURL: ")
  (url-retrieve url #'polymacs-html-to-org--callback (list url)))

(defun polymacs-html-to-org-at-point ()
  "Call `polymacs-html-to-org` with http/https link at point"
  (interactive)
  (let ((url (or (org-element-property :raw-link (org-element-context))
                 (thing-at-point 'url))))
    (if (and url (string-match-p "^https?://" url))
      (polymacs-html-to-org url)
      (message "No valid link at point."))))

(defun polymacs-html-to-org--callback (status url)
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move) ;; Remove headers
  (forward-char)
    (let* ((html-buffer (generate-new-buffer "*html*"))
	(bs4-buffer "*bs4*")
	(org-buffer (get-buffer-create "*polymacs-org*"))
        (html (buffer-substring-no-properties (point) (point-max)))
        (title (org-roam-node-slug (org-roam-node-create :title (polymacs--extract-html-title html))))
	(doc (make-polymacs-resource
              :title title
              :url url
              :file-path (concat polymacs-resources-directory (format-time-string "%Y%m%d%H%M%S-") title".org"))))
	 (setq polymacs--last-document doc)
      (with-current-buffer org-buffer
	(erase-buffer))
      (with-current-buffer html-buffer
	(insert html)
      (if (string-match-p "wikipedia.org" url)
	  (call-process-region (point-min) (point-max) (expand-file-name "../env/bin/python3" polymacs-pkg-directory) nil bs4-buffer nil (expand-file-name "../scripts/parse_bs4_wiki.py" polymacs-pkg-directory) url)
	  (call-process-region (point-min) (point-max) (expand-file-name "../env/bin/python3" polymacs-pkg-directory) nil bs4-buffer nil (expand-file-name "../scripts/parse_bs4.py" polymacs-pkg-directory) url))
      (kill-buffer html-buffer))
    (with-current-buffer bs4-buffer
      (call-process-region (point-min) (point-max) "pandoc" nil org-buffer nil "--wrap=none" "-f" "html" "-t" "org")
      (kill-buffer bs4-buffer))
    (with-current-buffer org-buffer
      (org-mode)
      (org-overview)
      (with-silent-modifications
      (org-delete-property-globally "CUSTOM_ID")
      (goto-char (point-min))
      (push-mark (point-max) nil t)
      (while (polymacs--region-contains-non-top-level-headings-p) 
	(org-do-promote))
      (deactivate-mark)
      (polymacs--remove-empty-org-targets)
      (polymacs--fix-org-duplicate-http-links)
      (polymacs--remove-caption-string)
      (polymacs--remove-nbsp)
      (polymacs--clean-org-superscript)
      (polymacs--fix-ref-link)
      (polymacs--align-all-org-tables)
      (switch-to-buffer (current-buffer)))
      (org-mode))))

;;;; Tool parsing functions
(defun polymacs--clean-org-superscript ()
  "Remove ^{...} markup."
  (save-excursion
    (goto-char (point-min))

    (while (re-search-forward "\\^\\({\\([^}]+\\)}\\)" nil t)
      (replace-match "\\2"))))

(defun polymacs--fix-ref-link ()
  "Fix Ref link."
  (save-excursion
    (goto-char (point-min))
    
    (while (re-search-forward
            "\\[\\[\\(https?://[^]]+\\)\\]\\[\\[\\([0-9]+\\)\\]\\]\\]" nil t)
      (let ((url (match-string 1))
            (num (match-string 2)))
        (replace-match (format "[[%s][(%s)]]" url num) t t)))))

(defun polymacs--remove-nbsp ()
  "Remove all NO-BREAK SPACE (U+00A0) characters from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\u00A0" nil t)
      (replace-match " "))))

(defun polymacs--remove-caption-string ()
  "Remove '#+caption' string in resource buffer to correctly display org links"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+caption: " nil t)
      (replace-match ""))))

(defun polymacs--fix-org-duplicate-http-links ()
  "Replace Org links like [[http...][http...]] with a single [[http...]] using the second URL."
  (save-excursion
    (goto-char (point-min))
        (while (re-search-forward
            "\\[\\[\\(https?://[^]]+\\)\\]\\[\\[\\[\\(https?://[^]]+\\)\\]\\]\\]\\]" nil t)
      (let ((url1 (match-string 1))
            (url2 (match-string 2)))
        (replace-match (format "[[%s][image]]" url2) t t)))))

(defun polymacs--extract-html-title (html)
  "Extract html title from a html string."
    (when (string-match "<title>\\(.*?\\)</title>" html)
    (let ((raw-title (match-string 1 html)))
      (string-trim raw-title))))

(defun polymacs--region-contains-non-top-level-headings-p ()
  "Return t if marked region contains org headings but not of level 1."
  (if (use-region-p)
      (save-excursion
        (let ((start (region-beginning))
              (end (region-end))
              (found-heading nil)
              (has-top-level nil))
          (goto-char (max (point-min) (1- start)))
          (while (re-search-forward "^\\*+ " end t)
            (setq found-heading t)
            (when (save-excursion
                    (beginning-of-line)
                    (looking-at "^\\* "))
              (setq has-top-level t)))
          (and found-heading (not has-top-level))))
    nil))

(defun polymacs--remove-empty-org-targets ()
  "Delete all <<>> occurences in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<<>>" nil t)
      (replace-match "" nil t))))

(defun polymacs--align-all-org-tables ()
  "Align and repair all Org tables."
  (save-excursion
    (goto-char (point-min))
    (with-silent-modifications
      (let ((in-table nil))
        (while (not (eobp))
          (let ((line (thing-at-point 'line t)))
            (cond
             ((string-match-p "^\\s-*|" line)
              (unless in-table
                (when (org-at-table-p)
                  (ignore-errors (org-table-align)))
                (setq in-table t)))
             (t (setq in-table nil))))
          (forward-line 1))))))

;;; Navigation
(defun polymacs-browse-current-buffer ()
  "Open the URL associated with the current document in a web browser."
  (interactive)
   (unless polymacs-mode
     (user-error "This command is only available when `polymacs-mode` is active."))
  (if polymacs--last-document
      (browse-url (polymacs-resource-url polymacs--last-document))
    (message "Not in a resource buffer.")))

(provide 'polymacs)
