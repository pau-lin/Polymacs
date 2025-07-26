;;; polymacs-file.el --- Polymacs file and parsing management -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025 P.M

;; Author: P.M (contact via github)
;; Version: 0.1
;; URL: https://github.com/pau-lin/polymacs
;; Package-Requires: ((emacs "26.1") (org "9.6"))

;; This file is not part of GNU Emacs.

;; This program ins free software; you can redistribute it and/or modify
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
;; Handle evrything related to file management, parsing, ...

;;; Code:

(require 'polymacs)

;;; File manipulation

;; Adapted from "org-roam-node-slug" in org-roam-node.el 2.3.1
(defun polymacs-slugify (title)
  "Return a slug from title."
  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                      ("__*" . "_")                   ;; remove sequential underscores
                      ("^_" . "")                     ;; remove starting underscore
                      ("_$" . "")))                   ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

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

(defun polymacs-html-to-org--callback (_status url)
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move) ;; Remove headers
  (forward-char)
    (let* ((html-buffer (generate-new-buffer "*html*"))
	(bs4-buffer "*bs4*")
	(org-buffer (get-buffer-create "*polymacs-org*"))
        (html (buffer-substring-no-properties (point) (point-max)))
        (title (polymacs--extract-html-title html))
	(slug (polymacs-slugify title))
	(doc (make-polymacs-resource
              :title title
              :url url
              :file-path (concat polymacs-resources-directory (format-time-string "%Y%m%d%H%M%S-") slug".org"))))
	 (setq polymacs--last-document doc)
      (with-current-buffer org-buffer
	(erase-buffer))
      (with-current-buffer html-buffer
	(insert html)
      (if (string-match-p "wikipedia.org" url)
	  (call-process-region (point-min) (point-max) (expand-file-name "env/bin/python3" polymacs-pkg-directory) nil bs4-buffer nil (expand-file-name "scripts/parse_bs4_wiki.py" polymacs-pkg-directory) url)
	  (call-process-region (point-min) (point-max) (expand-file-name "env/bin/python3" polymacs-pkg-directory) nil bs4-buffer nil (expand-file-name "scripts/parse_bs4.py" polymacs-pkg-directory) url))
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
      (switch-to-buffer (current-buffer))
      (org-open-line 2)
      (insert (concat "#+title: " title))
      (forward-line 1)
      (insert (concat "#+url: " url))
      (newline)
      (org-mode)))))

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
  "Remove all NO-BREAK SPACE (U+00A0) characters from the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\u00A0" nil t)
      (replace-match " "))))

(defun polymacs--remove-caption-string ()
  "Remove '#+caption' string in resource buffer to correctly display
org links"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+caption: " nil t)
      (replace-match ""))))

(defun polymacs--fix-org-duplicate-http-links ()
  "Replace Org links like [[http...][http...]] with a single
[[http...]] using the second URL."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\[\\[\\(https?://[^]]+\\)\\]\\[\\[\\[\\(https?://[^]]+\\)\\]\\]\\]\\]" nil t)
      (let ((_url1 (match-string 1))
            (url2 (match-string 2)))
        (replace-match (format "[[%s][image]]" url2) t t)))))

(defun polymacs--extract-html-title (html)
  "Extract html title from a html string."
  (with-temp-buffer
    (insert html)
    (set-buffer-file-coding-system 'utf-8)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (when (string-match "<title>\\(.*?\\)</title>" (buffer-string))
      (let ((raw-title (match-string 1 (buffer-string))))
        (string-trim raw-title)))))

(defun polymacs--region-contains-non-top-level-headings-p ()
  "Return t if marked region contains org headings but not of level
1."
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

(provide 'polymacs-file)
