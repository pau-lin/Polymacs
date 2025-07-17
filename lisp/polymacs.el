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

(provide 'polymacs)

(defcustom polymacs-resources-directory (expand-file-name "~/polymacs-resources/")
  "Default path to Polymacs resource files."
  :type 'directory
  :group 'polymacs)

(defconst polymacs-pkg-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Path to Polymacs source code.")

(defvar polymacs--hidden-overlays '()
  "List of overlays currently hiding text.")

(defun polymacs--wrap-region-with-markers (start end)
  "Insert `{{` and `}}` around region from START to END.
Returns new START and END positions (excluding the markers)."
  (save-excursion
    (goto-char end)
    (insert "}=")
    (goto-char start)
    (insert "={"))
  (list (+ start 2) (+ end 2)))

(defun polymacs-hide-region (start end &optional placeholder)
  "Hide region between START and END with PLACEHOLDER and wrap in {{...}} markers."
  (interactive "r")
  (let* ((ph (or placeholder "..."))
         (new-pos (polymacs--wrap-region-with-markers start end))
         (s (nth 0 new-pos))
         (e (nth 1 new-pos))
         (ov (make-overlay s e)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'display ph)
    (overlay-put ov 'polymacs-hidden t)
    (overlay-put ov 'modification-hooks
                 (list (lambda (o &rest _) (delete-overlay o))))
    (push ov polymacs--hidden-overlays)
    (message "Text hidden")))

(defun polymacs-show-all-hidden ()
  "Remove all overlays hiding text, leaving the {{...}} markers intact."
  (interactive)
  (dolist (ov polymacs--hidden-overlays)
    (when (overlay-get ov 'polymacs-hidden)
      (delete-overlay ov)))
  (setq polymacs--hidden-overlays nil)
  (message "All hidden text revealed"))

(defun polymacs-hide-all-marked-regions (&optional placeholder)
  "Scan buffer for `{{...}}` blocks and apply hiding overlays.
Does not affect existing overlays."
  (interactive)
  (let ((ph (or placeholder "...")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "={" nil t)
        (let ((start (point)))
          (when (search-forward "}=" nil t)
            (let* ((end (- (point) 2)) ;; exclude "}}"
                   (ov (make-overlay start end)))
              (overlay-put ov 'invisible t)
              (overlay-put ov 'display ph)
              (overlay-put ov 'polymacs-hidden t)
              (overlay-put ov 'modification-hooks
                           (list (lambda (o &rest _) (delete-overlay o))))
              (push ov polymacs--hidden-overlays)))))))
  (message "All blocks hidden"))

(cl-defstruct polymacs-source
  title
  url
  file-path)

(define-minor-mode polymacs-mode
  "Mode mineur pour activer les fonctions Polymacs spécifiques."
  :lighter " Poly")

(defun polymacs--maybe-enable-mode ()
  "Active `polymacs-mode` si le fichier courant est dans `polymacs-source-folder`."
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name polymacs-resources-directory)
                              (expand-file-name buffer-file-name)))
    (polymacs-mode 1)))

(add-hook 'find-file-hook #'polymacs--maybe-enable-mode)

(defun polymacs--extract-html-title (html)
    (when (string-match "<title>\\(.*?\\)</title>" html)
    (let ((raw-title (match-string 1 html)))
      (string-trim raw-title))))

(defun polymacs-region-contains-non-top-level-headings-p ()
  "Retourne t si la région active contient des headings Org,
mais aucun de niveau 1 (i.e. aucune ligne ne commence par '* ')."
  (if (use-region-p)
      (save-excursion
        (let ((start (region-beginning))
              (end (region-end))
              (found-heading nil)
              (has-top-level nil))
          ;; Positionner un caractère avant la région pour capturer le heading de la 1ère ligne
          (goto-char (max (point-min) (1- start)))
          (while (re-search-forward "^\\*+ " end t)
            (setq found-heading t)
            (when (save-excursion
                    (beginning-of-line)
                    (looking-at "^\\* "))
              (setq has-top-level t)))
          (and found-heading (not has-top-level))))
    nil))

(defun polymacs-remove-empty-org-targets ()
  "Supprime toutes les occurrences de <<>> dans le buffer actuel."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<<>>" nil t)
      (replace-match "" nil t))))

(defun polymacs-align-all-org-tables ()
  "Aligne chaque tableau du buffer une seule fois, de façon sécurisée."
  (save-excursion
    (goto-char (point-min))
    (with-silent-modifications
      (let ((in-table nil))
        (while (not (eobp))
          (let ((line (thing-at-point 'line t)))
            (cond
             ;; Ligne de tableau
             ((string-match-p "^\\s-*|" line)
              (unless in-table
                (when (org-at-table-p)
                  (ignore-errors (org-table-align)))
                (setq in-table t)))
             ;; Ligne vide ou pas un tableau → on reset l'état
             (t (setq in-table nil))))
          (forward-line 1))))))

(defun polymacs-html-to-org--callback (status url)
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move) ;; Trouve la ligne vide après les headers
  (forward-char)
    (let* ((html-buffer (generate-new-buffer "*html*"))
	(bs4-buffer "*bs4*")
	(org-buffer (get-buffer-create "*polymacs-org*"))
        (html (buffer-substring-no-properties (point) (point-max)))
        (title (org-roam-node-slug (org-roam-node-create :title (polymacs--extract-html-title html))))
	(doc (make-polymacs-source
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
      (when (polymacs-region-contains-non-top-level-headings-p) 
	(org-do-promote))
      (deactivate-mark)
      (polymacs-remove-empty-org-targets)
      (polymacs-align-all-org-tables)
      (switch-to-buffer (current-buffer)))
      (org-mode))))

(defun polymacs-html-to-org (url)
  "Parse a html page to org format."
  (interactive "sURL: ")
  (url-retrieve url #'polymacs-html-to-org--callback (list url)))

(defun polymacs-html-to-org-at-point ()
  "Appelle `polymacs-html-to-org` avec le lien à point, s’il y en a un."
  (interactive)
  (let ((url (or (org-element-property :raw-link (org-element-context))
                 (thing-at-point 'url))))
    (if (and url (string-match-p "^https?://" url))
        (polymacs-html-to-org url)
      (message "Aucun lien valide à point."))))

(defun polymacs-browse-current-buffer ()
  "Ouvre l’URL associée au document courant dans un navigateur."
  (interactive
   (unless polymacs-mode
     (user-error "This command is only available when `polymacs-mode` is active")))
  (browse-url (polymacs-source-url polymacs--last-document)))

(defun polymacs-register-source ()
  "Register selected buffer as source-file for polymacs : add it to the db and gain access to polymacs functionnalities"
  (interactive)
  (write-file (polymacs-source-file-path polymacs--last-document)))
