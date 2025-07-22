;;; polymacs-review.el --- Review-related functionalities -*- coding: utf-8; lexical-binding: t; -*-

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

;;; Code:

(require 'polymacs)

;; (defvar polymacs--hidden-overlays '()
;;   "List of overlays currently hiding text.")

;; (defun polymacs--wrap-region-with-markers (start end)
;;   "Insert `{{` and `}}` around region from START to END.
;; Returns new START and END positions (excluding the markers)."
;;   (save-excursion
;;     (goto-char end)
;;     (insert "}=")
;;     (goto-char start)
;;     (insert "={"))
;;   (list (+ start 2) (+ end 2)))

;; (defun polymacs-hide-region (start end &optional placeholder)
;;   "Hide region between START and END with PLACEHOLDER and wrap in {{...}} markers."
;;   (interactive "r")
;;   (let* ((ph (or placeholder "..."))
;;          (new-pos (polymacs--wrap-region-with-markers start end))
;;          (s (nth 0 new-pos))
;;          (e (nth 1 new-pos))
;;          (ov (make-overlay s e)))
;;     (overlay-put ov 'invisible t)
;;     (overlay-put ov 'display ph)
;;     (overlay-put ov 'polymacs-hidden t)
;;     (overlay-put ov 'modification-hooks
;;                  (list (lambda (o &rest _) (delete-overlay o))))
;;     (push ov polymacs--hidden-overlays)
;;     (message "Text hidden")))

;; (defun polymacs-show-all-hidden ()
;;   "Remove all overlays hiding text, leaving the {{...}} markers intact."
;;   (interactive)
;;   (dolist (ov polymacs--hidden-overlays)
;;     (when (overlay-get ov 'polymacs-hidden)
;;       (delete-overlay ov)))
;;   (setq polymacs--hidden-overlays nil)
;;   (message "All hidden text revealed"))

;; (defun polymacs-hide-all-marked-regions (&optional placeholder)
;;   "Scan buffer for `{{...}}` blocks and apply hiding overlays.
;; Does not affect existing overlays."
;;   (interactive)
;;   (let ((ph (or placeholder "...")))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward "={" nil t)
;;         (let ((start (point)))
;;           (when (search-forward "}=" nil t)
;;             (let* ((end (- (point) 2)) ;; exclude "}}"
;;                    (ov (make-overlay start end)))
;;               (overlay-put ov 'invisible t)
;;               (overlay-put ov 'display ph)
;;               (overlay-put ov 'polymacs-hidden t)
;;               (overlay-put ov 'modification-hooks
;;                            (list (lambda (o &rest _) (delete-overlay o))))
;;               (push ov polymacs--hidden-overlays)))))))
;;   (message "All blocks hidden"))

(provide 'polymacs-review)
