;;; polymacs-mode.el --- Major mode for special Polymacs buffer -*- coding: utf-8; lexical-binding: t; -*-

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

(defvar polymacs-resources-directory)

(define-minor-mode polymacs-mode
  "Mode mineur pour activer les fonctions Polymacs spécifiques."
  :lighter " Poly")

(defun polymacs--maybe-enable-mode ()
  "Active `polymacs-mode` si le fichier courant est dans
`polymacs-resource-folder`."
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name polymacs-resources-directory)
                              (expand-file-name buffer-file-name)))
    (polymacs-mode 1)))

(add-hook 'find-file-hook #'polymacs--maybe-enable-mode)

(provide 'polymacs-mode)
