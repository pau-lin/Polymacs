;;; polymacs-resource.el --- Resource related code -*- coding: utf-8; lexical-binding: t; -*-

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

(cl-defstruct polymacs-new-resource
  title
  url
  slug
  temp-buffer-name
  file-path)

(defun polymacs-register-resource ()
  "Register the current buffer as a resource file: save it in polymacs-resources-directory
gain access to polymacs functionalities."
  (interactive)
  (let ((resource (cl-find-if
                   (lambda (res)
                     (string= (polymacs-new-resource-temp-buffer-name res)
                              (buffer-name)))
                   polymacs--new-resources-cache)))
    (if resource
        (let ((file-path (polymacs-new-resource-file-path resource)))
          (write-file file-path)
          (find-file file-path)
          (org-id-get-create))
      (message "Not in a unregistered resource buffer."))))

(provide 'polymacs-resource)
