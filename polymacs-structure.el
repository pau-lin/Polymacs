;;; polymacs-structure.el --- Resources structure creation and manipulation -*- coding: utf-8; lexical-binding: t; -*-

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
;; This file implements tools to manipulate resources files
;; structures, manipualting topics (as in superMemo), generic extracts
;; and more.

;;; Commentary:

;;; Code:

(require 'polymacs)

(defun polymacs-create-extract ()
  "Transform either current heading, (future version: blocks or selection mark) into a polymacs-extract,
adding an ID, and an entry to the polytree."
  (interactive)
  (if (polymacs--is-resources-p)
      (cond ((org-at-heading-p) (polymacs--create-heading-extract))
	    (t (message "Not on a valid heading")))))

(defun polymacs--extract-heading ()
  "Extract current heading"
  (org-id-get-create))
;; Recursively go up to the root polynode to determine under which polynode to place the newly created one

;; Polytree
(defun polymacs--polytree-get-create (CREATE)
  "Return current buffer polytree or create it if it doesen't exists. If not in a polymacs resources buffer, return nil. If create equal t, create the polytree."
  (if (polymacs--is-resources-p)
      (let ((polytree (seq-find (lambda (el)
		  (string= (org-element-property :POLYTREE el) "t"))
				(org-element-map (org-element-parse-buffer) 'headline #'identity))))
	(if polytree
	    polytree
	  (if CREATE (polymacs--polytree-create))))
    ()))

(defun polymacs--polytree-create ()
  "Create a Polytree heading at the end of the buffer if it doesn't already exist."
  (when (and (not (polymacs--polytree-exists-p))
             (polymacs--is-resources-p))
    (goto-char (point-min))
    (let ((id (polymacs--get-current-topic-id)))
    (goto-char (point-max))
    (org-insert-heading nil nil 1)
    (insert "Polytree")
    (org-set-property "POLYTREE" "t")
    (polymacs--polynode-add-linkid id))))

(defun polymacs--get-current-topic-id ()
  "Return the ID of current topic"
  (org-entry-get (point) "ID"))

(defun polymacs--get-current-linkid ()
  "Return the ID of current topic"
  (org-entry-get (point) "LINKID"))

(defun polymacs--polynode-add-linkid (linkid)
  "Add the ID of the current topic node to the :LINKID: property of the Polymacs node at point."
  (org-set-property "LINKID" linkid))

(defun polymacs--polytree-exists-p ()
  "Return t if polytree exists"
  (if (polymacs--polytree-get-create) t nil))

;(defun polymacs--polytree-add-node (&optional PARENT-ID)
;  "Add a new node under PARENT node in the polytree, or behind root-node by default"
					;)

(defun polymacs-go-to-topic ()
  "Go from a polynode at point to the linked ID."
  (interactive)
  (let* ((id (polymacs--get-current-linkid))
         (org-data (org-element-parse-buffer 'headline))
         (target-pos nil))
    (if id
        (progn
	  (goto-char (point-min))
	  (if (polymacs--get-current-topic-id)
	      (setq target-pos (point-min)))
	  (if (not target-pos)
          (org-element-map org-data 'headline
            (lambda (hl)
              (when (string= (org-element-property :ID hl) id)
                (setq target-pos (org-element-property :begin hl))))))
          (if target-pos
              (goto-char target-pos)
            (message "ID not found in any headline")))
      (message "No LINKID found at point"))))

(defun polymacs-go-to-polynode ()
  "Go from a topic at point to the polynode."
  (interactive)
  (let* ((id (polymacs--get-current-topic-id))
         (org-data (org-element-parse-buffer 'headline))
         (target-pos nil))
    (if id
          (org-element-map org-data 'headline
            (lambda (hl)
              (when (string= (org-element-property :LINKID hl) id)
                (setq target-pos (org-element-property :begin hl))))))
          (if target-pos
              (goto-char target-pos)
            (message "LINKID not found in any headline")
      (message "No ID found at point"))))

(provide 'polymacs-structure)



