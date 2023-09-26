;;; org-dynamic-agenda.el --- Build your agenda as you work  -*- lexical-binding: t -*-

;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: data, files, tools
;; URL: https://git.sr.ht/~ngraves/org-dynamic-agenda.el

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Build your agenda as you work

;;; Code:
(require 'org-agenda)
(require 'org-element)
(require 'cl-lib)

;;; Common

(defun org-dynamic-agenda-update-file (&optional file)
  "Update variable `org-agenda-files'.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE."
  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
    (let ((files (org-agenda-files)))
      (if (org-dynamic-agenda-file-p file)
          (cl-pushnew (file-truename (buffer-file-name)) files
                      :test #'string-equal)
        (setq files (cl-delete (file-truename (buffer-file-name)) files
                               :test #'string-equal)))
      (org-store-new-agenda-file-list files))))

(defun org-dynamic-agenda-cleanup-files (&optional full)
  "Cleanup variable `org-agenda-files'.

If FULL, rechecks the files with `org-dynamic-agenda-file-p'."
  (org-store-new-agenda-file-list
   (cl-remove-if-not (if full #'org-dynamic-agenda-file-p
                       #'file-readable-p)
                     (org-agenda-files))))

(defun org-dynamic-agenda-predicate ()
  "Check if the file should be added to the variable `org-agenda-files'."
   (org-element-map
       (org-element-parse-buffer 'headline)
       'headline
     ;; This is the predicate matching if a headline makes an org-agenda-file.
     (lambda (h)
       (eq (org-element-property :todo-type h) 'todo))
     nil 'first-match))

(defun org-dynamic-agenda-file-p (&optional file)
  "Check if the file should be added to the variable `org-agenda-files'.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE."
  (if (not file)
      (org-dynamic-agenda-predicate)
    (message "org-dynamic-agenda-file-p: processing %s" file)
    (if-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (org-dynamic-agenda-predicate))
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (insert-file-contents file)
        (setq buffer-file-name file)
        (unwind-protect
            (org-dynamic-agenda-predicate)
          (setq buffer-file-name nil))))))

(defun org-dynamic-agenda-update-file-h ()
  "Conditionally add dynamic agenda hook to Org buffers."
  (when (and (buffer-file-name)
             (file-in-directory-p (buffer-file-name) org-directory))
    (add-hook 'before-save-hook #'org-dynamic-agenda-update-file nil t)))

(add-hook 'org-mode-hook #'org-dynamic-agenda-update-file-h)
(advice-add 'org-agenda :before #'org-dynamic-agenda-cleanup-files)
(advice-add 'org-agenda-redo :before #'org-dynamic-agenda-cleanup-files)
(advice-add 'org-todo-list :before #'org-dynamic-agenda-cleanup-files)

(provide 'org-dynamic-agenda)
;;; org-dynamic-agenda.el ends here
