;;; org-ql-dynamic-agenda.el --- Build your agenda as you work  -*- lexical-binding: t -*-

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
(require 'org-ql)
(require 'cl-lib)

;;; Common

(defun org-ql-dynamic-agenda--check-file-list (files)
  "Check that FILES is an existing file or a list of them.
Returns the list of valid files at compile time."
  (let ((valid-files '())
        (file-list (if (stringp files) (list files) files)))
    (dolist (file file-list)
      (unless (file-readable-p file)
        (error "\
File in org-dynamic-agenda-check-file-list is not readable: %s" file))
      (push file valid-files))
    (reverse valid-files)))

(defun org-ql-dynamic-agenda-update-file (&optional file)
  "Update variable `org-agenda-files'.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE."
  (interactive)
  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
    (let ((files (org-agenda-files)))
      (if (org-ql-dynamic-agenda-file-p file)
          (cl-pushnew (file-truename (buffer-file-name)) files
                      :test #'string-equal)
        (cl-delete (file-truename (buffer-file-name)) files
                   :test #'string-equal))
      (org-store-new-agenda-file-list files))))

(defun org-ql-dynamic-agenda-cleanup-files (&optional full)
  "Cleanup variable `org-agenda-files'.

If FULL, rechecks the files with `org-dynamic-agenda-file-p'."
  (interactive)
  (org-store-new-agenda-file-list
   (if full
       (cl-remove-if-not #'org-ql-dynamic-agenda-file-p (org-agenda-files))
     (cl-remove-if-not #'file-readable-p (org-agenda-files)))))

(defvar org-ql-dynamic-agenda-queries nil
  "Cache for `org-ql' queries defined from `org-agenda-custom-commands'.")

(defun org-ql-dynamic-agenda-extract-queries ()
  "Extract queries from an `org-ql' set of `org-agenda-custom-commands'."
  (let* ((blocks (apply #'append
                        (mapcar #'caddr org-agenda-custom-commands)))
         (org-ql-blocks (seq-filter (lambda (b)
                                      (string-equal (symbol-name (car b))
                                                    "org-ql-block"))
                                    blocks)))
    (mapcar #'cadr org-ql-blocks)))

(defun org-ql-dynamic-agenda-file-p (&optional file)
  "Check if the file should be added to the variable `org-agenda-files'.

This version of the function requires `org-agenda-custom-commands' to
be defined with `orq-ql-block'. The result of this function is cached,
meaning that it will load much faster on the second run.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE or list of files."
  (interactive)
  (unless org-ql-dynamic-agenda-queries
    (setq org-ql-dynamic-agenda-queries
          (org-ql-dynamic-agenda-extract-queries)))
  (let ((files (if file
                   (org-ql-dynamic-agenda--check-file-list file)
                 (list (buffer-file-name)))))
    (seq-reduce (lambda (query bool)
                  (and bool (org-ql-select files query)))
                org-ql-dynamic-agenda-queries
                t)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'org-ql-dynamic-agenda-update-file)))
(advice-add 'org-agenda :before #'org-ql-dynamic-agenda-cleanup-files)
(advice-add 'org-agenda-redo :before #'org-ql-dynamic-agenda-cleanup-files)
(advice-add 'org-todo-list :before #'org-ql-dynamic-agenda-cleanup-files)

(provide 'org-ql-dynamic-agenda)
;;; org-ql-dynamic-agenda.el ends here
