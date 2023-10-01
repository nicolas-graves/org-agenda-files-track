;;; org-dynamic-agenda-ql.el --- Build your agenda as you work  -*- lexical-binding: t -*-

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
(require 'org-ql-view)
(require 'cl-lib)

(defvar org-dynamic-agenda-ql-mode nil
  "Toggle org-dynamic-agenda-ql mode on or off.")

;;;###autoload
(define-minor-mode org-dynamic-agenda-ql-mode
  "Toggle org-dynamic-agenda-ql mode.
When org-dynamic-agenda-ql-mode is enabled, it updates the variable
`org-agenda-files' based on the presence of queries in
`org-agenda-custom-commands' and `org-ql-views'"
  :init-value nil
  :group 'org
  :global nil
  (if org-dynamic-agenda-ql-mode
      (add-hook 'org-mode-hook 'org-dynamic-agenda-ql-update-file-h)
    (remove-hook 'org-mode-hook 'org-dynamic-agenda-ql-update-file-h)
    (org-dynamic-agenda-ql-cleanup-files t)))

(defun org-dynamic-agenda-ql-update-file-h ()
  "Add hook to the current buffer when in org-dynamic-agenda-ql mode."
  (when (and (buffer-file-name)
             (file-in-directory-p (buffer-file-name) org-directory))
    (add-hook 'before-save-hook #'org-dynamic-agenda-ql-update-file nil t)))

(defun org-dynamic-agenda-ql-update-file (&optional file)
  "Update variable `org-agenda-files'.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE."
  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
    (let ((files (org-agenda-files)))
      (if (org-dynamic-agenda-ql-file-p file)
          (cl-pushnew (file-truename (buffer-file-name)) files
                      :test #'string-equal)
        (setq files (cl-delete (file-truename (buffer-file-name)) files
                               :test #'string-equal)))
      (org-store-new-agenda-file-list files))))

(defun org-dynamic-agenda-ql-cleanup-files (&optional full)
  "Cleanup variable `org-agenda-files'.

If FULL, rechecks the files with `org-dynamic-agenda-file-p'."
  (org-store-new-agenda-file-list
   (cl-remove-if-not (if full #'org-dynamic-agenda-ql-file-p
                       #'file-readable-p)
                     (org-agenda-files))))

(defun org-dynamic-agenda-ql-extract-queries ()
  "Extract queries from user-defined custom variables.

Extracts queries from an `org-ql' set of
`org-agenda-custom-commands' as well as `org-ql-views'."
  (let* ((blocks (apply #'append
                        (mapcar #'caddr org-agenda-custom-commands)))
         (org-ql-blocks (seq-filter (lambda (b)
                                      (string-equal (symbol-name (car b))
                                                    "org-ql-block"))
                                    blocks)))
    (append
     (mapcar #'cadr org-ql-blocks)
     ;; ignore views that take a function, which build the query at runtime
     (seq-filter #'identity
                 (mapcar (lambda (view)
                           (plist-get (cdr view) :query)) org-ql-views)))))

(defun org-dynamic-agenda-ql-file-p (&optional file)
  "Check if the file should be added to the variable `org-agenda-files'.

This version of the function requires `org-agenda-custom-commands' to
be defined with `orq-ql-block'.  The result of this function is cached,
meaning that it will load much faster on the second run.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE."
  (when file
    (message "org-dynamic-agenda-ql-file-p: processing %s" file))
  (seq-reduce (lambda (bool query)
                (or bool (org-ql-select
                           (or file (current-buffer))
                           query
                           ;; just matching, don’t run
                           ;; org-element-headline-parser
                           :action #'point)))
              (org-dynamic-agenda-ql-extract-queries)
              nil))

(provide 'org-dynamic-agenda-ql)
;;; org-dynamic-agenda-ql.el ends here
