;;; org-dynamic-agenda.el --- Fine-track `org-agenda-files' to speed-up `org-agenda' -*- lexical-binding: t -*-

;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: data, files, tools
;; URL: https://git.sr.ht/~ngraves/org-dynamic-agenda

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
;; Fine-track `org-agenda-files' to speed-up `org-agenda'
;; See more info here: https://git.sr.ht/~ngraves/org-dynamic-agenda/blob/master/README.org

;;; Code:
(require 'org-agenda)
(require 'org-element)
(require 'cl-lib)

(defvar org-dynamic-agenda-mode nil
  "Toggle org-dynamic-agenda mode on or off.")

;;;###autoload
(define-minor-mode org-dynamic-agenda-mode
  "Toggle org-dynamic-agenda mode.
When org-dynamic-agenda-mode is enabled, it updates the variable
`org-agenda-files' based on the `org-mode' files matching
`org-dynamic-agenda-predicate' within `org-directory'."
  :init-value nil
  :group 'org
  :global nil
  (if org-dynamic-agenda-mode
      (add-hook 'org-mode-hook #'org-dynamic-agenda-update-file-h)
    (remove-hook 'org-mode-hook #'org-dynamic-agenda-update-file-h)
    (org-dynamic-agenda-cleanup-files t)))

(defun org-dynamic-agenda-update-file-h ()
  "Add dynamic agenda hook to the current buffer when in org-dynamic-agenda mode."
  (when (and (buffer-file-name)
             (file-in-directory-p (buffer-file-name) org-directory))
    (add-hook 'before-save-hook #'org-dynamic-agenda-update-file nil t)))

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

(provide 'org-dynamic-agenda)
;;; org-dynamic-agenda.el ends here
