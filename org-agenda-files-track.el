;;; org-agenda-files-track.el --- Fine-track `org-agenda-files' to speed-up `org-agenda' -*- lexical-binding: t -*-

;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: data, files, tools
;; URL: https://git.sr.ht/~ngraves/org-agenda-files-track

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

;; When an agenda buffer is built, Emacs visits each file listed in
;; `org-agenda-files'.  In case your tasks or events are recorded in
;; an ever-extending journal and/or roam directories, `org-agenda' can
;; become sluggish.
;; This package aims to dynamically update the `org-agenda-files'
;; variable by appending/deleting a candidate org file when it is
;; saved.  This limits the number of files to visit when building the
;; agenda.  The agenda buffer thus builds faster.  Candidate selection
;; is governed by `org-agenda-files-track-predicate'.

;; See more info here:
;; https://git.sr.ht/~ngraves/org-agenda-files-track/blob/master/README.org

;;; Code:
(require 'org-agenda)
(require 'org-element)
(require 'cl-lib)

(defvar org-agenda-files-track-mode nil
  "Toggle org-agenda-files-track mode on or off.")

;;;###autoload
(define-minor-mode org-agenda-files-track-mode
  "Toggle org-agenda-files-track mode.
When org-agenda-files-track-mode is enabled, it updates the variable
`org-agenda-files' based on the `org-mode' files matching
`org-agenda-files-track-predicate' within `org-directory'."
  :init-value nil
  :group 'org
  :global nil
  (if org-agenda-files-track-mode
      (add-hook 'org-mode-hook #'org-agenda-files-track-update-file-h)
    (remove-hook 'org-mode-hook #'org-agenda-files-track-update-file-h)
    (org-agenda-files-track-cleanup-files t)))

(defun org-agenda-files-track-update-file-h ()
  "Add dynamic agenda hook to the current buffer when in org-agenda-files-track mode."
  (when (and (buffer-file-name)
             (file-in-directory-p (buffer-file-name) org-directory))
    (add-hook 'before-save-hook #'org-agenda-files-track-update-file nil t)))

(defun org-agenda-files-track-update-file (&optional file)
  "Update variable `org-agenda-files'.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE."
  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
    (let ((files (org-agenda-files)))
      (if (org-agenda-files-track-file-p file)
          (cl-pushnew (file-truename (buffer-file-name)) files
                      :test #'string-equal)
        (setq files (cl-delete (file-truename (buffer-file-name)) files
                               :test #'string-equal)))
      (org-store-new-agenda-file-list files))))

(defun org-agenda-files-track-cleanup-files (&optional full)
  "Cleanup variable `org-agenda-files'.

If FULL, rechecks the files with `org-agenda-files-track-file-p'."
  (org-store-new-agenda-file-list
   (cl-remove-if-not (if full #'org-agenda-files-track-file-p
                       #'file-readable-p)
                     (org-agenda-files))))

(defun org-agenda-files-track-predicate ()
  "Check if the file should be added to the variable `org-agenda-files'."
   (org-element-map
       (org-element-parse-buffer 'headline)
       'headline
     ;; This is the predicate matching if a headline makes an org-agenda-file.
     (lambda (h)
       (eq (org-element-property :todo-type h) 'todo))
     nil 'first-match))

(defun org-agenda-files-track-file-p (&optional file)
  "Check if the file should be added to the variable `org-agenda-files'.

The function is supposed to be run in an `org-mode' file, or in an
optional provided FILE."
  (if (not file)
      (org-agenda-files-track-predicate)
    (message "org-agenda-files-track-file-p: processing %s" file)
    (if-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (org-agenda-files-track-predicate))
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (insert-file-contents file)
        (setq buffer-file-name file)
        (unwind-protect
            (org-agenda-files-track-predicate)
          (setq buffer-file-name nil))))))

(provide 'org-agenda-files-track)
;;; org-agenda-files-track.el ends here
