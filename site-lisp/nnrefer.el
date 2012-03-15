;;; nnnil.el --- empty backend for Gnus

;; This file is in the public domain.

;; Author: Paul Jarc <prj@po.cwru.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; nnrefer is a Gnus backend that provides no groups or articles.  It's useful
;; as a primary select method when you want all your real select methods to
;; be secondary or foreign.

;;; Code:

(eval-and-compile
  (require 'nnheader))

(defvar nnrefer-status-string "")

(defun nnrefer-retrieve-headers (articles &optional group server fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  'nov)

(defun nnrefer-open-server (server &optional definitions)
  t)

(defun nnrefer-close-server (&optional server)
  t)

(defun nnrefer-request-close ()
  t)

(defun nnrefer-server-opened (&optional server)
  t)

(defun nnrefer-status-message (&optional server)
  nnrefer-status-string)

(defun nnrefer-request-article (article &optional group server to-buffer)
  t)

(defun nnrefer-request-group (group &optional server fast info)
  (let (deactivate-mark)
    (with-current-buffer nntp-server-buffer
      (nnheader-report 'nnrefer "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" 1 1 1
       (prin1-to-string group)
       t))))

(defun nnrefer-close-group (group &optional server)
  t)

(defun nnrefer-request-list (&optional server)
  t)

(defun nnrefer-request-post (&optional server)
  (setq nnrefer-status-string "Read-only server")
  t)

(provide 'nnrefer)

;;; nnrefer.el ends here
