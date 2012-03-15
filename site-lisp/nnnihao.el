;;; nnjump.el

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

;; nnjump is a Gnus backend that provides no groups or articles.  It's useful
;; as a primary select method when you want all your real select methods to
;; be secondary or foreign.

;;; Code:

(eval-and-compile
  (require 'nnheader))

;;; Interface functions

(nnoo-define-basics nnjump)

(deffoo nnjump-retrieve-headers (articles &optional newsgroup server fetch-old)
  (when (nnjump-possibly-change-buffer newsgroup server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (let (article entry)
	(if (stringp (car articles))
	    'headers
	  (while articles
	    (when (setq entry (cdr (assq (setq article (pop articles))
					 nnjump-dissection-alist)))
	      (let ((start (point)))
		(insert (format "221 %d Article retrieved.\n" article))
		(if nnjump-generate-head-function
		    (funcall nnjump-generate-head-function article)
		  (insert-buffer-substring
		   nnjump-current-buffer (car entry) (nth 1 entry)))
		(goto-char (point-max))
		(unless (eq (char-after (1- (point))) ?\n)
		  (insert "\n"))
		(insert (format "Lines: %d\n" (nth 4 entry)))
		(insert ".\n")
		(when nnjump-header-transform-function
		  (save-excursion
		    (save-restriction
		      (narrow-to-region start (point))
		      (funcall nnjump-header-transform-function entry)))))))
	  (nnheader-fold-continuation-lines)
	  'headers)))))

(deffoo nnjump-request-article (article &optional newsgroup server buffer)
  (nnjump-possibly-change-buffer newsgroup server)
  (save-excursion
    (let ((buffer (or buffer nntp-server-buffer))
	  (entry (cdr (assq article nnjump-dissection-alist)))
	  beg)
      (set-buffer buffer)
      (erase-buffer)
      (when entry
	(cond
	 ((stringp article) nil)
	 (nnjump-generate-article-function
	  (funcall nnjump-generate-article-function article))
	 (t
	  (insert-buffer-substring
	   nnjump-current-buffer (car entry) (nth 1 entry))
	  (insert "\n")
	  (setq beg (point))
	  (insert-buffer-substring
	   nnjump-current-buffer (nth 2 entry) (nth 3 entry))
	  (goto-char beg)
	  (when nnjump-prepare-body-function
	    (funcall nnjump-prepare-body-function))
	  (when nnjump-article-transform-function
	    (funcall nnjump-article-transform-function article))
	  t))))))

(deffoo nnjump-request-group (group &optional server dont-check info)
  "Select news GROUP."
  (let (number)
    (cond
     ((not (nnjump-possibly-change-buffer group server))
      (nnheader-report 'nnjump "No such file or buffer: %s"
		       nnjump-address))
     (dont-check
      (nnheader-report 'nnjump "Selected group %s" group)
      t)
     ((zerop (setq number (length nnjump-dissection-alist)))
      (nnjump-close-group group)
      (nnheader-report 'nnjump "No articles in group %s" group))
     (t
      (nnheader-insert "211 %d %d %d %s\n" number 1 number group)))))

(deffoo nnjump-retrieve-groups (groups &optional server)
  (dolist (group groups)
    (nnjump-request-group group server))
  t)

(deffoo nnjump-request-type (group &optional article)
  (cond ((not article) 'unknown)
	(nnjump-post-type nnjump-post-type)
	(t 'unknown)))

(deffoo nnjump-close-group (group &optional server)
  (nnjump-possibly-change-buffer group server)
  (and nnjump-current-buffer
       (buffer-name nnjump-current-buffer)
       (kill-buffer nnjump-current-buffer))
  (setq nnjump-group-alist (delq (assoc group nnjump-group-alist)
				nnjump-group-alist))
  (setq nnjump-current-buffer nil)
  (nnoo-close-server 'nnjump server)
  (setq nnjump-dissection-alist nil)
  t)

(deffoo nnjump-request-list (&optional server)
  t)

(deffoo nnjump-request-newgroups (date &optional server)
  nil)

(deffoo nnjump-request-list-newsgroups (&optional server)
  nil)

(provide 'nnjump)

;;; nnjump.el ends here
