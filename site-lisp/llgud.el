;;; llgud.el --- Grand Unified Debugger mode for running GDB and other debuggers

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: FSF
;; Keywords: unix, tools

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 2000, 2001, 2002, 2003,
;;  2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The ancestral gdb.el was by W. Schelter <wfs@rascal.ics.utexas.edu> It was
;; later rewritten by rms.  Some ideas were due to Masanobu.  Grand
;; Unification (sdb/dbx support) by Eric S. Raymond <esr@thyrsus.com> Barry
;; Warsaw <bwarsaw@cen.com> hacked the mode to use comint.el.  Shane Hartman
;; <shane@spr.com> added support for xdb (HPUX debugger).  Rick Sladkey
;; <jrs@world.std.com> wrote the GDB command completion code.  Dave Love
;; <d.love@dl.ac.uk> added the IRIX kluge, re-implemented the Mips-ish variant
;; and added a menu. Brian D. Carlstrom <bdc@ai.mit.edu> combined the IRIX
;; kluge with the llgud-xdb-directories hack producing llgud-dbx-directories.
;; Derek L. Davies <ddavies@world.std.com> added support for jdb (Java
;; debugger.)

;;; Code:

(eval-when-compile (require 'cl)) ; for case macro

(require 'comint)

(defvar gdb-active-process)
(defvar gdb-define-alist)
(defvar gdb-macro-info)
(defvar gdb-server-prefix)
(defvar gdb-show-changed-values)
(defvar gdb-var-list)
(defvar gdb-speedbar-auto-raise)
(defvar tool-bar-map)

;; ======================================================================
;; LLGUD commands must be visible in C buffers visited by LLGUD

(defgroup llgud nil
  "Grand Unified Debugger mode for gdb and other debuggers under Emacs.
Supported debuggers include gdb, sdb, dbx, xdb, perldb, pdb (Python) and jdb."
  :group 'unix
  :group 'tools)


(defcustom llgud-key-prefix "\C-x\C-a"
  "Prefix of all LLGUD commands valid in C buffers."
  :type 'string
  :group 'llgud)

(global-set-key (concat llgud-key-prefix "\C-l") 'llgud-refresh)
(define-key ctl-x-map " " 'llgud-break)	;; backward compatibility hack

(defvar llgud-marker-filter nil)
(put 'llgud-marker-filter 'permanent-local t)
(defvar llgud-find-file nil)
(put 'llgud-find-file 'permanent-local t)

(defun llgud-marker-filter (&rest args)
  (apply llgud-marker-filter args))

(defvar llgud-minor-mode nil)
(put 'llgud-minor-mode 'permanent-local t)

(defvar llgud-comint-buffer nil)

(defvar llgud-keep-buffer nil)

(defun llgud-symbol (sym &optional soft minor-mode)
  "Return the symbol used for SYM in MINOR-MODE.
MINOR-MODE defaults to `llgud-minor-mode'.
The symbol returned is `llgud-<MINOR-MODE>-<SYM>'.
If SOFT is non-nil, returns nil if the symbol doesn't already exist."
  (unless (or minor-mode llgud-minor-mode) (error "Llgud internal error"))
  (funcall (if soft 'intern-soft 'intern)
	   (format "llgud-%s-%s" (or minor-mode llgud-minor-mode) sym)))

(defun llgud-val (sym &optional minor-mode)
  "Return the value of `llgud-symbol' SYM.  Default to nil."
  (let ((sym (llgud-symbol sym t minor-mode)))
    (if (boundp sym) (symbol-value sym))))

(defvar llgud-running nil
  "Non-nil if debugged program is running.
Used to grey out relevant toolbar icons.")

(defvar gdb-ready nil)

;; Use existing Info buffer, if possible.
(defun llgud-goto-info ()
  "Go to relevant Emacs info node."
  (interactive)
  (let ((same-window-regexps same-window-regexps)
	(display-buffer-reuse-frames t))
    (catch 'info-found
      (walk-windows
       '(lambda (window)
	  (if (eq (window-buffer window) (get-buffer "*info*"))
	      (progn
		(setq same-window-regexps nil)
		(throw 'info-found nil))))
       nil 0)
      (select-frame (make-frame)))
    (if (memq llgud-minor-mode '(gdbmi gdba))
	(info "(emacs)GDB Graphical Interface")
      (info "(emacs)Debuggers"))))

(defun llgud-tool-bar-item-visible-no-fringe ()
  (not (or (eq (buffer-local-value 'major-mode (window-buffer)) 'speedbar-mode)
	   (and (memq llgud-minor-mode '(gdbmi gdba))
		(> (car (window-fringes)) 0)))))

(defun llgud-stop-subjob ()
  (interactive)
  (with-current-buffer llgud-comint-buffer
    (if (string-equal llgud-target-name "emacs")
	(comint-stop-subjob)
      (comint-interrupt-subjob))))

(easy-mmode-defmap llgud-menu-map
  '(([help]     "Info" . llgud-goto-info)
    ([tooltips] menu-item "Show LLGUD tooltips" llgud-tooltip-mode
                  :enable (and (not emacs-basic-display)
			       (display-graphic-p)
			       (fboundp 'x-show-tip))
		  :visible (memq llgud-minor-mode
				'(lldb gdbmi gdba dbx sdb xdb pdb))
	          :button (:toggle . llgud-tooltip-mode))
    ([refresh]	"Refresh" . llgud-refresh)
    ([run]	menu-item "Run" llgud-run
                  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode '(lldb gdbmi gdb dbx jdb)))
    ([go]	menu-item (if gdb-active-process "Continue" "Run") llgud-go
		  :visible (and (not llgud-running)
				(eq llgud-minor-mode 'gdba)))
    ([stop]	menu-item "Stop" llgud-stop-subjob
		  :visible (or (not (memq llgud-minor-mode '(gdba pdb)))
			       (and llgud-running
				    (eq llgud-minor-mode 'gdba))))
    ([until]	menu-item "Continue to selection" llgud-until
                  :enable (not llgud-running)
		  :visible (and (memq llgud-minor-mode '(gdbmi gdba gdb perldb))
				(llgud-tool-bar-item-visible-no-fringe)))
    ([remove]	menu-item "Remove Breakpoint" llgud-remove
                  :enable (not llgud-running)
		  :visible (llgud-tool-bar-item-visible-no-fringe))
    ([tbreak]	menu-item "Temporary Breakpoint" llgud-tbreak
                  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode
				'(lldb gdbmi gdba gdb sdb xdb)))
    ([break]	menu-item "Set Breakpoint" llgud-break
                  :enable (not llgud-running)
		  :visible (llgud-tool-bar-item-visible-no-fringe))
    ([up]	menu-item "Up Stack" llgud-up
		  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode
				 '(lldb gdbmi gdba gdb dbx xdb jdb pdb)))
    ([down]	menu-item "Down Stack" llgud-down
		  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode
				 '(lldb gdbmi gdba gdb dbx xdb jdb pdb)))
    ([pp]	menu-item "Print S-expression" llgud-pp
                  :enable (and (not llgud-running)
				  gdb-active-process)
		  :visible (and (string-equal
				 (buffer-local-value
				  'llgud-target-name llgud-comint-buffer) "emacs")
				(eq llgud-minor-mode 'gdba)))
    ([print*]	menu-item "Print Dereference" llgud-pstar
                  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode '(lldb gdbmi gdba gdb)))
    ([print]	menu-item "Print Expression" llgud-print
                  :enable (not llgud-running))
    ([watch]	menu-item "Watch Expression" llgud-watch
		  :enable (not llgud-running)
	  	  :visible (memq llgud-minor-mode '(gdbmi gdba)))
    ([finish]	menu-item "Finish Function" llgud-finish
                  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode
				 '(lldb gdbmi gdba gdb xdb jdb pdb)))
    ([stepi]	menu-item "Step Instruction" llgud-stepi
                  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode '(lldb gdbmi gdba gdb dbx)))
    ([nexti]	menu-item "Next Instruction" llgud-nexti
                  :enable (not llgud-running)
		  :visible (memq llgud-minor-mode '(lldb gdbmi gdba gdb dbx)))
    ([step]	menu-item "Step Line" llgud-step
                  :enable (not llgud-running))
    ([next]	menu-item "Next Line" llgud-next
                  :enable (not llgud-running))
    ([cont]	menu-item "Continue" llgud-cont
                  :enable (not llgud-running)
		  :visible (not (eq llgud-minor-mode 'gdba))))
  "Menu for `llgud-mode'."
  :name "Llgud")

(easy-mmode-defmap llgud-minor-mode-map
  (append
     `(([menu-bar debug] . ("Llgud" . ,llgud-menu-map)))
     ;; Get tool bar like functionality from the menu bar on a text only
     ;; terminal.
   (unless window-system
     `(([menu-bar down]
	. (,(propertize "down" 'face 'font-lock-doc-face) . llgud-down))
       ([menu-bar up]
	. (,(propertize "up" 'face 'font-lock-doc-face) . llgud-up))
       ([menu-bar finish]
	. (,(propertize "finish" 'face 'font-lock-doc-face) . llgud-finish))
       ([menu-bar step]
	. (,(propertize "step" 'face 'font-lock-doc-face) . llgud-step))
       ([menu-bar next]
	. (,(propertize "next" 'face 'font-lock-doc-face) . llgud-next))
       ([menu-bar until] menu-item
	,(propertize "until" 'face 'font-lock-doc-face) llgud-until
		  :visible (memq llgud-minor-mode '(gdbmi gdba gdb perldb)))
       ([menu-bar cont] menu-item
	,(propertize "cont" 'face 'font-lock-doc-face) llgud-cont
	:visible (not (eq llgud-minor-mode 'gdba)))
       ([menu-bar run] menu-item
	,(propertize "run" 'face 'font-lock-doc-face) llgud-run
	:visible (memq llgud-minor-mode '(gdbmi gdb dbx jdb)))
       ([menu-bar go] menu-item
	,(propertize " go " 'face 'font-lock-doc-face) llgud-go
	:visible (and (not llgud-running)
		      (eq llgud-minor-mode 'gdba)))
       ([menu-bar stop] menu-item
	,(propertize "stop" 'face 'font-lock-doc-face) llgud-stop-subjob
	:visible (or llgud-running
		     (not (eq llgud-minor-mode 'gdba))))
       ([menu-bar print]
	. (,(propertize "print" 'face 'font-lock-doc-face) . llgud-print))
       ([menu-bar tools] . undefined)
       ([menu-bar buffer] . undefined)
       ([menu-bar options] . undefined)
       ([menu-bar edit] . undefined)
       ([menu-bar file] . undefined))))
  "Map used in visited files.")

(let ((m (assq 'llgud-minor-mode minor-mode-map-alist)))
  (if m (setcdr m llgud-minor-mode-map)
    (push (cons 'llgud-minor-mode llgud-minor-mode-map) minor-mode-map-alist)))

(defvar llgud-mode-map
  ;; Will inherit from comint-mode via define-derived-mode.
  (make-sparse-keymap)
  "`llgud-mode' keymap.")

(defvar llgud-tool-bar-map
  (if (display-graphic-p)
      (let ((map (make-sparse-keymap)))
	(dolist (x '((llgud-break . "llgud/break")
		     (llgud-remove . "llgud/remove")
		     (llgud-print . "llgud/print")
		     (llgud-pstar . "llgud/pstar")
		     (llgud-pp . "llgud/pp")
		     (llgud-watch . "llgud/watch")
		     (llgud-run . "llgud/run")
		     (llgud-go . "llgud/go")
		     (llgud-stop-subjob . "llgud/stop")
		     (llgud-cont . "llgud/cont")
		     (llgud-until . "llgud/until")
		     (llgud-next . "llgud/next")
		     (llgud-step . "llgud/step")
		     (llgud-finish . "llgud/finish")
		     (llgud-nexti . "llgud/nexti")
		     (llgud-stepi . "llgud/stepi")
		     (llgud-up . "llgud/up")
		     (llgud-down . "llgud/down")
		     (llgud-goto-info . "info"))
		   map)
	  (tool-bar-local-item-from-menu
	   (car x) (cdr x) map llgud-minor-mode-map)))))

(defun llgud-file-name (f)
  "Transform a relative file name to an absolute file name.
Uses `llgud-<MINOR-MODE>-directories' to find the source files."
  (if (file-exists-p f) (expand-file-name f)
    (let ((directories (llgud-val 'directories))
	  (result nil))
      (while directories
	(let ((path (expand-file-name f (car directories))))
	  (if (file-exists-p path)
	      (setq result path
		    directories nil)))
	(setq directories (cdr directories)))
      result)))

(defun llgud-find-file (file)
  ;; Don't get confused by double slashes in the name that comes from GDB.
  (while (string-match "//+" file)
    (setq file (replace-match "/" t t file)))
  (let ((minor-mode llgud-minor-mode)
	(buf (funcall (or llgud-find-file 'llgud-file-name) file)))
    (when (stringp buf)
      (setq buf (and (file-readable-p buf) (find-file-noselect buf 'nowarn))))
    (when buf
      ;; Copy `llgud-minor-mode' to the found buffer to turn on the menu.
      (with-current-buffer buf
	(set (make-local-variable 'llgud-minor-mode) minor-mode)
	(set (make-local-variable 'tool-bar-map) llgud-tool-bar-map)
	(when (and llgud-tooltip-mode
		   (memq llgud-minor-mode '(gdbmi gdba)))
	  (make-local-variable 'gdb-define-alist)
	  (unless  gdb-define-alist (gdb-create-define-alist))
	  (add-hook 'after-save-hook 'gdb-create-define-alist nil t))
	(make-local-variable 'llgud-keep-buffer))
      buf)))

;; ======================================================================
;; command definition

;; This macro is used below to define some basic debugger interface commands.
;; Of course you may use `llgud-def' with any other debugger command, including
;; user defined ones.

;; A macro call like (llgud-def FUNC CMD KEY DOC) expands to a form
;; which defines FUNC to send the command CMD to the debugger, gives
;; it the docstring DOC, and binds that function to KEY in the LLGUD
;; major mode.  The function is also bound in the global keymap with the
;; LLGUD prefix.

(defmacro llgud-def (func cmd key &optional doc)
  "Define FUNC to be a command sending CMD and bound to KEY, with
optional doc string DOC.  Certain %-escapes in the string arguments
are interpreted specially if present.  These are:

  %f -- Name (without directory) of current source file.
  %F -- Name (without directory or extension) of current source file.
  %d -- Directory of current source file.
  %l -- Number of current source line.
  %e -- Text of the C lvalue or function-call expression surrounding point.
  %a -- Text of the hexadecimal address surrounding point.
  %b -- Text of the most recently created breakpoint id.
  %p -- Prefix argument to the command (if any) as a number.
  %c -- Fully qualified class name derived from the expression
        surrounding point (jdb only).

  The `current' source file is the file of the current buffer (if
we're in a C file) or the source file current at the last break or
step (if we're in the LLGUD buffer).
  The `current' line is that of the current buffer (if we're in a
source file) or the source line number at the last break or step (if
we're in the LLGUD buffer)."
  `(progn
     (defun ,func (arg)
       ,@(if doc (list doc))
       (interactive "p")
       (if (not llgud-running)
	 ,(if (stringp cmd)
	      `(llgud-call ,cmd arg)
	    cmd)))
     ,(if key `(local-set-key ,(concat "\C-c" key) ',func))
     ,(if key `(global-set-key (vconcat llgud-key-prefix ,key) ',func))))

;; Where llgud-display-frame should put the debugging arrow; a cons of
;; (filename . line-number).  This is set by the marker-filter, which scans
;; the debugger's output for indications of the current program counter.
(defvar llgud-last-frame nil)

;; Used by llgud-refresh, which should cause llgud-display-frame to redisplay
;; the last frame, even if it's been called before and llgud-last-frame has
;; been set to nil.
(defvar llgud-last-last-frame nil)

;; All debugger-specific information is collected here.
;; Here's how it works, in case you ever need to add a debugger to the mode.
;;
;; Each entry must define the following at startup:
;;
;;<name>
;; comint-prompt-regexp
;; llgud-<name>-massage-args
;; llgud-<name>-marker-filter
;; llgud-<name>-find-file
;;
;; The job of the massage-args method is to modify the given list of
;; debugger arguments before running the debugger.
;;
;; The job of the marker-filter method is to detect file/line markers in
;; strings and set the global llgud-last-frame to indicate what display
;; action (if any) should be triggered by the marker.  Note that only
;; whatever the method *returns* is displayed in the buffer; thus, you
;; can filter the debugger's output, interpreting some and passing on
;; the rest.
;;
;; The job of the find-file method is to visit and return the buffer indicated
;; by the car of llgud-tag-frame.  This may be a file name, a tag name, or
;; something else.

;; ======================================================================
;; speedbar support functions and variables.
(eval-when-compile (require 'speedbar))	;For speedbar-with-attached-buffer.

(defvar llgud-last-speedbar-stackframe nil
  "Description of the currently displayed LLGUD stack.
The value t means that there is no stack, and we are in display-file mode.")

(defvar llgud-speedbar-key-map nil
  "Keymap used when in the buffers display mode.")

(defun llgud-speedbar-item-info ()
  "Display the data type of the watch expression element."
  (let ((var (nth (- (line-number-at-pos (point)) 2) gdb-var-list)))
    (if (nth 6 var)
	(speedbar-message "%s: %s" (nth 6 var) (nth 3 var))
      (speedbar-message "%s" (nth 3 var)))))

(defun llgud-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance llgud/gdb."
  (if llgud-speedbar-key-map
      nil
    (setq llgud-speedbar-key-map (speedbar-make-specialized-keymap))

    (define-key llgud-speedbar-key-map "j" 'speedbar-edit-line)
    (define-key llgud-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key llgud-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key llgud-speedbar-key-map " " 'speedbar-toggle-line-expansion)
    (define-key llgud-speedbar-key-map "D" 'gdb-var-delete)
    (define-key llgud-speedbar-key-map "p" 'llgud-pp))

  (speedbar-add-expansion-list '("LLGUD" llgud-speedbar-menu-items
				 llgud-speedbar-key-map
				 llgud-expansion-speedbar-buttons))

  (add-to-list
   'speedbar-mode-functions-list
   '("LLGUD" (speedbar-item-info . llgud-speedbar-item-info)
     (speedbar-line-directory . ignore))))

(defvar llgud-speedbar-menu-items
  '(["Jump to stack frame" speedbar-edit-line
     :visible (not (memq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		    '(gdbmi gdba)))]
    ["Edit value" speedbar-edit-line
     :visible (memq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		    '(gdbmi gdba))]
    ["Delete expression" gdb-var-delete
     :visible (memq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		    '(gdbmi gdba))]
    ["Auto raise frame" gdb-speedbar-auto-raise
     :style toggle :selected gdb-speedbar-auto-raise
     :visible (memq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		    '(gdbmi gdba))]
    ("Output Format"
     :visible (memq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		    '(gdbmi gdba))
     ["Binary" (gdb-var-set-format "binary") t]
     ["Natural" (gdb-var-set-format  "natural") t]
     ["Hexadecimal" (gdb-var-set-format "hexadecimal") t]))
  "Additional menu items to add to the speedbar frame.")

;; Make sure our special speedbar mode is loaded
(if (featurep 'speedbar)
    (llgud-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'llgud-install-speedbar-variables))

(defun llgud-expansion-speedbar-buttons (directory zero)
  "Wrapper for call to `speedbar-add-expansion-list'.
DIRECTORY and ZERO are not used, but are required by the caller."
  (llgud-speedbar-buttons llgud-comint-buffer))

(defun llgud-speedbar-buttons (buffer)
  "Create a speedbar display based on the current state of LLGUD.
If the LLGUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode.  BUFFER is not used, but is
required by the caller."
  (when (and llgud-comint-buffer
	     ;; llgud-comint-buffer might be killed
	     (buffer-name llgud-comint-buffer))
    (let* ((minor-mode (with-current-buffer buffer llgud-minor-mode))
	  (window (get-buffer-window (current-buffer) 0))
	  (start (window-start window))
	  (p (window-point window)))
      (cond
       ((memq minor-mode '(gdbmi gdba))
	(erase-buffer)
	(insert "Watch Expressions:\n")
	(if gdb-speedbar-auto-raise
	    (raise-frame speedbar-frame))
	(let ((var-list gdb-var-list) parent)
	  (while var-list
	    (let* (char (depth 0) (start 0) (var (car var-list))
			(varnum (car var)) (expr (nth 1 var))
			(type (if (nth 3 var) (nth 3 var) " "))
			(value (nth 4 var)) (status (nth 5 var)))
	      (put-text-property
	       0 (length expr) 'face font-lock-variable-name-face expr)
	      (put-text-property
	       0 (length type) 'face font-lock-type-face type)
	      (while (string-match "\\." varnum start)
		(setq depth (1+ depth)
		      start (1+ (match-beginning 0))))
	      (if (eq depth 0) (setq parent nil))
	      (if (or (equal (nth 2 var) "0")
		      (and (equal (nth 2 var) "1")
			   (string-match "char \\*$" type)))
		  (speedbar-make-tag-line
		   'bracket ?? nil nil
		   (concat expr "\t" value)
		   (if (or parent (eq status 'out-of-scope))
		       nil 'gdb-edit-value)
		   nil
		   (if gdb-show-changed-values
		       (or parent (case status
				    (changed 'font-lock-warning-face)
				    (out-of-scope 'shadow)
				    (t t)))
		     t)
		   depth)
		(if (eq status 'out-of-scope) (setq parent 'shadow))
		(if (and (nth 1 var-list)
			 (string-match (concat varnum "\\.")
				       (car (nth 1 var-list))))
		    (setq char ?-)
		  (setq char ?+))
		(if (string-match "\\*$\\|\\*&$" type)
		    (speedbar-make-tag-line
		     'bracket char
		     'gdb-speedbar-expand-node varnum
		     (concat expr "\t" type "\t" value)
		     (if (or parent (eq status 'out-of-scope))
			 nil 'gdb-edit-value)
		     nil
		     (if gdb-show-changed-values
			 (or parent (case status
				      (changed 'font-lock-warning-face)
				      (out-of-scope 'shadow)
				      (t t)))
		       t)
		     depth)
		  (speedbar-make-tag-line
		   'bracket char
		   'gdb-speedbar-expand-node varnum
		   (concat expr "\t" type)
		   nil nil
		   (if (and (or parent status) gdb-show-changed-values)
		       'shadow t)
		   depth))))
	    (setq var-list (cdr var-list)))))
       (t (unless (and (save-excursion
			 (goto-char (point-min))
			 (looking-at "Current Stack:"))
		       (equal llgud-last-last-frame llgud-last-speedbar-stackframe))
	    (let ((llgud-frame-list
	    (cond ((eq minor-mode 'gdb)
		   (llgud-gdb-get-stackframe buffer))
		  ;; Add more debuggers here!
		  (t (speedbar-remove-localized-speedbar-support buffer)
		     nil))))
	      (erase-buffer)
	      (if (not llgud-frame-list)
		  (insert "No Stack frames\n")
		(insert "Current Stack:\n"))
	      (dolist (frame llgud-frame-list)
		(insert (nth 1 frame) ":\n")
		(if (= (length frame) 2)
		(progn
		  (speedbar-insert-button (car frame)
					  'speedbar-directory-face
					  nil nil nil t))
		(speedbar-insert-button
		 (car frame)
		 'speedbar-file-face
		 'speedbar-highlight-face
		 (cond ((memq minor-mode '(gdbmi gdba gdb))
			'llgud-gdb-goto-stackframe)
		       (t (error "Should never be here")))
		 frame t))))
	    (setq llgud-last-speedbar-stackframe llgud-last-last-frame))))
      (set-window-start window start)
      (set-window-point window p))))


;; ======================================================================
;; gdb functions

;; History of argument lists passed to gdb.
(defvar llgud-gdb-history nil)

(defcustom llgud-llgud-gdb-command-name "gdb --fullname"
  "Default command to run an executable under GDB in text command mode.
The option \"--fullname\" must be included in this value."
   :type 'string
   :group 'llgud)

(defvar llgud-gdb-marker-regexp
  ;; This used to use path-separator instead of ":";
  ;; however, we found that on both Windows 32 and MSDOS
  ;; a colon is correct here.
  (concat "\032\032\\(.:?[^" ":" "\n]*\\)" ":"
	  "\\([0-9]*\\)" ":" ".*\n"))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar llgud-marker-acc "")
(make-variable-buffer-local 'llgud-marker-acc)

(defun llgud-gdb-marker-filter (string)
  (setq llgud-marker-acc (concat llgud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match llgud-gdb-marker-regexp llgud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       llgud-last-frame (cons (match-string 1 llgud-marker-acc)
			    (string-to-number (match-string 2 llgud-marker-acc)))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring llgud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       llgud-marker-acc (substring llgud-marker-acc (match-end 0))))

    ;; Check for annotations and change llgud-minor-mode to 'gdba if
    ;; they are found.
    (while (string-match "\n\032\032\\(.*\\)\n" llgud-marker-acc)
      (let ((match (match-string 1 llgud-marker-acc)))

	(setq
	 ;; Append any text before the marker to the output we're going
	 ;; to return - we don't include the marker in this text.
	 output (concat output
			(substring llgud-marker-acc 0 (match-beginning 0)))

	 ;; Set the accumulator to the remaining text.

	 llgud-marker-acc (substring llgud-marker-acc (match-end 0)))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; llgud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\n\\(\032.*\\)?\\'" llgud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring llgud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq llgud-marker-acc
		(substring llgud-marker-acc (match-beginning 0))))

      (setq output (concat output llgud-marker-acc)
	    llgud-marker-acc ""))

    output))

(easy-mmode-defmap llgud-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of llgud startup command."
  :inherit minibuffer-local-map)

(defun llgud-query-cmdline (minor-mode &optional init)
  (let* ((hist-sym (llgud-symbol 'history nil minor-mode))
	 (cmd-name (llgud-val 'command-name minor-mode)))
    (unless (boundp hist-sym) (set hist-sym nil))
    (read-from-minibuffer
     (format "Run %s (like this): " minor-mode)
     (or (car-safe (symbol-value hist-sym))
	 (concat (or cmd-name (symbol-name minor-mode))
		 " "
		 (or init
		     (let ((file nil))
		       (dolist (f (directory-files default-directory) file)
			 (if (and (file-executable-p f)
				  (not (file-directory-p f))
				  (or (not file)
				      (file-newer-than-file-p f file)))
			     (setq file f)))))))
     llgud-minibuffer-local-map nil
     hist-sym)))

(defvar gdb-first-prompt t)

(defvar llgud-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in `llgud-filter'.")

;; The old gdb command (text command mode).  The new one is in gdb-ui.el.
;;;###autoload
(defun llgud-gdb (command-line)
  "Run gdb on program FILE in buffer *llgud-FILE*.
The directory containing FILE becomes the initial working
directory and source-file directory for your debugger."
  (interactive (list (llgud-query-cmdline 'llgud-gdb)))

  (when (and llgud-comint-buffer
	   (buffer-name llgud-comint-buffer)
	   (get-buffer-process llgud-comint-buffer)
	   (with-current-buffer llgud-comint-buffer (eq llgud-minor-mode 'gdba)))
	(gdb-restore-windows)
	(error
	 "Multiple debugging requires restarting in text command mode"))

  (llgud-common-init command-line nil 'llgud-gdb-marker-filter)
  (set (make-local-variable 'llgud-minor-mode) 'gdb)

  (llgud-def llgud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
  (llgud-def llgud-tbreak "tbreak %f:%l" "\C-t"
	   "Set temporary breakpoint at current line.")
  (llgud-def llgud-remove "clear %f:%l" "\C-d" "Remove breakpoint at current line")
  (llgud-def llgud-step   "step %p"     "\C-s" "Step one source line with display.")
  (llgud-def llgud-stepi  "stepi %p"    "\C-i" "Step one instruction with display.")
  (llgud-def llgud-next   "next %p"     "\C-n" "Step one line (skip functions).")
  (llgud-def llgud-nexti  "nexti %p" nil   "Step one instruction (skip functions).")
  (llgud-def llgud-cont   "cont"     "\C-r" "Continue with display.")
  (llgud-def llgud-finish "finish"   "\C-f" "Finish executing current function.")
  (llgud-def llgud-jump
	   (progn (llgud-call "tbreak %f:%l") (llgud-call "jump %f:%l"))
	   "\C-j" "Set execution address to current line.")

  (llgud-def llgud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
  (llgud-def llgud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
  (llgud-def llgud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (llgud-def llgud-pstar  "print* %e" nil
	   "Evaluate C dereferenced pointer expression at point.")

  ;; For debugging Emacs only.
  (llgud-def llgud-pv "pv1 %e"      "\C-v" "Print the value of the lisp variable.")

  (llgud-def llgud-until  "until %l" "\C-u" "Continue to current line.")
  (llgud-def llgud-run    "run"	 nil    "Run the program.")

  (local-set-key "\C-i" 'llgud-gdb-complete-command)
  (setq comint-prompt-regexp "^(.*gdb[+]?) *")
  (setq paragraph-start comint-prompt-regexp)
  (setq gdb-first-prompt t)
  (setq llgud-running nil)
  (setq gdb-ready nil)
  (setq llgud-filter-pending-text nil)
  (run-hooks 'llgud-gdb-mode-hook))

;; One of the nice features of GDB is its impressive support for
;; context-sensitive command completion.  We preserve that feature
;; in the LLGUD buffer by using a GDB command designed just for Emacs.

;; The completion process filter indicates when it is finished.
(defvar llgud-gdb-fetch-lines-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar llgud-gdb-fetch-lines-string)

;; We need to know how much of the completion to chop off.
(defvar llgud-gdb-fetch-lines-break)

;; The completion list is constructed by the process filter.
(defvar llgud-gdb-fetched-lines)

(defun llgud-gdb-complete-command (&optional command a b)
  "Perform completion on the GDB command preceding point.
This is implemented using the GDB `complete' command which isn't
available with older versions of GDB."
  (interactive)
  (if command
      ;; Used by llgud-watch in mini-buffer.
      (setq command (concat "p " command))
    ;; Used in LLGUD buffer.
    (let ((end (point)))
      (setq command (buffer-substring (comint-line-beginning-position) end))))
  (let* ((command-word
	  ;; Find the word break.  This match will always succeed.
	  (and (string-match "\\(\\`\\| \\)\\([^ ]*\\)\\'" command)
	       (substring command (match-beginning 2))))
	 (complete-list
	  (llgud-gdb-run-command-fetch-lines (concat "complete " command)
					   (current-buffer)
					   ;; From string-match above.
					   (match-beginning 2))))
    ;; Protect against old versions of GDB.
    (and complete-list
	 (string-match "^Undefined command: \"complete\"" (car complete-list))
	 (error "This version of GDB doesn't support the `complete' command"))
    ;; Sort the list like readline.
    (setq complete-list (sort complete-list (function string-lessp)))
    ;; Remove duplicates.
    (let ((first complete-list)
	  (second (cdr complete-list)))
      (while second
	(if (string-equal (car first) (car second))
	    (setcdr first (setq second (cdr second)))
	  (setq first second
		second (cdr second)))))
    ;; Add a trailing single quote if there is a unique completion
    ;; and it contains an odd number of unquoted single quotes.
    (and (= (length complete-list) 1)
	 (let ((str (car complete-list))
	       (pos 0)
	       (count 0))
	   (while (string-match "\\([^'\\]\\|\\\\'\\)*'" str pos)
	     (setq count (1+ count)
		   pos (match-end 0)))
	   (and (= (mod count 2) 1)
		(setq complete-list (list (concat str "'"))))))
    ;; Let comint handle the rest.
    (comint-dynamic-simple-complete command-word complete-list)))

;; The completion process filter is installed temporarily to slurp the
;; output of GDB up to the next prompt and build the completion list.
(defun llgud-gdb-fetch-lines-filter (string filter)
  "Filter used to read the list of lines output by a command.
STRING is the output to filter.
It is passed through FILTER before we look at it."
  (setq string (funcall filter string))
  (setq string (concat llgud-gdb-fetch-lines-string string))
  (while (string-match "\n" string)
    (push (substring string llgud-gdb-fetch-lines-break (match-beginning 0))
	  llgud-gdb-fetched-lines)
    (setq string (substring string (match-end 0))))
  (if (string-match comint-prompt-regexp string)
      (progn
	(setq llgud-gdb-fetch-lines-in-progress nil)
	string)
    (progn
      (setq llgud-gdb-fetch-lines-string string)
      "")))

;; gdb speedbar functions

(defun llgud-gdb-goto-stackframe (text token indent)
  "Goto the stackframe described by TEXT, TOKEN, and INDENT."
  (speedbar-with-attached-buffer
   (llgud-basic-call (concat "server frame " (nth 1 token)))
   (sit-for 1)))

(defvar llgud-gdb-fetched-stack-frame nil
  "Stack frames we are fetching from GDB.")

;(defun llgud-gdb-get-scope-data (text token indent)
;  ;; checkdoc-params: (indent)
;  "Fetch data associated with a stack frame, and expand/contract it.
;Data to do this is retrieved from TEXT and TOKEN."
;  (let ((args nil) (scope nil))
;    (llgud-gdb-run-command-fetch-lines "info args")
;
;    (llgud-gdb-run-command-fetch-lines "info local")
;
;    ))

(defun llgud-gdb-get-stackframe (buffer)
  "Extract the current stack frame out of the LLGUD GDB BUFFER."
  (let ((newlst nil)
	(fetched-stack-frame-list
	 (llgud-gdb-run-command-fetch-lines "server backtrace" buffer)))
    (if (and (car fetched-stack-frame-list)
	     (string-match "No stack" (car fetched-stack-frame-list)))
	;; Go into some other mode???
	nil
      (dolist (e fetched-stack-frame-list)
	(let ((name nil) (num nil))
	  (if (not (or
		    (string-match "^#\\([0-9]+\\) +[0-9a-fx]+ in \\([:0-9a-zA-Z_]+\\) (" e)
		    (string-match "^#\\([0-9]+\\) +\\([:0-9a-zA-Z_]+\\) (" e)))
	      (if (not (string-match
			"at \\([-0-9a-zA-Z_.]+\\):\\([0-9]+\\)$" e))
		  nil
		(setcar newlst
			(list (nth 0 (car newlst))
			      (nth 1 (car newlst))
			      (match-string 1 e)
			      (match-string 2 e))))
	    (setq num (match-string 1 e)
		  name (match-string 2 e))
	    (setq newlst
		  (cons
		   (if (string-match
			"at \\([-0-9a-zA-Z_.]+\\):\\([0-9]+\\)$" e)
		       (list name num (match-string 1 e)
			     (match-string 2 e))
		     (list name num))
		   newlst)))))
      (nreverse newlst))))

;(defun llgud-gdb-selected-frame-info (buffer)
;  "Learn GDB information for the currently selected stack frame in BUFFER."
;  )

(defun llgud-gdb-run-command-fetch-lines (command buffer &optional skip)
  "Run COMMAND, and return the list of lines it outputs.
BUFFER is the current buffer which may be the LLGUD buffer in which to run.
SKIP is the number of chars to skip on each line, it defaults to 0."
  (with-current-buffer llgud-comint-buffer
    (if (and (eq llgud-comint-buffer buffer)
	     (save-excursion
	       (goto-char (point-max))
	       (forward-line 0)
	       (not (looking-at comint-prompt-regexp))))
	nil
      ;; Much of this copied from GDB complete, but I'm grabbing the stack
      ;; frame instead.
      (let ((llgud-gdb-fetch-lines-in-progress t)
	    (llgud-gdb-fetched-lines nil)
	    (llgud-gdb-fetch-lines-string nil)
	    (llgud-gdb-fetch-lines-break (or skip 0))
	    (llgud-marker-filter
	     `(lambda (string)
		(llgud-gdb-fetch-lines-filter string ',llgud-marker-filter))))
	;; Issue the command to GDB.
	(llgud-basic-call command)
	;; Slurp the output.
	(while llgud-gdb-fetch-lines-in-progress
	  (accept-process-output (get-buffer-process llgud-comint-buffer)))
	(nreverse llgud-gdb-fetched-lines)))))


;; ======================================================================
;; lldb functions

;; History of argument lists passed to lldb.
(defvar llgud-lldb-history nil)

;; Keeps track of breakpoint created.  In the following case, the id is "1".
;; It is used to implement temporary breakpoint.
;; (lldb) b main.c:39
;; breakpoint set --file 'main.c' --line 39
;; Breakpoint created: 1: file ='main.c', line = 39, locations = 1
(defvar llgud-breakpoint-id nil)

(defun lldb-extract-breakpoint-id (string)
  ;; Search for "Breakpoint created: \\([^:\n]*\\):" pattern.
  ;(message "llgud-marker-acc string is: |%s|" string)
  (if (string-match "Breakpoint created: \\([^:\n]*\\):" string)
      (progn
        (setq llgud-breakpoint-id (match-string 1 string))
        (message "breakpoint id: %s" llgud-breakpoint-id)))
)

(defun llgud-lldb-marker-filter (string)
  (setq llgud-marker-acc
	(if llgud-marker-acc (concat llgud-marker-acc string) string))
  (lldb-extract-breakpoint-id llgud-marker-acc)
  (let (start)
    ;; Process all complete markers in this chunk
    (while (or
            ;; (lldb) r
            ;; Process 15408 launched: '/Volumes/data/lldb/svn/trunk/test/conditional_break/a.out' (x86_64)
            ;; (lldb) Process 15408 stopped
            ;; * thread #1: tid = 0x2e03, 0x0000000100000de8 a.out`c + 7 at main.c:39, stop reason = breakpoint 1.1, queue = com.apple.main-thread
            (string-match " at \\([^:\n]*\\):\\([0-9]*\\), stop reason = .*\n"
                          llgud-marker-acc start)
            ;; (lldb) frame select -r 1
            ;; frame #1: 0x0000000100000e09 a.out`main + 25 at main.c:44
            (string-match "^frame.* at \\([^:\n]*\\):\\([0-9]*\\)\n"
                           llgud-marker-acc start))
      ;(message "llgud-marker-acc matches our pattern....")
      (setq llgud-last-frame
            (cons (match-string 1 llgud-marker-acc)
                  (string-to-number (match-string 2 llgud-marker-acc)))
            start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" llgud-marker-acc start)
      (setq start (match-end 0)))

    ;; If we have an incomplete line, store it in llgud-marker-acc.
    (setq llgud-marker-acc (substring llgud-marker-acc (or start 0))))
  string)

;; Keeps track of whether the Python lldb_oneshot_break function definition has
;; been exec'ed.
(defvar lldb-oneshot-break-defined nil)

;;;###autoload
(defun lldb (command-line)
  "Run lldb on program FILE in buffer *llgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (llgud-query-cmdline 'lldb)))

  (llgud-common-init command-line nil 'llgud-lldb-marker-filter)
  (set (make-local-variable 'llgud-minor-mode) 'lldb)
  (setq lldb-oneshot-break-defined nil)

  ;; Make lldb dump fullpath instead of basename for a file.
  ;; See also llgud-lldb-marker-filter where llgud-last-frame is grokked from lldb output.
  (progn
    (llgud-call "settings set frame-format frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}\\n")
    (sit-for 1)
    (llgud-call "settings set thread-format thread #${thread.index}: tid = ${thread.id}{, ${frame.pc}}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}{, stop reason = ${thread.stop-reason}}\\n")
    (sit-for 1))

  (llgud-def llgud-listb  "breakpoint list"
                      "l"    "List all breakpoints.")
  (llgud-def llgud-bt     "thread backtrace"
                      "b"    "Show stack for the current thread.")
  (llgud-def llgud-bt-all "thread backtrace all"
                      "B"    "Show stacks for all the threads.")

  (llgud-def llgud-break  "breakpoint set -f %f -l %l"
                      "\C-b" "Set breakpoint at current line.")
  (llgud-def llgud-tbreak
	   (progn (llgud-call "breakpoint set -f %f -l %l")
                  (sit-for 1)
                  (if (not lldb-oneshot-break-defined)
                      (progn
                        ;; The "\\n"'s are required to escape the newline chars
                        ;; passed to the lldb process.
                        (llgud-call (concat "script exec \"def lldb_oneshot_break(frame, bp_loc):\\n"
                                                        "    target=frame.GetThread().GetProcess().GetTarget()\\n"
                                                        "    bp=bp_loc.GetBreakpoint()\\n"
                                                        "    print 'Deleting oneshot breakpoint:', bp\\n"
                                                        "    target.BreakpointDelete(bp.GetID())\""))
                        (sit-for 1)
                        ;; Set the flag since Python knows about the function def now.
                        (setq lldb-oneshot-break-defined t)))
                  (llgud-call "breakpoint command add -p %b -o 'lldb_oneshot_break(frame, bp_loc)'"))
	              "\C-t" "Set temporary breakpoint at current line.")
  (llgud-def llgud-remove "breakpoint clear -f %f -l %l"
                      "\C-d" "Remove breakpoint at current line")
  (llgud-def llgud-step   "thread step-in"
                      "\C-s" "Step one source line with display.")
  (llgud-def llgud-stepi  "thread step-inst"
                      "\C-i" "Step one instruction with display.")
  (llgud-def llgud-next   "thread step-over"
                      "\C-n" "Step one line (skip functions).")
  (llgud-def llgud-nexti  "thread step-inst-over"
                      nil    "Step one instruction (skip functions).")
  (llgud-def llgud-cont   "process continue"
                      "\C-r" "Continue with display.")
  (llgud-def llgud-finish "thread step-out"
                      "\C-f" "Finish executing current function.")
  (llgud-def llgud-up
           (progn (llgud-call "frame select -r 1")
                  (sit-for 1))
                      "<"    "Up 1 stack frame.")
  (llgud-def llgud-down
           (progn (llgud-call "frame select -r -1")
                  (sit-for 1))
                      ">"    "Down 1 stack frame.")
  (llgud-def llgud-print  "expression -- %e"
                      "\C-p" "Evaluate C expression at point.")
  (llgud-def llgud-pstar  "expression -- *%e"
                      nil    "Evaluate C dereferenced pointer expression at point.")
  (llgud-def llgud-run    "run"
                      "r"    "Run the program.")
  (llgud-def llgud-stop-subjob    "process kill"
                      "s"    "Stop the program.")

  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'lldb-mode-hook)
  )


;; ======================================================================
;; sdb functions

;; History of argument lists passed to sdb.
(defvar llgud-sdb-history nil)

(defvar llgud-sdb-needs-tags (not (file-exists-p "/var"))
  "If nil, we're on a System V Release 4 and don't need the tags hack.")

(defvar llgud-sdb-lastfile nil)

(defun llgud-sdb-marker-filter (string)
  (setq llgud-marker-acc
	(if llgud-marker-acc (concat llgud-marker-acc string) string))
  (let (start)
    ;; Process all complete markers in this chunk
    (while
	(cond
	 ;; System V Release 3.2 uses this format
	 ((string-match "\\(^\\|\n\\)\\*?\\(0x\\w* in \\)?\\([^:\n]*\\):\\([0-9]*\\):.*\n"
			llgud-marker-acc start)
	  (setq llgud-last-frame
		(cons (match-string 3 llgud-marker-acc)
		      (string-to-number (match-string 4 llgud-marker-acc)))))
	 ;; System V Release 4.0 quite often clumps two lines together
	 ((string-match "^\\(BREAKPOINT\\|STEPPED\\) process [0-9]+ function [^ ]+ in \\(.+\\)\n\\([0-9]+\\):"
			llgud-marker-acc start)
	  (setq llgud-sdb-lastfile (match-string 2 llgud-marker-acc))
	  (setq llgud-last-frame
		(cons llgud-sdb-lastfile
		      (string-to-number (match-string 3 llgud-marker-acc)))))
	 ;; System V Release 4.0
	 ((string-match "^\\(BREAKPOINT\\|STEPPED\\) process [0-9]+ function [^ ]+ in \\(.+\\)\n"
			llgud-marker-acc start)
	  (setq llgud-sdb-lastfile (match-string 2 llgud-marker-acc)))
	 ((and llgud-sdb-lastfile (string-match "^\\([0-9]+\\):"
					      llgud-marker-acc start))
	       (setq llgud-last-frame
		     (cons llgud-sdb-lastfile
			   (string-to-number (match-string 1 llgud-marker-acc)))))
	 (t
	  (setq llgud-sdb-lastfile nil)))
      (setq start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" llgud-marker-acc start)
      (setq start (match-end 0)))

    ;; If we have an incomplete line, store it in llgud-marker-acc.
    (setq llgud-marker-acc (substring llgud-marker-acc (or start 0))))
  string)

(defun llgud-sdb-find-file (f)
  (if llgud-sdb-needs-tags (find-tag-noselect f) (find-file-noselect f)))

;;;###autoload
(defun sdb (command-line)
  "Run sdb on program FILE in buffer *llgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (llgud-query-cmdline 'sdb)))

  (if llgud-sdb-needs-tags (require 'etags))
  (if (and llgud-sdb-needs-tags
	   (not (and (boundp 'tags-file-name)
		     (stringp tags-file-name)
		     (file-exists-p tags-file-name))))
      (error "The sdb support requires a valid tags table to work"))

  (llgud-common-init command-line nil 'llgud-sdb-marker-filter 'llgud-sdb-find-file)
  (set (make-local-variable 'llgud-minor-mode) 'sdb)

  (llgud-def llgud-break  "%l b" "\C-b"   "Set breakpoint at current line.")
  (llgud-def llgud-tbreak "%l c" "\C-t"   "Set temporary breakpoint at current line.")
  (llgud-def llgud-remove "%l d" "\C-d"   "Remove breakpoint at current line")
  (llgud-def llgud-step   "s %p" "\C-s"   "Step one source line with display.")
  (llgud-def llgud-stepi  "i %p" "\C-i"   "Step one instruction with display.")
  (llgud-def llgud-next   "S %p" "\C-n"   "Step one line (skip functions).")
  (llgud-def llgud-cont   "c"    "\C-r"   "Continue with display.")
  (llgud-def llgud-print  "%e/"  "\C-p"   "Evaluate C expression at point.")

  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'sdb-mode-hook)
  )

;; ======================================================================
;; dbx functions

;; History of argument lists passed to dbx.
(defvar llgud-dbx-history nil)

(defcustom llgud-dbx-directories nil
  "*A list of directories that dbx should search for source code.
If nil, only source files in the program directory
will be known to dbx.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
		 (repeat :value ("")
			 directory))
  :group 'llgud)

(defun llgud-dbx-massage-args (file args)
  (nconc (let ((directories llgud-dbx-directories)
	       (result nil))
	   (while directories
	     (setq result (cons (car directories) (cons "-I" result)))
	     (setq directories (cdr directories)))
	   (nreverse result))
	 args))

(defun llgud-dbx-marker-filter (string)
  (setq llgud-marker-acc (if llgud-marker-acc (concat llgud-marker-acc string) string))

  (let (start)
    ;; Process all complete markers in this chunk.
    (while (or (string-match
		"stopped in .* at line \\([0-9]*\\) in file \"\\([^\"]*\\)\""
		llgud-marker-acc start)
	       (string-match
		"signal .* in .* at line \\([0-9]*\\) in file \"\\([^\"]*\\)\""
		llgud-marker-acc start))
      (setq llgud-last-frame
	    (cons (match-string 2 llgud-marker-acc)
		  (string-to-number (match-string 1 llgud-marker-acc)))
	    start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" llgud-marker-acc start)
      (setq start (match-end 0)))

    ;; If the incomplete line APPEARS to begin with another marker, keep it
    ;; in the accumulator.  Otherwise, clear the accumulator to avoid an
    ;; unnecessary concat during the next call.
    (setq llgud-marker-acc
	  (if (string-match "\\(stopped\\|signal\\)" llgud-marker-acc start)
	      (substring llgud-marker-acc (match-beginning 0))
	    nil)))
  string)

;; Functions for Mips-style dbx.  Given the option `-emacs', documented in
;; OSF1, not necessarily elsewhere, it produces markers similar to gdb's.
(defvar llgud-mips-p
  (or (string-match "^mips-[^-]*-ultrix" system-configuration)
      ;; We haven't tested llgud on this system:
      (string-match "^mips-[^-]*-riscos" system-configuration)
      ;; It's documented on OSF/1.3
      (string-match "^mips-[^-]*-osf1" system-configuration)
      (string-match "^alpha[^-]*-[^-]*-osf" system-configuration))
  "Non-nil to assume the MIPS/OSF dbx conventions (argument `-emacs').")

(defvar llgud-dbx-command-name
  (concat "dbx" (if llgud-mips-p " -emacs")))

;; This is just like the gdb one except for the regexps since we need to cope
;; with an optional breakpoint number in [] before the ^Z^Z
(defun llgud-mipsdbx-marker-filter (string)
  (setq llgud-marker-acc (concat llgud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match
	    ;; This is like th gdb marker but with an optional
	    ;; leading break point number like `[1] '
	    "[][ 0-9]*\032\032\\([^:\n]*\\):\\([0-9]*\\):.*\n"
	    llgud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       llgud-last-frame
       (cons (match-string 1 llgud-marker-acc)
	     (string-to-number (match-string 2 llgud-marker-acc)))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring llgud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       llgud-marker-acc (substring llgud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; llgud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "[][ 0-9]*\032.*\\'" llgud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring llgud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq llgud-marker-acc
		(substring llgud-marker-acc (match-beginning 0))))

      (setq output (concat output llgud-marker-acc)
	    llgud-marker-acc ""))

    output))

;; The dbx in IRIX is a pain.  It doesn't print the file name when
;; stopping at a breakpoint (but you do get it from the `up' and
;; `down' commands...).  The only way to extract the information seems
;; to be with a `file' command, although the current line number is
;; available in $curline.  Thus we have to look for output which
;; appears to indicate a breakpoint.  Then we prod the dbx sub-process
;; to output the information we want with a combination of the
;; `printf' and `file' commands as a pseudo marker which we can
;; recognise next time through the marker-filter.  This would be like
;; the gdb marker but you can't get the file name without a newline...
;; Note that llgud-remove won't work since Irix dbx expects a breakpoint
;; number rather than a line number etc.  Maybe this could be made to
;; work by listing all the breakpoints and picking the one(s) with the
;; correct line number, but life's too short.
;;   d.love@dl.ac.uk (Dave Love) can be blamed for this

(defvar llgud-irix-p
  (and (string-match "^mips-[^-]*-irix" system-configuration)
       (not (string-match "irix[6-9]\\.[1-9]" system-configuration)))
  "Non-nil to assume the interface appropriate for IRIX dbx.
This works in IRIX 4, 5 and 6, but `llgud-dbx-use-stopformat-p' provides
a better solution in 6.1 upwards.")
(defvar llgud-dbx-use-stopformat-p
  (string-match "irix[6-9]\\.[1-9]" system-configuration)
  "Non-nil to use the dbx feature present at least from Irix 6.1
whereby $stopformat=1 produces an output format compatible with
`llgud-dbx-marker-filter'.")
;; [Irix dbx seems to be a moving target.  The dbx output changed
;; subtly sometime between OS v4.0.5 and v5.2 so that, for instance,
;; the output from `up' is no longer spotted by llgud (and it's probably
;; not distinctive enough to try to match it -- use C-<, C->
;; exclusively) .  For 5.3 and 6.0, the $curline variable changed to
;; `long long'(why?!), so the printf stuff needed changing.  The line
;; number was cast to `long' as a compromise between the new `long
;; long' and the original `int'.  This is reported not to work in 6.2,
;; so it's changed back to int -- don't make your sources too long.
;; From Irix6.1 (but not 6.0?) dbx supports an undocumented feature
;; whereby `set $stopformat=1' reportedly produces output compatible
;; with `llgud-dbx-marker-filter', which we prefer.

;; The process filter is also somewhat
;; unreliable, sometimes not spotting the markers; I don't know
;; whether there's anything that can be done about that.  It would be
;; much better if SGI could be persuaded to (re?)instate the MIPS
;; -emacs flag for gdb-like output (which ought to be possible as most
;; of the communication I've had over it has been from sgi.com).]

;; this filter is influenced by the xdb one rather than the gdb one
(defun llgud-irixdbx-marker-filter (string)
  (let (result (case-fold-search nil))
    (if (or (string-match comint-prompt-regexp string)
	    (string-match ".*\012" string))
	(setq result (concat llgud-marker-acc string)
	      llgud-marker-acc "")
      (setq llgud-marker-acc (concat llgud-marker-acc string)))
    (if result
	(cond
	 ;; look for breakpoint or signal indication e.g.:
	 ;; [2] Process  1267 (pplot) stopped at [params:338 ,0x400ec0]
	 ;; Process  1281 (pplot) stopped at [params:339 ,0x400ec8]
	 ;; Process  1270 (pplot) Floating point exception [._read._read:16 ,0x452188]
	 ((string-match
	   "^\\(\\[[0-9]+] \\)?Process +[0-9]+ ([^)]*) [^[]+\\[[^]\n]*]\n"
	   result)
	  ;; prod dbx into printing out the line number and file
	  ;; name in a form we can grok as below
	  (process-send-string (get-buffer-process llgud-comint-buffer)
			       "printf \"\032\032%1d:\",(int)$curline;file\n"))
	 ;; look for result of, say, "up" e.g.:
	 ;; .pplot.pplot(0x800) ["src/pplot.f":261, 0x400c7c]
	 ;; (this will also catch one of the lines printed by "where")
	 ((string-match
	   "^[^ ][^[]*\\[\"\\([^\"]+\\)\":\\([0-9]+\\), [^]]+]\n"
	   result)
	  (let ((file (match-string 1 result)))
	    (if (file-exists-p file)
		(setq llgud-last-frame
		      (cons (match-string 1 result)
			    (string-to-number (match-string 2 result))))))
	  result)
	 ((string-match			; kluged-up marker as above
	   "\032\032\\([0-9]*\\):\\(.*\\)\n" result)
	  (let ((file (llgud-file-name (match-string 2 result))))
	    (if (and file (file-exists-p file))
		(setq llgud-last-frame
		      (cons file
			    (string-to-number (match-string 1 result))))))
	  (setq result (substring result 0 (match-beginning 0))))))
    (or result "")))

(defvar llgud-dgux-p (string-match "-dgux" system-configuration)
  "Non-nil means to assume the interface approriate for DG/UX dbx.
This was tested using R4.11.")

;; There are a couple of differences between DG's dbx output and normal
;; dbx output which make it nontrivial to integrate this into the
;; standard dbx-marker-filter (mainly, there are a different number of
;; backreferences).  The markers look like:
;;
;;     (0) Stopped at line 10, routine main(argc=1, argv=0xeffff0e0), file t.c
;;
;; from breakpoints (the `(0)' there isn't constant, it's the breakpoint
;; number), and
;;
;;     Stopped at line 13, routine main(argc=1, argv=0xeffff0e0), file t.c
;;
;; from signals and
;;
;;     Frame 21, line 974, routine command_loop(), file keyboard.c
;;
;; from up/down/where.

(defun llgud-dguxdbx-marker-filter (string)
  (setq llgud-marker-acc (if llgud-marker-acc
			   (concat llgud-marker-acc string)
			 string))
  (let ((re (concat "^\\(\\(([0-9]+) \\)?Stopped at\\|Frame [0-9]+,\\)"
		    " line \\([0-9]+\\), routine .*, file \\([^ \t\n]+\\)"))
	start)
    ;; Process all complete markers in this chunk.
    (while (string-match re llgud-marker-acc start)
      (setq llgud-last-frame
	    (cons (match-string 4 llgud-marker-acc)
		  (string-to-number (match-string 3 llgud-marker-acc)))
	    start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" llgud-marker-acc start)
      (setq start (match-end 0)))

    ;; If the incomplete line APPEARS to begin with another marker, keep it
    ;; in the accumulator.  Otherwise, clear the accumulator to avoid an
    ;; unnecessary concat during the next call.
    (setq llgud-marker-acc
	  (if (string-match "Stopped\\|Frame" llgud-marker-acc start)
	      (substring llgud-marker-acc (match-beginning 0))
	    nil)))
  string)

;;;###autoload
(defun dbx (command-line)
  "Run dbx on program FILE in buffer *llgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (llgud-query-cmdline 'dbx)))

  (cond
   (llgud-mips-p
    (llgud-common-init command-line nil 'llgud-mipsdbx-marker-filter))
   (llgud-irix-p
    (llgud-common-init command-line 'llgud-dbx-massage-args
		     'llgud-irixdbx-marker-filter))
   (llgud-dgux-p
    (llgud-common-init command-line 'llgud-dbx-massage-args
		     'llgud-dguxdbx-marker-filter))
   (t
    (llgud-common-init command-line 'llgud-dbx-massage-args
		     'llgud-dbx-marker-filter)))

  (set (make-local-variable 'llgud-minor-mode) 'dbx)

  (cond
   (llgud-mips-p
    (llgud-def llgud-up	"up %p"	  "<" "Up (numeric arg) stack frames.")
    (llgud-def llgud-down	"down %p" ">" "Down (numeric arg) stack frames.")
    (llgud-def llgud-break  "stop at \"%f\":%l"
				  "\C-b" "Set breakpoint at current line.")
    (llgud-def llgud-finish "return"  "\C-f" "Finish executing current function."))
   (llgud-irix-p
    (llgud-def llgud-break  "stop at \"%d%f\":%l"
				  "\C-b" "Set breakpoint at current line.")
    (llgud-def llgud-finish "return"  "\C-f" "Finish executing current function.")
    (llgud-def llgud-up	"up %p; printf \"\032\032%1d:\",(int)$curline;file\n"
	     "<" "Up (numeric arg) stack frames.")
    (llgud-def llgud-down "down %p; printf \"\032\032%1d:\",(int)$curline;file\n"
	     ">" "Down (numeric arg) stack frames.")
    ;; Make dbx give out the source location info that we need.
    (process-send-string (get-buffer-process llgud-comint-buffer)
			 "printf \"\032\032%1d:\",(int)$curline;file\n"))
   (t
    (llgud-def llgud-up	"up %p"   "<" "Up (numeric arg) stack frames.")
    (llgud-def llgud-down	"down %p" ">" "Down (numeric arg) stack frames.")
    (llgud-def llgud-break "file \"%d%f\"\nstop at %l"
				  "\C-b" "Set breakpoint at current line.")
    (if llgud-dbx-use-stopformat-p
	(process-send-string (get-buffer-process llgud-comint-buffer)
			     "set $stopformat=1\n"))))

  (llgud-def llgud-remove "clear %l"  "\C-d" "Remove breakpoint at current line")
  (llgud-def llgud-step   "step %p"   "\C-s" "Step one line with display.")
  (llgud-def llgud-stepi  "stepi %p"  "\C-i" "Step one instruction with display.")
  (llgud-def llgud-next   "next %p"   "\C-n" "Step one line (skip functions).")
  (llgud-def llgud-nexti  "nexti %p"   nil  "Step one instruction (skip functions).")
  (llgud-def llgud-cont   "cont"      "\C-r" "Continue with display.")
  (llgud-def llgud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (llgud-def llgud-run    "run"	     nil    "Run the program.")

  (setq comint-prompt-regexp  "^[^)\n]*dbx) *")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'dbx-mode-hook)
  )

;; ======================================================================
;; xdb (HP PARISC debugger) functions

;; History of argument lists passed to xdb.
(defvar llgud-xdb-history nil)

(defcustom llgud-xdb-directories nil
  "*A list of directories that xdb should search for source code.
If nil, only source files in the program directory
will be known to xdb.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
		 (repeat :value ("")
			 directory))
  :group 'llgud)

(defun llgud-xdb-massage-args (file args)
  (nconc (let ((directories llgud-xdb-directories)
	       (result nil))
	   (while directories
	     (setq result (cons (car directories) (cons "-d" result)))
	     (setq directories (cdr directories)))
	   (nreverse result))
	 args))

;; xdb does not print the lines all at once, so we have to accumulate them
(defun llgud-xdb-marker-filter (string)
  (let (result)
    (if (or (string-match comint-prompt-regexp string)
	    (string-match ".*\012" string))
	(setq result (concat llgud-marker-acc string)
	      llgud-marker-acc "")
      (setq llgud-marker-acc (concat llgud-marker-acc string)))
    (if result
	(if (or (string-match "\\([^\n \t:]+\\): [^:]+: \\([0-9]+\\)[: ]"
			      result)
                (string-match "[^: \t]+:[ \t]+\\([^:]+\\): [^:]+: \\([0-9]+\\):"
                              result))
            (let ((line (string-to-number (match-string 2 result)))
                  (file (llgud-file-name (match-string 1 result))))
              (if file
                  (setq llgud-last-frame (cons file line))))))
    (or result "")))

;;;###autoload
(defun xdb (command-line)
  "Run xdb on program FILE in buffer *llgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable `llgud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory."
  (interactive (list (llgud-query-cmdline 'xdb)))

  (llgud-common-init command-line 'llgud-xdb-massage-args
		   'llgud-xdb-marker-filter)
  (set (make-local-variable 'llgud-minor-mode) 'xdb)

  (llgud-def llgud-break  "b %f:%l"    "\C-b" "Set breakpoint at current line.")
  (llgud-def llgud-tbreak "b %f:%l\\t" "\C-t"
	   "Set temporary breakpoint at current line.")
  (llgud-def llgud-remove "db"         "\C-d" "Remove breakpoint at current line")
  (llgud-def llgud-step   "s %p"       "\C-s" "Step one line with display.")
  (llgud-def llgud-next   "S %p"       "\C-n" "Step one line (skip functions).")
  (llgud-def llgud-cont   "c"          "\C-r" "Continue with display.")
  (llgud-def llgud-up     "up %p"      "<"    "Up (numeric arg) stack frames.")
  (llgud-def llgud-down   "down %p"    ">"    "Down (numeric arg) stack frames.")
  (llgud-def llgud-finish "bu\\t"      "\C-f" "Finish executing current function.")
  (llgud-def llgud-print  "p %e"       "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp  "^>")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'xdb-mode-hook))

;; ======================================================================
;; perldb functions

;; History of argument lists passed to perldb.
(defvar llgud-perldb-history nil)

(defun llgud-perldb-massage-args (file args)
  "Convert a command line as would be typed normally to run perldb
into one that invokes an Emacs-enabled debugging session.
\"-emacs\" is inserted where it will be $ARGV[0] (see perl5db.pl)."
  ;; FIXME: what if the command is `make perldb' and doesn't accept those extra
  ;; arguments ?
  (let* ((new-args nil)
	 (seen-e nil)
	 (shift (lambda () (push (pop args) new-args))))

    ;; Pass all switches and -e scripts through.
    (while (and args
		(string-match "^-" (car args))
		(not (equal "-" (car args)))
		(not (equal "--" (car args))))
      (when (equal "-e" (car args))
	;; -e goes with the next arg, so shift one extra.
	(or (funcall shift)
	    ;; -e as the last arg is an error in Perl.
	    (error "No code specified for -e"))
	(setq seen-e t))
      (funcall shift))

    (unless seen-e
      (if (or (not args)
	      (string-match "^-" (car args)))
	  (error "Can't use stdin as the script to debug"))
      ;; This is the program name.
      (funcall shift))

    ;; If -e specified, make sure there is a -- so -emacs is not taken
    ;; as -e macs.
    (if (and args (equal "--" (car args)))
	(funcall shift)
      (and seen-e (push "--" new-args)))

    (push "-emacs" new-args)
    (while args
      (funcall shift))

    (nreverse new-args)))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun llgud-perldb-marker-filter (string)
  (setq llgud-marker-acc (concat llgud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match "\032\032\\(\\([a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\):.*\n"
			 llgud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       llgud-last-frame
       (cons (match-string 1 llgud-marker-acc)
	     (string-to-number (match-string 3 llgud-marker-acc)))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
		      (substring llgud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       llgud-marker-acc (substring llgud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; llgud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" llgud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring llgud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq llgud-marker-acc
		(substring llgud-marker-acc (match-beginning 0))))

      (setq output (concat output llgud-marker-acc)
	    llgud-marker-acc ""))

    output))

(defcustom llgud-perldb-command-name "perl -d"
  "Default command to execute a Perl script under debugger."
  :type 'string
  :group 'llgud)

;;;###autoload
(defun perldb (command-line)
  "Run perldb on program FILE in buffer *llgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (llgud-query-cmdline 'perldb
			    (concat (or (buffer-file-name) "-e 0") " "))))

  (llgud-common-init command-line 'llgud-perldb-massage-args
		   'llgud-perldb-marker-filter)
  (set (make-local-variable 'llgud-minor-mode) 'perldb)

  (llgud-def llgud-break  "b %l"         "\C-b" "Set breakpoint at current line.")
  (llgud-def llgud-remove "B %l"         "\C-d" "Remove breakpoint at current line")
  (llgud-def llgud-step   "s"            "\C-s" "Step one source line with display.")
  (llgud-def llgud-next   "n"            "\C-n" "Step one line (skip functions).")
  (llgud-def llgud-cont   "c"            "\C-r" "Continue with display.")
;  (llgud-def llgud-finish "finish"       "\C-f" "Finish executing current function.")
;  (llgud-def llgud-up     "up %p"        "<" "Up N stack frames (numeric arg).")
;  (llgud-def llgud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
  (llgud-def llgud-print  "p %e"          "\C-p" "Evaluate perl expression at point.")
  (llgud-def llgud-until  "c %l"          "\C-u" "Continue to current line.")


  (setq comint-prompt-regexp "^  DB<+[0-9]+>+ ")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'perldb-mode-hook))

;; ======================================================================
;; pdb (Python debugger) functions

;; History of argument lists passed to pdb.
(defvar llgud-pdb-history nil)

;; Last group is for return value, e.g. "> test.py(2)foo()->None"
;; Either file or function name may be omitted: "> <string>(0)?()"
(defvar llgud-pdb-marker-regexp
  "^> \\([-a-zA-Z0-9_/.:\\]*\\|<string>\\)(\\([0-9]+\\))\\([a-zA-Z0-9_]*\\|\\?\\|<module>\\)()\\(->[^\n]*\\)?\n")
(defvar llgud-pdb-marker-regexp-file-group 1)
(defvar llgud-pdb-marker-regexp-line-group 2)
(defvar llgud-pdb-marker-regexp-fnname-group 3)

(defvar llgud-pdb-marker-regexp-start "^> ")

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun llgud-pdb-marker-filter (string)
  (setq llgud-marker-acc (concat llgud-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match llgud-pdb-marker-regexp llgud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       llgud-last-frame
       (let ((file (match-string llgud-pdb-marker-regexp-file-group
				 llgud-marker-acc))
	     (line (string-to-number
		    (match-string llgud-pdb-marker-regexp-line-group
				  llgud-marker-acc))))
	 (if (string-equal file "<string>")
	     llgud-last-frame
	   (cons file line)))

       ;; Output everything instead of the below
       output (concat output (substring llgud-marker-acc 0 (match-end 0)))
;;	  ;; Append any text before the marker to the output we're going
;;	  ;; to return - we don't include the marker in this text.
;;	  output (concat output
;;		      (substring llgud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       llgud-marker-acc (substring llgud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; llgud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match llgud-pdb-marker-regexp-start llgud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring llgud-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq llgud-marker-acc
		(substring llgud-marker-acc (match-beginning 0))))

      (setq output (concat output llgud-marker-acc)
	    llgud-marker-acc ""))

    output))

(defcustom llgud-pdb-command-name "pdb"
  "File name for executing the Python debugger.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'llgud)

;;;###autoload
(defun pdb (command-line)
  "Run pdb on program FILE in buffer `*llgud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (llgud-query-cmdline 'pdb)))

  (llgud-common-init command-line nil 'llgud-pdb-marker-filter)
  (set (make-local-variable 'llgud-minor-mode) 'pdb)

  (llgud-def llgud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
  (llgud-def llgud-remove "clear %f:%l"  "\C-d" "Remove breakpoint at current line")
  (llgud-def llgud-step   "step"         "\C-s" "Step one source line with display.")
  (llgud-def llgud-next   "next"         "\C-n" "Step one line (skip functions).")
  (llgud-def llgud-cont   "continue"     "\C-r" "Continue with display.")
  (llgud-def llgud-finish "return"       "\C-f" "Finish executing current function.")
  (llgud-def llgud-up     "up"           "<" "Up one stack frame.")
  (llgud-def llgud-down   "down"         ">" "Down one stack frame.")
  (llgud-def llgud-print  "p %e"         "\C-p" "Evaluate Python expression at point.")
  ;; Is this right?
  (llgud-def llgud-statement "! %e"      "\C-e" "Execute Python statement at point.")

  ;; (setq comint-prompt-regexp "^(.*pdb[+]?) *")
  (setq comint-prompt-regexp "^(Pdb) *")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'pdb-mode-hook))

;; ======================================================================
;;
;; JDB support.
;;
;; AUTHOR:	Derek Davies <ddavies@world.std.com>
;;		Zoltan Kemenczy <zoltan@ieee.org;zkemenczy@rim.net>
;;
;; CREATED:	Sun Feb 22 10:46:38 1998 Derek Davies.
;; UPDATED:	Nov 11, 2001 Zoltan Kemenczy
;;              Dec 10, 2002 Zoltan Kemenczy - added nested class support
;;
;; INVOCATION NOTES:
;;
;; You invoke jdb-mode with:
;;
;;    M-x jdb <enter>
;;
;; It responds with:
;;
;;    Run jdb (like this): jdb
;;
;; type any jdb switches followed by the name of the class you'd like to debug.
;; Supply a fully qualfied classname (these do not have the ".class" extension)
;; for the name of the class to debug (e.g. "COM.the-kind.ddavies.CoolClass").
;; See the known problems section below for restrictions when specifying jdb
;; command line switches (search forward for '-classpath').
;;
;; You should see something like the following:
;;
;;    Current directory is ~/src/java/hello/
;;    Initializing jdb...
;;    0xed2f6628:class(hello)
;;    >
;;
;; To set an initial breakpoint try:
;;
;;    > stop in hello.main
;;    Breakpoint set in hello.main
;;    >
;;
;; To execute the program type:
;;
;;    > run
;;    run hello
;;
;;    Breakpoint hit: running ...
;;    hello.main (hello:12)
;;
;; Type M-n to step over the current line and M-s to step into it.  That,
;; along with the JDB 'help' command should get you started.  The 'quit'
;; JDB command will get out out of the debugger.  There is some truly
;; pathetic JDB documentation available at:
;;
;;     http://java.sun.com/products/jdk/1.1/debugging/
;;
;; KNOWN PROBLEMS AND FIXME's:
;;
;; Not sure what happens with inner classes ... haven't tried them.
;;
;; Does not grok UNICODE id's.  Only ASCII id's are supported.
;;
;; You must not put whitespace between "-classpath" and the path to
;; search for java classes even though it is required when invoking jdb
;; from the command line.  See llgud-jdb-massage-args for details.
;; The same applies for "-sourcepath".
;;
;; Note: The following applies only if `llgud-jdb-use-classpath' is nil;
;; refer to the documentation of `llgud-jdb-use-classpath' and
;; `llgud-jdb-classpath',`llgud-jdb-sourcepath' variables for information
;; on using the classpath for locating java source files.
;;
;; If any of the source files in the directories listed in
;; llgud-jdb-directories won't parse you'll have problems.  Make sure
;; every file ending in ".java" in these directories parses without error.
;;
;; All the .java files in the directories in llgud-jdb-directories are
;; syntactically analyzed each time llgud jdb is invoked.  It would be
;; nice to keep as much information as possible between runs.  It would
;; be really nice to analyze the files only as neccessary (when the
;; source needs to be displayed.)  I'm not sure to what extent the former
;; can be accomplished and I'm not sure the latter can be done at all
;; since I don't know of any general way to tell which .class files are
;; defined by which .java file without analyzing all the .java files.
;; If anyone knows why JavaSoft didn't put the source file names in
;; debuggable .class files please clue me in so I find something else
;; to be spiteful and bitter about.
;;
;; ======================================================================
;; llgud jdb variables and functions

(defcustom llgud-jdb-command-name "jdb"
  "Command that executes the Java debugger."
  :type 'string
  :group 'llgud)

(defcustom llgud-jdb-use-classpath t
  "If non-nil, search for Java source files in classpath directories.
The list of directories to search is the value of `llgud-jdb-classpath'.
The file pathname is obtained by converting the fully qualified
class information output by jdb to a relative pathname and appending
it to `llgud-jdb-classpath' element by element until a match is found.

This method has a significant jdb startup time reduction advantage
since it does not require the scanning of all `llgud-jdb-directories'
and parsing all Java files for class information.

Set to nil to use `llgud-jdb-directories' to scan java sources for
class information on jdb startup (original method)."
  :type 'boolean
  :group 'llgud)

(defvar llgud-jdb-classpath nil
  "Java/jdb classpath directories list.
If `llgud-jdb-use-classpath' is non-nil, llgud-jdb derives the `llgud-jdb-classpath'
list automatically using the following methods in sequence
\(with subsequent successful steps overriding the results of previous
steps):

1) Read the CLASSPATH environment variable,
2) Read any \"-classpath\" argument used to run jdb,
   or detected in jdb output (e.g. if jdb is run by a script
   that echoes the actual jdb command before starting jdb),
3) Send a \"classpath\" command to jdb and scan jdb output for
   classpath information if jdb is invoked with an \"-attach\" (to
   an already running VM) argument (This case typically does not
   have a \"-classpath\" command line argument - that is provided
   to the VM when it is started).

Note that method 3 cannot be used with oldjdb (or Java 1 jdb) since
those debuggers do not support the classpath command.  Use 1) or 2).")

(defvar llgud-jdb-sourcepath nil
  "Directory list provided by an (optional) \"-sourcepath\" option to jdb.
This list is prepended to `llgud-jdb-classpath' to form the complete
list of directories searched for source files.")

(defvar llgud-marker-acc-max-length 4000
  "Maximum number of debugger output characters to keep.
This variable limits the size of `llgud-marker-acc' which holds
the most recent debugger output history while searching for
source file information.")

(defvar llgud-jdb-history nil
  "History of argument lists passed to jdb.")


;; List of Java source file directories.
(defvar llgud-jdb-directories (list ".")
  "*A list of directories that llgud jdb should search for source code.
The file names should be absolute, or relative to the current
directory.

The set of .java files residing in the directories listed are
syntactically analyzed to determine the classes they define and the
packages in which these classes belong.  In this way llgud jdb maps the
package-qualified class names output by the jdb debugger to the source
file from which the class originated.  This allows llgud mode to keep
the source code display in sync with the debugging session.")

(defvar llgud-jdb-source-files nil
  "List of the java source files for this debugging session.")

;; Association list of fully qualified class names (package + class name)
;; and their source files.
(defvar llgud-jdb-class-source-alist nil
  "Association list of fully qualified class names and source files.")

;; This is used to hold a source file during analysis.
(defvar llgud-jdb-analysis-buffer nil)

(defvar llgud-jdb-classpath-string nil
  "Holds temporary classpath values.")

(defun llgud-jdb-build-source-files-list (path extn)
  "Return a list of java source files (absolute paths).
PATH gives the directories in which to search for files with
extension EXTN.  Normally EXTN is given as the regular expression
 \"\\.java$\" ."
  (apply 'nconc (mapcar (lambda (d)
			  (when (file-directory-p d)
			    (directory-files d t extn nil)))
			path)))

;; Move point past whitespace.
(defun llgud-jdb-skip-whitespace ()
  (skip-chars-forward " \n\r\t\014"))

;; Move point past a "// <eol>" type of comment.
(defun llgud-jdb-skip-single-line-comment ()
  (end-of-line))

;; Move point past a "/* */" or "/** */" type of comment.
(defun llgud-jdb-skip-traditional-or-documentation-comment ()
  (forward-char 2)
  (catch 'break
    (while (not (eobp))
      (if (eq (following-char) ?*)
	  (progn
	    (forward-char)
	    (if (not (eobp))
		(if (eq (following-char) ?/)
		    (progn
		      (forward-char)
		      (throw 'break nil)))))
	(forward-char)))))

;; Move point past any number of consecutive whitespace chars and/or comments.
(defun llgud-jdb-skip-whitespace-and-comments ()
  (llgud-jdb-skip-whitespace)
  (catch 'done
    (while t
      (cond
       ((looking-at "//")
	(llgud-jdb-skip-single-line-comment)
	(llgud-jdb-skip-whitespace))
       ((looking-at "/\\*")
	(llgud-jdb-skip-traditional-or-documentation-comment)
	(llgud-jdb-skip-whitespace))
       (t (throw 'done nil))))))

;; Move point past things that are id-like.  The intent is to skip regular
;; id's, such as class or interface names as well as package and interface
;; names.
(defun llgud-jdb-skip-id-ish-thing ()
  (skip-chars-forward "^ /\n\r\t\014,;{"))

;; Move point past a string literal.
(defun llgud-jdb-skip-string-literal ()
  (forward-char)
  (while (not (cond
	       ((eq (following-char) ?\\)
		(forward-char))
	       ((eq (following-char) ?\042))))
    (forward-char))
  (forward-char))

;; Move point past a character literal.
(defun llgud-jdb-skip-character-literal ()
  (forward-char)
  (while
      (progn
	(if (eq (following-char) ?\\)
	    (forward-char 2))
	(not (eq (following-char) ?\')))
    (forward-char))
  (forward-char))

;; Move point past the following block.  There may be (legal) cruft before
;; the block's opening brace.  There must be a block or it's the end of life
;; in petticoat junction.
(defun llgud-jdb-skip-block ()

  ;; Find the begining of the block.
  (while
      (not (eq (following-char) ?{))

    ;; Skip any constructs that can harbor literal block delimiter
    ;; characters and/or the delimiters for the constructs themselves.
    (cond
     ((looking-at "//")
      (llgud-jdb-skip-single-line-comment))
     ((looking-at "/\\*")
      (llgud-jdb-skip-traditional-or-documentation-comment))
     ((eq (following-char) ?\042)
      (llgud-jdb-skip-string-literal))
     ((eq (following-char) ?\')
      (llgud-jdb-skip-character-literal))
     (t (forward-char))))

  ;; Now at the begining of the block.
  (forward-char)

  ;; Skip over the body of the block as well as the final brace.
  (let ((open-level 1))
    (while (not (eq open-level 0))
      (cond
       ((looking-at "//")
	(llgud-jdb-skip-single-line-comment))
       ((looking-at "/\\*")
	(llgud-jdb-skip-traditional-or-documentation-comment))
       ((eq (following-char) ?\042)
	(llgud-jdb-skip-string-literal))
       ((eq (following-char) ?\')
	(llgud-jdb-skip-character-literal))
       ((eq (following-char) ?{)
	(setq open-level (+ open-level 1))
	(forward-char))
       ((eq (following-char) ?})
	(setq open-level (- open-level 1))
	(forward-char))
       (t (forward-char))))))

;; Find the package and class definitions in Java source file FILE.  Assumes
;; that FILE contains a legal Java program.  BUF is a scratch buffer used
;; to hold the source during analysis.
(defun llgud-jdb-analyze-source (buf file)
  (let ((l nil))
    (set-buffer buf)
    (insert-file-contents file nil nil nil t)
    (goto-char 0)
    (catch 'abort
      (let ((p ""))
	(while (progn
		 (llgud-jdb-skip-whitespace)
		 (not (eobp)))
	  (cond

	   ;; Any number of semi's following a block is legal.  Move point
	   ;; past them.  Note that comments and whitespace may be
	   ;; interspersed as well.
	   ((eq (following-char) ?\073)
	    (forward-char))

	   ;; Move point past a single line comment.
	   ((looking-at "//")
	    (llgud-jdb-skip-single-line-comment))

	   ;; Move point past a traditional or documentation comment.
	   ((looking-at "/\\*")
	    (llgud-jdb-skip-traditional-or-documentation-comment))

	   ;; Move point past a package statement, but save the PackageName.
	   ((looking-at "package")
	    (forward-char 7)
	    (llgud-jdb-skip-whitespace-and-comments)
	    (let ((s (point)))
	      (llgud-jdb-skip-id-ish-thing)
	      (setq p (concat (buffer-substring s (point)) "."))
	      (llgud-jdb-skip-whitespace-and-comments)
	      (if (eq (following-char) ?\073)
		  (forward-char))))

	   ;; Move point past an import statement.
	   ((looking-at "import")
	    (forward-char 6)
	    (llgud-jdb-skip-whitespace-and-comments)
	    (llgud-jdb-skip-id-ish-thing)
	    (llgud-jdb-skip-whitespace-and-comments)
	    (if (eq (following-char) ?\073)
		(forward-char)))

	   ;; Move point past the various kinds of ClassModifiers.
	   ((looking-at "public")
	    (forward-char 6))
	   ((looking-at "abstract")
	    (forward-char 8))
	   ((looking-at "final")
	    (forward-char 5))

	   ;; Move point past a ClassDeclaraction, but save the class
	   ;; Identifier.
	   ((looking-at "class")
	    (forward-char 5)
	    (llgud-jdb-skip-whitespace-and-comments)
	    (let ((s (point)))
	      (llgud-jdb-skip-id-ish-thing)
	      (setq
	       l (nconc l (list (concat p (buffer-substring s (point)))))))
	    (llgud-jdb-skip-block))

	   ;; Move point past an interface statement.
	   ((looking-at "interface")
	    (forward-char 9)
	    (llgud-jdb-skip-block))

	   ;; Anything else means the input is invalid.
	   (t
	    (message "Error parsing file %s." file)
	    (throw 'abort nil))))))
    l))

(defun llgud-jdb-build-class-source-alist-for-file (file)
  (mapcar
   (lambda (c)
     (cons c file))
   (llgud-jdb-analyze-source llgud-jdb-analysis-buffer file)))

;; Return an alist of fully qualified classes and the source files
;; holding their definitions.  SOURCES holds a list of all the source
;; files to examine.
(defun llgud-jdb-build-class-source-alist (sources)
  (setq llgud-jdb-analysis-buffer (get-buffer-create " *llgud-jdb-scratch*"))
  (prog1
      (apply
       'nconc
       (mapcar
	'llgud-jdb-build-class-source-alist-for-file
	sources))
    (kill-buffer llgud-jdb-analysis-buffer)
    (setq llgud-jdb-analysis-buffer nil)))

;; Change what was given in the minibuffer to something that can be used to
;; invoke the debugger.
(defun llgud-jdb-massage-args (file args)
  ;; The jdb executable must have whitespace between "-classpath" and
  ;; its value while llgud-common-init expects all switch values to
  ;; follow the switch keyword without intervening whitespace.  We
  ;; require that when the user enters the "-classpath" switch in the
  ;; EMACS minibuffer that they do so without the intervening
  ;; whitespace.  This function adds it back (it's called after
  ;; llgud-common-init).  There are more switches like this (for
  ;; instance "-host" and "-password") but I don't care about them
  ;; yet.
  (if args
      (let (massaged-args user-error)

	(while (and args (not user-error))
	  (cond
	   ((setq user-error (string-match "-classpath$" (car args))))
	   ((setq user-error (string-match "-sourcepath$" (car args))))
	   ((string-match "-classpath\\(.+\\)" (car args))
	    (setq massaged-args
		  (append massaged-args
			  (list "-classpath"
				(setq llgud-jdb-classpath-string
				      (match-string 1 (car args)))))))
	   ((string-match "-sourcepath\\(.+\\)" (car args))
	    (setq massaged-args
		  (append massaged-args
			  (list "-sourcepath"
				(setq llgud-jdb-sourcepath
				      (match-string 1 (car args)))))))
	   (t (setq massaged-args (append massaged-args (list (car args))))))
	  (setq args (cdr args)))

	;; By this point the current directory is all screwed up.  Maybe we
	;; could fix things and re-invoke llgud-common-init, but for now I think
	;; issueing the error is good enough.
	(if user-error
	    (progn
	      (kill-buffer (current-buffer))
	      (error "Error: Omit whitespace between '-classpath or -sourcepath' and its value")))
	massaged-args)))

;; Search for an association with P, a fully qualified class name, in
;; llgud-jdb-class-source-alist.  The asssociation gives the fully
;; qualified file name of the source file which produced the class.
(defun llgud-jdb-find-source-file (p)
  (cdr (assoc p llgud-jdb-class-source-alist)))

;; Note: Reset to this value every time a prompt is seen
(defvar llgud-jdb-lowest-stack-level 999)

(defun llgud-jdb-find-source-using-classpath (p)
  "Find source file corresponding to fully qualified class P.
Convert P from jdb's output, converted to a pathname
relative to a classpath directory."
  (save-match-data
    (let
      (;; Replace dots with slashes and append ".java" to generate file
       ;; name relative to classpath
       (filename
	(concat
	 (mapconcat 'identity
		    (split-string
		     ;; Eliminate any subclass references in the class
		     ;; name string. These start with a "$"
		     ((lambda (x)
			(if (string-match "$.*" x)
			    (replace-match "" t t x) p))
		      p)
		     "\\.") "/")
	 ".java"))
       (cplist (append llgud-jdb-sourcepath llgud-jdb-classpath))
       found-file)
    (while (and cplist
		(not (setq found-file
			   (file-readable-p
			    (concat (car cplist) "/" filename)))))
      (setq cplist (cdr cplist)))
    (if found-file (concat (car cplist) "/" filename)))))

(defun llgud-jdb-find-source (string)
  "Alias for function used to locate source files.
Set to `llgud-jdb-find-source-using-classpath' or `llgud-jdb-find-source-file'
during jdb initialization depending on the value of
`llgud-jdb-use-classpath'."
  nil)

(defun llgud-jdb-parse-classpath-string (string)
  "Parse the classpath list and convert each item to an absolute pathname."
  (mapcar (lambda (s) (if (string-match "[/\\]$" s)
			  (replace-match "" nil nil s) s))
	  (mapcar 'file-truename
		  (split-string
		   string
		   (concat "[ \t\n\r,\"" path-separator "]+")))))

;; See comentary for other debugger's marker filters - there you will find
;; important notes about STRING.
(defun llgud-jdb-marker-filter (string)

  ;; Build up the accumulator.
  (setq llgud-marker-acc
	(if llgud-marker-acc
	    (concat llgud-marker-acc string)
	  string))

  ;; Look for classpath information until llgud-jdb-classpath-string is found
  ;; (interactive, multiple settings of classpath from jdb
  ;;  not supported/followed)
  (if (and llgud-jdb-use-classpath
	   (not llgud-jdb-classpath-string)
	   (or (string-match "classpath:[ \t[]+\\([^]]+\\)" llgud-marker-acc)
	       (string-match "-classpath[ \t\"]+\\([^ \"]+\\)" llgud-marker-acc)))
      (setq llgud-jdb-classpath
	    (llgud-jdb-parse-classpath-string
	     (setq llgud-jdb-classpath-string
		   (match-string 1 llgud-marker-acc)))))

  ;; We process STRING from left to right.  Each time through the
  ;; following loop we process at most one marker. After we've found a
  ;; marker, delete llgud-marker-acc up to and including the match
  (let (file-found)
    ;; Process each complete marker in the input.
    (while

	;; Do we see a marker?
	(string-match
	 ;; jdb puts out a string of the following form when it
	 ;; hits a breakpoint:
	 ;;
	 ;;	<fully-qualified-class><method> (<class>:<line-number>)
	 ;;
	 ;; <fully-qualified-class>'s are composed of Java ID's
	 ;; separated by periods.  <method> and <class> are
	 ;; also Java ID's.  <method> begins with a period and
	 ;; may contain less-than and greater-than (constructors,
	 ;; for instance, are called <init> in the symbol table.)
	 ;; Java ID's begin with a letter followed by letters
	 ;; and/or digits.  The set of letters includes underscore
	 ;; and dollar sign.
	 ;;
	 ;; The first group matches <fully-qualified-class>,
	 ;; the second group matches <class> and the third group
	 ;; matches <line-number>.  We don't care about using
	 ;; <method> so we don't "group" it.
	 ;;
	 ;; FIXME: Java ID's are UNICODE strings, this matches ASCII
	 ;; ID's only.
         ;;
         ;; The ".," in the last square-bracket are necessary because
         ;; of Sun's total disrespect for backwards compatibility in
         ;; reported line numbers from jdb - starting in 1.4.0 they
         ;; print line numbers using LOCALE, inserting a comma or a
         ;; period at the thousands positions (how ingenious!).

	 "\\(\\[[0-9]+] \\)*\\([a-zA-Z0-9.$_]+\\)\\.[a-zA-Z0-9$_<>(),]+ \
\\(([a-zA-Z0-9.$_]+:\\|line=\\)\\([0-9.,]+\\)"
	 llgud-marker-acc)

      ;; A good marker is one that:
      ;; 1) does not have a "[n] " prefix (not part of a stack backtrace)
      ;; 2) does have an "[n] " prefix and n is the lowest prefix seen
      ;;    since the last prompt
      ;; Figure out the line on which to position the debugging arrow.
      ;; Return the info as a cons of the form:
      ;;
      ;;     (<file-name> . <line-number>) .
      (if (if (match-beginning 1)
	      (let (n)
		(setq n (string-to-number (substring
					llgud-marker-acc
					(1+ (match-beginning 1))
					(- (match-end 1) 2))))
		(if (< n llgud-jdb-lowest-stack-level)
		    (progn (setq llgud-jdb-lowest-stack-level n) t)))
	    t)
	  (if (setq file-found
		    (llgud-jdb-find-source (match-string 2 llgud-marker-acc)))
	      (setq llgud-last-frame
		    (cons file-found
			  (string-to-number
			   (let
                               ((numstr (match-string 4 llgud-marker-acc)))
                             (if (string-match "[.,]" numstr)
                                 (replace-match "" nil nil numstr)
                               numstr)))))
	    (message "Could not find source file.")))

      ;; Set the accumulator to the remaining text.
      (setq llgud-marker-acc (substring llgud-marker-acc (match-end 0))))

    (if (string-match comint-prompt-regexp llgud-marker-acc)
	(setq llgud-jdb-lowest-stack-level 999)))

  ;; Do not allow llgud-marker-acc to grow without bound. If the source
  ;; file information is not within the last 3/4
  ;; llgud-marker-acc-max-length characters, well,...
  (if (> (length llgud-marker-acc) llgud-marker-acc-max-length)
      (setq llgud-marker-acc
	    (substring llgud-marker-acc
		       (- (/ (* llgud-marker-acc-max-length 3) 4)))))

  ;; We don't filter any debugger output so just return what we were given.
  string)

(defvar llgud-jdb-command-name "jdb" "Command that executes the Java debugger.")

;;;###autoload
(defun jdb (command-line)
  "Run jdb with command line COMMAND-LINE in a buffer.
The buffer is named \"*llgud*\" if no initial class is given or
\"*llgud-<initial-class-basename>*\" if there is.  If the \"-classpath\"
switch is given, omit all whitespace between it and its value.

See `llgud-jdb-use-classpath' and `llgud-jdb-classpath' documentation for
information on how jdb accesses source files.  Alternatively (if
`llgud-jdb-use-classpath' is nil), see `llgud-jdb-directories' for the
original source file access method.

For general information about commands available to control jdb from
llgud, see `llgud-mode'."
  (interactive
   (list (llgud-query-cmdline 'jdb)))
  (setq llgud-jdb-classpath nil)
  (setq llgud-jdb-sourcepath nil)

  ;; Set llgud-jdb-classpath from the CLASSPATH environment variable,
  ;; if CLASSPATH is set.
  (setq llgud-jdb-classpath-string (getenv "CLASSPATH"))
  (if llgud-jdb-classpath-string
      (setq llgud-jdb-classpath
	    (llgud-jdb-parse-classpath-string llgud-jdb-classpath-string)))
  (setq llgud-jdb-classpath-string nil)	; prepare for next

  (llgud-common-init command-line 'llgud-jdb-massage-args
		   'llgud-jdb-marker-filter)
  (set (make-local-variable 'llgud-minor-mode) 'jdb)

  ;; If a -classpath option was provided, set llgud-jdb-classpath
  (if llgud-jdb-classpath-string
      (setq llgud-jdb-classpath
	    (llgud-jdb-parse-classpath-string llgud-jdb-classpath-string)))
  (setq llgud-jdb-classpath-string nil)	; prepare for next
  ;; If a -sourcepath option was provided, parse it
  (if llgud-jdb-sourcepath
      (setq llgud-jdb-sourcepath
	    (llgud-jdb-parse-classpath-string llgud-jdb-sourcepath)))

  (llgud-def llgud-break  "stop at %c:%l" "\C-b" "Set breakpoint at current line.")
  (llgud-def llgud-remove "clear %c:%l"   "\C-d" "Remove breakpoint at current line")
  (llgud-def llgud-step   "step"          "\C-s" "Step one source line with display.")
  (llgud-def llgud-next   "next"          "\C-n" "Step one line (skip functions).")
  (llgud-def llgud-cont   "cont"          "\C-r" "Continue with display.")
  (llgud-def llgud-finish "step up"       "\C-f" "Continue until current method returns.")
  (llgud-def llgud-up     "up\C-Mwhere"   "<"    "Up one stack frame.")
  (llgud-def llgud-down   "down\C-Mwhere" ">"    "Up one stack frame.")
  (llgud-def llgud-run    "run"           nil    "Run the program.") ;if VM start using jdb
  (llgud-def llgud-print  "print %e"  "\C-p" "Evaluate Java expression at point.")


  (setq comint-prompt-regexp "^> \\|^[^ ]+\\[[0-9]+\\] ")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'jdb-mode-hook)

  (if llgud-jdb-use-classpath
      ;; Get the classpath information from the debugger
      (progn
	(if (string-match "-attach" command-line)
	    (llgud-call "classpath"))
	(fset 'llgud-jdb-find-source
	      'llgud-jdb-find-source-using-classpath))

    ;; Else create and bind the class/source association list as well
    ;; as the source file list.
    (setq llgud-jdb-class-source-alist
	  (llgud-jdb-build-class-source-alist
	   (setq llgud-jdb-source-files
		 (llgud-jdb-build-source-files-list llgud-jdb-directories
						  "\\.java$"))))
    (fset 'llgud-jdb-find-source 'llgud-jdb-find-source-file)))

;;
;; End of debugger-specific information
;;


;; When we send a command to the debugger via llgud-call, it's annoying
;; to see the command and the new prompt inserted into the debugger's
;; buffer; we have other ways of knowing the command has completed.
;;
;; If the buffer looks like this:
;; --------------------
;; (gdb) set args foo bar
;; (gdb) -!-
;; --------------------
;; (the -!- marks the location of point), and we type `C-x SPC' in a
;; source file to set a breakpoint, we want the buffer to end up like
;; this:
;; --------------------
;; (gdb) set args foo bar
;; Breakpoint 1 at 0x92: file make-docfile.c, line 49.
;; (gdb) -!-
;; --------------------
;; Essentially, the old prompt is deleted, and the command's output
;; and the new prompt take its place.
;;
;; Not echoing the command is easy enough; you send it directly using
;; process-send-string, and it never enters the buffer.  However,
;; getting rid of the old prompt is trickier; you don't want to do it
;; when you send the command, since that will result in an annoying
;; flicker as the prompt is deleted, redisplay occurs while Emacs
;; waits for a response from the debugger, and the new prompt is
;; inserted.  Instead, we'll wait until we actually get some output
;; from the subprocess before we delete the prompt.  If the command
;; produced no output other than a new prompt, that prompt will most
;; likely be in the first chunk of output received, so we will delete
;; the prompt and then replace it with an identical one.  If the
;; command produces output, the prompt is moving anyway, so the
;; flicker won't be annoying.
;;
;; So - when we want to delete the prompt upon receipt of the next
;; chunk of debugger output, we position llgud-delete-prompt-marker at
;; the start of the prompt; the process filter will notice this, and
;; delete all text between it and the process output marker.  If
;; llgud-delete-prompt-marker points nowhere, we leave the current
;; prompt alone.
(defvar llgud-delete-prompt-marker nil)


(put 'llgud-mode 'mode-class 'special)

(define-derived-mode llgud-mode comint-mode "Debugger"
  "Major mode for interacting with an inferior debugger process.

   You start it up with one of the commands M-x gdb, M-x sdb, M-x dbx,
M-x perldb, M-x xdb, M-x jdb, or M-x lldb.  Each entry point finishes by
executing a hook; `gdb-mode-hook', `sdb-mode-hook', `dbx-mode-hook',
`perldb-mode-hook', `xdb-mode-hook', `jdb-mode-hook', or `lldb-mode-hook'
respectively.

After startup, the following commands are available in both the LLGUD
interaction buffer and any source buffer LLGUD visits due to a breakpoint stop
or step operation:

\\[llgud-break] sets a breakpoint at the current file and line.  In the
LLGUD buffer, the current file and line are those of the last breakpoint or
step.  In a source buffer, they are the buffer's file and current line.

\\[llgud-remove] removes breakpoints on the current file and line.

\\[llgud-refresh] displays in the source window the last line referred to
in the llgud buffer.

\\[llgud-step], \\[llgud-next], and \\[llgud-stepi] do a step-one-line,
step-one-line (not entering function calls), and step-one-instruction
and then update the source window with the current file and position.
\\[llgud-cont] continues execution.

\\[llgud-print] tries to find the largest C lvalue or function-call expression
around point, and sends it to the debugger for value display.

The above commands are common to all supported debuggers except xdb which
does not support stepping instructions.

Under gdb, sdb and xdb, \\[llgud-tbreak] behaves exactly like \\[llgud-break],
except that the breakpoint is temporary; that is, it is removed when
execution stops on it.

Under gdb, dbx, xdb, and lldb, \\[llgud-up] pops up through an enclosing stack
frame.  \\[llgud-down] drops back down through one.

If you are using gdb or xdb, \\[llgud-finish] runs execution to the return from
the current function and stops.

All the keystrokes above are accessible in the LLGUD buffer
with the prefix C-c, and in all buffers through the prefix C-x C-a.

All pre-defined functions for which the concept make sense repeat
themselves the appropriate number of times if you give a prefix
argument.

You may use the `llgud-def' macro in the initialization hook to define other
commands.

Other commands for interacting with the debugger process are inherited from
comint mode, which see."
  (setq mode-line-process '(":%s"))
  (define-key (current-local-map) "\C-c\C-l" 'llgud-refresh)
  (set (make-local-variable 'llgud-last-frame) nil)
  (set (make-local-variable 'tool-bar-map) llgud-tool-bar-map)
  (make-local-variable 'comint-prompt-regexp)
  ;; Don't put repeated commands in command history many times.
  (set (make-local-variable 'comint-input-ignoredups) t)
  (make-local-variable 'paragraph-start)
  (set (make-local-variable 'llgud-delete-prompt-marker) (make-marker))
  (add-hook 'kill-buffer-hook 'llgud-kill-buffer-hook nil t))

;; Cause our buffers to be displayed, by default,
;; in the selected window.
;;;###autoload (add-hook 'same-window-regexps "\\*llgud-.*\\*\\(\\|<[0-9]+>\\)")

(defcustom llgud-chdir-before-run t
  "Non-nil if LLGUD should `cd' to the debugged executable."
  :group 'llgud
  :type 'boolean)

(defvar llgud-target-name "--unknown--"
  "The apparent name of the program being debugged in a llgud buffer.")

;; Perform initializations common to all debuggers.
;; The first arg is the specified command line,
;; which starts with the program to debug.
;; The other three args specify the values to use
;; for local variables in the debugger buffer.
(defun llgud-common-init (command-line massage-args marker-filter
				     &optional find-file)
  (let* ((words (split-string-and-unquote command-line))
	 (program (car words))
	 (dir default-directory)
	 ;; Extract the file name from WORDS
	 ;; and put t in its place.
	 ;; Later on we will put the modified file name arg back there.
	 (file-word (let ((w (cdr words)))
		      (while (and w (= ?- (aref (car w) 0)))
			(setq w (cdr w)))
		      (and w
			   (prog1 (car w)
			     (setcar w t)))))
	 (file-subst
	  (and file-word (substitute-in-file-name file-word)))
	 (args (cdr words))
	 ;; If a directory was specified, expand the file name.
	 ;; Otherwise, don't expand it, so GDB can use the PATH.
	 ;; A file name without directory is literally valid
	 ;; only if the file exists in ., and in that case,
	 ;; omitting the expansion here has no visible effect.
	 (file (and file-word
		    (if (file-name-directory file-subst)
			(expand-file-name file-subst)
		      file-subst)))
	 (filepart (and file-word (concat "-" (file-name-nondirectory file))))
	 (existing-buffer (get-buffer (concat "*llgud" filepart "*"))))
    (pop-to-buffer (concat "*llgud" filepart "*"))
    (when (and existing-buffer (get-buffer-process existing-buffer))
      (error "This program is already being debugged"))
    ;; Set the dir, in case the buffer already existed with a different dir.
    (setq default-directory dir)
    ;; Set default-directory to the file's directory.
    (and file-word
	 llgud-chdir-before-run
	 ;; Don't set default-directory if no directory was specified.
	 ;; In that case, either the file is found in the current directory,
	 ;; in which case this setq is a no-op,
	 ;; or it is found by searching PATH,
	 ;; in which case we don't know what directory it was found in.
	 (file-name-directory file)
	 (setq default-directory (file-name-directory file)))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    ;; Put the substituted and expanded file name back in its place.
    (let ((w args))
      (while (and w (not (eq (car w) t)))
	(setq w (cdr w)))
      (if w
	  (setcar w file)))
    (apply 'make-comint (concat "llgud" filepart) program nil
	   (if massage-args (funcall massage-args file args) args))
    ;; Since comint clobbered the mode, we don't set it until now.
    (llgud-mode)
    (set (make-local-variable 'llgud-target-name)
	 (and file-word (file-name-nondirectory file))))
  (set (make-local-variable 'llgud-marker-filter) marker-filter)
  (if find-file (set (make-local-variable 'llgud-find-file) find-file))
  (setq llgud-last-last-frame nil)

  (set-process-filter (get-buffer-process (current-buffer)) 'llgud-filter)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'llgud-sentinel)
  (llgud-set-buffer))

(defun llgud-set-buffer ()
  (when (eq major-mode 'llgud-mode)
    (setq llgud-comint-buffer (current-buffer))))

(defvar llgud-filter-defer-flag nil
  "Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")

;; These functions are responsible for inserting output from your debugger
;; into the buffer.  The hard work is done by the method that is
;; the value of llgud-marker-filter.

(defun llgud-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if llgud-filter-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later.
	    (setq llgud-filter-pending-text
		  (concat (or llgud-filter-pending-text "") string))

	  ;; If we have to ask a question during the processing,
	  ;; defer any additional text that comes from the debugger
	  ;; during that time.
	  (let ((llgud-filter-defer-flag t))
	    ;; Process now any text we previously saved up.
	    (if llgud-filter-pending-text
		(setq string (concat llgud-filter-pending-text string)
		      llgud-filter-pending-text nil))

	    (with-current-buffer (process-buffer proc)
	      ;; If we have been so requested, delete the debugger prompt.
	      (save-restriction
		(widen)
		(if (marker-buffer llgud-delete-prompt-marker)
		    (let ((inhibit-read-only t))
		      (delete-region (process-mark proc)
				     llgud-delete-prompt-marker)
		      (comint-update-fence)
		      (set-marker llgud-delete-prompt-marker nil)))
		;; Save the process output, checking for source file markers.
		(setq output (llgud-marker-filter string))
		;; Check for a filename-and-line number.
		;; Don't display the specified file
		;; unless (1) point is at or after the position where output appears
		;; and (2) this buffer is on the screen.
		(setq process-window
		      (and llgud-last-frame
			   (>= (point) (process-mark proc))
			   (get-buffer-window (current-buffer)))))

	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
	      (comint-output-filter proc output))

	    ;; Put the arrow on the source line.
	    ;; This must be outside of the save-excursion
	    ;; in case the source file is our current buffer.
	    (if process-window
		(with-selected-window process-window
		  (llgud-display-frame))
	      ;; We have to be in the proper buffer, (process-buffer proc),
	      ;; but not in a save-excursion, because that would restore point.
	      (with-current-buffer (process-buffer proc)
		(llgud-display-frame))))

	  ;; If we deferred text that arrived during this processing,
	  ;; handle it now.
	  (if llgud-filter-pending-text
	      (llgud-filter proc ""))))))

(defvar llgud-minor-mode-type nil)
(defvar llgud-overlay-arrow-position nil)
(add-to-list 'overlay-arrow-variable-list 'llgud-overlay-arrow-position)

(defun llgud-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq llgud-overlay-arrow-position nil)
	 (set-process-buffer proc nil)
	 (if (and (boundp 'speedbar-frame)
		  (string-equal speedbar-initial-expansion-list-name "LLGUD"))
	     (speedbar-change-initial-expansion-list
	      speedbar-previously-used-expansion-list-name))
	 (if (memq llgud-minor-mode-type '(gdbmi gdba))
	     (gdb-reset)
	   (llgud-reset)))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq llgud-overlay-arrow-position nil)
	 (if (memq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		   '(gdba gdbmi))
	     (gdb-reset)
	   (llgud-reset))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in the LLGUD buffer and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Fix the mode line.
		 (setq mode-line-process
		       (concat ":"
			       (symbol-name (process-status proc))))
		 (force-mode-line-update)
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the llgud buffer.
	     (set-buffer obuf))))))

(defun llgud-kill-buffer-hook ()
  (setq llgud-minor-mode-type llgud-minor-mode)
  (condition-case nil
      (kill-process (get-buffer-process (current-buffer)))
    (error nil)))

(defun llgud-reset ()
  (dolist (buffer (buffer-list))
    (unless (eq buffer llgud-comint-buffer)
      (with-current-buffer buffer
	(when llgud-minor-mode
	  (setq llgud-minor-mode nil)
	  (kill-local-variable 'tool-bar-map))))))

(defun llgud-display-frame ()
  "Find and obey the last filename-and-line marker from the debugger.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (when llgud-last-frame
    (llgud-set-buffer)
    (llgud-display-line (car llgud-last-frame) (cdr llgud-last-frame))
    (setq llgud-last-last-frame llgud-last-frame
	  llgud-last-frame nil)))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.
;; Most of the trickiness in here comes from wanting to preserve the current
;; region-restriction if that's possible.  We use an explicit display-buffer
;; to get around the fact that this is called inside a save-excursion.

(defun llgud-display-line (true-file line)
  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
	 (buffer
	  (with-current-buffer llgud-comint-buffer
	    (llgud-find-file true-file)))
	 (window (and buffer
		      (or (get-buffer-window buffer)
			  (if (memq llgud-minor-mode '(gdbmi gdba))
			      (or (if (get-buffer-window buffer 0)
				      (display-buffer buffer nil 0))
				  (unless (gdb-display-source-buffer buffer)
				    (gdb-display-buffer buffer nil))))
			  (display-buffer buffer))))
	 (pos))
    (if buffer
	(progn
	  (with-current-buffer buffer
	    (unless (or (verify-visited-file-modtime buffer) llgud-keep-buffer)
		  (if (yes-or-no-p
		       (format "File %s changed on disk.  Reread from disk? "
			       (buffer-name)))
		      (revert-buffer t t)
		    (setq llgud-keep-buffer t)))
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (setq pos (point))
	      (or llgud-overlay-arrow-position
		  (setq llgud-overlay-arrow-position (make-marker)))
	      (set-marker llgud-overlay-arrow-position (point) (current-buffer))
	      ;; If they turned on hl-line, move the hl-line highlight to
	      ;; the arrow's line.
	      (when (featurep 'hl-line)
		(cond
		 (global-hl-line-mode
		  (global-hl-line-highlight))
		 ((and hl-line-mode hl-line-sticky-flag)
		  (hl-line-highlight)))))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (when window
	    (set-window-point window llgud-overlay-arrow-position)
	    (if (memq llgud-minor-mode '(gdbmi gdba))
		(setq gdb-source-window window)))))))

;; The llgud-call function must do the right thing whether its invoking
;; keystroke is from the LLGUD buffer itself (via major-mode binding)
;; or a C buffer.  In the former case, we want to supply data from
;; llgud-last-frame.  Here's how we do it:

(defun llgud-format-command (str arg)
  (let ((insource (not (eq (current-buffer) llgud-comint-buffer)))
	(frame (or llgud-last-frame llgud-last-last-frame))
	result)
    (while (and str
		(let ((case-fold-search nil))
		  (string-match "\\([^%]*\\)%\\([abdefFlpc]\\)" str)))
      (let ((key (string-to-char (match-string 2 str)))
	    subst)
	(cond
	 ((eq key ?f)
	  (setq subst (file-name-nondirectory (if insource
						  (buffer-file-name)
						(car frame)))))
	 ((eq key ?F)
	  (setq subst (file-name-sans-extension
		       (file-name-nondirectory (if insource
						   (buffer-file-name)
						 (car frame))))))
	 ((eq key ?d)
	  (setq subst (file-name-directory (if insource
					       (buffer-file-name)
					     (car frame)))))
	 ((eq key ?l)
	  (setq subst (int-to-string
		       (if insource
			   (save-restriction
			     (widen)
			     (+ (count-lines (point-min) (point))
				(if (bolp) 1 0)))
			 (cdr frame)))))
	 ((eq key ?e)
	  (setq subst (llgud-find-expr)))
	 ((eq key ?a)
	  (setq subst (llgud-read-address)))
	 ((eq key ?b)
	  (setq subst llgud-breakpoint-id))
	 ((eq key ?c)
	  (setq subst
                (llgud-find-class
                 (if insource
                      (buffer-file-name)
                    (car frame))
                 (if insource
                      (save-restriction
                        (widen)
                        (+ (count-lines (point-min) (point))
                           (if (bolp) 1 0)))
                    (cdr frame)))))
	 ((eq key ?p)
	  (setq subst (if arg (int-to-string arg)))))
	(setq result (concat result (match-string 1 str) subst)))
      (setq str (substring str (match-end 2))))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun llgud-read-address ()
  "Return a string containing the core-address found in the buffer at point."
  (save-match-data
    (save-excursion
      (let ((pt (point)) found begin)
	(setq found (if (search-backward "0x" (- pt 7) t) (point)))
	(cond
	 (found (forward-char 2)
		(buffer-substring found
				  (progn (re-search-forward "[^0-9a-f]")
					 (forward-char -1)
					 (point))))
	 (t (setq begin (progn (re-search-backward "[^0-9]")
			       (forward-char 1)
			       (point)))
	    (forward-char 1)
	    (re-search-forward "[^0-9]")
	    (forward-char -1)
	    (buffer-substring begin (point))))))))

(defun llgud-call (fmt &optional arg)
  (let ((msg (llgud-format-command fmt arg)))
    (message "Command: %s" msg)
    (sit-for 0)
    (llgud-basic-call msg)))

(defun llgud-basic-call (command)
  "Invoke the debugger COMMAND displaying source in other window."
  (interactive)
  (llgud-set-buffer)
  (let ((proc (get-buffer-process llgud-comint-buffer)))
    (or proc (error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer llgud-comint-buffer)
      (save-restriction
	(widen)
	(if (marker-position llgud-delete-prompt-marker)
	    ;; We get here when printing an expression.
	    (goto-char llgud-delete-prompt-marker)
	  (goto-char (process-mark proc))
	  (forward-line 0))
	(if (looking-at comint-prompt-regexp)
	    (set-marker llgud-delete-prompt-marker (point)))
	(if (memq llgud-minor-mode '(gdbmi gdba))
	    (apply comint-input-sender (list proc command))
	  (process-send-string proc (concat command "\n")))))))

(defun llgud-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive "P")
  (or llgud-last-frame (setq llgud-last-frame llgud-last-last-frame))
  (llgud-display-frame)
  (recenter arg))

;; Code for parsing expressions out of C or Fortran code.  The single entry
;; point is llgud-find-expr, which tries to return an lvalue expression from
;; around point.

(defvar llgud-find-expr-function 'llgud-find-c-expr)

(defun llgud-find-expr (&rest args)
  (let ((expr (if (and transient-mark-mode mark-active)
		  (buffer-substring (region-beginning) (region-end))
		(apply llgud-find-expr-function args))))
    (save-match-data
      (if (string-match "\n" expr)
	  (error "Expression must not include a newline"))
      (with-current-buffer llgud-comint-buffer
	(save-excursion
	  (goto-char (process-mark (get-buffer-process llgud-comint-buffer)))
	  (forward-line 0)
	  (when (looking-at comint-prompt-regexp)
	    (set-marker llgud-delete-prompt-marker (point))
	    (set-marker-insertion-type llgud-delete-prompt-marker t))
	  (unless (eq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		      'jdb)
	      (insert (concat  expr " = "))))))
    expr))

;; The next eight functions are hacked from gdbsrc.el by
;; Debby Ayers <ayers@asc.slb.com>,
;; Rich Schaefer <schaefer@asc.slb.com> Schlumberger, Austin, Tx.

(defun llgud-find-c-expr ()
  "Returns the expr that surrounds point."
  (interactive)
  (save-excursion
    (let ((p (point))
	  (expr (llgud-innermost-expr))
	  (test-expr (llgud-prev-expr)))
      (while (and test-expr (llgud-expr-compound test-expr expr))
	(let ((prev-expr expr))
	  (setq expr (cons (car test-expr) (cdr expr)))
	  (goto-char (car expr))
	  (setq test-expr (llgud-prev-expr))
	  ;; If we just pasted on the condition of an if or while,
	  ;; throw it away again.
	  (if (member (buffer-substring (car test-expr) (cdr test-expr))
		      '("if" "while" "for"))
	      (setq test-expr nil
		    expr prev-expr))))
      (goto-char p)
      (setq test-expr (llgud-next-expr))
      (while (llgud-expr-compound expr test-expr)
	(setq expr (cons (car expr) (cdr test-expr)))
	(setq test-expr (llgud-next-expr)))
      (buffer-substring (car expr) (cdr expr)))))

(defun llgud-innermost-expr ()
  "Returns the smallest expr that point is in; move point to beginning of it.
The expr is represented as a cons cell, where the car specifies the point in
the current buffer that marks the beginning of the expr and the cdr specifies
the character after the end of the expr."
  (let ((p (point)) begin end)
    (llgud-backward-sexp)
    (setq begin (point))
    (llgud-forward-sexp)
    (setq end (point))
    (if (>= p end)
	(progn
	 (setq begin p)
	 (goto-char p)
	 (llgud-forward-sexp)
	 (setq end (point)))
      )
    (goto-char begin)
    (cons begin end)))

(defun llgud-backward-sexp ()
  "Version of `backward-sexp' that catches errors."
  (condition-case nil
      (backward-sexp)
    (error t)))

(defun llgud-forward-sexp ()
  "Version of `forward-sexp' that catches errors."
  (condition-case nil
     (forward-sexp)
    (error t)))

(defun llgud-prev-expr ()
  "Returns the previous expr, point is set to beginning of that expr.
The expr is represented as a cons cell, where the car specifies the point in
the current buffer that marks the beginning of the expr and the cdr specifies
the character after the end of the expr"
  (let ((begin) (end))
    (llgud-backward-sexp)
    (setq begin (point))
    (llgud-forward-sexp)
    (setq end (point))
    (goto-char begin)
    (cons begin end)))

(defun llgud-next-expr ()
  "Returns the following expr, point is set to beginning of that expr.
The expr is represented as a cons cell, where the car specifies the point in
the current buffer that marks the beginning of the expr and the cdr specifies
the character after the end of the expr."
  (let ((begin) (end))
    (llgud-forward-sexp)
    (llgud-forward-sexp)
    (setq end (point))
    (llgud-backward-sexp)
    (setq begin (point))
    (cons begin end)))

(defun llgud-expr-compound-sep (span-start span-end)
  "Scan from SPAN-START to SPAN-END for punctuation characters.
If `->' is found, return `?.'.  If `.' is found, return `?.'.
If any other punctuation is found, return `??'.
If no punctuation is found, return `? '."
  (let ((result ?\s)
	(syntax))
    (while (< span-start span-end)
      (setq syntax (char-syntax (char-after span-start)))
      (cond
       ((= syntax ?\s) t)
       ((= syntax ?.) (setq syntax (char-after span-start))
	(cond
	 ((= syntax ?.) (setq result ?.))
	 ((and (= syntax ?-) (= (char-after (+ span-start 1)) ?>))
	  (setq result ?.)
	  (setq span-start (+ span-start 1)))
	 (t (setq span-start span-end)
	    (setq result ??)))))
      (setq span-start (+ span-start 1)))
    result))

(defun llgud-expr-compound (first second)
  "Non-nil if concatenating FIRST and SECOND makes a single C expression.
The two exprs are represented as a cons cells, where the car
specifies the point in the current buffer that marks the beginning of the
expr and the cdr specifies the character after the end of the expr.
Link exprs of the form:
      Expr -> Expr
      Expr . Expr
      Expr (Expr)
      Expr [Expr]
      (Expr) Expr
      [Expr] Expr"
  (let ((span-start (cdr first))
	(span-end (car second))
	(syntax))
    (setq syntax (llgud-expr-compound-sep span-start span-end))
    (cond
     ((= (car first) (car second)) nil)
     ((= (cdr first) (cdr second)) nil)
     ((= syntax ?.) t)
     ((= syntax ?\s)
      (setq span-start (char-after (- span-start 1)))
      (setq span-end (char-after span-end))
      (cond
       ((= span-start ?)) t)
      ((= span-start ?]) t)
     ((= span-end ?() t)
      ((= span-end ?[) t)
       (t nil)))
     (t nil))))

(defun llgud-find-class (f line)
  "Find fully qualified class in file F at line LINE.
This function uses the `llgud-jdb-classpath' (and optional
`llgud-jdb-sourcepath') list(s) to derive a file
pathname relative to its classpath directory.  The values in
`llgud-jdb-classpath' are assumed to have been converted to absolute
pathname standards using file-truename.
If F is visited by a buffer and its mode is CC-mode(Java),
syntactic information of LINE is used to find the enclosing (nested)
class string which is appended to the top level
class of the file (using s to separate nested class ids)."
  ;; Convert f to a standard representation and remove suffix
  (if (and llgud-jdb-use-classpath (or llgud-jdb-classpath llgud-jdb-sourcepath))
      (save-match-data
        (let ((cplist (append llgud-jdb-sourcepath llgud-jdb-classpath))
              (fbuffer (get-file-buffer f))
              syntax-symbol syntax-point class-found)
          (setq f (file-name-sans-extension (file-truename f)))
          ;; Syntax-symbol returns the symbol of the *first* element
          ;; in the syntactical analysis result list, syntax-point
          ;; returns the buffer position of same
          (fset 'syntax-symbol (lambda (x) (c-langelem-sym (car x))))
          (fset 'syntax-point (lambda (x) (c-langelem-pos (car x))))
          ;; Search through classpath list for an entry that is
          ;; contained in f
          (while (and cplist (not class-found))
            (if (string-match (car cplist) f)
                (setq class-found
		      (mapconcat 'identity
                                 (split-string
                                   (substring f (+ (match-end 0) 1))
                                  "/") ".")))
            (setq cplist (cdr cplist)))
          ;; if f is visited by a java(cc-mode) buffer, walk up the
          ;; syntactic information chain and collect any 'inclass
          ;; symbols until 'topmost-intro is reached to find out if
          ;; point is within a nested class
          (if (and fbuffer (equal (symbol-file 'java-mode) "cc-mode"))
              (save-excursion
                (set-buffer fbuffer)
                (let ((nclass) (syntax))
                  ;; While the c-syntactic information does not start
                  ;; with the 'topmost-intro symbol, there may be
                  ;; nested classes...
                  (while (not (eq 'topmost-intro
                                  (syntax-symbol (c-guess-basic-syntax))))
                    ;; Check if the current position c-syntactic
                    ;; analysis has 'inclass
                    (setq syntax (c-guess-basic-syntax))
                    (while
                        (and (not (eq 'inclass (syntax-symbol syntax)))
                             (cdr syntax))
                      (setq syntax (cdr syntax)))
                    (if (eq 'inclass (syntax-symbol syntax))
                        (progn
                          (goto-char (syntax-point syntax))
                          ;; Now we're at the beginning of a class
                          ;; definition.  Find class name
                          (looking-at
                           "[A-Za-z0-9 \t\n]*?class[ \t\n]+\\([^ \t\n]+\\)")
                          (setq nclass
                                (append (list (match-string-no-properties 1))
                                        nclass)))
                      (setq syntax (c-guess-basic-syntax))
                      (while (and (not (syntax-point syntax)) (cdr syntax))
                        (setq syntax (cdr syntax)))
                      (goto-char (syntax-point syntax))
                      ))
                  (string-match (concat (car nclass) "$") class-found)
                  (setq class-found
                        (replace-match (mapconcat 'identity nclass "$")
                                       t t class-found)))))
          (if (not class-found)
              (message "llgud-find-class: class for file %s not found!" f))
          class-found))
    ;; Not using classpath - try class/source association list
    (let ((class-found (rassoc f llgud-jdb-class-source-alist)))
      (if class-found
	  (car class-found)
	(message "llgud-find-class: class for file %s not found in llgud-jdb-class-source-alist!" f)
	nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GDB script mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gdb-script-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defvar gdb-script-font-lock-keywords
  '(("^define\\s-+\\(\\(\\w\\|\\s_\\)+\\)" (1 font-lock-function-name-face))
    ("\\$\\(\\w+\\)" (1 font-lock-variable-name-face))
    ("^\\s-*\\(\\w\\(\\w\\|\\s_\\)*\\)" (1 font-lock-keyword-face))))

(defvar gdb-script-font-lock-syntactic-keywords
  '(("^document\\s-.*\\(\n\\)" (1 "< b"))
    ("^end\\>"
     (0 (unless (eq (match-beginning 0) (point-min))
          ;; We change the \n in front, which is more difficult, but results
          ;; in better highlighting.  If the doc is empty, the single \n is
          ;; both the beginning and the end of the docstring, which can't be
          ;; expressed in syntax-tables.  Instead, we place the "> b" after
          ;; placing the "< b", so the start marker is overwritten by the
          ;; termination marker and in the end Emacs simply considers that
          ;; there's no docstring at all, which is fine.
          (put-text-property (1- (match-beginning 0)) (match-beginning 0)
                             'syntax-table (eval-when-compile
                                             (string-to-syntax "> b")))
          ;; Make sure that rehighlighting the previous line won't erase our
          ;; syntax-table property.
          (put-text-property (1- (match-beginning 0)) (match-end 0)
                             'font-lock-multiline t)
          nil)))))

(defun gdb-script-font-lock-syntactic-face (state)
  (cond
   ((nth 3 state) font-lock-string-face)
   ((nth 7 state) font-lock-doc-face)
   (t font-lock-comment-face)))

(defvar gdb-script-basic-indent 2)

(defun gdb-script-skip-to-head ()
  "We're just in front of an `end' and we need to go to its head."
  (while (and (re-search-backward "^\\s-*\\(\\(end\\)\\|define\\|document\\|if\\|while\\|commands\\)\\>" nil 'move)
	      (match-end 2))
    (gdb-script-skip-to-head)))

(defun gdb-script-calculate-indentation ()
  (cond
   ((looking-at "end\\>")
    (gdb-script-skip-to-head)
    (current-indentation))
   ((looking-at "else\\>")
    (while (and (re-search-backward "^\\s-*\\(if\\|\\(end\\)\\)\\>" nil 'move)
		(match-end 2))
      (gdb-script-skip-to-head))
    (current-indentation))
   (t
    (forward-comment (- (point-max)))
    (forward-line 0)
    (skip-chars-forward " \t")
    (+ (current-indentation)
       (if (looking-at "\\(if\\|while\\|define\\|else\\|commands\\)\\>")
	   gdb-script-basic-indent 0)))))

(defun gdb-script-indent-line ()
  "Indent current line of GDB script."
  (interactive)
  (if (and (eq (get-text-property (point) 'face) font-lock-doc-face)
	   (save-excursion
	     (forward-line 0)
	     (skip-chars-forward " \t")
	     (not (looking-at "end\\>"))))
      'noindent
    (let* ((savep (point))
	   (indent (condition-case nil
		       (save-excursion
			 (forward-line 0)
			 (skip-chars-forward " \t")
			 (if (>= (point) savep) (setq savep nil))
			 (max (gdb-script-calculate-indentation) 0))
		     (error 0))))
      (if savep
	  (save-excursion (indent-line-to indent))
	(indent-line-to indent)))))

;; Derived from cfengine.el.
(defun gdb-script-beginning-of-defun ()
  "`beginning-of-defun' function for Gdb script mode.
Treats actions as defuns."
  (unless (<= (current-column) (current-indentation))
    (end-of-line))
  (if (re-search-backward "^define \\|^document " nil t)
      (beginning-of-line)
    (goto-char (point-min)))
  t)

;; Derived from cfengine.el.
(defun gdb-script-end-of-defun ()
  "`end-of-defun' function for Gdb script mode.
Treats actions as defuns."
  (end-of-line)
  (if (re-search-forward "^end" nil t)
      (beginning-of-line)
    (goto-char (point-max)))
  t)

;; Besides .gdbinit, gdb documents other names to be usable for init
;; files, cross-debuggers can use something like
;; .PROCESSORNAME-gdbinit so that the host and target gdbinit files
;; don't interfere with each other.
;;;###autoload
(add-to-list 'auto-mode-alist '("/\\.[a-z0-9-]*gdbinit" . gdb-script-mode))

;;;###autoload
(define-derived-mode gdb-script-mode nil "GDB-Script"
  "Major mode for editing GDB scripts."
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'outline-regexp) "[ \t]")
  (set (make-local-variable 'imenu-generic-expression)
       '((nil "^define[ \t]+\\(\\w+\\)" 1)))
  (set (make-local-variable 'indent-line-function) 'gdb-script-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       #'gdb-script-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'gdb-script-end-of-defun)
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-script-font-lock-keywords nil nil ((?_ . "w")) nil
	 (font-lock-syntactic-keywords
	  . gdb-script-font-lock-syntactic-keywords)
	 (font-lock-syntactic-face-function
	  . gdb-script-font-lock-syntactic-face))))


;;; tooltips for LLGUD

;;; Customizable settings

(define-minor-mode llgud-tooltip-mode
  "Toggle the display of LLGUD tooltips."
  :global t
  :group 'llgud
  :group 'tooltip
  (require 'tooltip)
  (if llgud-tooltip-mode
      (progn
	(add-hook 'change-major-mode-hook 'llgud-tooltip-change-major-mode)
	(add-hook 'pre-command-hook 'tooltip-hide)
	(add-hook 'tooltip-hook 'llgud-tooltip-tips)
	(define-key global-map [mouse-movement] 'llgud-tooltip-mouse-motion))
    (unless tooltip-mode (remove-hook 'pre-command-hook 'tooltip-hide)
    (remove-hook 'change-major-mode-hook 'llgud-tooltip-change-major-mode)
    (remove-hook 'tooltip-hook 'llgud-tooltip-tips)
    (define-key global-map [mouse-movement] 'ignore)))
  (llgud-tooltip-activate-mouse-motions-if-enabled)
  (if (and llgud-comint-buffer
	   (buffer-name llgud-comint-buffer); llgud-comint-buffer might be killed
	   (memq (buffer-local-value 'llgud-minor-mode llgud-comint-buffer)
		 '(gdbmi gdba)))
      (if llgud-tooltip-mode
	  (progn
	    (dolist (buffer (buffer-list))
	      (unless (eq buffer llgud-comint-buffer)
		(with-current-buffer buffer
		  (when (and (memq llgud-minor-mode '(gdbmi gdba))
			     (not (string-match "\\`\\*.+\\*\\'"
						(buffer-name))))
		    (make-local-variable 'gdb-define-alist)
		    (gdb-create-define-alist)
		    (add-hook 'after-save-hook
			      'gdb-create-define-alist nil t))))))
	(kill-local-variable 'gdb-define-alist)
	(remove-hook 'after-save-hook 'gdb-create-define-alist t))))

(defcustom llgud-tooltip-modes '(llgud-mode c-mode c++-mode fortran-mode
					python-mode)
  "List of modes for which to enable LLGUD tooltips."
  :type 'sexp
  :group 'llgud
  :group 'tooltip)

(defcustom llgud-tooltip-display
  '((eq (tooltip-event-buffer llgud-tooltip-event)
	(marker-buffer llgud-overlay-arrow-position)))
  "List of forms determining where LLGUD tooltips are displayed.

Forms in the list are combined with AND.  The default is to display
only tooltips in the buffer containing the overlay arrow."
  :type 'sexp
  :group 'llgud
  :group 'tooltip)

(defcustom llgud-tooltip-echo-area nil
  "Use the echo area instead of frames for LLGUD tooltips."
  :type 'boolean
  :group 'llgud
  :group 'tooltip)

(define-obsolete-variable-alias 'tooltip-llgud-modes
                                'llgud-tooltip-modes "22.1")
(define-obsolete-variable-alias 'tooltip-llgud-display
                                'llgud-tooltip-display "22.1")

;;; Reacting on mouse movements

(defun llgud-tooltip-change-major-mode ()
  "Function added to `change-major-mode-hook' when tooltip mode is on."
  (add-hook 'post-command-hook 'llgud-tooltip-activate-mouse-motions-if-enabled))

(defun llgud-tooltip-activate-mouse-motions-if-enabled ()
  "Reconsider for all buffers whether mouse motion events are desired."
  (remove-hook 'post-command-hook
	       'llgud-tooltip-activate-mouse-motions-if-enabled)
  (dolist (buffer (buffer-list))
    (save-excursion
      (set-buffer buffer)
      (if (and llgud-tooltip-mode
	       (memq major-mode llgud-tooltip-modes))
	  (llgud-tooltip-activate-mouse-motions t)
	(llgud-tooltip-activate-mouse-motions nil)))))

(defvar llgud-tooltip-mouse-motions-active nil
  "Locally t in a buffer if tooltip processing of mouse motion is enabled.")

;; We don't set track-mouse globally because this is a big redisplay
;; problem in buffers having a pre-command-hook or such installed,
;; which does a set-buffer, like the summary buffer of Gnus.  Calling
;; set-buffer prevents redisplay optimizations, so every mouse motion
;; would be accompanied by a full redisplay.

(defun llgud-tooltip-activate-mouse-motions (activatep)
  "Activate/deactivate mouse motion events for the current buffer.
ACTIVATEP non-nil means activate mouse motion events."
  (if activatep
      (progn
	(make-local-variable 'llgud-tooltip-mouse-motions-active)
	(setq llgud-tooltip-mouse-motions-active t)
	(make-local-variable 'track-mouse)
	(setq track-mouse t))
    (when llgud-tooltip-mouse-motions-active
      (kill-local-variable 'llgud-tooltip-mouse-motions-active)
      (kill-local-variable 'track-mouse))))

(defun llgud-tooltip-mouse-motion (event)
  "Command handler for mouse movement events in `global-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))

;;; Tips for `llgud'

(defvar llgud-tooltip-original-filter nil
  "Process filter to restore after LLGUD output has been received.")

(defvar llgud-tooltip-dereference nil
  "Non-nil means print expressions with a `*' in front of them.
For C this would dereference a pointer expression.")

(defvar llgud-tooltip-event nil
  "The mouse movement event that led to a tooltip display.
This event can be examined by forms in `llgud-tooltip-display'.")

(defun llgud-tooltip-dereference (&optional arg)
  "Toggle whether tooltips should show `* expr' or `expr'.
With arg, dereference expr if ARG is positive, otherwise do not derereference."
 (interactive "P")
  (setq llgud-tooltip-dereference
	(if (null arg)
	    (not llgud-tooltip-dereference)
	  (> (prefix-numeric-value arg) 0)))
  (message "Dereferencing is now %s."
	   (if llgud-tooltip-dereference "on" "off")))

(define-obsolete-function-alias 'tooltip-llgud-toggle-dereference
                                'llgud-tooltip-dereference "22.1")

; This will only display data that comes in one chunk.
; Larger arrays (say 400 elements) are displayed in
; the tooltip incompletely and spill over into the llgud buffer.
; Switching the process-filter creates timing problems and
; it may be difficult to do better. Using annotations as in
; gdb-ui.el gets round this problem.
(defun llgud-tooltip-process-output (process output)
  "Process debugger output and show it in a tooltip window."
  (set-process-filter process llgud-tooltip-original-filter)
  (tooltip-show (tooltip-strip-prompt process output)
		(or llgud-tooltip-echo-area tooltip-use-echo-area)))

(defun llgud-tooltip-print-command (expr)
  "Return a suitable command to print the expression EXPR."
  (case llgud-minor-mode
        ; '-o' to print the objc object description if available
        (lldb (concat "expression -o -- " expr))
	(gdba (concat "server print " expr))
	((dbx gdbmi) (concat "print " expr))
	((xdb pdb) (concat "p " expr))
	(sdb (concat expr "/"))))

(defun llgud-tooltip-tips (event)
  "Show tip for identifier or selection under the mouse.
The mouse must either point at an identifier or inside a selected
region for the tip window to be shown.  If `llgud-tooltip-dereference' is t,
add a `*' in front of the printed expression.  In the case of a C program
controlled by GDB, show the associated #define directives when program is
not executing.

This function must return nil if it doesn't handle EVENT."
  (let (process)
    (when (and (eventp event)
	       llgud-tooltip-mode
	       llgud-comint-buffer
	       (buffer-name llgud-comint-buffer); might be killed
	       (setq process (get-buffer-process llgud-comint-buffer))
	       (posn-point (event-end event))
	       (or (and (eq llgud-minor-mode 'gdba) (not gdb-active-process))
		   (progn (setq llgud-tooltip-event event)
			  (eval (cons 'and llgud-tooltip-display)))))
      (let ((expr (tooltip-expr-to-print event)))
	(when expr
	  (if (and (eq llgud-minor-mode 'gdba)
		   (not gdb-active-process))
	      (progn
		(with-current-buffer (tooltip-event-buffer event)
		  (let ((define-elt (assoc expr gdb-define-alist)))
		    (unless (null define-elt)
		      (tooltip-show
		       (cdr define-elt)
		       (or llgud-tooltip-echo-area tooltip-use-echo-area))
		      expr))))
	    (when llgud-tooltip-dereference
	      (setq expr (concat "*" expr)))
	    (let ((cmd (llgud-tooltip-print-command expr)))
	      (when (and llgud-tooltip-mode (eq llgud-minor-mode 'gdb))
		(llgud-tooltip-mode -1)
		(message-box "Using LLGUD tooltips in this mode is unsafe\n\
so they have been disabled."))
	      (unless (null cmd) ; CMD can be nil if unknown debugger
		(if (memq llgud-minor-mode '(gdba gdbmi))
		      (if gdb-macro-info
			  (gdb-enqueue-input
			   (list (concat
				  gdb-server-prefix "macro expand " expr "\n")
				 `(lambda () (gdb-tooltip-print-1 ,expr))))
			(gdb-enqueue-input
			 (list  (concat cmd "\n")
 				 `(lambda () (gdb-tooltip-print ,expr)))))
		  (setq llgud-tooltip-original-filter (process-filter process))
		  (set-process-filter process 'llgud-tooltip-process-output)
		  (llgud-basic-call cmd))
		expr))))))))

(provide 'llgud)

;;; arch-tag: 6d990948-df65-461a-be39-1c7fb83ac4c4
;;; llgud.el ends here
