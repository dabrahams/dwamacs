;;; wg-bufs.el --- a minor mode for workgroup-relative buffer lists

;; Copyright (c) 2011 Alp Aker

;; Author: Alp Aker <alp.tekin.aker@gmail.com>
;; Version: 1.93
;; Keywords: convenience, buffers

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary: 

;; wg-bufs extends Emacs's buffer menu so that it understands a
;; distinction between those buffers that associated with a workgroup and those
;; that are not.  The buffer menu can be toggled between listing all buffers
;; and listing only those buffers associated with the workgroup.  Buffers can be
;; added to and removed from the list of associated buffers.  The criteria
;; governing which buffers are automatically associated with a workgroup can be
;; customized.

;; wg-bufs does not alter the `buffer-list' or `buried-buffer-list' workgroup
;; parameters of any workgroup.  These latter lists record which buffers have
;; been selected on a workgroup.  wg-bufs keeps its own record of the buffers
;; associated with each workgroup; this list can be both manually edited and
;; governed by criteria other than selection.

;; Installation
;; ============

;; Put this file in your load path and put:
;;
;;   (require 'wg-bufs)
;;
;; in your .emacs.  To toggle wg-bufs mode on and off, use the command
;;`wg-bufs-mode'.  To turn it on automatically when starting Emacs, put:
;;
;;  (wg-bufs-mode t) 
;;
;; in your .emacs.  

;; Usage
;; =====

;; wg-bufs operates fairly transparently.  The buffer menu now has two
;; modes.  In "full-list" mode, it lists all buffers; in "workgroup-list" mode it
;; lists only those buffers that are associated with the selected workgroup.  One
;; can toggle between the modes by typing "F".

;; In full-list mode, there is a new fourth column after the initial CRM
;; columns--the `F' column.  Buffers associated with the selected workgroup are
;; indicated with an `o' in this column.  In workgroup-list mode, the fourth `F'
;; column is suppressed.  (Full-list/workgroup-list status is also indicated in
;; the mode line.)

;; The typical way a buffer becomes associated with a workgroup is by being
;; selected in a window on the workgroup.  One can manually associate buffers
;; with a workgroup, and disassociate them as well, using two other new commands
;; in the buffer menu.  By typing `A' a buffer can be marked as to be added
;; to the buffers associated with the selected workgroup.  By typing `N' a buffer
;; can be marked as to have its association with the selected workgroup
;; severed.  As with other actions in the buffer menu, these changes take
;; effect when `Buffer-menu-execute' is called.

;; When first called, the buffer menu open in full-list mode.  In subsequent
;; calls it opens in whatever mode it was last in.

;; Criteria That Control Buffer-Workgroup Association
;; ==============================================

;; The association between buffers and workgroups is dynamic:  if a buffer is
;; selected on a workgroup, then it becomes associated with that workgroup.  Note,
;; then, that a buffer can be associated with more than one workgroup.

;; In addition, several other variables control which buffers automatically
;; become associated with a workgroup:

;; o If `wg-bufs-include-displayed-buffers' is non-nil, then buffers that
;;   are merely displayed on a workgroup become associated with the workgroup, even
;;   if they have not been selected.

;; o If a buffer's name is a member of `wg-bufs-always-include-names' then
;;   that buffer is automatically associated with every workgroup.  The default
;;   value is ("*scratch*").

;; o Three variables control which buffers are associated with a newly created
;;   workgroup:
;;
;;   - `wg-bufs-new-workgroups-inherit': If non-nil, then the buffers
;;      associated with a new workgroup include (at least) the buffers that were
;;      associated with the new workgroup's "parent," i.e., the workgroup that was
;;      selected when the new workgroup was created.
;;   - `wg-bufs-include-new-buffers': If non-nil, and the command that
;;      creates a new workgroup also creates new buffers, the new buffers are
;;      associated with the new workgroup.  (This applies only to buffers that
;;      are created *after* the new workgroup is created.)
;;   - `wg-bufs-include-init-buffer':  If non-nil, then the buffer that is
;;      current when a new workgroup is created will be associated with the new
;;      workgroup.  If nil, it will not.  (Note that
;;      wg-bufs-new-workgroups-inherit takes precedence over this
;;      variable.  Also note:  If the buffer in question is displayed on the
;;      new workgroup when the workgroup-creating command terminates, it will still
;;      be associated with the new workgroup.)

;; Other Commands and Features
;; ===========================

;; o If `wg-bufs-use-buffer-predicate' is non-nil, each workgroup's buffer
;;   predicate is set so that `other-buffer' will prefer buffers associated
;;   with the selected workgroup.  Thus, when a buffer is removed from a window
;;   and automatically replaced with another (as happens, say, when one kills
;;   a buffer), the newly displayed buffer will, if possible, be another
;;   workgroup-associated buffer.  The default value of this variable is t.

;; wg-bufs provides three other commands that are available everywhere,
;; not just in the buffer menu:

;; o `wg-bufs-dismiss-buffer' is somewhat analogous to `bury-buffer'.  It
;;   removes a buffer from the list of buffers associated with a workgroup, and
;;   if that buffer is displayed in any windows on the selected workgroup, it is
;;   replaced by another buffer (if `wg-bufs-use-buffer-predicate' is
;;   non-nil, the will be a buffer associated with the selected workgroup, if
;;   possible).  When called with no arguments, it acts on the current
;;   buffer, severing its association with the selected workgroup.

;; o `wg-bufs-reset-workgroup' resets a workgroup's associated-buffer list;
;;   specifically, it sets the list of associated buffers to the list of
;;   buffers that have been selected on the workgroup.  When called with no
;;   argument, it acts on the current workgroup.

;; o `wg-bufs-reset-all-workgroups' resets the associated buffers of all
;;   workgroups.

;; By default, none of these commands has a key binding.

;; Other Customization Options
;; ===========================

;; o To rebind the new buffer menu commands, alter their bindings in the
;;   keymap `wg-bufs-mode-map'.

;; o The indicator bit used for workgroup-associated buffers (default `o') can be
;;   set via the variable `wg-bufs-associated-buffer-bit'.

;; o The strings used to indicate workgroup-list/full-list state in the buffer
;;   menu's mode line can be changed by setting the variables
;;   `wg-bufs-mode-line-workgroup-list-string' and
;;   `wg-bufs-mode-line-full-list-string'.  The mode-line indication can
;;   be turned off by setting `wg-bufs-mode-line-indication' to
;;   nil.  (This latter variable can be set to any valid mode-line construct;
;;   users setting this variable to a custom mode-line construct will
;;   probably want to make use of the variable `wg-bufs-full-list'.)

;; Compatibility
;; =============

;; wg-bufs is compatible with buff-menu+.  It does not affect the
;; operation of `electric-buffer-list'.

;; Using Wg-Bufs in Programs
;; ============================

;; o To use a workgroup's associated-buffer list from within a Lisp progam, it is
;;   recommended that you work with the list returned by the function
;;   `wg-bufs-buffer-list'; don't use the value of the
;;   wg-bufs-buffer-list workgroup parameter.  The latter can contain internal
;;   buffers (buffers whose names starts with a space) and dead buffers; it
;;   is not guaranteed to respect `wg-bufs-always-include-names'; and its
;;   order is meaningless.  The list returned by `wg-bufs-buffer-list'
;;   will contain only live, non-internal buffers; be updated to reflect the
;;   current value of wg-bufs-always-include-names; and be sorted
;;   stably by selection order on the current workgroup.


;; Acknowledgements
;; ============================

;; Thanks to Greg Bognar for alpha testing and to Drew Adams for suggesting
;; many improvements.

;;; Code:

(when (< emacs-major-version 22)
  (error "Wg-Bufs requires version 22 or later"))

;;; ---------------------------------------------------------------------
;;; User Options
;;; ---------------------------------------------------------------------

(defgroup wg-bufs nil
  "Extend buffer-menu to allow listing of buffers associated with particular workgroup."
  :group 'convenience)

(defcustom wg-bufs-mode-hook nil
  "Hook run when wg-bufs mode is enabled or disabled."
  :group 'wg-bufs
  :type 'hook)

(defcustom wg-bufs-mode-on-hook nil
  "Hook run when wg-bufs mode is enabled."
  :group 'wg-bufs
  :type 'hook)

(defcustom wg-bufs-mode-off-hook nil
  "Hook run when wg-bufs mode is disabled."
  :group 'wg-bufs
  :type 'hook)

(defcustom wg-bufs-use-buffer-predicate t
  "If non-nil, wg-bufs adjusts the buffer-predicate workgroup parameter of every workgroup.
Specifically, wg-bufs sets the buffer predicate of each workgroup
so that `other-buffer' will prefer buffers associated with that
workgroup.  If nil, `other-buffer' does not prefer workgroup-associated buffers.

Changes to this variable do not take effect until the
mode-function `wg-bufs-mode' is run."
  :group 'wg-bufs
  :type 'boolean)

(defcustom wg-bufs-always-include-names '("*scratch*")
  "If a buffer's name is in this list, that buffer is associated with every workgroup.
The value of the variable should be a list of strings."
  :group 'wg-bufs
  :type '(repeat string))

(defcustom wg-bufs-include-displayed-buffers nil
  "If non-nil, buffers displayed on a workgroup becomes associated with it.
If nil, buffers becomes associated with a workgroup only if they are
selected on that workgroup, not merely displayed."
  :group 'wg-bufs
  :type 'boolean)

(defcustom wg-bufs-include-new-buffers nil
  "Include new buffers in a new workgroup's associated-buffer list.
If non-nil, and the command that creates a new workgroup also creates
new buffers, those buffers will be associated with the new workgroup,
even if they have not been selected.  (Buffers created before the
new workgroup is created are not affected by this variable.)"
  :group 'wg-bufs
  :type 'boolean)

(defcustom wg-bufs-new-workgroups-inherit nil
  "Whether a new workgroup inherits the associations  of its \"parent\".
If non-nil, the associated buffers of a newly created workgroup
include (at least) those buffers that were associated with the
workgroup that was selected when the workgroup-creating command was
called."
  :group 'wg-bufs
  :type 'boolean)

(defcustom wg-bufs-include-init-buffer nil
  "Whether a new workgroup's associated buffers include the last buffer before creation.
If non-nil, then the buffer that is current when a workgroup-creating
command is called--the \"init buffer\"--is associated with the
new workgroup.  If nil, it is not.

Note:  If the init buffer is displayed on the new workgroup after the
workgroup-creating command terminates, then it will be associated
with the new workgroup, even if this variable is nil.  Also note:
`wg-bufs-new-workgroups-inherit' takes precedence over this
variable."
  :group 'wg-bufs
  :type 'boolean)

(defcustom wg-bufs-assoc-rules nil
  ""
  :group 'wg-bufs
  :type 'list)

(defcustom wg-bufs-mode-line-local-list-string "[Workgroup List]" 
  "Mode-line indication that the buffer menu is in workgroup-list mode."
  :group 'wg-bufs
  :type 'string)

(defcustom wg-bufs-mode-line-full-list-string "[Full List]" 
  "Mode-line indication that the buffer menu is in full-list mode."
  :group 'wg-bufs
  :type 'string)

(defcustom wg-bufs-mode-line-identification 
  '((wg-bufs-full-list
     (:eval (propertize wg-bufs-mode-line-full-list-string
                        'local-map wg-bufs-mode-line-keymap
                        'help-echo  (concat "List of all buffers\n"
                                            "mouse-1 for local list")))
     (:eval (propertize wg-bufs-mode-line-local-list-string
                        'local-map wg-bufs-mode-line-keymap
                        'help-echo (concat "Buffer list for workgroup \""
                                           (workgroup-parameter nil 'name)
                                           "\"\n" 
                                           "mouse-1 for full list")))))
  "Mode-line indication of the buffer menu's state.
When wg-bufs is enabled, this variable is inserted into the
value of `mode-line-format' in the buffer menu, after
`mode-line-buffer-identification'.  If this variable is set to
nil, no special information appears in the mode-line.  The value
should be a valid mode-line construct.

When customizing this variable, users will probably want to make
use of the variable `wg-bufs-full-list'."
  :group 'wg-bufs
  :type 'sexp)

(defcustom wg-bufs-associated-buffer-bit ?o 
  "Character used to indicate workgroup-associated buffers in the buffer menu."
  :group 'wg-bufs
  :type 'character)

;;; ---------------------------------------------------------------------
;;; Internal Variables
;;; ---------------------------------------------------------------------

(defvar wg-bufs-full-list t
  "Records whether the buffer menu is in full-list or workgroup-list state.")

;; The following are used in initializing the associated-buffer list of a
;; newly created workgroup.

;; This is let-bound to t during execution of list-buffers or
;; buffer-menu-other-window.  In case those commands display the buffer menu
;; on a different workgroup, we don't want normal associated-buffer list
;; initialization performed on that workgroup (our advice around those functions
;; handles list initialization in a way suitable for that special case).
(defvar wg-bufs-no-list-initialization nil)

;; Records which buffer is current when a new workgroup is created.  Used when
;; `wg-bufs-include-new-buffers' is non-nil.
(defvar wg-bufs-init-buffer nil)

;; Records the associated buffers of the selected workgroup before a new workgroup is
;; created.  Used when `per-workgroup-new-workgroups-inherit' is non-nil.
(defvar wg-bufs-parent-buffer-list nil)

;; Records which buffers are already in existence when a new workgroup is
;; created.  Used when `wg-bufs-include-new-buffers' is non-nil.
(defvar wg-bufs-prev-buffers nil)

;; When a new workgroup is created, records the identity of that workgroup.  Used by
;; `wg-bufs-initialize-new-workgroup' in conjunction with the previous
;; variables to initialized the associated-buffer list.
(defvar wg-bufs-new-workgroup nil)

(defconst wg-bufs-advised-fns 
  '(electric-buffer-list
    list-buffers
    buffer-menu-other-window
    wg-switch-to-workgroup))

(defconst wg-bufs-hook-assignments
  '((Buffer-menu-mode-hook . wg-bufs-set-up-buff-menu)
    (window-configuration-change-hook . wg-bufs-window-change)
    (before-make-workgroup-hook . wg-bufs-before-make-workgroup)
    (after-make-workgroup-functions . wg-bufs-after-make-workgroup)))

;;; ---------------------------------------------------------------------
;;; Mode Definitions and Keymaps
;;; ---------------------------------------------------------------------

(defvar wg-bufs-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "F" 'wg-bufs-toggle-full-list)
    (define-key map "A" 'wg-bufs-make-associated)
    (define-key map "N" 'wg-bufs-make-non-associated)
    map)
    "Keymap for `wg-bufs-mode'.  
See the documentation of that command for details.")

;; We use Buffer-menu-mode-hook to set wg-bufs-mode-map as the the local
;; keymap in the buffer menu, so make sure it includes all the
;; Buffer-menu-mode-map bindings.
(set-keymap-parent wg-bufs-mode-map Buffer-menu-mode-map)

(defvar wg-bufs-mode-line-keymap
  ;; Set up a keymap so that clicking on our mode line information toggles
  ;; full-list/local-list mode.
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'wg-bufs-mode-line-toggle-full-list)
    map)
"Keymap for `wg-bufs-mode-line-identification'.")

(defvar wg-bufs-mode nil 
  "Non-nil if wg-bufs mode is enabled.

Do not set this variable directly.  Use the command
`wg-bufs-mode' instead.")

;; Make sure our info is available via `C-h m'.
(add-to-list 'minor-mode-list 'wg-bufs-mode)

(defun wg-bufs-mode (&optional arg) 
  "Toggle wg-bufs-mode on and off.

wg-bufs-mode tracks which buffers are associated with a given
workgroup (the \"workgroup-associated\" buffers) and extends the buffer
menu to take advantage of this information.  The buffer menu can
be toggled between listing all buffers and listing only
workgroup-associated buffers.  

When listing all buffers, there is a fourth column in the buffer
menu after the CRM columns: the F column.  Buffers associated with the
current workgroup are indicated with an `o' in this column .  When
listing only workgroup-associated buffers, this fourth column is
suppressed.  Full-list/workgroup-list status is also indicated in the
mode line.

The list of buffers associated with a workgroup can be manually
edited from within the buffer menu.

The following new commands are available in the buffer
menu:

\\<wg-bufs-mode-map>\\[wg-bufs-toggle-full-list] -- Toggle between listing workgroup-associated buffers and all buffers.
\\[wg-bufs-make-associated] -- Mark a buffer to be added to the associated buffer list.
\\[wg-bufs-make-non-associated] -- Mark a buffer to be removed from the associated buffer list.

Requested changes in workgroup-buffer associations are effected by
calling `Buffer-menu-execute'.

Buffers automatically become associated with a workgroup if they are
selected in one of the workgroup's windows.  Further control over
which buffers are automatically associated with a workgroup is
provided by the variables `wg-bufs-include-displayed-buffers',
`wg-bufs-always-include-names',
`wg-bufs-include-new-buffers',
`wg-bufs-new-workgroups-inherit', and
`wg-bufs-include-init-buffer'.

For further customization options, see the documentation of the
variables `wg-bufs-associated-buffer-bit', `wg-bufs-use-buffer-predicate',
`wg-bufs-mode-line-workgroup-list-string', 
`wg-bufs-mode-line-full-list-string', and 
`wg-bufs-mode-line-identification'."
  (interactive "P")
  (setq wg-bufs-mode (if (not arg) 
                         (not wg-bufs-mode)
                       (> (prefix-numeric-value arg) 0)))
  (if wg-bufs-mode
      ;; Enabling.
      (progn
        (dolist (workgroup (workgroup-list))
          (wg-bufs-set-buffer-predicate workgroup wg-bufs-use-buffer-predicate)
          (wg-bufs-initialize-existing-workgroup workgroup))
        (setq Buffer-menu-buffer-column 5)
        (ad-enable-regexp "wg-bufs")
        (dolist (fn wg-bufs-advised-fns)
          (ad-activate fn))
        (dolist (hook wg-bufs-hook-assignments)
          (add-hook (car hook) (cdr hook)))
        ;; In case we toggle the mode while the buffer menu exists.
        (let ((buf (get-buffer "*Buffer List*")))
          (when buf
            (with-current-buffer buf
              (revert-buffer))))
        (run-hooks 'wg-bufs-mode-on-hook)
        (message "Per-workgroup buffer menus are enabled"))
    ;; Disabling.
    (dolist (workgroup (workgroup-list))
      (wg-bufs-set-buffer-predicate workgroup nil))
    (setq Buffer-menu-buffer-column 4)
    (ad-disable-regexp "wg-bufs")
    (dolist (fn wg-bufs-advised-fns)
      (ad-activate fn))
    (dolist (hook wg-bufs-hook-assignments)
      (remove-hook (car hook) (cdr hook)))
    ;; Again, in case we toggle the mode while the buffer menu exists, but
    ;; this time with a hack to make sure Buffer-menu-revert-function finds
    ;; the right buffer despite the change in Buffer-menu-buffer-column.
    (let ((buf (get-buffer "*Buffer List*")))
      (when buf
        (with-current-buffer buf
          (unless (eobp)
            (let ((buffer-read-only nil)
                  (pos (+ (line-beginning-position) 4)))
              (put-text-property pos 
                                 (1+ pos)
                                 'buffer
                                 (get-text-property (1+ pos) 'buffer))))
          (revert-buffer))))
    (run-hooks 'wg-bufs-mode-off-hook)
    (message "Per-workgroup buffer menus are disabled"))
  (run-mode-hooks 'wg-bufs-mode-hook))

;;; ---------------------------------------------------------------------
;;; Workgroup Initialization and Clean Up
;;; ---------------------------------------------------------------------

;; Set the associated-buffer list for workgroups already in existence when wg-bufs
;; is enabled.  We try to DTRT as much as possible:  If buffers have been
;; selected, they belong to the associated list.  If the user wants displayed
;; buffers as well, we grab all the currently displayed buffers.  If
;; wg-bufs had previously been enabled and is now being re-enabled, we
;; don't overwrite the existing associated list, but just add to it.
(defun wg-bufs-initialize-existing-workgroup (workgroup)
  (wg-bufs-add-buffers (append (workgroup-parameter workgroup 'buffer-list)
                                  (workgroup-parameter workgroup 'buried-buffer-list)
                                  (if wg-bufs-include-displayed-buffers
                                      (mapcar #'(lambda (x) (window-buffer x))
                                              (window-list workgroup 'no-minibuf))))
                          workgroup))

;; The next three functions handle initialization of the associated-buffer
;; list for newly created workgroups.  The procedure is as follows: 

;; (1) wg-bufs-before-make-workgroup is called before the new workgroup is
;;     created, and records the current buffer, the associated-buffer list of
;;     the selected workgroup, and the list of all current existing buffers.

;; (2) wg-bufs-after-make-workgroup is called immediately after the new workgroup
;;     is created.  It sets the buffer predicate of the new workgroup (if
;;     necessary), and arranges for wg-bufs-initialize-new-workgroup to be
;;     called after the current command (the one creating the new workgroup)
;;     terminates.  We defer initialization of the associated-buffer list of
;;     the new workgroup for the sake of the option
;;     `wg-bufs-include-new-buffers'.  At the time the workgroup is created,
;;     we aren't in a position to determine what buffers are created by the
;;     command that also creates the new workgroup.  So we put off initialization
;;     until we have the information.

;; (3) wg-bufs-initialize-new-workgroup then performs all the
;;     associate-buffer-list initialization.  Specifically, it sets the new
;;     workgroup's associated-buffer list according to the variables `
;;     wg-bufs-new-workgroups-inherit', `wg-bufs-include-new-buffers', and
;;     `wg-bufs-include-init-buffer'.

;; Note that we do not engage in normal list initialization when the new
;; workgroup is created by a call to list-buffers or buffer-menu-other-window
;; (e.g., when pop-up-workgroups is non-nil).  In that case, the buffer menu is
;; displayed on a new workgroup, and we want the buffer menu's associated-buffer
;; list to be just like that of the workgroup from which the buffer menu is
;; called.  So those functions let-bind `wg-bufs-no-list-initialization'
;; to t, disabling the above routine (aside from setting the buffer predicate
;; of the new workgroup), and then handle associated-buffer list initialization
;; themselves.

(defun wg-bufs-before-make-workgroup ()
  (unless wg-bufs-no-list-initialization
    (setq wg-bufs-init-buffer (current-buffer)
          wg-bufs-prev-buffers (buffer-list)
          wg-bufs-parent-buffer-list (copy-sequence
                                         (workgroup-parameter (selected-workgroup) 
                                                          'wg-bufs-buffer-list)))))

(defun wg-bufs-after-make-workgroup (workgroup)
  (wg-bufs-set-buffer-predicate workgroup wg-bufs-use-buffer-predicate)
  (unless wg-bufs-no-list-initialization
    (add-hook 'post-command-hook 'wg-bufs-initialize-new-workgroup)
    (setq wg-bufs-new-workgroup workgroup)))

(defun wg-bufs-initialize-new-workgroup ()
  (remove-hook 'post-command-hook 'wg-bufs-initialize-new-workgroup)
  (unwind-protect
      (when (workgroup-live-p wg-bufs-new-workgroup)
        (when wg-bufs-include-new-buffers
          (wg-bufs-add-buffers (wg-bufs-set-minus wg-bufs-prev-buffers 
                                                        (buffer-list))
                                  wg-bufs-new-workgroup))
        (unless (or wg-bufs-include-init-buffer
                    (memq wg-bufs-init-buffer 
                          (mapcar #'(lambda (x) (window-buffer x))
                                  (window-list wg-bufs-new-workgroup 'no-minibuf))))
          (wg-bufs-remove-buffer wg-bufs-init-buffer wg-bufs-new-workgroup))
        (when wg-bufs-new-workgroups-inherit
          (wg-bufs-add-buffers wg-bufs-parent-buffer-list wg-bufs-new-workgroup))
        ;; Enforce custom buffer-workgroup associations.
        (wg-bufs-enforce-rules (list wg-bufs-new-workgroup)))
    (setq wg-bufs-new-workgroup nil
          wg-bufs-parent-buffer-list nil
          wg-bufs-init-buffer nil
          wg-bufs-prev-buffers nil)))

;; Set the buffer predicate.  If ON is non-nil, set the buffer-predicate to
;; our buffer predicate, and also save any existing buffer predicate so we
;; can check that too when our buffer predicate is called (as opposed to
;; quashing the existing buffer predicate).  If ON is nil, remove our
;; buffer predicate if present and restore any saved buffer predicate.
(defun wg-bufs-set-buffer-predicate (workgroup on)
  (let ((buffer-pred  (workgroup-parameter workgroup 'buffer-predicate)))
    (if on
        (unless (eq buffer-pred 'wg-bufs-ok-to-display-p)
          (set-workgroup-parameter workgroup
                               'wg-bufs-saved-buffer-pred
                               buffer-pred)
          (set-workgroup-parameter workgroup 
                               'buffer-predicate 
                               'wg-bufs-ok-to-display-p))
      (when (eq buffer-pred 'wg-bufs-ok-to-display-p)
        (set-workgroup-parameter workgroup 
                             'buffer-predicate 
                             (workgroup-parameter workgroup 'wg-bufs-saved-buffer-predicate))
        (set-workgroup-parameter workgroup 
                             'wg-bufs-saved-buffer-predicate 
                             nil)))))

;;; ---------------------------------------------------------------------
;;; Associated-Buffer List Maintenance and Manipulation
;;; ---------------------------------------------------------------------

;; Called by window-configuration-change-hook to update the associated-buffer
;; list.
(defun wg-bufs-window-change ()
  (let ((workgroup (selected-workgroup)))
    (dolist (win (window-list workgroup 'no-minibuf))
      (let ((buf (window-buffer win)))
        ;; If merely displayed buffers are ok add buf.  If not, add buf if
        ;; it's been selected on the workgroup.
        (when (or wg-bufs-include-displayed-buffers
                  (memq buf (workgroup-parameter workgroup 'buffer-list))
                  (memq buf (workgroup-parameter workgroup 'buried-buffer-list)))
          (wg-bufs-add-buffer buf workgroup))))))

(defun wg-bufs-remove-buffer (buf workgroup)
  "Remove BUF from WORKGROUP's associated-buffer list."
  (set-workgroup-parameter workgroup
                       'wg-bufs-buffer-list
                       (delq buf (workgroup-parameter workgroup 'wg-bufs-buffer-list))))

(defun wg-bufs-add-buffer (buf workgroup)
  "Add BUF to WORKGROUP's associated-buffer list if not already present."
  (unless (bufferp buf)
    (signal 'wrong-type-argument (list 'bufferp buf)))
  (let ((associated-bufs (workgroup-parameter workgroup 'wg-bufs-buffer-list)))
    (unless (memq buf associated-bufs)
      (set-workgroup-parameter workgroup 'wg-bufs-buffer-list (cons buf associated-bufs)))))

(defun wg-bufs-add-buffers (bufs workgroup)
  "Add each member of BUFS to WORKGROUP's associated-buffer list if it not
already present."
  (dolist (buf bufs)
    (wg-bufs-add-buffer buf workgroup)))

(defun wg-bufs-buffer-list (workgroup &optional full)
  "When called with argument FULL non-nil, return the same result
as (buffer-list WORKGROUP).  With FULL nil, update the
associated-buffer list and return it, sorted by selection order on
WORKGROUP.  The return value is a copy of the list, not the list
itself."
  ;; Filter out internal buffers.
  (wg-bufs-filter-buffers
   (if full
       ;; The full list.
       (buffer-list workgroup)
     ;; The workgroup-associated list.
     ;; Include members of wg-bufs-always-include-names
     (dolist (bufname wg-bufs-always-include-names)
       (when (get-buffer bufname)
         (wg-bufs-add-buffer (get-buffer bufname) workgroup)))
     ;; Enforce custom buffer-workgroup associations.
     (wg-bufs-enforce-rules (list workgroup))
     ;; Return the associated-buffer list, sorted appropriately for this workgroup.
     (wg-bufs-sort-buffers workgroup (workgroup-parameter workgroup 'wg-bufs-buffer-list)))))

(defun wg-bufs-enforce-rules (&optional workgroups)
  (or workgroups (setq workgroups (workgroup-list)))
  (dolist (rule wg-bufs-assoc-rules)
    (cond
     ((eq (car rule) 'sufficient)
      (wg-bufs-enforce-suff workgroups rule))
     ((eq (car rule) 'necessary)
      (wg-bufs-enforce-nec workgroups rule))
     ((eq (car rule) 'iff)
      (wg-bufs-enforce-iff workgroups rule)))))

(defun wg-bufs-enforce-suff (workgroups rule)
  (dolist (workgroup workgroups)
    (when (eval (nth 2 rule))
      (dolist (buffer (buffer-list))
        (when (with-current-buffer buffer (eval (nth 1 rule)))
          (wg-bufs-add-buffer buffer workgroup))))))

(defun wg-bufs-enforce-nec (workgroup rule)
  (dolist (workgroup workgroups)
  ;; Remove dead buffers.  (Should be able to do this as the buffers are
  ;; killed, via kill-buffer-hook, but there are a few corner cases that
  ;; let dead buffers slip through that way.)
  (set-workgroup-parameter workgroup 
                       'wg-bufs-buffer-list
                       (delq nil 
                             (mapcar #'(lambda (x) (if (buffer-live-p x) x))
                                     (workgroup-parameter workgroup
                                                      'wg-bufs-buffer-list))))
    (when (not (eval (nth 2 rule)))
      (dolist (buffer (workgroup-parameter workgroup 'wg-bufs-buffer-list))
        (when (with-current-buffer buffer (not (eval (nth 1 rule))))
          (wg-bufs-remove-buffer buffer workgroup))))))

(defun wg-bufs-enforce-iff (workgroup rule)
  (wg-bufs-enforce-suff workgroup rule)
  (wg-bufs-enforce-nec workgroup rule))

;;; ---------------------------------------------------------------------
;;; Utilities and Predicates
;;; ---------------------------------------------------------------------

;; Return a list in which BUFS are sorted according to selection order on
;; WORKGROUP.
(defun wg-bufs-sort-buffers (workgroup bufs)
  (let ((l (buffer-list workgroup)))
    ;; Copy list since sort is destructive.
    (sort (copy-sequence bufs) #'(lambda (x y) (> (length (memq x l))
                                                  (length (memq y l)))))))

;; Remove internal buffers from BUFS.
(defun wg-bufs-filter-buffers (bufs)
  (delq nil
        (mapcar #'(lambda (x) (if (not (string-match "^ " (buffer-name x))) x))
                bufs)))

;; Simple destructive set difference.
(defun wg-bufs-set-minus (subtrahend minuend)
  (dolist (element subtrahend)
    (setq minuend (delq element minuend)))
  minuend)

;; Set as the buffer predicate for all workgroups when
;; wg-bufs-use-buffer-predicate is non-nil.  Check BUF against any other
;; predicate that might have been present, then check whether BUF is
;; associated with the current workgroup.  Return t if both tests succeed.
(defun wg-bufs-ok-to-display-p (buf)
  (let ((other-pred (workgroup-parameter nil 'wg-bufs-saved-buffer-pred)))
    (and (wg-bufs-associated-p buf)
         (if (functionp other-pred)
             (funcall other-pred buf)
           t))))

;; Check if BUF is associated with WORKGROUP.
(defun wg-bufs-associated-p (buf &optional workgroup)
  (memq buf (workgroup-parameter workgroup 'wg-bufs-buffer-list)))

;; Return bit info for BUF appropriate for the 4th column in the buffer-menu.
(defun wg-bufs-bit-info (buf)
  (if (and wg-bufs-full-list 
           (wg-bufs-associated-p buf))
      (char-to-string wg-bufs-associated-buffer-bit) 
    " "))

;;; ---------------------------------------------------------------------
;;; Commands (Not Specific to the Buffer Menu)
;;; ---------------------------------------------------------------------

(defun wg-bufs-dismiss-buffer (&optional buf workgroup)
  "Remove assocation between BUF and WORKGROUP without entering the buffer menu.
In addition, if any windows on WORKGROUP are currently displaying
BUF, replace BUF in those windows with some other buffer.  When
called with no arguments, acts on the current buffer and the
selected workgroup."
  (interactive)
  (unless buf 
    (setq buf (current-buffer)))
  (unless workgroup
    (setq workgroup (selected-workgroup)))
  (wg-bufs-remove-buffer buf workgroup)
  ;; We loop over the windows ourselves because replace-buffer-in-windows
  ;; acts on all workgroups; we only want to act on the selected workgroup.
  (dolist (win (get-buffer-window-list buf 'no-minibuf workgroup))
    (set-window-buffer win (other-buffer buf))))

(defun wg-bufs-reset-workgroup (&optional workgroup)
  "Reset WORKGROUP's associated-buffer list.
Set list of buffers associated with WORKGROUP to the list of all
buffers that have been selected on WORKGROUP, and no others.  When
called with no argument, act on the selected workgroup."
  (interactive)
  (unless workgroup 
    (setq workgroup (selected-workgroup)))
  (set-workgroup-parameter workgroup 
                       'wg-bufs-buffer-list
                       ;; Make sure we get copies, not the lists themselves.
                       (append 
                        (workgroup-parameter workgroup 'buffer-list)
                        (workgroup-parameter workgroup 'buried-buffer-list)
                        '())))

(defun wg-bufs-reset-all-workgroups ()
  "Reset the associated-buffer list of all workgroups.
Call `wg-bufs-reset-workgroup' on all live workgroups."
  (interactive)
  (dolist (workgroup (workgroup-list))
    (wg-bufs-reset-workgroup workgroup)))

;;; ---------------------------------------------------------------------
;;; Buffer Menu Initialization
;;; ---------------------------------------------------------------------

(defun wg-bufs-set-up-buff-menu ()
  ;; Set the keymap to our keymap.  Note that wg-bufs-mode-map has
  ;; Buffer-menu-mode-map as its parent.
  (use-local-map wg-bufs-mode-map)
  ;; Install indicator of workgroup-list/full-list status in the mode-line, after
  ;; mode-line-buffer-identification.
  (let ((before (reverse (memq 'mode-line-buffer-identification
                                (reverse mode-line-format))))
        (after (cdr (memq 'mode-line-buffer-identification mode-line-format))))
  (setq mode-line-format (append before
                                 wg-bufs-mode-line-identification
                                 after))))

;;; ---------------------------------------------------------------------
;;; New Buffer Menu Commands
;;; ---------------------------------------------------------------------

(defun wg-bufs-toggle-full-list (&optional arg)
  "Toggle whether the buffer-menu displays only buffers associated with this workgroup.
With a positive or true ARG display only workgroup-associated buffers.  With
zero, negative, or nil ARG, display all buffers."
  (interactive "P")
  (setq wg-bufs-full-list
        (cond ((not arg) (not wg-bufs-full-list))
              ((<= (prefix-numeric-value arg) 0) t)))
  (revert-buffer))

(defun wg-bufs-mode-line-toggle-full-list (e)
  "Toggle whether the buffer-menu displays only buffers associated with this workgroup."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start e)))
    (wg-bufs-toggle-full-list)))

(defun wg-bufs-make-associated (&optional arg)
  "Mark buffer on this line to be associated with this workgroup by \\<Buffer-memu-mode-map>\\[Buffer-menu-execute].
Prefix arg is how many buffers to associate.  Negative arg means
work backwards."
  (interactive "p")
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (when (or (null arg) (= arg 0))
        (setq arg 1))
      (while (> arg 0)
        (forward-char 3)
        (when (or (not (wg-bufs-associated-p (Buffer-menu-buffer nil)))
                  (looking-at "N"))
          (delete-char 1)
          (insert 
           (if (wg-bufs-associated-p (Buffer-menu-buffer nil))
               (if wg-bufs-full-list
                   wg-bufs-associated-buffer-bit
                 " ")
             "A")))
        (forward-line 1)
        (setq arg (1- arg)))
      (while (and (< arg 0)
                  (Buffer-menu-no-header))
        (forward-char 3)
        (when (or (not (wg-bufs-associated-p (Buffer-menu-buffer nil)))
                  (looking-at "N"))
          (delete-char 1)
          (insert 
           (if (wg-bufs-associated-p (Buffer-menu-buffer nil))
               (if wg-bufs-full-list
                   wg-bufs-associated-buffer-bit
                 " ")
             "A")))
        (forward-line -1)
        (setq arg (1+ arg))))))

(defun wg-bufs-make-non-associated (&optional arg)
  "Mark buffer on this line to be made non-associated by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute].
Prefix arg is how many buffers to make non-associated.  Negative
arg means work backwards."
  (interactive "p")
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (when (or (null arg) (= arg 0))
        (setq arg 1))
      (while (> arg 0)
        (forward-char 3)
        (when (or (wg-bufs-associated-p (Buffer-menu-buffer nil))
                  (looking-at "A"))
          (delete-char 1)
          (insert (if (or (not wg-bufs-full-list)
                          (wg-bufs-associated-p (Buffer-menu-buffer nil)))
                      "N"
                    " ")))
        (forward-line 1)
        (setq arg (1- arg)))
      (while (and (< arg 0)
                  (Buffer-menu-no-header))
        (forward-char 3)
        (when (or (wg-bufs-associated-p (Buffer-menu-buffer nil))
                  (looking-at "A"))
          (delete-char 1)
          (insert (if (or (not wg-bufs-full-list)
                          (wg-bufs-associated-p (Buffer-menu-buffer nil)))
                      "N"
                    " ")))
        (forward-line -1)
        (setq arg (1+ arg))))))

;;; ---------------------------------------------------------------------
;;;  Advised Buffer Menu Commands 
;;; ---------------------------------------------------------------------

;; Advice around both these functions serves the same purpose:  If
;; display-buffer or switch-to-buffer-other-window creates a new workgroup (as
;; when, e.g., pop-up-workgroups is non-nil), the buffer menu will be displayed
;; on a different workgroup.  We need to detect when that happens and make
;; the new workgroup's various buffer list workgroup parameters be copies of those of
;; its "parent" workgroup.

(defadvice buffer-menu-other-window (around wg-bufs)
  ;; Disable normal new workgroup initialization in case this creates a new workgroup
  ;; and record current workgroup so we can detect whether a different workgroup is
  ;; used for display.
  (let ((wg-bufs-no-list-initialization t)
        (oworkgroup (selected-workgroup)))
    ad-do-it
    ;; If we display the buffer menu on a different workgroup, reset the workgroup
    ;; parameters on the buffer menu's workgroup.
    (unless (eq (selected-workgroup) oworkgroup)
      (dolist (param '(buffer-list buried-buffer-list wg-bufs-buffer-list))
        (set-workgroup-parameter (selected-workgroup) 
                             param
                             (copy-sequence (workgroup-parameter oworkgroup param)))))))

(defadvice list-buffers (around wg-bufs)
  ;; Disable normal new workgroup initialization in case this creates a new workgroup
  ;; and record current workgroup so we can detect whether a different workgroup is
  ;; used for display.
  (let ((wg-bufs-no-list-initialization t)
        (oworkgroup (selected-workgroup)))
    ad-do-it
    ;; If we display the buffer menu on a different workgroup, reset the workgroup
    ;; parameters on the buffer menu's workgroup.
    (unless (eq (window-workgroup ad-return-value) oworkgroup)
      (dolist (param '(buffer-list buried-buffer-list wg-bufs-buffer-list))
        (set-workgroup-parameter (window-workgroup ad-return-value)
                             param
                             (copy-sequence (workgroup-parameter oworkgroup param)))))))

;;; ---------------------------------------------------------------------
;;;  Redefined Buffer Menu Commands 
;;; ---------------------------------------------------------------------

(defun Buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (when (Buffer-menu-no-header))
  (let* ((buf (Buffer-menu-buffer t))
         (mod (if (buffer-modified-p buf)
                  "*" " "))
         (readonly (if (with-current-buffer buf buffer-read-only)
                       "%" " "))
         (associated (if wg-bufs-mode
                    (wg-bufs-bit-info buf)
                  ""))
         (buffer-read-only nil))
    (delete-char (if wg-bufs-mode 4 3))
    (insert (concat " " readonly mod associated)))
  (forward-line (if backup -1 1)))

(unless (featurep 'buff-menu+)
  (defun Buffer-menu-execute ()
    "Save and/or delete buffers marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-save] or \\<Buffer-menu-mode-map>\\[Buffer-menu-delete]."
    (interactive)
    (when wg-bufs-mode
      (wg-bufs-buffer-menu-execute))
    (save-excursion
      (Buffer-menu-beginning)
      (while (re-search-forward "^..S" nil t)
        (let ((modp nil))
          (with-current-buffer (Buffer-menu-buffer t)
            (save-buffer)
            (setq modp (buffer-modified-p)))
          (let ((buffer-read-only nil))
            (delete-char -1)
            (insert (if modp "*" " "))))))
    (save-excursion
      (Buffer-menu-beginning)
      (let ((buff-menu-buffer (current-buffer))
            (buffer-read-only nil))
        (while (re-search-forward "^D" nil t)
          (forward-char -1)
          (let ((buf (Buffer-menu-buffer nil)))
            (or (eq buf nil)
                (eq buf buff-menu-buffer)
                (save-excursion (kill-buffer buf)))
            (if (and buf (buffer-name buf))
                (progn (delete-char 1)
                       (insert " "))
              (delete-region (point) (progn (forward-line 1) (point)))
              (unless (bobp)
                (forward-char -1)))))))))

;; We split this off from Buffer-menu-execute for the convenience of
;; buff-menu+.
(defun wg-bufs-buffer-menu-execute ()
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buffer-read-only nil))
      (while (re-search-forward "^...A" nil t)
        (forward-char -1)
        (let ((buf (Buffer-menu-buffer t)))
          (wg-bufs-add-buffer buf (selected-workgroup))
          (delete-char 1)
          (insert wg-bufs-associated-buffer-bit)))))
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buffer-read-only nil))
      (while (re-search-forward "^...N" nil t)
        (let ((buf (Buffer-menu-buffer t)))
          (wg-bufs-dismiss-buffer buf (selected-workgroup))
          (if wg-bufs-full-list
              (progn
                (forward-char -1)
                (let ((props (text-properties-at (point))))
                  (delete-char 1)
                  (insert (apply 'propertize " " props))))
            (forward-char -4)
            (delete-region (point) (progn (forward-line 1) (point)))
            (unless (bobp)
              (forward-char -1))))))))

;;; ---------------------------------------------------------------------
;;;  Redefined Buffer Menu Internal Functions
;;; ---------------------------------------------------------------------

(unless (featurep 'buff-menu+)
  (defun list-buffers-noselect (&optional files-only buffer-list)
    "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.

Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

If BUFFER-LIST is non-nil, it should be a list of buffers;
it means list those buffers and no others."
    (let* ((old-buffer (current-buffer))
           (standard-output standard-output)
           (mode-end (make-string (- Buffer-menu-mode-width
                                     2) ? ))
           (header (concat (if wg-bufs-mode
                               (if wg-bufs-full-list "CRMF " "CRM  ")
                             "CRM ")
                           (Buffer-menu-buffer+size
                            (Buffer-menu-make-sort-button "Buffer" 2)
                            (Buffer-menu-make-sort-button "Size" 3))
                           "  "
                           (Buffer-menu-make-sort-button "Mode" 4) mode-end
                           (Buffer-menu-make-sort-button "File" 5) 
                           "\n"))
           list desired-point)
      (when Buffer-menu-use-header-line
        (let ((pos 0))
          ;; Turn spaces in the header into stretch specs so they work
          ;; regardless of the header-line face.
          (while (string-match "[ \t\n]+" header pos)
            (setq pos (match-end 0))
            (put-text-property (match-beginning 0) pos 'display
                               ;; Assume fixed-size chars in the buffer.
                               (list 'space :align-to pos)
                               header)))
        ;; Try to better align the one-char headers.
        (put-text-property 0 (if wg-bufs-mode 4 3) 'face 'fixed-pitch header)
        ;; Add a "dummy" leading space to align the beginning of the header
        ;; line with the beginning of the text (rather than with the left
        ;; scrollbar or the left fringe).  --Stef
        (setq header (concat (propertize " " 'display '(space :align-to 0))
                             header)))
      (with-current-buffer (get-buffer-create "*Buffer List*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq standard-output (current-buffer))
        (unless Buffer-menu-use-header-line
          (let ((underline (if (char-displayable-p #x2014) #x2014 ?-)))
            (insert header
                    (apply 'string
                           (mapcar #'(lambda (c)
                                       (if (memq c '(?\n ?\s)) c underline))
                                   header)))))
        ;; Collect info for every buffer we're interested in.
        (dolist (buffer (or buffer-list
                            (and wg-bufs-mode 
                                 (wg-bufs-buffer-list (selected-workgroup) wg-bufs-full-list))
                            (buffer-list (and (boundp 'Buffer-menu-use-workgroup-buffer-list)
                                              Buffer-menu-use-workgroup-buffer-list))))
          (with-current-buffer buffer
            (let ((name (buffer-name))
                  (file buffer-file-name))
              (unless (and (null buffer-list)
                           (or
                            ;; Don't mention internal buffers.
                            (and (string= (substring name 0 1) " ") (null file))
                            ;; Maybe don't mention buffers without files.
                            (and files-only (not file))
                            (string= name "*Buffer List*")))
                ;; Otherwise output info.
                (let ((mode (concat (format-mode-line mode-name nil nil buffer)
                                    (if mode-line-process
                                        (format-mode-line mode-line-process
                                                          nil nil buffer))))
                      (bits 
                       (concat
                        (if (eq buffer old-buffer) "." " ")
                        ;; Handle readonly status.  The output buffer
                        ;; is special cased to appear readonly; it is
                        ;; actually made so at a later date.
                        (if (or (eq buffer standard-output)
                                buffer-read-only)
                            "%" " ")
                        ;; Identify modified buffers.
                        (if (buffer-modified-p) "*" " ")
                        ;; associated status
                        (if wg-bufs-mode
                            (wg-bufs-bit-info buffer)
                          "")
                        ;; Space separator.
                        " ")))
                  (unless file
                    ;; No visited file.  Check local value of
                    ;; list-buffers-directory and, for Info buffers,
                    ;; Info-current-file.
                    (cond ((and (boundp 'list-buffers-directory)
                                list-buffers-directory)
                           (setq file list-buffers-directory))
                          ((eq major-mode 'Info-mode)
                           (setq file Info-current-file)
                           (cond
                            ((equal file "dir")
                             (setq file "*Info Directory*"))
                            ((eq file 'apropos)
                             (setq file "*Info Apropos*"))
                            ((eq file 'history)
                             (setq file "*Info History*"))
                            ((eq file 'toc)
                             (setq file "*Info TOC*"))
                            ((not (stringp file))  ;; avoid errors
                             (setq file nil))
                            (t
                             (setq file (concat "("
                                                (file-name-nondirectory file)
                                                ") "
                                                Info-current-node)))))))
                  (push (list buffer bits name (buffer-size) mode file)
                        list))))))
        ;; Preserve the original buffer-list ordering, just in case.
        (setq list (nreverse list))
        ;; Place the buffers's info in the output buffer, sorted if necessary.
        (dolist (buffer
                 (if Buffer-menu-sort-column
                     (sort list
                           (if (eq Buffer-menu-sort-column 3)
                               (lambda (a b)
                                 (< (nth Buffer-menu-sort-column a)
                                    (nth Buffer-menu-sort-column b)))
                             (lambda (a b)
                               (string< (nth Buffer-menu-sort-column a)
                                        (nth Buffer-menu-sort-column b)))))
                   list))
          (when (eq (car buffer) old-buffer)
            (setq desired-point (point)))
          (insert (cadr buffer)
                  ;; Put the buffer name into a text property
                  ;; so we don't have to extract it from the text.
                  ;; This way we avoid problems with unusual buffer names.
                  (let ((name (nth 2 buffer))
                        (size (int-to-string (nth 3 buffer))))
                    (Buffer-menu-buffer+size name size
                                             `(buffer-name ,name
                                                           buffer ,(car buffer)
                                                           font-lock-face buffer-menu-buffer
                                                           mouse-face highlight
                                                           help-echo
                                                           ,(if (>= (length name)
                                                                    (- Buffer-menu-buffer+size-width
                                                                       (max (length size) 3)
                                                                       2))
                                                                name
                                                              "mouse-2: select this buffer"))))
                  "  "
                  (if (> (string-width (nth 4 buffer)) Buffer-menu-mode-width)
                      (truncate-string-to-width (nth 4 buffer)
                                                Buffer-menu-mode-width)
                    (nth 4 buffer)))
          (when (nth 5 buffer)
            (indent-to (+ Buffer-menu-buffer-column Buffer-menu-buffer+size-width
                          Buffer-menu-mode-width 4) 1)
            (princ (abbreviate-file-name (nth 5 buffer))))
          (princ "\n"))
        (Buffer-menu-mode)
        (setq Buffer-menu-files-only files-only)
        (when (boundp 'Buffer-menu--buffers)
          (setq Buffer-menu--buffers buffer-list))
        (when Buffer-menu-use-header-line
          (setq header-line-format header))
        ;; DESIRED-POINT doesn't have to be set; it is not when the
        ;; current buffer is not displayed for some reason.
        (when desired-point
          (goto-char desired-point))
        (set-buffer-modified-p nil)
        (current-buffer)))))

;; The definitions of the following three commands in buff-menu.el hard-code
;; the default value (4) of Buffer-menu-buffer-column.  We need them to
;; respect other values of Buffer-menu-buffer-column.  That's the only change
;; made to them.

(unless (featurep 'buff-menu+)
  (defun Buffer-menu-sort (column)
    "Sort the buffer menu by COLUMN."
    ;; It's not clear why the buff-menu version of this function is
    ;; interactive, since the user only runs it by calling
    ;; Buffer-menu-sort-by-column
    ;;(interactive "P")
    (when column
      (setq column (prefix-numeric-value column))
      (when (< column 2)
        (setq column 2))
      (when (> column 5)
        (setq column 5)))
    (setq Buffer-menu-sort-column column)
    (let ((inhibit-read-only t) l buf m1 m2)
      (save-excursion
        (Buffer-menu-beginning)
        (while (not (eobp))
          (when (buffer-live-p (setq buf (get-text-property (+ (point) Buffer-menu-buffer-column) 'buffer)))
            (setq m1 (char-after)
                  m1 (if (memq m1 '(?> ?D)) m1)
                  m2 (char-after (+ (point) 2))
                  m2 (if (eq m2 ?S) m2))
            (when (or m1 m2)
              (push (list buf m1 m2) l)))
          (forward-line)))
      (revert-buffer)
      (save-excursion
        (Buffer-menu-beginning)
        (while (not (eobp))
          (when (setq buf (assq (get-text-property (+ (point) Buffer-menu-buffer-column) 'buffer) l))
            (setq m1 (cadr buf)
                  m2 (cadr (cdr buf)))
            (when m1
              (delete-char 1)
              (insert m1)
              (backward-char 1))
            (when m2
              (forward-char 2)
              (delete-char 1)
              (insert m2)))
          (forward-line))))))

(unless (featurep 'buff-menu+)
  (defun Buffer-menu-buffer+size (name size &optional name-props size-props)
    (if (> (+ (string-width name) (string-width size) 2)
           Buffer-menu-buffer+size-width)
        (setq name
              (let ((tail
                     (if (string-match "<[0-9]+>$" name)
                         (match-string 0 name)
                       "")))
                (concat (truncate-string-to-width
                         name
                         (- Buffer-menu-buffer+size-width
                            (max (string-width size) 3)
                            (string-width tail)
                            2))
                        Buffer-menu-short-ellipsis
                        tail)))
      ;; Don't put properties on (buffer-name).
      (setq name (copy-sequence name)))
    (add-text-properties 0 (length name) name-props name)
    (add-text-properties 0 (length size) size-props size)
    (let ((name+space-width (- Buffer-menu-buffer+size-width
                               (string-width size))))
      (concat name
              (propertize (make-string (- name+space-width (string-width name))
                                       ?\s)
                          'display `(space :align-to ,(+ Buffer-menu-buffer-column name+space-width)))
              size))))

(unless (featurep 'buff-menu+)
  (defun Buffer-menu-revert-function (ignore1 ignore2)
    (or (eq buffer-undo-list t)
        (setq buffer-undo-list nil))
    ;; We can not use save-excursion here.  The buffer gets erased.
    (let ((ocol (current-column))
          (oline (progn (move-to-column Buffer-menu-buffer-column)
                        (get-text-property (point) 'buffer)))
          (prop (point-min))
          ;; do not make undo records for the reversion.
          (buffer-undo-list t))
      (with-current-buffer (window-buffer)
        (list-buffers-noselect Buffer-menu-files-only))
      (while (setq prop (next-single-property-change prop 'buffer))
        (when (eq (get-text-property prop 'buffer) oline)
          (goto-char prop)
          (move-to-column ocol)))
      (when (eobp)
        (goto-char (point-min))
        (unless Buffer-menu-use-header-line
          (forward-line 2))))))

;;; ---------------------------------------------------------------------
;;;  Electric Buffer List Accomodation
;;; ---------------------------------------------------------------------

;; Make sure we don't interfere with electric-buffer-list.  Dynamic scoping
;; to the rescue.
(defadvice electric-buffer-list (around wg-bufs)
  (let ((wg-bufs-mode nil)
        (Buffer-menu-buffer-column 4))
    ad-do-it))

(provide 'wg-bufs)

;;; wg-bufs.el ends here

