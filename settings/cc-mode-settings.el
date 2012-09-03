;;
;; C/C++
;;
(require 'modal)

(defun my-c-leading-comma-p ()
  (save-excursion
    (beginning-of-line)
    (c-forward-token-2 0 nil (c-point 'eol))
    (eq (char-after) ?,)))

(defun my-c-comma-unindent (langelem)
  "Unindent for leading commas"
  (if (my-c-leading-comma-p) '/))

(defun my-c-comma-indent (langelem)
  "Indent for leading commas"
  (if (my-c-leading-comma-p) '*))

(defun my-cleanup-pp-output ()
  "Clean up preprocessor output so that it's at least semi-readable"
  (interactive)

  (let ((selection (my-selection))
        (start (make-marker))
        (end (make-marker))
        )
    (set-marker start (car selection))
    (set-marker end (cdr selection))
    
    (c++-mode)
    ;; CR before function declaration id
    (replace-regexp "\\([a-zA-Z0-9_]\\) +\\([a-zA-Z_][a-zA-Z0-9_]*(\\)" "\\1\n\\2" nil start end)
    (replace-regexp "\\(\\<return\\>\\|\\<new\\>\\)\n" "\\1 " nil start end)

    ;; CR after template parameter list
    (replace-regexp "\\<template\\> *<\\([^<>]+\\)>" "template <\\1>\n" nil start end)

    (replace-regexp " *\\(\\s.\\|[()]\\) *" "\\1" nil start end)
    (replace-regexp " +" " " nil start end)
    
    (replace-regexp "\\([{}];*\\)" "\\1\n" nil start end)  ;
    (replace-regexp "\\([^ ].*\\)\\([{}]\\)" "\\1\n\\2" nil start end)
    
    (replace-regexp ";\\(.\\)" ";\n\\1" nil start end)
    
    (replace-regexp "\\([(]+\\)\\([(]\\)" "\\1\n\\2" nil start end)
    (replace-regexp ">\\(\\<struct\\>\\|\\<class\\>\\)" ">\n\\1" nil start end)
    (indent-region start end nil)
  ))

(defun my-empty-braces ()
  "insert {}"
  (interactive "*")
  (insert-string "{}")
  (indent-according-to-mode)
  )

(defun my-electric-braces ()
  "Insert a pair of braces surrounding a blank line, indenting each according to the mode"
  (interactive "*")
  (let ((bolp
         (save-excursion (skip-chars-backward " \t")
                         (equal (current-column) 0))))
    (insert-string "{}")
    (if bolp
        (eval (list indent-line-function)))
    )
    (backward-char)
    (newline-and-indent)
    (previous-line 1)
    (end-of-line)
    (newline-and-indent))

(setq my-initials "dwa")

(defun boost-copyright ()
  "Return the appropriate boost copyright for the current user and year"
  (concat "Copyright " (user-full-name) " " (number-to-string (nth 5 (decode-time)))
          ". Distributed under the Boost\n\
Software License, Version 1.0. (See accompanying\n\
file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)"))
      
(defun fluid-copyright ()
  "Return the appropriate FluidObjects copyright for the current user and year"
  (concat "Copyright FluidObjects Software " (number-to-string (nth 5 (decode-time)))
          ". All rights reserved."))

(defun my-split-filename (filename)
  "split a FILENAME string into a (basename . extension) pair"
  (let* ((fname filename)

         (prefix
          (if (string-match "\\(.*\\)\\(\\..*\\)" fname)
              (substring fname 0 (match-end 1))
            fname))
         (extension
          (if (match-beginning 2)
              (substring fname (+ 1 (match-beginning 2)))
            nil)))
    (cons prefix extension)
  ))

(defun my-split-current-filename ()
  (my-split-filename (file-name-nondirectory (buffer-file-name))))

(defun my-include-guard (path-elts)
  "Compute the appropriate #include guard based on the current buffer's name and the given path elements"
  (let* ((split-name (my-split-current-filename))
         (time (decode-time))
         (prefix (car split-name))
         (ext (cdr split-name))
         (extension (if ext (concat "_" ext) ""))
         )
    (upcase
     (concat
      (eval (cons 'concat (mapcar (lambda (s) (concat s "_")) path-elts))) 
      prefix "_" my-initials
      (number-to-string (nth 5 time))
      (number-to-string (nth 4 time))
      (number-to-string (nth 3 time)) extension))))
                  
(defun my-split-path (path)
  (let ((result nil) (elt nil))
    (while (and path
                (not (equal (setq elt (file-name-nondirectory path)) "")))
      (setq result (cons elt result))
      (setq path (file-name-directory path))
      (setq path (and path (directory-file-name path))))
    result))

(defun my-path-elts ()
  (subseq (my-split-path (buffer-file-name)) 0 -1))

(defcustom my-namespace-roots
      '(("boost". boost-copyright) ("fluid" . fluid-copyright))
      "An alist of root directory names and associated copyright
      functions from which to deduce C++ namespace names."
      ':type 'alist )

(defun my-prepare-source ()
  (let* ((all-path-elts (my-path-elts))
    
         ;; prune off the head of path-elts up to the last occurrence
         ;; of boost, if any otherwise, path-elts will be nil

         ;; this is the index of the namespace root in the path
         (index (position-if
                   (lambda (x)
                     (find-if
                      (lambda (y) (equal (car y) x))
                      my-namespace-roots))
                   all-path-elts :from-end 0))

         ;; the name of the root element
         (root (and index (nth index all-path-elts)))

         ;; the path elements to use for the namespace
         (top-path-elts (and index (subseq all-path-elts index)))

         (path-elts
          (if (and top-path-elts
                   (equal root "boost")
                   (equal "libs" (cadr top-path-elts))
                   (equal "src" (cadddr top-path-elts)))
              (append (list root (caddr top-path-elts)) (cddddr top-path-elts))
            top-path-elts))
           
         (copyright-function
          (and index
               (cdr (find-if (lambda (y) (equal (car y) root)) my-namespace-roots))))
           
         (copyright
          (and index
               (eval (list copyright-function))))
                 
         )
    (cons path-elts copyright)))

;;;###autoload
(defun my-begin-cc-header ()
  "Begin a C/C++ header with include guards and a copyright."
  (interactive)
  (let* (
         (source-prep (my-prepare-source))
         (path-elts (car source-prep))
         (guard (my-include-guard path-elts))
         (copyright (cdr source-prep)))

    (beginning-of-buffer)
    (if copyright
        (my-copyright copyright)
      (my-copyright))
      
    (insert-string (concat "#ifndef " guard "\n"
                   "# define " guard "\n"))

    (let ((final nil) ;; final position
          (nsfini (if path-elts "\n" "")))
      
      ;; opening namespace stuff
      (insert-string nsfini)
      (mapc (lambda (n) (insert-string (concat "namespace " n " { ")))
            path-elts)
      (insert-string nsfini)
        
      (setq final (point))
      (newline)
      
      (end-of-buffer)
      ;; make sure the next stuff goes on its own line
      (if (not (equal (current-column) 0))
          (newline))
      
      ;; closing namespace stuff
      (mapc (lambda (n) (insert-string "}")) path-elts)
      (reduce (lambda (prefix n)
                (insert-string (concat prefix n)) "::")
              path-elts
              :initial-value " // namespace ")
      (insert-string nsfini)
      (insert-string nsfini)
      (insert-string (concat "#endif // " guard))
      (goto-char final))
    )
  )


;;;###autoload
(defun my-begin-cc-source ()
  "Begin a C/C++ source file"
  (interactive)
  (let* ((source-prep (my-prepare-source))
         (path-elts (car source-prep))
         (copyright (cdr source-prep))
         (basename (car (my-split-current-filename)))
         )

    
    (beginning-of-buffer)
    (if copyright
        (my-copyright copyright)
      (my-copyright))
    
    (let ((final nil) ;; final position
          (nsfini (if path-elts "\n" "")))
      
      ;; opening namespace stuff
      (insert-string nsfini)
      (if path-elts
          (progn
            (insert-string "#include \"")
            (mapc (lambda (n) (insert-string n "/"))
                  path-elts)
            (insert-string (concat (downcase basename) ".hpp\"\n\n"))))
      
      (mapc (lambda (n) (insert-string (concat "namespace " n " { ")))
            path-elts)
      
      (insert-string nsfini)
        
      (setq final (point))
      (newline)
      
      (end-of-buffer)
      ;; make sure the next stuff goes on its own line
      (if (not (equal (current-column) 0))
          (newline))
      
      ;; closing namespace stuff
      (mapc (lambda (n) (insert-string "}")) path-elts)
      (reduce (lambda (prefix n)
                (insert-string (concat prefix n)) "::")
              path-elts
              :initial-value " // namespace ")
      (insert-string nsfini)
      (goto-char final)
      )
    )
  )

(defcustom my-default-copyright
      'boost-copyright
      "A symbol naming a function which generates the default copyright message"
      ':type 'symbol
      )

(defun my-at-preprocessor-directive-p ()
  "return non-nil if point is sitting at the beginning of a preprocessor directive name"
  (and
   (save-excursion
     (re-search-backward "^\\([ \t]*\\)#\\([ \t]*\\)" (line-beginning-position) t))
   (>= (point) (match-beginning 2))
   (<= (point) (match-end 2))
    ))

(defun my-preprocessor-indentation ()
  (save-excursion
    (beginning-of-line)
    (re-search-backward "^[ \t]*#[ \t]*" nil t)
    (goto-char (match-end 0))
    (+ (current-column)
       (if (looking-at "\\(if\\)\\|\\(el\\)") 1 0))))
  
(defun my-electric-pound-< ()
  (interactive)
  (my-maybe-insert-incude "<" ">"))

(defun my-electric-pound-quote ()
  (interactive)
  (my-maybe-insert-incude "\"" "\""))

(defun my-maybe-insert-incude (open close)
  (if (my-at-preprocessor-directive-p)
      (progn
        (move-to-column (my-preprocessor-indentation) t)
        (insert-string (concat "include " open))
        (save-excursion
          (insert-string close)))
    (insert-string open)))

(defun my-electric-pound ()
  (interactive)
  (insert-string "#")
  (if (my-at-preprocessor-directive-p)
      (progn
        (delete-region (match-beginning 1) (match-end 1))
        (move-to-column (my-preprocessor-indentation) t))))

(defun my-electric-pound-e ()
  (interactive)
  
  (if (my-at-preprocessor-directive-p)
      (progn
        (move-to-column (- (my-preprocessor-indentation) 1))))
  (insert-string "e"))
        
(defun my-c-namespace-indent (langelem)
  "Used with c-set-offset, indents namespace scope elements 2 spaces
from the namespace declaration iff the open brace sits on a line by itself."
  (save-excursion
    (if (progn (goto-char (cdr langelem))
               (setq column (current-column))
               (end-of-line)
               (while (and (search-backward "{" nil t)
                           (assoc 'incomment (c-guess-basic-syntax))))
               (skip-chars-backward " \t")
               (bolp))
        2)))

(defun my-c-backward-template-prelude ()
  "Back up over expressions that end with a template argument list.

Examples include:

        typename foo<bar>::baz::mumble

        foo(bar, baz).template bing
"
  (while
      (save-excursion
        ;; Inspect the previous token or balanced pair to
        ;; see whether to skip backwards over it
        (c-backward-syntactic-ws)
        (or
         ;; is it the end of a nested template argument list?
         (and
          (eq (char-before) ?>)
          (c-backward-token-2 1 t) ;; skips over balanced "<>" pairs
          (eq (char-after) ?<))
                   
         (and
          (c-backward-token-2 1 t)
          (looking-at "[A-Za-z_\\[(.]\\|::\\|->"))))
              
    (c-backward-token-2 1 t)))
  
(defun my-lineup-first-template-args (langelem)
  "Align lines beginning with the first template argument.

To allow this function to be used in a list expression, nil is
returned if we don't appear to be in a template argument list.

Works with: template-args-cont."
  (let ((leading-comma (my-c-leading-comma-p)))
    (save-excursion
      (c-with-syntax-table c++-template-syntax-table
        (beginning-of-line)
        (backward-up-list 1)
        (if (eq (char-after) ?<)
            
            (progn
              (my-c-backward-template-prelude)
              
              (vector
               (+ (current-column)
                  (if leading-comma (/ c-basic-offset 2) c-basic-offset)
                  ))

              ))))))


(defun my-lineup-more-template-args (langelem)
  "Line up template argument lines under the first argument,
adjusting for leading commas. To allow this function to be used in
a list expression, nil is returned if there's no template
argument on the first line.

Works with: template-args-cont."
  (let ((result (c-lineup-template-args langelem)))
    (if (not (eq result nil))
        (if (my-c-leading-comma-p)
            (vector (- (aref result 0) (/ c-basic-offset 2)))
          result))))

(defun my-lineup-template-close (langelem)
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (beginning-of-line)
      (c-forward-syntactic-ws (c-point 'eol))
      (if (and
           (eq (char-after) ?>)
           (progn
             (forward-char)
             (c-backward-token-2 1 t)
             (eq (char-after) ?<)))
          (progn
            (my-c-backward-template-prelude)
            (vector (current-column)))))))

(defun my-c-electric-comma (arg)
  "Amend the regular comma insertion by possibly appending a
  space."
  (interactive "*P") ; Require a writable buffer/take a prefix arg in raw form

  ;; Do the regular action.  Perhaps we should be using defadvice here?
  (c-electric-semi&comma arg)

  ;; Insert the space if this comma is the first token on the line, or
  ;; if there are preceding commas followed by a space.
  (and (eq (char-before) ?,)
       (save-excursion
         (backward-char)
         (skip-syntax-backward " ")
         (bolp)
         )
       (insert-string " "))
  )
         
(defun my-c-electric-gt (arg)
  "Insert a greater-than character.
The line will be re-indented if the buffer is in C++ mode.
Exceptions are when a numeric argument is supplied, point is inside a
literal, or `c-syntactic-indentation' is nil, in which case the line
will not be re-indented."
  (interactive "*P")
  (let ((indentp (and c-syntactic-indentation
		      (not arg)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(indent-according-to-mode))))

(defun my-c-namespace-open-indent (langelem)
  "Used with c-set-offset, indents namespace opening braces to the
same indentation as the line on which the namespace declaration
starts."
  (save-excursion
    (goto-char (cdr langelem))
    (let ((column (current-column)))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (- (current-column) column))))

(defun my-c-defun-block-intro-indent (langelem)
  "Used with c-set-offset, makes sure defun-block-intro doesn't
  cause indentation when we're just inside a namespace."
  (if (assoc 'innamespace (c-guess-basic-syntax))
      0
    '+))

(defun my-c-tab ()
  (interactive "*")
  (delete-region (car (my-selection)) (cdr (my-selection)))
  (c-indent-command)
  )
  
(defun my-c-mode-hook ()
  (setq c-default-style "bsd"
        c-backspace-function 'backward-delete-char
        c-basic-offset 4
        c-tab-always-indent t)

  ;; Add 2 spaces of indentation when the open brace is on a line by itself
  (c-set-offset 'innamespace 'my-c-namespace-indent)
  
  ;; indent solo opening braces to the same indentation as the line on
  ;; which the namespace starts
  (c-set-offset 'namespace-open 'my-c-namespace-open-indent)

  ;; I don't know why this is required, but recent cc-modes seem to
  ;; need it to get my namespace indenting right.
  (c-set-offset 'defun-block-intro 'my-c-defun-block-intro-indent)
  
  ;; indent access labels public/private/protected by 1 space, as in 'M'. I
  ;; kinda like that.
  (c-set-offset 'access-label -3)


  ;;
  ;;fixup template indentation
  ;;
  (c-set-offset 'template-args-cont
                (quote
                 (my-lineup-more-template-args
                  my-lineup-template-close
                  my-lineup-first-template-args
                  +)))
  
  (set-variable 'c-backslash-max-column 200)
  
  (require 'code-settings)
  (my-code-mode-hook)

;  (local-set-key [tab] 'my-c-tab)
  (local-set-key [?\M-{] 'my-electric-braces)
  (local-set-key [(control ?{)] 'my-empty-braces)
  (local-set-key [(meta \`)] 'my-cpp-toggle-src-hdr)
  (local-set-key [?#] 'my-electric-pound)
  (local-set-key [?<] 'my-electric-pound-<)
  (local-set-key [?>] 'my-c-electric-gt)
  (local-set-key [?\"] 'my-electric-pound-quote)
  (local-set-key [?e] 'my-electric-pound-e)
  (local-set-key [?,] 'my-c-electric-comma)
  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces nil)
)

(add-hook 'idl-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'java-mode-hook 'my-c-mode-hook)

;; Since pretty much all my .h files are actually C++ headers, use c++-mode instead of
;; c-mode for these files.
(setq auto-mode-alist
      (cons '("\\.h$" . c++-mode) auto-mode-alist))
