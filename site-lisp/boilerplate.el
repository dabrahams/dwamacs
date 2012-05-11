(defun boost-copyright ()
  "Return the appropriate boost copyright for the current user and year"
  (concat "Copyright " (user-full-name) " " (number-to-string (nth 5 (decode-time)))
          ". Distributed under the Boost\n\
Software License, Version 1.0. (See accompanying\n\
file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)"))
      
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

(defun my-copyright (&optional copyright)
  "Insert a commented COPYRIGHT string. If COPYRIGHT
is not supplied, the boost copyright is used by default"
  (interactive)
  (let ((copy-start (point)))
      
    (insert-string (concat (or copyright
                       (or (and (my-path-elts) (boost-copyright))
                           (eval (list my-default-copyright))))
                   "\n"))
      
    (comment-region copy-start (point))))

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

(defun my-begin-header ()
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


(defun my-begin-source ()
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

(defcustom my-buffer-initialization-alist
      '(
        ("\\.[ih]\\(pp\\|xx\\)?$" . my-begin-header)
        ("\\.c\\(pp\\|xx\\)$" . my-begin-source)
        ("\\.\\(jam\\|\\html?\\|sh\\|py\\|rst\\|xml\\)$" . my-copyright)
        )
      "A list of pairs (PATTERN . FUNCTION) describing how to initialize an empty buffer whose
file name matches PATTERN."
      ':type 'alist
      )

(defcustom my-default-copyright
      'boost-copyright
      "A symbol naming a function which generates the default copyright message"
      ':type 'symbol
      )

(defadvice find-file (after my-gud-translate-cygwin-paths activate)
  ;; if the file doesn't exist yet and is empty
  (if (and (equal (buffer-size) 0)
           (not (file-exists-p (buffer-file-name))))

      ;; try to find an initialization function
      (let ((initializer
             (find-if
              (lambda (pair) (string-match (car pair) (buffer-file-name)))
              my-buffer-initialization-alist)))

        ;; if found, call it
        (if initializer
            (progn (eval (list (cdr initializer)))
                   (set-buffer-modified-p nil)))
      )))

