(when nil
(require 'find-func)
(let ((cedet-lisp (file-name-directory (find-library-name "cedet"))))
  (add-to-list 'load-path (expand-file-name "../../contrib" cedet-lisp))
;  (add-to-list 'load-path (expand-file-name "srecode" cedet-lisp))
;  (add-to-list 'load-path (expand-file-name "semantic" cedet-lisp))
)

;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
;;(add-to-list 'semantic-default-submodes ')

(require 'semantic/bovine/el)
(require 'semantic/canned-configs)
(semantic-load-enable-gaudy-code-helpers)
;; dwa
; (semantic-load-enable-excessive-code-helpers)

;; Activate semantic
(semantic-mode 1)

(setq semantic-clang-binary "/opt/local/bin/clang")
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/clang)
(require 'semantic/ia)
(require 'semantic/decorate/include)
(require 'semantic/lex-spp)
(require 'eassist)
(require 'auto-complete)


;; customisation of modes
(defun alexott/cedet-hook ()
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

  (add-to-list 'ac-sources 'ac-source-semantic)
  )
;; (add-hook 'semantic-init-hooks 'alexott/cedet-hook)
(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
 ;; (local-set-key "." 'semantic-complete-self-insert)
 ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)

;;  (add-to-list 'ac-sources 'ac-source-etags)
  (add-to-list 'ac-sources 'ac-source-gtags)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

(require 'cedet-global)
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

(semanticdb-enable-cscope-databases :noerror)

(ignore-errors 
  (when (cedet-ectag-version-check t)
    (semantic-load-enable-primary-ectags-support)))

;; SRecode
(global-srecode-minor-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

;;; Correct the handling of generic cmake projects in  upstream EDE
;;;
;;; CMAKE
;; (defclass ede-generic-cmake-project (ede-generic-project)
;;   ((buildfile :initform "CMakeLists.txt")
;;    )
;;   "Generic Project for cmake.")

;; (defmethod ede-generic-setup-configuration ((proj ede-generic-cmake-project) config)
;;   "Setup a configuration for CMake."
;;   (oset config build-command "cmake")
;;   (oset config debug-command "gdb ")
;;   )

;; (ede-generic-new-autoloader "generic-cmake" "CMake"
;;                             "CMakeLists.txt" 'ede-generic-cmake-project)
;;;


(defun recur-list-files (dir re)
  "Returns list of files in directory matching to given regex"
  (when (file-accessible-directory-p dir)
    (let ((files (directory-files dir t))
          matched)
      (dolist (file files matched)
        (let ((fname (file-name-nondirectory file)))
          (cond
           ((or (string= fname ".")
                (string= fname "..")) nil)
           ((and (file-regular-p file)
                 (string-match re fname))
            (setq matched (cons file matched)))
           ((file-directory-p file)
            (let ((tfiles (recur-list-files file re)))
              (when tfiles (setq matched (append matched tfiles)))))))))))

(defun c++-setup-boost (boost-root)
  (when (file-accessible-directory-p boost-root)
    (let ((cfiles (recur-list-files boost-root "\\(config\\|user\\)\\.hpp")))
      (dolist (file cfiles)
        (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))



;; my functions for EDE
(defun alexott/ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

;; setup compile package
(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

(defun alexott/compile ()
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive)
  (save-some-buffers t)
  (let* ((r (alexott/ede-get-local-var
             (or (buffer-file-name (current-buffer)) default-directory)
             'compile-command))
         (cmd (if (functionp r) (funcall r) r)))
    (set (make-local-variable 'compile-command) (or cmd compile-command))
    (compile compile-command)))

(global-set-key [f9] 'alexott/compile)

;;
(defun alexott/gen-std-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj)))
    (concat "cd " root-dir "; make -j2")))

;;
(defun alexott/gen-cmake-debug-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (subdir "")
         )
    (when (string-match root-dir current-dir)
      (setf subdir (substring current-dir (match-end 0))))
    (concat "cd " root-dir "Debug/" "; make -j3")))

(defun alexott/gen-cmake-debug/release-compile-string ()
  "Generates compile string for compiling CMake project in debug & release modes"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (subdir "")
         )
    (when (string-match root-dir current-dir)
      (setf subdir (substring current-dir (match-end 0))))
    (concat "cd " root-dir "Debug/ && make -j3 && cd " root-dir "Release/ && make -j3" )))

;; Projects

(when (file-exists-p "~/src/LLVM/llvm/CMakeLists.txt")
  (setq llvm-project
	(ede-cpp-root-project "llvm"
			      :file "~/src/LLVM/llvm/CMakeLists.txt"
                              :include-path 
                              '(
                                "/include"
                                "/tools/clang/include"
                               )
			      :local-variables '((compile-command . 'alexott/gen-cmake-debug-compile-string)
						)
			      )))


(when (file-exists-p "~/src/corp/cree/CMakeLists.txt")
  (setq cree-project
	(ede-cpp-root-project "cree"
			      :file "~/src/corp/cree/CMakeLists.txt"
                              :include-path 
                              '("/src"
                                "/include"
                                "/lib"
                                "/lib_src"
                                "/ext/llvm/include"
                                "/ext/llvm/tools/clang/include"
                                "."
                               )
                              :spp-table '(("LLVM_DELETED_FUNCTION".""))
			      :local-variables '((compile-command . "cd ~/Products/cree && PATH=\"$HOME/Products/LLVM/cree/bin:$PATH\" cmake ~/src/corp/cree -G Ninja && ninja")
						)
			      ))
)

(when (file-exists-p "~/src/llvm/abi/CMakeLists.txt")
  (setq abi-project
	(ede-cpp-root-project "abi"
			      :file "~/src/llvm/abi/CMakeLists.txt"
                              :include-path 
                              '("/include"
                                "/tools/clang/include"
                                "."
                               )
                              :spp-table '(("LLVM_DELETED_FUNCTION".""))
			      :local-variables '((compile-command . "cd ~/Products/abi && cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang ~/src/LLVM/abi -G Ninja && ninja")
						)
			      ))
)


;; cpp-tests project definition
(when (file-exists-p "~/projects/lang-exp/cpp/CMakeLists.txt")
  (setq cpp-tests-project
	(ede-cpp-root-project "cpp-tests"
			      :file "~/projects/lang-exp/cpp/CMakeLists.txt"
			      :system-include-path '("/home/ott/exp/include"
						     boost-base-directory)
			      :local-variables (list
						(cons 'compile-command 'alexott/gen-cmake-debug-compile-string)
						)
			      )))

(when (file-exists-p "~/projects/squid-gsb/README")
  (setq squid-gsb-project
	(ede-cpp-root-project "squid-gsb"
			      :file "~/projects/squid-gsb/README"
			      :system-include-path '("/home/ott/exp/include"
						     boost-base-directory)
			      :local-variables (list
						(cons 'compile-command 'alexott/gen-cmake-debug-compile-string)
						)
			      )))

;; (when (file-exists-p "~/work/emacs-head/README")
;;   (setq emacs-project
;; 	(ede-emacs-project "emacs-head"
;; 			   :file "~/work/emacs-head/README")))


;; (setq arabica-project
;;       (ede-cpp-root-project "arabica"
;;                             :file "~/projects/arabica-devel/README"
;;                             :system-include-path '("/home/ott/exp/include"
;;                                                    boost-base-directory)
;;                             :local-variables (list
;;                                               (cons 'compile-command 'alexott/gen-std-compile-string)
;;                                               )
;;                             ))


;;; emacs-rc-cedet.el ends here
(provide 'semantic-settings)
)
