;; -*- Mode: Emacs-Lisp -*-
;;
;; Mule $B>e$G(B Namazu $B$rMxMQ$7$?8!:w$r9T$&$?$a$N(B elisp $B$G$9!#(B
;;
;;  $Id: namazu.el,v 1.13 2000/02/24 06:48:33 kose Exp $

(defconst namazu-version "namazu.el 1.0.3")

;; Namazu $B$K$h$k8!:w7k2L$,;X$9%I%-%e%a%s%H(B($BN`(B)$B$,(B
;; $B%m!<%+%k%G%#%9%/>e$K$"$k>l9g$K$O$=$l$rD>@\;2>H$7!"(B
;; $B%M%C%H%o!<%/>e$K$"$k>l9g$K$O(B brouse-url $B$N5!G=$rMQ$$$F(B
;; $B$=$l$r;2>H$9$k$3$H$,$G$-$^$9!#(B
;;
;; $B%m!<%+%k%G%#%9%/>e$N%I%-%e%a%s%H(B($BN`(B)$B$,05=L$7$F$"$k>l9g!"(B
;; jka-compr $B$N5!G=$rMQ$$$F$=$l$rE83+$7$h$&$H$7$^$9!#(B
;;
;; Namazu $B%b!<%I$G$O%m!<%+%k%G%#%9%/$K$J$$%U%!%$%k$X$N(B
;; $B%"%/%;%9$K$D$$$F$O(B brouse-url $B$K0lG$$7$F$$$^$9!#(B
;; $B$=$&$$$C$?$o$1$G$9$N$G(B browse-url-browser-function $B$K(B
;; $BE,@Z$J@_Dj$r$7$F$*$+$J$$$H!"30It$K$"$k%j%=!<%9$r(B
;; $B;2>H$9$k$3$H$O$G$-$^$;$s!#$?$H$($P$3$s$J@_Dj$r$7$^$9(B:
;;
;;  (autoload 'namazu "namazu" nil t)
;;  (setq browse-url-browser-function 'browse-url-netscape)
;;
;; font-lock $B$d(B hilit19 $B$,%m!<%I$7$F$"$l$P(B
;; $B8!:w7k2L$O(B($B$$$/$i$+(B)$B2Z$d$+$KI=<($5$l$k$G$7$g$&!#(B
;;
;; $BMQ0U$9$kI,MW$,$"$k$+$bCN$l$J$$$b$N(B:
;; $BA0=R$NDL$j(B browse-url $B$,I,MW$G$9$,!"(B19.28 $B%Y!<%9$N(B Mule $B$K$O(B
;; $B$3$l$,4^$^$l$F$$$J$$$h$&$G$9!#F1:-$N(B "browse-url-for-emacs-19.28.el"
;; $B$r$*;H$$2<$5$$!#(B
;;
;; $B8!:wJ}K!(B:
;; $B>e5-$N@_Dj$r=*$($?$i(B M-x namazu $B$H%?%$%W$7$F$/$@$5$$!#(B
;; $B$9$k$H8!:w$N%-!<(B($B>r7o<0(B)$B$rJ9$$$F$-$^$9$N$G(B namazu $B$H$+(B
;; ruby & perl $B$H$$$C$?8!:w%-!<$rF~NO$7$^$9!#(B
;; $BF~NO$r=*$($k$H(B Namazu $B$,5/F0$5$l!"(B
;; $B8!:w7k2L$rI=<($9$k%P%C%U%!$,:n$i$l$^$9!#(B
;; $B$3$N%P%C%U%!Fb$G$O0J2<$N%-!<A`:n$,Dj5A$5$l$F$$$^$9!#(B
;;
;;             $BA0%Z!<%8(B    $BA09`(B     $B<!9`(B    $B<!%Z!<%8(B
;;   $BI8=`(B(1)      P         p        n         N
;;   $BI8=`(B(2)              [BkSp]  [Space]
;;   $BI8=`(B(3)              M-[Tab]  [Tab]
;;   vi $BIw(B        h         k        j         l
;;   $B%+!<%=%k(B   [left]     [up]    [down]   [right]
;;
;;   $B%Z!<%8$N@hF,$X(B        <
;;   $B%Z!<%8$NKvHx$X(B        >
;;   $B%I%-%e%a%s%H$N;2>H(B    g $B$^$?$O(B [Enter]
;;   $B>r7o$rDI2C$7$F:F8!:w(B  r
;;   $B8!:w7k2L$N>C5n(B        q
;;   namazu $B=*N;(B           Q
;;   $B%X%k%WI=<((B            ?
;;
;; $B$^$?!"(Bmouse $B$N$^$s$J$+$N%\%?%s$r2!$9$H!"2!$7$?0LCV$K$h$C$F!"(B
;; "$B%I%-%e%a%s%H$N;2>H(B"$B!"(B"$BA0%Z!<%8(B"$B!"(B "$B<!%Z!<%8(B" $B$N$I$l$+$NF0:n$r9T$J(B
;; $B$$$^$9!#(B
;;
;; $B%G%U%)%k%H0J30$N%$%s%G%C%/%9(B(NMZ.*)$B$r;HMQ$7$?$$>l9g$O!"(B
;; C-u M-x namazu $B$H%?%$%W$9$k$3$H$G%$%s%G%C%/%9$NCV$$$F$"$k%G%#%l%/%H(B
;; $B%j$r;XDj$9$k$3$H$b=PMh$^$9!#$^$?!"8D!9$N%G%#%l%/%H%j$KBP$7$FJLL>$r(B
;; $BDj5A$9$k$3$H$b2DG=$G$9!#@_DjNc$J$I>\$7$$$3$H$O(B namazu-dir-alist $B$N(B
;; $B@bL@$r;2>H$7$F$/$@$5$$(B :-P$B!#(B
;;
;; $B:G?7HG$NF~<j$K$D$$$F(B:
;; namazu.el $B$N:G?7HG$O(B namazu $B$KIUB0$9$k$b$N$d!"(Bnamazu ML $B$J$I$G(B
;; $BF~<j2DG=$G$9(B.
;;
;; $BG[I[>r7o$J$I(B:
;; $B$3$l$O(B $B$^$D$b$H(B $B$f$-$R$m(B <matz@netlab.co.jp> $B$5$s$,:n@.$7!"(B
;; $BF1;a$H(B $BGO>l(B $BH%(B <baba@kusastro.kyoto-u.ac.jp> $B$5$s!"(B
;; $B9bNS(B $BE/(B <ccsatoru@vega.aichi-u.ac.jp> $B$5$s!"(B
;; Toshiaki Tanaka <toshiaki@ksj1.kansai-sc.toshiba.co.jp>$B$5$s!"(B
;; $B1|@>(B $BF#OB(B <fuji0924@mbox.kyoto-inet.or.jp> $B$5$s$N$46(NO$N2<$G(B
;; $B$d$^$@(B $B$"$-$i(B <akira@linux.or.jp> $B$5$s$,2~B$$r2C$($?$b$N$r!"(B
;; $BEZ20(B $B2mL-(B <tsuchiya@pine.kuee.kyoto-u.ac.jp> $B$5$s!"(B
;; $BKY8}63B@O:(B <kyota@po.ntts.co.jp> $B$5$sC#$N<8S#7cNe$K$h$j(B
;; Namazu Project $B$,2~B$$7$F$$$k$b$N$G$9!#(B
;; $B$$$+$J$k7A$G$NMxMQ!&:FG[I[$K$D$$$F$b8"Mx$NN`$O0l@Z<gD%$7$^$;$s!#(B
;; $B<+M3$K07$C$F$b$i$C$F9=$$$^$;$s!#(B
;;
;; $B$*LsB+(B:
;; $B$3$l$O2?$i$+$NJ]>Z$rH<$&$b$N$G$O$"$j$^$;$s!#(B
;; $BDs6!$5$l$k5!G=$r;H$C$?7k2L!"MxMQ<T$,D>@\E*$"$k$$$O4V@\E*$K(B
;; $BB;32$rHo$C$?$H$7$F$b!"$=$l$O:n<TC#$N4XCN$9$k$H$3$m$G$O$"$j$^$;$s!#(B
;; $B$"$/$^$G(B at your own risk $B$G$4MxMQ2<$5$$!#(B
;;

;; CUSTOM emulation derived from BBDB and APEL.
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))
    (defmacro defface (var value doc &rest args)
      (` (make-face (, var))))
    (defmacro define-widget (&rest args)
      nil)))

(defgroup namazu nil
  "Namazu front-end for Emacs."
  :group 'external)

(defcustom namazu-command "namazu"
  "*Namazu $B$N8!:wMQ%W%m%0%i%`L>$G$9!#(B
$BDL>o$O(B namazu $B$J$I$G$7$g$&$,!"$=$&$G$O$J$$>l9g$d(B
PATH $B$,DL$C$F$$$J$$>l9g$K$OE,Ev$J%W%m%0%i%`L>$r;XDj$7$^$9!#(B"
  :type 'string
  :group 'namazu)

(defcustom namazu-search-num 30
  "*Namazu $B$N8!:w7k2L$r0lEY$KI=<($9$k7o?t$G$9!#(B"
  :type 'integer
  :group 'namazu)

(defcustom namazu-default-dir nil
  "*Namazu $B$,;2>H$9$k%$%s%G%C%/%9$NCV$$$F$"$k%G%#%l%/%H%jL>$G$9!#(B
$BFC$K;XDj$7$J$1$l$P%G%U%)%k%H$N%$%s%G%C%/%9$r;2>H$7$^$9!#(B
$BJ#?t$N%$%s%G%C%/%9$r;XDj$9$k>l9g$K$O$=$l$>$l$r6uGr$G6h@Z$C$F$/$@$5$$!#(B"
  :type '(choice
	  (item :tag "Auto" :value nil)
	  (directory :tag "Default Index"))
  :group 'namazu)

(defcustom namazu-dir-alist nil
  "*$B%$%s%G%C%/%9$,CV$$$F$"$k%G%#%l%/%H%j$K(B
$B%7%s%\%j%C%/$JL>A0$r$D$1$k$?$a$N(B alist $B$G$9!#(B
  '((\"Namazu\" . \"/usr/doc/namazu/index /var/lib/namazu/index\")
    (\"Ruby\" . \"/usr/doc/ruby/namazu\"))
$B$J$I$N$h$&$K@_Dj$7$F$*$/$H!"8D!9$N%$%s%G%C%/%9%U%!%$%k$N$"$k(B
$B%G%#%l%/%H%jL>$r;XDj$9$kBe$o$j$K(B Namazu $B$d(B Ruby $B$H$$$C$?(B
$B$$$o$PJLL>$r;XDj$9$k$3$H$,$G$-$^$9!#(B
$BJ#?t$N%$%s%G%C%/%9$r;XDj$9$k>l9g$K$O$=$l$>$l$r6uGr$G6h@Z$C$F$/$@$5$$!#(B"
  :type '(repeat (cons :format "%v"
		       (string :tag "Alias")
		       (string :tag "Index path")))
  :group 'namazu)

(defcustom namazu-always-query-index-directory nil
  "*nil $B0J30$NCM$r@_Dj$9$k$H!"?tCM0z?t$,$J$$$H$-$K(B
$B%$%s%G%C%/%9%U%!%$%k$r;XDj$G$-!"?tCM0z?t$,$"$k$H$-$K(B
$B%G%U%)%k%H$N%$%s%G%C%/%9$r;2>H$9$k$h$&$K$J$j$^$9!#(B
$B>o$K%$%s%G%C%/%9%U%!%$%k$r;XDj$7$F8!:w$r9T$$$?$$(B
$B>l9g$J$I$KJXMx$+$b$7$l$^$;$s!#(B"
  :type 'boolean
  :group 'namazu)

(defcustom namazu-auto-turn-page nil
  "*nil $B0J30$NCM$r@_Dj$9$k$H!"<+F0E*$K%Z!<%8$a$/$j$r$7$^$9!#(B"
  :type 'boolean
  :group 'namazu)

(defcustom namazu-mode-hook nil
  "*Namazu $B%b!<%I$r:n@.$9$k%?%$%_%s%0$G8F$P$l$k(B hook $B$G$9!#(B"
  :type 'hook
  :group 'namazu)

(defcustom namazu-display-hook nil
  "*Namazu $B$N=PNO$rI=<($9$k$H$-$K8F$P$l$k(B hook $B$G$9!#(B"
  :type 'hook
  :group 'namazu)

(defcustom namazu-url-regex "^\\(https?://\\|ftp://\\)"
  "*URL $B$H8+$J$9%U%!%$%kL>$N%Q%?!<%s$r@_Dj$7$^$9!#(B"
  :type 'regexp
  :group 'namazu)

(defcustom namazu-view-function-alist
  '(("[^/]+\\.s?html?" . namazu-browse-url)
    ("/Mail\\|News/.*/[1-9][0-9]*$" . namazu-view-msg)
    ("man/man" . namazu-man)
    ;; ("/usr/local/info/\\|\\.info" . namazu-info) ;; $BL$:n@.(B
    ("." . namazu-view-file))
  "*$B%U%!%$%kL>$N%Q%?!<%s$H$=$l$KBP1~$9$k1\Mw4X?t$r@_Dj$7$^$9!#(B"
      :type '(repeat (cons :format "%v"
			   (regexp :tag "Filename Regexp")
			   (symbol :tag "Function Name")))
      :group 'namazu)

(defcustom namazu-view-other-window nil
  "*If non-nil, make an other window when namazu-view."
  :type 'boolean
  :group 'namazu)

(defcustom namazu-view-other-frame nil
  "*If non-nil, make an other frame when namazu-view."
  :type 'boolean
  :group 'namazu)

(defcustom namazu-msg-visible-field (list "subject" "from" "to" "newsgroups" "date")
  "*Visible header list for namazu-view-msg."
  :type '(repeat (string :tag "Header"))
  :group 'namazu)

(defcustom namazu-msg-highlight-function nil
  "*A function, view-msg highlight method.
e.g.
  namazu-msg-highlight-mew -- use Mew functions(require Mew 1.94 or later)."
  :type '(radio (function-item :tag "use Mew functions"
			       :format "%t\n"
			       namazu-msg-highlight-mew)
		(function :tag "Other"))
  :group 'namazu)

(defvar namazu-cs-write
  (if (memq system-type '(OS/2 emx windows-nt))
      (if (> emacs-major-version 19) 'sjis-dos '*sjis*dos)
    (if (> emacs-major-version 19) 'euc-jp '*euc-japan*))
  "*Coding system for namazu process (output).")

(defvar namazu-cs-read
  (if (> emacs-major-version 19) 'undecided '*autoconv*)
  "*Coding system for namazu process (input).")

(defvar namazu-config-file-path
  (list (getenv "NAMAZUCONFPATH")
	(getenv "NAMAZUCONF")		; obsolete?
	"./.namazurc"
	"~/.namazurc"
	"/usr/local/etc/namazu/namazurc"
	"/usr/local/namazu/lib/namazurc") ;obsolete?
  "*Search path for a Namazu configuration file.")

(defvar namazu-argument "-H"
  "*Namazu $B$N8!:wMQ%W%m%0%i%`$r5/F0$9$k:]$K;XDj$9$k0z?t$G$9!#(B")

;;
;; $B$3$3$+$i@h$r$$$8$C$F!"AGE($K$J$C$?$i65$($F$/$@$5$$$M!#(B
;;

(defvar namazu-fill-prefix "\t")
(defvar namazu-header-prefix "   ")
(defvar namazu-index-history '(""))
(defvar namazu-keyword-history '(""))
(defvar namazu-mode-map nil)
(defvar namazu-minibuffer-map nil)
(defvar namazu-minibuffer-field-map nil)
(defvar namazu-buffer "*namazu*")
(defvar namazu-last-dir nil
  "$B8=:_$N8!:w$G;2>H$7$F$$$k%$%s%G%C%/%9$N:_=h(B")
(defvar namazu-current-page 0
  "$B1\MwCf$N8!:w7k2L$N%Z!<%8HV9f(B")
(defvar namazu-max-page 0
  "$B8=:_$N8!:w7k2L$N:GBg%Z!<%8HV9f(B")
(defvar namazu-output-title-pattern
  "^\\([0-9]+\\.\\) \\(.*\\) \\(([^)]*)\\)$"
  "$B8!:w7k2L$NCf$N%I%-%e%a%s%H$N%?%$%H%k$r<($99T$N%Q%?!<%s(B")
(defvar namazu-output-header-pattern
  (format "^%s\\([^:]+:.*\\)$" namazu-header-prefix)
  "$B8!:w7k2L$NCf$N(B From$B!"(BDate $B%X%C%@$r<($9%Q%?!<%s(B")
(defvar namazu-output-url-pattern
  "^\\(\\(~?/\\|[a-z]+:\\)[^ ]+\\) \\(.*\\)$"
  "$B8!:w7k2L$NCf$N%I%-%e%a%s%H$N:_=h(B(URL)$B$r<($99T$N%Q%?!<%s(B")
(defvar namazu-output-current-list-pattern
  "^[^:]+: [0-9]+ - [0-9]+$"
  "$B8!:w7k2L$NCf$N$I$NItJ,$r1\MwCf$+$r<($99T$N%Q%?!<%s(B")
(defvar namazu-output-pages-pattern
  "^[^:]+: \\(\\[[0-9]+\\]\\)*\\[\\([0-9]+\\)\\]$"
  "$B8!:w7k2L$N%Z!<%8?t$r<($99T$N%Q%?!<%s(B")
(defvar namazu-view-vismark nil)

(and (locate-library "browse-url") (require 'browse-url))
(and (locate-library "jka-compr") (require 'jka-compr))
(provide 'namazu)

(defun namazu (&optional page-num namazu-dir key)
  "namazu-command $B$r5/F0$7$F8!:w$r9T$$$^$9!#(B"
  (interactive
   (list
    0
    (if (or (and (not namazu-always-query-index-directory) current-prefix-arg)
	    (and namazu-always-query-index-directory (not current-prefix-arg)))
	(read-from-minibuffer "Namazu index directory: " nil
			      namazu-minibuffer-map nil 'namazu-index-history)
      nil)
    (read-from-minibuffer "Enter Keyword: " nil
			  namazu-minibuffer-field-map nil 'namazu-keyword-history)))
  (let ((buffer (get-buffer-create namazu-buffer))
	(dir (or namazu-dir
		 (progn
		   (or namazu-default-dir
		       (setq namazu-default-dir (namazu-get-default-index-dir)))
		   (expand-file-name namazu-default-dir))))
	(arg-list (if (listp namazu-argument)
		      namazu-argument (list namazu-argument))))
    (setq arg-list (append
		    arg-list
		    (list "-n" (int-to-string namazu-search-num)
			  "-w" (int-to-string (* page-num namazu-search-num))
			  key)))
    (if (and dir (not (string= dir "")) (string-match "[^ \t]" dir))
	(setq arg-list (append arg-list
			       (namazu-split-dir (namazu-expand-dir-alias dir)))))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (message "Namazu running ...")
    (let ((default-process-coding-system (cons namazu-cs-read namazu-cs-write))
	  (process-input-coding-system namazu-cs-read)
	  (process-output-coding-system namazu-cs-write)
	  (coding-system-for-read namazu-cs-read)
	  (coding-system-for-write namazu-cs-write))
      (apply (function call-process) namazu-command nil t nil arg-list))
    (if (not (and buffer
		  (get-buffer buffer)
		  (buffer-name (get-buffer buffer))))
	(message "Namazu exits with no output")
      (pop-to-buffer buffer)
      (goto-char (point-min))
      (save-excursion
	(namazu-fill)
	(if (re-search-forward namazu-output-pages-pattern nil t)
	    (setq namazu-max-page
		  (+ -1 (string-to-int (buffer-substring
					(match-beginning 2) (match-end 2)))))
	  (setq namazu-max-page 0)))

      ;(goto-char (point-min))
      (setq namazu-current-page page-num)
      (setq namazu-last-dir dir)
      (namazu-mode)
      (setq buffer-read-only t)
      (run-hooks 'namazu-display-hook)
      (message "Namazu running ... done.") )))

(defun namazu-fill ()
  "namazu-command $B$G$N8!:w7k2L$r@07A$7$^$9!#(B"
  (while (re-search-forward "^[0-9]+\. " nil t)
    (beginning-of-line 2)
    (let ((start-point (point)))
      (re-search-forward "^$" nil t)
      (forward-line -1)
      ;; there is URL or file name
      (if (looking-at namazu-output-url-pattern)
	  (forward-line -1))
      ;; there is description
      (if (> (point) start-point)
	  (save-excursion
	    (while (> (point) start-point)
	      (forward-line -1)
	      (insert namazu-header-prefix)
	      (beginning-of-line))
	  ))
      ;; there is description
      (let ((fill-column default-fill-column)
	    (fill-prefix namazu-fill-prefix)
	    (enable-kinsoku nil))
	(insert namazu-fill-prefix)
	(fill-region (point)
		     (save-excursion (forward-line 1) (point))))
      ;; $BM>J,$J6u9T$r$H$C$Q$i$&$?$a$NEXNO(B
      (re-search-forward "^$" nil t)
      (while (looking-at "^$")
	(delete-char 1)
	(forward-line 1))
      )))

(defun namazu-re-search (&optional key)
  "$B8=:_$N8!:w%-!<$rJQ99$7$?>e$G:F8!:w$7$^$9!#(B"
  (interactive
   (list
    (save-excursion
      (read-from-minibuffer "Enter Keyword: "
			    (cons (car namazu-keyword-history) 1)
			    namazu-minibuffer-field-map
			    nil 'namazu-keyword-history))))
  (namazu 0 namazu-last-dir key))

(defun namazu-next-page ()
  "$B<!$N%Z!<%8$N8!:w7k2L$X0\F0$7$^$9!#(B"
  (interactive)
  (if (< namazu-current-page namazu-max-page)
      (namazu (+ 1 namazu-current-page) namazu-last-dir (car namazu-keyword-history))
    t))

(defun namazu-prev-page ()
  "$BA0$N%Z!<%8$N8!:w7k2L$X0\F0$7$^$9!#(B"
  (interactive)
  (if (> namazu-current-page 0)
      (namazu (+ -1 namazu-current-page) namazu-last-dir (car namazu-keyword-history))
    t))

(defun namazu-dir-complete ()
  "$B%G%#%l%/%H%jL>$^$?$O(B namazu-dir-alist $B$+$i$N(B
$BJ8;zNsJd40$r9T$$$^$9!#(B"
  (interactive)
  (let ((input (buffer-substring 1 (point)))
	(alist namazu-dir-alist)
	dir file files compl all sub-input mb)
    (if (string-match "\\(^\\|\\(\\\\\\\\\\)*[^\\\\] \\)\\(\\(\\(\\\\\\\\\\)*\\\\ \\|[^ ]\\)*/\\)?\\([^/]*\\)$" input)
	(progn
	  (setq mb (match-end 1))
	  (save-match-data
	    (setq sub-input
		  (namazu-unescape-dir
		   (substring input mb (match-end 6)))))
	  (save-match-data
	    (setq dir
		  (namazu-unescape-dir
		   (substring input mb (match-beginning 6)))))
	  (setq file (substring input (match-beginning 6) (match-end 6)))
	  ;; HOME $B$+$i$NAjBP%Q%9$N=hM}(B
	  (if (and (string= dir "")
		   (string-match  "^~" file))
	      (progn (setq dir file) (setq file "")))
	  ;; $B%G%#%l%/%H%j$N>l9g$N=hM}(B
	  (setq files (and (file-exists-p dir)
			   (file-directory-p dir)
			   (directory-files dir t "^[^.]")))
	  (while files
	    (if (file-directory-p (car files))
		(setq alist
		      (append alist
			      (list (cons (concat (car files) "/")
					  (car files))))))
	    (setq files (cdr files)))
	  ;; Completion-List $B$N:n@.(B
	  (setq compl (or (try-completion sub-input alist)
			  (try-completion (expand-file-name sub-input) alist)))
	  (setq all (or (all-completions sub-input alist)
			(all-completions (expand-file-name sub-input) alist)))
	  (cond ((stringp compl)
		 (delete-region (+ mb 1) (point-max))
		 (insert (namazu-escape-dir compl))
		 (with-output-to-temp-buffer "*Completions*"
		   (display-completion-list all)))
		(compl
		 nil)
		(t
		 (beep)))
	  )
      (beep))))

(defun namazu-escape-dir (dir)
  "$B%G%#%l%/%H%jCf$N(B \"\\\" $B$H(B \" \" $B$r%(%9%1!<%W$7$^$9!#(B"
  (let ((tmpdir1 dir) (tmpdir2 ""))
    (while (string-match "\\([ \\]\\)" tmpdir1)
      (setq tmpdir2
	    (concat tmpdir2
		    (substring tmpdir1 0 (match-beginning 0))
		    "\\" (substring tmpdir1
				    (match-beginning 1) (match-end 1))))
      (setq tmpdir1 (substring tmpdir1 (match-end 0))))
    (concat tmpdir2 tmpdir1)))

(defun namazu-unescape-dir (dir)
  "$B%G%#%l%/%H%jCf$N(B \"\\\" $B$H(B \" \" $B$r%(%9%1!<%W$7$^$9!#(B"
  (let ((tmpdir1 dir) (tmpdir2 ""))
    (while (string-match "\\\\\\([ \\]\\)" tmpdir1)
      (setq tmpdir2
	    (concat tmpdir2
		    (substring tmpdir1 0 (match-beginning 0))
		    (substring tmpdir1
			       (match-beginning 1) (match-end 1))))
      (setq tmpdir1 (substring tmpdir1 (match-end 0))))
    (concat tmpdir2 tmpdir1)))

(defun namazu-split-dir (dirs)
  "$B%$%s%G%C%/%9%G%#%l%/%H%jJ8;zNs$rJ,3d$7!"(B\"~\" $B$J$I$rE83+$7$^$9!#(B"
  (let ((tmpdir1 dirs) (dir-list (list))
	(nmz-expand-filename (function (lambda (f)
		(expand-file-name (namazu-unescape-dir 
		    (or (cdr (assoc f namazu-dir-alist)) f)))))))
    (while (string-match "\\([^\\\\]\\) " tmpdir1)
      (save-match-data
	(setq dir-list
	      (append dir-list
		      (list (funcall nmz-expand-filename
			      (substring tmpdir1 0 (match-end 1)))))))
      (setq tmpdir1 (substring tmpdir1 (match-end 0))))
    (if dirs
	(append dir-list (list (funcall nmz-expand-filename tmpdir1)))
      dir-list)))

(defun namazu-expand-dir-alias (dir)
  "$B%$%s%G%C%/%9%G%#%l%/%H%jJ8;zNsCf$N%(%$%j%"%9$rE83+$7$^$9!#(B"
  (and dir namazu-dir-alist
       (let ((alist namazu-dir-alist))
	 (while alist
	   (while (string-match
		   (concat "\\(^\\| \\|\t\\)\\("
			   (regexp-quote (car (car alist)))
			   "\\)\\( \\|\t\\|$\\)") dir)
	     (setq dir (concat (substring dir 0 (match-beginning 2))
			       (cdr (car alist))
			       (substring dir (match-beginning 3)))))
	   (setq alist (cdr alist)))))
  dir)

(defun namazu-field-complete ()
  "+to:field $B$NJd40$r$7$^$9!#(B"
  (interactive)
  (goto-char (point-max))
  (let ((p (point))
        (alist (namazu-make-field-completion-alist namazu-last-dir))
        (completion-buffer "*Completions*")
        word start result)
    (save-excursion
      (if (re-search-backward "\\+[^ \t]*" nil t)
	  (progn
	    (setq start (match-beginning 0))
	    (setq word (match-string 0))
	    (setq result (try-completion word alist)))))
    (cond
     ((eq result t)
      (ding))
     ((eq result nil)
      (ding))
     ((string= result word)
      (with-output-to-temp-buffer completion-buffer
        (display-completion-list
         (all-completions word alist))))
     (t
      (delete-region start p)
      (insert result)
      (if (eq t (try-completion result alist))
          ()
        (ding))))))

(defun namazu-make-field-completion-alist (namazu-dirs)
  "make \'+files:\' completion alist."
  (let (dir flist fields fname el
	 (dirs (namazu-split-dir 
		(or namazu-dirs namazu-default-dir
		    (setq namazu-default-dir (namazu-get-default-index-dir))))))
    (while (setq dir (car dirs))
      (if (file-exists-p dir)
	  (setq flist (append (directory-files dir) flist)))
      (setq dirs (cdr dirs)))
    (while (setq fname (car flist))
      (and (string-match "NMZ.field.\\([^.]+\\)\\'" fname)
	   (setq el (list (format "+%s:"
              (substring fname (match-beginning 1) (match-end 1)))))
	   (if (not (member el fields))
	       (setq fields (append (list el) fields))))
      (setq flist (cdr flist)))
    fields))

(defun namazu-search-config-file ()
  "Search namazu-config-file-path for a Namazu configuration file.
Return the abosolute file name of the configuration.  When the file is
not found, return nil "
  (let ((config-file-list namazu-config-file-path) config-file)
    (setq config-file-list (delq nil config-file-list))
    (if (catch 'found
	  (while config-file-list
	    (setq config-file (expand-file-name (car config-file-list)))
	    (and (file-exists-p config-file)
		 (throw 'found t))
	    (setq config-file-list (cdr config-file-list))))
	config-file
      nil)))

(defun namazu-read-config-file (file)
  "Read a namazu configuration file and return an alist of directive
and value(s) pairs.
FILE indicates the absolute file name of the configuration file. FILE
must exists."
  (let* (conf-alist
	 (buffer (get-file-buffer file))
	 (buffer-already-there-p buffer))
    (or buffer-already-there-p
	(setq buffer (find-file-noselect file)))
    (unwind-protect
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (let (directive value1 value2)
	    (while (re-search-forward "\\(^[ \t]*\\(INDEX\\|BASE\\|\
LOGGING\\|LANG\\|SCORING\\)[ \t]+\\([^ \t\n#]+\\)\\)\\|\
\\(^[ \t]*\\(REPLACE\\)[ \t]+\\([^ \t\n#]+\\)[ \t]+\\([^ \t\n#]+\\)\\)" nil t)
	      (cond ((match-string 1)   ; only 1 value
		     (setq directive (match-string 2))
		     (setq value1 (match-string 3))
		     (setq conf-alist
			   (delete (assoc directive conf-alist) conf-alist))
		     (setq conf-alist
			   (cons (cons directive value1) conf-alist)))
		    ((match-string 4)	; 2 values
		     (setq directive (match-string 5))
		     (setq value1 (match-string 6))
		     (setq value2 (match-string 7))
		     (setq conf-alist
			   (delete (assoc directive conf-alist) conf-alist))
		     (setq conf-alist
			   (cons (list directive value1 value2)
				 conf-alist)))))))
      (if (not buffer-already-there-p)
	  (kill-buffer buffer)))
    conf-alist))

(defun namazu-get-default-index-dir ()
  "Get a Namazu default index directory from a Namazu configuration file.
Return \"/usr/local/namazu/index\" if the configuration file is not
found."
  (let (config-file conf-alist cell dir)
    (setq config-file (namazu-search-config-file))
    (if config-file
	(progn
	  (setq conf-alist (namazu-read-config-file config-file))
	  (setq cell (assoc "INDEX" conf-alist))
	  (and cell
	       (setq dir (cdr cell)))
	  dir)
      "/usr/local/namazu/index")))

(defun namazu-mode ()
  "Namazu $B$N8!:w7k2L$r1\Mw$9$k$?$a$N%b!<%I$G$9!#(B

binding          key
-------          ---
$BA0$N%Z!<%8(B       P           / h / [left]
$BA0$N9`L\(B         p / [BkSp]  / k / [up]    / M-[Tab]
$B8e$N9`L\(B         n / [Space] / j / [down]  / [Tab]
$B8e$N%Z!<%8(B       N           / l / [right]

$B%Z!<%8$N@hF,$X(B   <
$B%Z!<%8$NKvHx$X(B   >
$BJ8=q$r;2>H(B       g / [Enter]
$B:F8!:w(B           r / f
$B8!:w7k2L>C5n(B     q
Namazu $B=*N;(B      Q
$B%X%k%WI=<((B       ?

mouse $B$N??$sCf$N%\%?%s$r2!$9$H!"2!$7$?0LCV$K$h$C$F!"(B\"$BJ8>O$r;2>H(B\"$B!"(B
\"$BA0$N%Z!<%8(B\"$B!"(B\"$B8e$m$N%Z!<%8(B\" $B$N$I$l$+$N=hM}$r<B9T$7$^$9!#(B
"
  (interactive)
  (save-excursion
    (if (eq major-mode 'namazu-mode)
	()
      (kill-all-local-variables)
      (use-local-map namazu-mode-map)
      (setq mode-name "Namazu")
      (setq major-mode 'namazu-mode)
      (run-hooks 'namazu-mode-hook))))

(defun namazu-jump-next ()
  "$B8!:w7k2L$N<!$N9`L\$X0\F0$7$^$9!#(B"
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (if (re-search-forward namazu-output-url-pattern nil t)
	(beginning-of-line)
      (goto-char pos)
      (if (and namazu-auto-turn-page
	       (< namazu-current-page namazu-max-page))
	  (progn
	    (namazu-next-page)
	    (namazu-jump-next))))))

(defun namazu-jump-prev ()
  "$B8!:w7k2L$N0l$DA0$N9`L\$X0\F0$7$^$9!#(B"
  (interactive)
  (if (re-search-backward namazu-output-url-pattern nil t)
      (if (save-excursion
	    (let ((ws (window-start)))
	      (if (re-search-backward "^$" nil t)
		  (and (>= ws (point))
		       (< 1 (count-lines ws (point))))
		nil)))
	  (recenter))
    (if (and namazu-auto-turn-page
	     (> namazu-current-page 0))
	(progn
	  (namazu-prev-page)
	  (end-of-buffer)
	  (namazu-jump-prev)))))

(defun namazu-view-at-mouse (event)
  "mouse $B$r;H$C$F%V%i%&%:$7$?$j%Z!<%8$r0\F0$7$?$j$7$^$9!#(B"
  (interactive "e")
  (set-buffer (event-buffer event))
  (goto-char (event-point event))
  (let ((pos (point))
	pos-title pos-url)
    (end-of-line)
    (and (re-search-backward namazu-output-title-pattern nil t)
	 (setq pos-title (point))
	 (goto-char pos)
	 (re-search-forward namazu-output-title-pattern nil t)
	 (re-search-backward namazu-output-url-pattern nil t)
	 (> (point) pos-title)
	 (setq pos-url (point))
	 (setq pos (point)))
    (goto-char pos)
    (beginning-of-line)
    (and (not pos-url)
	 (re-search-forward namazu-output-url-pattern nil t)
	 (setq pos-url (point)))
    (goto-char pos)
    (cond
     ((and pos-title pos-url)
      (namazu-view))
     ((and pos-url (> namazu-current-page 0))
      (namazu-prev-page))
     ((and pos-title (< namazu-current-page namazu-max-page))
      (namazu-next-page))
     (t (message "nothing to do.")))))

;; emacs $B8~$1$NDj5A(B
(eval-and-compile
  (or (fboundp 'event-buffer)
      (defun event-buffer (event)
	(window-buffer (posn-window (event-start event))))))

(eval-and-compile
  (or (fboundp 'event-point)
      (defun event-point (event)
	(posn-point (event-start event)))))

(eval-and-compile
  (or (fboundp 'match-string)
      (defun match-string (num &optional string)
	(if (match-beginning num)
	    (if string
		(substring string (match-beginning num) (match-end num))
	      (buffer-substring (match-beginning num) (match-end num)))))))

(defun namazu-view ()
  "$B%]%$%s%H$,0LCV$9$k9`L\$r%V%i%&%:$7$^$9!#(B"
  (interactive)
  (beginning-of-line)
  (if (re-search-forward namazu-output-url-pattern nil t)
      (let ((url (buffer-substring (match-beginning 1) (match-end 1))))
        (beginning-of-line)
        (sit-for 0)
        (and (string-match "^/\\([a-zA-Z]\\)|\\(/.*\\)$" url)
	     ;; if DOS/Windows /c|...
	     (setq url
		   (concat (substring url (match-beginning 1) (match-end 1))
			   ":"
			   (substring url (match-beginning 2) (match-end 2)))))
	(if (string-match namazu-url-regex url)
	    (namazu-browse-url url)
	  (let ((ext '("" ".gz" ".Z" "bz2"))
		(fl namazu-view-function-alist)
		(file (expand-file-name url)) (name "") path done)
	    (and (string-match "\\(.*\\)\\(#.*\\)$" url)
		 (setq file (substring url (match-beginning 1) (match-end 1)))
		 (setq name (substring url (match-beginning 2) (match-end 2))))
	    (while (and (null done) ext)
	      (setq path (concat file (car ext)))
	      (and (file-exists-p path)
		   (setq done t)
		   (while fl
		     (if (string-match (car (car fl)) path)
			 (progn
			   (funcall (cdr (car fl)) (concat path name))
			   (setq fl nil)))
                     (setq fl (cdr fl))))
	      (setq ext (cdr ext))))))))

(defun namazu-view-file (&optional file)
  "View file function."
  (interactive "fView message: ")
  (if (and window-system namazu-view-other-frame)
      (view-file-other-frame file)
    (if namazu-view-other-window
	(view-file-other-window file)
      (view-file file)))
  ;; xxx
  (if (and (boundp 'view-mode-map) view-mode-map)
      (define-key view-mode-map "," 'namazu-view-top))
  (if (and (boundp 'view-minor-mode-map) view-minor-mode-map)
      (define-key view-minor-mode-map "," 'namazu-view-top))
  (make-local-variable 'namazu-view-vismark))

(defun namazu-view-msg (&optional file)
  "View message function."
  (namazu-view-file file)
  (let ((buffer-read-only nil)
	(vis-head "")
	hspos)
    (goto-char (point-min))
    (if (not (re-search-forward "^$" nil t))
	()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (mapcar (function
		   (lambda (head)
		     (goto-char (point-min))
		     (if (not (re-search-forward (concat "^" head ":") nil t))
			 ()
		       (beginning-of-line)
		       (setq hspos (point))
		       (forward-line 1)
		       (while (looking-at "^[ \t]+")
			 (forward-line 1))
		       (setq vis-head
			     (concat vis-head (buffer-substring hspos (point))))
		       (delete-region hspos (point)))))
		  namazu-msg-visible-field)
	  (goto-char (point-max))
	  (setq namazu-view-vismark (point-marker))
	  (insert vis-head)
	  (condition-case err
	      (cond
	       ((fboundp 'mew-header-decode-region)
		(mew-header-decode-region 'text (point-min) (point-max) t))
	       ((fboundp 'eword-decode-region)
		(eword-decode-region (point-min) (point-max) t)))
	    (error nil))
	  (widen)))
      (goto-char namazu-view-vismark)
      (recenter 0)
      (if namazu-msg-highlight-function
	  (funcall namazu-msg-highlight-function))
      (set-visited-file-name nil)
      (set-buffer-modified-p nil))))

(defun namazu-view-top ()
  "goto namazu view top point."
  (interactive)
  (if (and (boundp 'namazu-view-vismark)
	   (markerp namazu-view-vismark))
      (goto-char namazu-view-vismark)
    (goto-char (point-min)))
  (recenter 0))

(defun namazu-browse-url (url)
  "browse-url $B$r;H$C$FI=<($7$^$9!#(B
$B;HMQ$9$k(B browser $B$O(B browse-url-browser-function $B$G;XDj$7$^$9!#(B"
  (interactive)
  (setq url (browse-url-file-url url))
  (if (fboundp 'browse-url)
      (browse-url url)
    (funcall browse-url-browser-function url)))

(defun namazu-man (file)
  "manual $B$rI=<($7$^$9!#(B"
  (interactive)
  (require 'man)
  (let ((manual-program "nroff -man -h"))
    (Man-getpage-in-background file)))

(defun namazu-exit ()
  "namazu $B$r=*N;$7$^$9!#(B"
  (interactive)
  (if (and (get-buffer namazu-buffer)
	   (buffer-name (get-buffer namazu-buffer)))
      (kill-buffer namazu-buffer)))

(if namazu-mode-map
    nil
  (setq namazu-mode-map (make-keymap))
  (suppress-keymap namazu-mode-map)
  (define-key namazu-mode-map "P"     'namazu-prev-page)
  (define-key namazu-mode-map "p"     'namazu-jump-prev)
  (define-key namazu-mode-map "n"     'namazu-jump-next)
  (define-key namazu-mode-map "N"     'namazu-next-page)

  (define-key namazu-mode-map "\177"  'namazu-jump-prev)
  (define-key namazu-mode-map " "     'namazu-jump-next)

  (define-key namazu-mode-map "\M-\t" 'namazu-jump-prev)
  (define-key namazu-mode-map "\t"    'namazu-jump-next)

  (define-key namazu-mode-map "h"     'namazu-prev-page)
  (define-key namazu-mode-map "k"     'namazu-jump-prev)
  (define-key namazu-mode-map "j"     'namazu-jump-next)
  (define-key namazu-mode-map "l"     'namazu-next-page)

  (define-key namazu-mode-map [left]  'namazu-prev-page)
  (define-key namazu-mode-map [up]    'namazu-jump-prev)
  (define-key namazu-mode-map [down]  'namazu-jump-next)
  (define-key namazu-mode-map [right] 'namazu-next-page)

  (define-key namazu-mode-map "<"     'beginning-of-buffer)
  (define-key namazu-mode-map ">"     'end-of-buffer)
  (define-key namazu-mode-map "\r"    'namazu-view)
  (define-key namazu-mode-map "g"     'namazu-view)
  (define-key namazu-mode-map "r"     'namazu-re-search)
  (define-key namazu-mode-map "q"     'bury-buffer)
  (define-key namazu-mode-map "Q"     'namazu-exit)
  (define-key namazu-mode-map "?"     'describe-mode)

  (if (string-match "XEmacs" emacs-version)
      (define-key namazu-mode-map [(button2)] 'namazu-view-at-mouse)
    (define-key namazu-mode-map [mouse-2] 'namazu-view-at-mouse)))

(if namazu-minibuffer-map
    nil
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map "\t" 'namazu-dir-complete)
    (setq namazu-minibuffer-map map)))

(if namazu-minibuffer-field-map
    nil
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map "\t" 'namazu-field-complete)
    (setq namazu-minibuffer-field-map map)))

(cond
 ((featurep 'font-lock)
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face))
  (or (boundp 'font-lock-reference-face)
      (setq font-lock-reference-face font-lock-function-name-face))
  (if (boundp 'font-lock-defaults)
      (progn
	(defvar namazu-font-lock-keywords
	  (list
	   (list namazu-output-title-pattern
		 '(1 font-lock-comment-face)
		 '(2 font-lock-keyword-face)
		 '(3 font-lock-reference-face))
	   (list namazu-output-header-pattern
		 1 'font-lock-variable-name-face)
	   (list namazu-output-url-pattern
		 '(1 (progn
		       (set-text-properties (match-beginning 1) (match-end 1)
					    '(mouse-face highlight))
		       font-lock-function-name-face))
		 '(3 font-lock-type-face))
	   (list namazu-output-current-list-pattern
		 0 'font-lock-comment-face)
	   (list namazu-output-pages-pattern 0 'font-lock-comment-face)))
	(add-hook
	 'namazu-display-hook
	 (lambda ()
	   (make-local-variable 'font-lock-defaults)
	   (setq font-lock-defaults
		 '((namazu-font-lock-keywords) t))
	   (font-lock-mode 1))))
    (defvar namazu-font-lock-keywords
      (list
       (list namazu-output-title-pattern 1 'font-lock-comment-face)
       (list namazu-output-title-pattern 2 'font-lock-keyword-face)
       (list namazu-output-title-pattern 3 'font-lock-reference-face)
       (list namazu-output-header-pattern 1 'font-lock-variable-name-face)
       (list namazu-output-url-pattern 1 'font-lock-function-name-face)
       (list namazu-output-url-pattern 3 'font-lock-type-face)
       (list namazu-output-current-list-pattern  0 'font-lock-comment-face)
       (list namazu-output-pages-pattern 0 'font-lock-comment-face))
      "Namazu $B$G$N8!:w7k2L$K$*2=>Q$r$9$k$?$a$N@_Dj$G$9(B. ")
    (add-hook 'namazu-display-hook
	      (lambda ()
		(setq font-lock-keywords namazu-font-lock-keywords)
		(font-lock-mode 1)))))
 ((featurep 'hilit19)
  (if (and (boundp 'hilit-background-mode)
	   (eq hilit-background-mode 'dark))
      (hilit-set-mode-patterns
       'namazu-mode
       (list
	(list namazu-output-title-pattern  1 'red-bold-underline)
	(list namazu-output-title-pattern  2 'yellow-bold)
	(list namazu-output-title-pattern  3 'grey80)
	(list namazu-output-header-pattern 1 'palegreen)
	(list namazu-output-url-pattern    1 'gold-underline)
	(list namazu-output-url-pattern    3 'grey80)))
    (hilit-set-mode-patterns
     'namazu-mode
     (list
      (list namazu-output-title-pattern  1 'red-bold-underline)
      (list namazu-output-title-pattern  2 'purple)
      (list namazu-output-title-pattern  3 'grey40)
      (list namazu-output-header-pattern 1 'DarkGoldenrod)
      (list namazu-output-url-pattern    1 'blue-bold-underline)
      (list namazu-output-url-pattern    3 'grey40))))
  (add-hook 'namazu-display-hook
	    'hilit-rehighlight-buffer-quietly)))

;; Message highlight functions. 
;; e.g. 
;; (setq namazu-msg-highlight-function 'namazu-msg-highlight-mew)

;;
;; for Mew freak.
(defun namazu-msg-highlight-mew ()
  "namazu message highlight use Mew functions (1.94 or later)."
  (save-excursion
    (condition-case err
	(progn
	  (if (not (and (boundp 'mew-version)
			mew-version))
	      (save-excursion
		(require 'mew)
		(mew-init)
		(if (get-buffer mew-buffer-hello)
		    (kill-buffer mew-buffer-hello))))
	  (goto-char (point-min))
	  (if (and (fboundp 'mew-highlight-header-region)
		   (re-search-forward "^$" nil t))
	      (progn
		(mew-highlight-header-region (point-min) (point))
		(put-text-property (point) (1+ (point)) 'read-only t))) ;; header-end
	  (cond
	   ((fboundp 'mew-cite-color)
	    (mew-cite-color))
	   ((fboundp 'mew-highlight-body)
	    (mew-highlight-body)))
	  (and (fboundp 'mew-highlight-url)
	       (mew-highlight-url)))
      (error nil))))

;; end here.
