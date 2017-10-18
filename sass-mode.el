;; sass-mode.el --- A minimal and non-awful mode for the SAS language  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mazzullo

;; Author: Chris Mazzullo <chris.mazzullo@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; INDENTATION

;; Issues:
;; Comments aren't taken care of properly

(setq sass-indent-amount 2)
(setq sass-indent-amount-continuation 4) ;; how much to indent continuation lines

(setq sass-indent-re "^\\s-*\\(proc\\|data\\|.*\\_<do\\|%macro\\)\\_>")
(setq sass-deindent-re "\\_<\\(end\\|run\\|quit\\|%mend\\)\\s-*;")

;; RE for the last SAS expression:
;; (a semicolon or buffer start) (whitespace or comments) (tokens ended by a semicolon)
(setq sass-last-exp-re "\\(?:\\`\\|;\\\)\\(?:\\s-\\|/\\*[^*]*\\*\/\\)*\\(\\_<[^;\]*;\\\)")


(defun sass-inside-comment ()
  "Return whether or not the current point is inside a comment"
  (save-excursion
    (nth 4 (syntax-ppss (point)))))

(defun sass-is-comment-end ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \\t]*\\*/")))

(defun sass-get-last-exp ()
  "Search backwards for the last SAS expression, return its match data."
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward sass-last-exp-re nil t)
	(match-data)
      nil)))


(defun sass-last-exp-indent ()
  "Return the indentation of the last SAS expression."
  (save-excursion
    (set-match-data (sass-get-last-exp))
    (let ((prev-indent (match-beginning 1)))
      (if prev-indent
	  (progn
	    (goto-char prev-indent)
	    (current-indentation))
	0))))


(defun sass-is-continuation-line ()
  "Return t if the current line is a continuation line, else nil."
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward "\\S-" nil t)
	(not (string-match "[;/]" (match-string 0)))
      nil)))


(defun sass-is-deindent-line ()
  "Determines if the current expression is a block-ending line (t/nil)."
  (save-excursion
    (beginning-of-line)
    (looking-at sass-deindent-re)))


(defun sass-is-indentation-line ()
  "Determines if the last expression was a block-starting line (t/nil)."
  (set-match-data (sass-get-last-exp))
  (let ((match (match-string 1)))
    (if match
	(string-match sass-indent-re match)
      nil)))


(defun sass-get-offset ()
  "Returns the amount of spaces by which the current line should be indented."
  (if (sass-in-comment)
      nil
    (if (sass-is-continuation-line)
	sass-indent-amount-continuation
      (+ (if (sass-is-indentation-line)
	     sass-indent-amount 0)
	 (if (sass-is-deindent-line)
	     (- sass-indent-amount) 0))))


(defun sass-indent-line ()
  "Indent the current line according to SAS conventions."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (+ (sass-last-exp-indent) (sass-get-offset))))
  (if (looking-at "[\t ]") (back-to-indentation)))


;; END INDENTATION


(setq sass-template-dir "/prj/plcoims/study_wide/data_library/data_file_documentation/monthly/complete_cohort/jan17/03.02.17/final_mf_templates/")
(defun sass-get-template-fname (cancer)
  (concat sass-template-dir cancer ".template.sas"))

(defun sass-extract-template (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward "\*\*Template code to build")
      (beginning-of-line)
      (delete-region (point-min) (point))
      (re-search-forward "^data .*(keep=$")
      (re-search-forward "^run;$")
      (delete-region (point) (point-max))
      (buffer-string))))


(setq sass-cancer-template-names
      (list "biliary"
	    "bladder"
	    "breast"
	    "colorectal"
	    "endometrial"
	    "glioma"
	    "head_and_neck"
	    "hematopoietic"
	    "hsq"
	    "liver"
	    "lung"
	    "male_breast"
	    "melanoma"
	    "ovarian"
	    "pancreas"
	    "prostate"
	    "renal"
	    "scu"
	    "sqx"
	    "thyroid"
	    "uppergi"))



(defun sass-contents ()
  (interactive)

  (async-shell-command (concat "mysas " buffer-file-name) (concat "*sas-output*<" (buffer-name) ">"))

  (let ((newname (concat "*sass-contents*<" (buffer-name) ">")))
    (call-process "contents" nil newname nil (ffap-guess-file-name-at-point))
    (pop-to-buffer newname)
    (goto-char (point-min))
    (save-excursion
      (search-forward " # ")
      (delete-region (point-min) (point-at-bol)))
    (sass-output-mode)))


(defun sass-import-ims-database (dname)
  (interactive "sData block name: ")
  (let ((filename (read-file-name "Database: " "/prj/plcoims/maindata/mastdata/monthly/09t13/jan17/03.02.17/" nil t)))
    (insert
     (concat
      "filename " dname " pipe \"gunzip -c " filename "\";\nproc cimport infile=" dname " data=" dname ";\n"
      "proc sort data=" dname ";\n  by plco_id;\nrun;\n"
      ))))


(defun sass-insert-prsn-template ()
  (interactive)
  (insert
   (sass-extract-template
    (sass-get-template-fname (completing-read "Cancer: " sass-cancer-template-names nil t)))))


(add-to-list 'auto-mode-alist '("\\.sas\\'" . sass-mode))

(require 'sass-output-mode)
(defvar sass-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\\ "."  st)  ;; backslash is punctuation
    (modify-syntax-entry ?+  "."  st)
    (modify-syntax-entry ?-  "."  st)
    (modify-syntax-entry ?=  "."  st)
    (modify-syntax-entry ?<  "."  st)
    (modify-syntax-entry ?>  "."  st)
    (modify-syntax-entry ?|  "."  st)
    (modify-syntax-entry ?<  "."  st)
    (modify-syntax-entry ?>  "."  st)
    (modify-syntax-entry ?/  ". 14"  st) ; comment character
    (modify-syntax-entry ?*  ". 23"  st) ; comment character
    (modify-syntax-entry ?_  "_"  st)
    (modify-syntax-entry ?.  "_"  st)
    (modify-syntax-entry ?%  "w"  st)
    (modify-syntax-entry ?&  "w"  st)
    st))


(defvar sass-font-lock-keywords
  (list
   '("\\(^\\|*\\)\\([[:space:]]*\\*+.*?;\\)" . font-lock-comment-face)
   ;;   '("^[[:space:]]*?\\*+?.*?;" . font-lock-comment-face)
   `(,(regexp-opt
       '("_null_"
	 "abort" "array" "attrib" "and"
	 "by"
	 "call" "cards" "cards4" "center"
	 "data" "delete"  "do" "drop" "dim"
	 "else" "end" "eof"
	 "file" "format" "formchar" "footenote" "footenote1" "footenote2" "footenote3"
	 "footenote4" "footenote5" "footenote6" "footenote7" "footenote8" "footenote9"
	 "footenote10" "firstobs" "filename" "go" "if" "infile" "informat" "input" "in"
	 "keep" "label" "length" "link" "lostcard" "ls" "libname" "left" "lrecl" "lastobs"
	 "merge" "missing" "mtrace" "mprint" "min" "max" "mlogic" "missover" "mdy" "nonumber"
	 "nobs" "nomprint" "nomtrace" "nosymbolgen" "noovp" "null" "over" "output" "out"
	 "options" "or" "otherwise" "put" "ps" "pad" "rename" "retain" "return" "run" "rank"
	 "select" "set" "skip" "stop" "symbolgen" "sum" "source2" "symput" "same" "sum"
	 "substr" "then" "to" "title" "title1" "title2" "title3" "title4" "title5" "title6"
	 "title7" "title8" "title9" "title10" "trim" "time" "until" "update" "where" "while"
	 "window" "when")
       'symbols) . font-lock-keyword-face)
   `(,(regexp-opt
       '("as" "after" "append"
	 "break"
	 "calender" "catalog" "chart" "cimport" "class" "contents" "compare" "contents" "copy" "corr" "cport" "create" "column"
	 "datasets" "define" "display"
	 "ERROR" "endsas"
	 "format" "forms" "freq" "from"
	 "group"
	 "headline" "headskip"
	 "id" "intervals" "into"
	 "lifetest" "list" "line" "lsmeans" "leave"
	 "method" "means"  "model"
	 "new" "noobs" "noprint" "n" "NOTE"
	 "options" "out" "order" "obs" "outsurv"
	 "proc" "print" "plots" "plot" "pmenu" "printto"  "pageby"
	 "rank" "report" "repeated"
	 "sort" "spell" "strata" "standard" "summary" "sql" "split" "skip" "sumby"
	 "tables" "tabulate" "test" "timeplot" "ttest" "table"  "transpose"
	 "uniform" "univariate"
	 "var" "value"  "v5tov6"
	 "width" "WARNING")
       'symbols)
     . font-lock-constant-face)
   `(,(regexp-opt
       '("aceclus" "anova"
	 "calis" "cancorr" "candisc" "catmod" "cluster" "corresp"
	 "discrim"
	 "factor" "fastclus" "freq"
	 "genmod" "glm" "glmmod"
	 "inbreed"
	 "lattice" "lifereg" "lifetest" "logistic"
	 "mds" "mixed" "modeclus" "multtest"
	 "nested" "nlin" "npar1way"
	 "output" "orthoreg"
	 "phreg" "plan" "princomp" "prinqual"
	 "reg" "rsreg"
	 "score" "stepdisc"
	 "template" "transreg" "tree" "ttest"
	 "varclus" "varcomp" )
       'symbols)
     . font-lock-builtin-face)
   `(,(regexp-opt
       '("%bquote"
	 "%display" "%do"
	 "%end" "%else" "%eval"
	 "%goto" "%global"
	 "%if" "%include" "%input"
	 "%keydef"
	 "%label" "%let" "%local"
	 "%macro" "%mend"
	 "%put"
	 "%str" "%scan" "%syscall" "%sysexec" "%sysrput" "%symget" "%symput"
	 "%then" "%to"
	 "%upcase" "%until"
	 "%while" "%window"
	 "MPRINT" "MLOGIC" "MTRACE"
	 "SYMBOLGEN" )
       'symbols)
     . font-lock-function-name-face))
  "Default highlighting expressions for sass-mode")


(define-derived-mode sass-mode
  prog-mode "sass"
  "Major mode for editing SAS programs"
  :syntax-table sass-mode-syntax-table

  (defun sass-run ()
    (interactive)
    (async-shell-command (concat "mysas " buffer-file-name) (concat "*sas-output*<" (buffer-name) ">"))
    (message "Executing SAS program..."))

  (defun sass-find-lst ()
    (interactive)
    (find-file-other-window (concat (file-name-sans-extension (buffer-file-name)) ".lst")))

  (defun sass-find-rtf ()
    (interactive)
    (find-file-other-window (concat (file-name-sans-extension (buffer-file-name)) ".rtf")))

  (defun sass-find-log ()
    (interactive)
    (find-file-other-window (concat (file-name-sans-extension (buffer-file-name)) ".log")))

  (define-key sass-mode-map (kbd "<f5>") 'sass-run)
  (define-key sass-mode-map (kbd "<f6>") 'sass-find-lst)
  (define-key sass-mode-map (kbd "<f7>") 'sass-find-log)
  (define-key sass-mode-map (kbd "<f8>") 'sass-find-rtf)

  (setq indent-line-function 'sass-indent-line)

  (setq comment-start "/*"
	comment-end "*/")
  (set (make-local-variable 'font-lock-defaults) '(sass-font-lock-keywords))
  (set (make-local-variable 'indent-tabs-mode) nil))


(provide 'sass-mode)
;;; sass-mode.el ends here
