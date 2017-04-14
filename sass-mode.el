;;; sass-mode.el --- A minimal and non-awful mode for the SAS language  -*- lexical-binding: t; -*-

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
(require 'sass-output-mode)

(defvar sass-mode-hook nil)

;; SAS FUNCTIONS

(defun sass-run ()
  (interactive)
  (async-shell-command (concat "mysas " buffer-file-name) "*sas-output*" "*sas-error*")
  (message "Executing SAS program..."))

(defun sass-find-lst ()
  (interactive)
  (find-file-other-window (concat (file-name-sans-extension (buffer-file-name)) ".lst")))

(defun sass-find-log ()
  (interactive)
  (find-file-other-window (concat (file-name-sans-extension (buffer-file-name)) ".log")))

(defvar sass-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<f5>") 'sass-run)
    (define-key map (kbd "<f6>") 'sass-find-lst)
    (define-key map (kbd "<f7>") 'sass-find-log)
    map)
  "Keymap for SAS major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sas\\'" . sass-mode))

(defvar sass-font-lock-keywords
  (list
   `(,(regexp-opt
       '("_null_"
	 "abort" "array" "attrib" "and"
	 "by"
	 "call" "cards" "cards4" "center"
	 "data" "delete"  "do" "drop" "dim"
	 "else" "end" "eof"
	 "file" "format" "formchar" "footenote" "footenote1" "footenote2" "footenote3" "footenote4"
	 "footenote5" "footenote6" "footenote7" "footenote8" "footenote9" "footenote10" "firstobs"
	 "filename"
	 "go"
	 "if" "infile" "informat" "input" "in"
	 "keep"
	 "label" "length" "link" "lostcard" "ls" "libname" "left" "lrecl"  "lastobs"
	 "merge" "missing" "mtrace" "mprint" "min" "max"  "mlogic" "missover"  "mdy"
	 "nonumber" "nobs" "nomprint" "nomtrace" "nosymbolgen" "noovp" "null"
	 "over" "output" "out" "options" "or" "otherwise"
	 "put" "ps" "pad"
	 "rename" "retain" "return" "run" "rank"
	 "select" "set" "skip" "stop" "symbolgen" "sum"  "source2"  "symput"  "same"  "sum" "substr"
	 "then" "to" "title" "title1" "title2" "title3" "title4" "title5" "title6" "title7" "title8" "title9"
	 "title10" "trim"  "time"
	 "until" "update"
	 "where" "while" "window"  "when")
       'symbols)
     . font-lock-keyword-face)
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
	 "lifetest" "list" "line" "lsmeans"
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

(defvar sass-mode-syntax-table
  (let ((st (make-syntax-table)))

  (modify-syntax-entry ?\\ "."  st)  ;; backslash is punctuation
  (modify-syntax-entry ?+  "."  st)
  (modify-syntax-entry ?-  "."  st)
  (modify-syntax-entry ?=  "."  st)
  (modify-syntax-entry ?%  "w"  st)
  (modify-syntax-entry ?<  "."  st)
  (modify-syntax-entry ?>  "."  st)
  (modify-syntax-entry ?&  "w"  st)
  (modify-syntax-entry ?|  "."  st)
  (modify-syntax-entry ?\' "\"" st)
  (modify-syntax-entry ?*  ". 123b"  st) ; comment character
  (modify-syntax-entry ?\n  ". 4b"  st) ; comment character
  (modify-syntax-entry ?\; ". 3b"  st)
  (modify-syntax-entry ?_  "w"  st)
  (modify-syntax-entry ?<  "."  st)
  (modify-syntax-entry ?>  "."  st)
  (modify-syntax-entry ?/  ". 14"  st) ; comment character
  (modify-syntax-entry ?.  "w"  st)
   st))

(defun sass-mode ()
  "Major mode for editing SAS files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sass-mode-syntax-table)
  (use-local-map sass-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(sass-font-lock-keywords))
  (setq major-mode 'sass-mode)
  (setq mode-name "SAS")
  (run-hooks 'sass-mode-hook))

(provide 'sass-mode)
;;; sass-mode.el ends here



;; % .. &		w 	which means: word
;; '		" 	which means: string
;; *		. 23	which means: punctuation,
;; 	  is the second character of a comment-start sequence,
;; 	  is the first character of a comment-end sequence
;; +		. 	which means: punctuation
;; -		. 	which means: punctuation
;; .		w 	which means: word
;; /		. 14	which means: punctuation,
;; 	  is the first character of a comment-start sequence,
;; 	  is the second character of a comment-end sequence
;; ; .. >		. 	which means: punctuation
;; \		. 	which means: punctuation
;; _		w 	which means: word
;; |		. 	which means: punctuation
