;;; sass-indentation.el --- Indentation functions for sass-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mazzullo, Christopher (IMS)

;; Author: Mazzullo, Christopher (IMS) <mazzullc@login-btp-02>
;; Keywords:

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

;; This is a library for sass-mode.el providing an indentation
;; function for SAS code. To use in any mode, evaluate:

;; (setq indent-line-function 'sass-indent-line)

;;

;;; Code:


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



(setq sass-indent-amount 2)
(setq sass-indent-amount-continuation 4) ;; how much to indent continuation lines
(setq sass-indent-amount-comment 2) ;; how much to indent comment lines

(setq sass-indent-re "^\\s-*\\(proc\\|data\\|.*\\_<do\\|%macro\\)\\_>")
(setq sass-deindent-re "\\_<\\(end\\|run\\|quit\\|%mend\\)\\s-*;")

;; RE for the last SAS expression:
;; (a semicolon or buffer start) (whitespace or comments) (tokens ended by a semicolon)
(setq sass-last-exp-re "\\(?:\\`\\|;\\\)\\(?:\\s-\\|/\\*[^*]*\\*\/\\)*\\(\\_<[^;\]*;\\\)")


(defun sass-inside-comment ()
  "Return whether or not the current point is inside a comment"
  (save-excursion
    (beginning-of-line)
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
  (if (sass-inside-comment)
      (if (sass-is-comment-end)
	  0
	sass-indent-amount-comment)
    (if (sass-is-continuation-line)
	sass-indent-amount-continuation
      (+ (if (sass-is-indentation-line)
	     sass-indent-amount 0)
	 (if (sass-is-deindent-line)
	     (- sass-indent-amount) 0)))))


(defun sass-indent-line ()
  "Indent the current line according to SAS conventions."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (+ (sass-last-exp-indent) (sass-get-offset))))
  (if (looking-at "[\t ]") (back-to-indentation)))


(provide 'sass-indentation)
;;; sass-indentation.el ends here
