;;; sass-output-mode.el --- Mode for viewing files output by SAS  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mazzullo

;; Author: Mazzullo <mazzullc@login-btp-02>
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

;;

;;; Code:

(defvar sass-output-mode-hook nil)

(defun sass-output-run ()
  (interactive)
  (async-shell-command (concat "mysas " (concat (file-name-sans-extension (buffer-file-name)) ".sas")) "*sas-output*" "*sas-error*")
  (message "Executing SAS program..."))

(defun sass-output-find-lst ()
  (interactive)
  (let ((lst-file (concat (file-name-sans-extension (buffer-file-name)) ".lst"))
	(sas-file (concat (file-name-sans-extension (buffer-file-name)) ".sas")))
    (if
	(equal lst-file (buffer-file-name)) (find-file-other-window sas-file)
      (find-file-other-window lst-file))))

(defun sass-output-find-log ()
  (interactive)
  (let ((log-file (concat (file-name-sans-extension (buffer-file-name)) ".log"))
	(sas-file (concat (file-name-sans-extension (buffer-file-name)) ".sas")))
    (message log-file)
    (if
	(equal log-file (buffer-file-name)) (find-file-other-window sas-file)
      (find-file-other-window log-file))))

(defvar sass-output-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<f5>") 'sass-output-run)
    (define-key map (kbd "<f6>") 'sass-output-find-lst)
    (define-key map (kbd "<f7>") 'sass-output-find-log)
    map)
  "Keymap for SAS-output major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(lst\\|\\log\\)\\'" . sass-output-mode))

(defun sass-output-mode ()
  "Major mode for viewing SAS output files"
  (interactive)
  (kill-all-local-variables)

  (use-local-map sass-output-mode-map)
  (define-key sass-output-mode-map (kbd "q") 'kill-buffer)

  (setq major-mode 'sass-output-mode)
  (setq mode-name "SAS-out")
  (run-hooks 'sass-output-mode-hook))


(provide 'sass-output-mode)
;;; sass-output-mode.el ends here
