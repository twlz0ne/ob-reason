;;; ob-reason.el --- Babel Functions for Reason        -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2019 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating reason source code.  This one will
;; be sort of tricky because reason programs must be compiled before
;; they can be run, but reason code can also be run through an
;; interactive interpreter.
;;
;; For now lets only allow evaluation using the reason interpreter.

;;; Requirements:

;; - tuareg-mode :: http://www-rocq.inria.fr/~acohen/tuareg/

;;; Code:
(require 'ob)
(require 'comint)
(require 'org-macs)

(declare-function tuareg-run-ocaml "ext:tuareg" ())
(declare-function tuareg-interactive-send-input "ext:tuareg" ())

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("reason" . "re"))

(defvar org-babel-default-header-args:reason '())

(defvar org-babel-reason-eoe-indicator "\"org-babel-reason-eoe\";")
(defvar org-babel-reason-eoe-output "org-babel-reason-eoe")

(defcustom org-babel-reason-command "rtop"
  "Name of the command for executing Reason code."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:reason (body params)
  "Execute a block of Reason code with Babel."
  (let* ((full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:reason params)))
         (session (org-babel-prep-session:reason
		   (cdr (assq :session params)) params))
         (raw (org-babel-comint-with-output
		  (session org-babel-reason-eoe-output nil full-body)
		(insert
		 (concat
		  (org-babel-chomp full-body) ";\n"
		  org-babel-reason-eoe-indicator))
		(tuareg-interactive-send-input)))
	 (clean
	  (car (let ((re (regexp-quote org-babel-reason-eoe-output)) out)
		 (delq nil (mapcar (lambda (line)
				     (if out
					 (progn (setq out nil) line)
				       (when (string-match re line)
					 (progn (setq out t) nil))))
				   (mapcar #'org-trim (reverse raw)))))))
	 (raw (org-trim clean))
	 (result-params (cdr (assq :result-params params))))
    (string-match
     "\\(\\(.*\n\\)*\\)[^:\n]+ : \\([^=\n]+\\) =\\(\n\\| \\)\\(.+\\)$"
     raw)
    (let ((output (match-string 1 raw))
	  (type (match-string 3 raw))
	  (value (match-string 5 raw)))
      (org-babel-reassemble-table
       (org-babel-result-cond result-params
	 (cond
	  ((member "verbatim" result-params) raw)
	  ((member "output" result-params) output)
	  (t raw))
	 (if (and value type)
	     (org-babel-reason-parse-output value type)
	   raw))
       (org-babel-pick-name
	(cdr (assq :colname-names params)) (cdr (assq :colnames params)))
       (org-babel-pick-name
	(cdr (assq :rowname-names params)) (cdr (assq :rownames params)))))))

(defun org-babel-prep-session:reason (session _params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (require 'tuareg)

  (define-advice tuareg-interactive-send-input (:override () override)
    "Send the current phrase to the OCaml REPL or insert a newline.
If the point is next to \";\", the phrase is sent to the REPL,
otherwise a newline is inserted and the lines are indented."
    (interactive)
    (cond
     ((tuareg-in-literal-or-comment-p) (tuareg-interactive--indent-line))
     ((or (equal ";" (save-excursion (nth 2 (smie-backward-sexp))))
          (looking-at-p "[ \t\n\r]*;"))
      (comint-send-input))
     (t (tuareg-interactive--indent-line))))

  (add-to-list 'exec-path "~/.nodenv/shims")
  (let ((tuareg-interactive-program org-babel-reason-command)
        (tuareg-interactive-buffer-name (if (and (not (string= session "none"))
                                                 (not (string= session "default"))
                                                 (stringp session))
                                            session
                                          tuareg-interactive-buffer-name)))
    (save-window-excursion (if (fboundp 'reason-run-process-if-needed)
	 (reason-run-process-if-needed org-babel-reason-command)
       (tuareg-run-ocaml)))
    (get-buffer tuareg-interactive-buffer-name)))

(defun org-babel-variable-assignments:reason (params)
  "Return list of reason statements assigning the block's variables."
  (mapcar
   (lambda (pair) (format "let %s = %s;" (car pair)
			  (org-babel-reason-elisp-to-reason (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-reason-elisp-to-reason (val)
  "Return a string of reason code which evaluates to VAL."
  (if (listp val)
      (concat "[|" (mapconcat #'org-babel-reason-elisp-to-reason val "; ") "|]")
    (format "%S" val)))

(defun org-babel-reason-parse-output (value type)
  "Parse VALUE of type TYPE.
VALUE and TYPE are string output from an reason process."
  (cond
   ((string= "string" type)
    (org-babel-read value))
   ((or (string= "int" type)
	(string= "float" type))
    (string-to-number value))
   ((string-match "list" type)
    (org-babel-reason-read-list value))
   ((string-match "array" type)
    (org-babel-reason-read-array value))
   (t (message "don't recognize type %s" type) value)))

(defun org-babel-reason-read-list (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  ;; XXX: This probably does not behave as expected when a semicolon
  ;; is in a string in a list.  The same comment applies to
  ;; `org-babel-reason-read-array' below (with even more failure
  ;; modes).
  (org-babel-script-escape (replace-regexp-in-string ";" "," results)))

(defun org-babel-reason-read-array (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape
   (replace-regexp-in-string
    "\\[|" "[" (replace-regexp-in-string
		"|\\]" "]" (replace-regexp-in-string
			    "; " "," results)))))

(provide 'ob-reason)



;;; ob-reason.el ends here
