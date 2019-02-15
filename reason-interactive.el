(require 'comint)

(defvar tuareg-interactive-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-c\C-i" 'tuareg-interrupt-ocaml)
    (define-key map "\C-c\C-k" 'tuareg-kill-ocaml)
    (define-key map "\C-c`" 'tuareg-interactive-next-error-repl)
    (define-key map "\C-c?" 'tuareg-interactive-next-error-repl)
    (define-key map "\C-m" 'tuareg-interactive-send-input)
    (define-key map [(shift return)]
      'tuareg-interactive-send-input-end-of-phrase)
    (define-key map [(ctrl return)]
      'tuareg-interactive-send-input-end-of-phrase)
    (define-key map [kp-enter] 'tuareg-interactive-send-input-end-of-phrase)
    map))

(defconst tuareg-interactive-buffer-name "*OCaml*")

(defconst tuareg-interactive-error-range-regexp
  "[ \t]*Characters \\([0-9]+\\)-\\([1-9][0-9]*\\):\n"
  "Regexp matching the char numbers in OCaml REPL's error messages.")

(defconst tuareg-interactive-error-regexp
  "\n\\(Error: [^#]*\\)")

(defconst tuareg-interactive-exception-regexp
  "\\(Exception: [^#]*\\)")

(defvar tuareg-interactive-last-phrase-pos-in-source 0)

(defvar tuareg-interactive-last-phrase-pos-in-repl 0)

(defun tuareg-interactive-filter (_text)
  (when (eq major-mode 'tuareg-interactive-mode)
    (save-excursion
      (when (>= comint-last-input-end comint-last-input-start)
        (when tuareg-interactive-read-only-input
          (add-text-properties
           comint-last-input-start comint-last-input-end
           (list 'read-only t)))
        (when (and font-lock-mode tuareg-interactive-input-font-lock)
          (font-lock-fontify-region comint-last-input-start
                                    comint-last-input-end))
        (when tuareg-interactive-output-font-lock
          (save-excursion
            (goto-char (point-max))
            (re-search-backward comint-prompt-regexp
                                comint-last-input-end t)
            (add-text-properties
             comint-last-input-end (point)
             '(font-lock-face tuareg-font-lock-interactive-output-face))))
        (when tuareg-interactive-error-font-lock
          (save-excursion
            (goto-char comint-last-input-end)
            (cond
             ((looking-at tuareg-interactive-error-range-regexp)
              (let ((beg (string-to-number (match-string-no-properties 1)))
                    (end (string-to-number (match-string-no-properties 2))))
                (put-text-property
                 (+ comint-last-input-start beg)
                 (+ comint-last-input-start end)
                 'font-lock-face 'tuareg-font-lock-error-face))
              (goto-char comint-last-input-end)
              (when (re-search-forward tuareg-interactive-error-regexp nil t)
                (let ((errbeg (match-beginning 1))
                      (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'tuareg-font-lock-interactive-error-face))))
             ((looking-at tuareg-interactive-exception-regexp)
              (let ((errbeg (match-beginning 1))
                    (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'tuareg-font-lock-interactive-error-face)))
             )))))))

(easy-menu-define
  tuareg-interactive-mode-menu tuareg-interactive-mode-map
  "Tuareg Interactive Mode Menu."
  '("Tuareg"
    ("Interactive Mode"
     ["Run OCaml REPL" tuareg-run-ocaml t]
     ["Interrupt OCaml REPL" tuareg-interrupt-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Kill OCaml REPL" tuareg-kill-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Evaluate Region" tuareg-eval-region :active (region-active-p)]
     ["Evaluate Phrase" tuareg-eval-phrase t]
     ["Evaluate Buffer" tuareg-eval-buffer t])
    "---"
    ["Customize Tuareg Mode..." (customize-group 'tuareg) t]
    ("Tuareg Options" ["Dummy" nil t])
    ("Tuareg Interactive Options" ["Dummy" nil t])
    "---"
    ["About" tuareg-about t]
    ["Help" tuareg-interactive-help t]))

(define-derived-mode tuareg-interactive-mode comint-mode "Tuareg-Interactive"
  "Major mode for interacting with an OCaml process.
Runs an OCaml REPL as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in tuareg mode.

Short cuts for interactions with the REPL:
\\{tuareg-interactive-mode-map}"
  (add-hook 'comint-output-filter-functions #'tuareg-interactive-filter)
  (setq comint-prompt-regexp "^#  *")
  (setq comint-process-echoes nil)
  (setq comint-get-old-input 'tuareg-interactive-get-old-input)
  (setq comint-scroll-to-bottom-on-output
        tuareg-interactive-scroll-to-bottom-on-output)
  (set-syntax-table tuareg-mode-syntax-table)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comint-prompt-read-only t)

  (tuareg--common-mode-setup)
  (tuareg--install-font-lock t)
  (when (or tuareg-interactive-input-font-lock
            tuareg-interactive-output-font-lock
            tuareg-interactive-error-font-lock)
    (font-lock-mode 1))

  (easy-menu-add tuareg-interactive-mode-menu)
  (tuareg-update-options-menu))

;;;###autoload
(defun tuareg-run-ocaml ()
  "Run an OCaml REPL process.  I/O via buffer `*OCaml*'."
  (interactive)
  (tuareg-run-process-if-needed)
  (display-buffer tuareg-interactive-buffer-name))

;;;###autoload
(defalias 'run-ocaml 'tuareg-run-ocaml)

;;;###autoload
(add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))

(defun tuareg-run-process-if-needed (&optional cmd)
  "Run an OCaml REPL process if needed, with an optional command name.
I/O via buffer `*OCaml*'."
  (if cmd
      (setq tuareg-interactive-program cmd)
    (unless (comint-check-proc tuareg-interactive-buffer-name)
      (setq tuareg-interactive-program
            (read-shell-command "OCaml REPL to run: "
                                tuareg-interactive-program))))
  (unless (comint-check-proc tuareg-interactive-buffer-name)
    (let ((cmdlist (tuareg--split-args tuareg-interactive-program))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint) "OCaml"
                         (car cmdlist) nil (cdr cmdlist)))
      (tuareg-interactive-mode)
      (sleep-for 1))))

(defun tuareg--split-args (args)
  (condition-case nil
      (split-string-and-unquote args)
      (error (progn
               (message "Arguments ‘%s’ ill quoted.  Ignored." args)
               nil))))

(defun tuareg-interactive-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (re-search-backward comint-prompt-regexp (point-min) t)
      (when (looking-at comint-prompt-regexp)
        (re-search-forward comint-prompt-regexp))
      (buffer-substring-no-properties (point) end))))

(defconst tuareg-interactive--send-warning
  "Note: REPL processing requires a terminating `;;', or use S-return.")

(defun tuareg-interactive--indent-line ()
  (insert "\n")
  (indent-according-to-mode)
  (message tuareg-interactive--send-warning))

(defun tuareg-interactive-send-input ()
  "Send the current phrase to the OCaml REPL or insert a newline.
If the point is next to \";;\", the phrase is sent to the REPL,
otherwise a newline is inserted and the lines are indented."
  (interactive)
  (cond
   ((tuareg-in-literal-or-comment-p) (tuareg-interactive--indent-line))
   ((or (equal ";;" (save-excursion (nth 2 (smie-backward-sexp))))
        (looking-at-p "[ \t\n\r]*;;"))
    (comint-send-input))
   (t (tuareg-interactive--indent-line))))

(defun tuareg-interactive-send-input-end-of-phrase ()
  (interactive)
  (goto-char (point-max))
  (unless (equal ";;" (save-excursion (nth 2 (smie-backward-sexp))))
    (insert ";;"))
  (comint-send-input))

(defun tuareg-interactive--send-region (start end)
  "Send the region between START and END to the OCaml REPL.
It is assumed that the range START-END delimit valid OCaml phrases."
  (save-excursion (tuareg-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (let* ((phrases (buffer-substring-no-properties start end))
         (phrases (replace-regexp-in-string "[ \t\n]*\\(;;[ \t\n]*\\)?\\'" ""
                                            phrases))
         (phrases-colon (concat phrases ";;")))
    (if (string= phrases "")
        (message "Cannot send empty commands to OCaml REPL!")
      (with-current-buffer tuareg-interactive-buffer-name
        (goto-char (point-max))
        (setq tuareg-interactive-last-phrase-pos-in-repl (point))
        (comint-send-string tuareg-interactive-buffer-name phrases-colon)
        (let ((pos (point)))
          (comint-send-input)
          (when tuareg-interactive-echo-phrase
            (save-excursion
              (goto-char pos)
              (insert phrases-colon)))))))
  (when tuareg-display-buffer-on-eval
    (display-buffer tuareg-interactive-buffer-name)))

(defun tuareg-eval-region (start end)
  "Eval the current region in the OCaml REPL."
  (interactive "r")
  (setq tuareg-interactive-last-phrase-pos-in-source start)
  (setq start (car (tuareg-discover-phrase start)))
  (if start
      (progn
        (setq end (cadr (tuareg-discover-phrase end)))
        (if end
            (tuareg-interactive--send-region start end)
          (message "The expression after the point is not well braced.")))
    (message "The expression after the point is not well braced.")))

(defun tuareg-narrow-to-phrase ()
  "Narrow the editting window to the surrounding OCaml phrase (or block)."
  (interactive)
  (let ((phrase (tuareg-discover-phrase)))
    (if phrase
        (narrow-to-region (nth 0 phrase) (nth 1 phrase)))))

(defun tuareg--after-double-colon ()
  "Non nil if the current position is after or inside ';;'.  In
this case, the returned value is the position before ';;' (unless
it is the first position of the buffer)."
  (save-excursion
    (when (looking-at-p "[;[:blank:]]*$")
      (skip-chars-backward ";[:blank:]")
      (if (> (point) 1) (- (point) 1)))))

(defun tuareg-eval-phrase ()
  "Eval the surrounding OCaml phrase (or block) in the OCaml REPL."
  (interactive)
  (let* ((pos (tuareg--after-double-colon))
         (pos (if pos pos (point)))
         (phrase (tuareg-discover-phrase pos)))
    (if phrase
        (progn
          (tuareg-interactive--send-region (nth 0 phrase) (nth 1 phrase))
          (when tuareg-skip-after-eval-phrase
            (goto-char (nth 2 phrase))
            (tuareg--skip-double-colon)
            (tuareg-skip-blank-and-comments)))
      (message "The expression after the point is not well braced."))))

(defun tuareg-eval-buffer ()
  "Send the buffer to the Tuareg Interactive process."
  (interactive)
  (tuareg-interactive--send-region (point-min) (point-max)))

(defvar tuareg-interactive-next-error-olv (make-overlay 1 1))

(overlay-put tuareg-interactive-next-error-olv
             'face 'tuareg-font-lock-error-face)

(delete-overlay tuareg-interactive-next-error-olv)

(defun tuareg-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (with-current-buffer tuareg-interactive-buffer-name
      (goto-char tuareg-interactive-last-phrase-pos-in-repl)
      (setq error-pos
            (re-search-forward tuareg-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (match-string-no-properties 1))
              end (string-to-number (match-string-no-properties 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-source beg)
            end (+ tuareg-interactive-last-phrase-pos-in-source end))
      (goto-char beg)
      (move-overlay tuareg-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay tuareg-interactive-next-error-olv))
      )))

(defun tuareg-interactive-next-error-repl ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char tuareg-interactive-last-phrase-pos-in-repl)
      (setq error-pos
            (re-search-forward tuareg-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (match-string-no-properties 1))
              end (string-to-number (match-string-no-properties 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-repl beg)
            end (+ tuareg-interactive-last-phrase-pos-in-repl end))
      (move-overlay tuareg-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay tuareg-interactive-next-error-olv))
      (goto-char beg))))

(defun tuareg-interrupt-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-interrupt-subjob))))

(defun tuareg-kill-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-kill-subjob))))

(provide 'reason-interactive)
