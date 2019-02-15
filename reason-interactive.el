(require 'comint)

(defcustom reason-interactive-scroll-to-bottom-on-output nil
  "*Controls when to scroll to the bottom of the interactive buffer
upon evaluating an expression.

See `comint-scroll-to-bottom-on-output' for details."
  :group 'reason :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (when (boundp 'comint-scroll-to-bottom-on-output)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'reason-interactive-mode)
                 (setq-local comint-scroll-to-bottom-on-output val)))))))

(defcustom reason-interactive-read-only-input nil
  "*Non-nil means input sent to the ReasonML REPL is read-only."
  :group 'reason :type 'boolean)

(defcustom reason-interactive-echo-phrase t
  "*Non-nil means echo phrases in the REPL buffer when sending
them to the ReasonML REPL."
  :group 'reason :type 'boolean)

(defcustom reason-interactive-input-font-lock t
  "*Non nil means Font-Lock for REPL input phrases."
  :group 'reason :type 'boolean)

(defcustom reason-interactive-output-font-lock t
  "*Non nil means Font-Lock for REPL output messages."
  :group 'reason :type 'boolean)

(defcustom reason-interactive-error-font-lock t
  "*Non nil means Font-Lock for REPL error messages."
  :group 'reason :type 'boolean)

(defcustom reason-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
ReasonML REPL."
  :group 'reason :type 'boolean)

(defun reason-in-literal-or-comment-p (&optional pos)
  "Return non-nil if point is inside an ReasonML literal or comment."
  (nth 8 (syntax-ppss pos)))

(defvar reason-interactive-options-list
  '(("Skip phrase after evaluation" . 'reason-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'reason-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'reason-interactive-input-font-lock)
    ("Font-lock interactive output" . 'reason-interactive-output-font-lock)
    ("Font-lock interactive error" . 'reason-interactive-error-font-lock)
    "---"
    ("Read only input" . 'reason-interactive-read-only-input))
  "*List of menu-configurable Reason options.")

(defvar reason-interactive-program "reason"
  "*Default program name for invoking an ReasonML REPL (aka toplevel) from Emacs.")

(defvar reason-interactive-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-c\C-i" 'reason-interrupt-reason)
    (define-key map "\C-c\C-k" 'reason-kill-reason)
    (define-key map "\C-c`" 'reason-interactive-next-error-repl)
    (define-key map "\C-c?" 'reason-interactive-next-error-repl)
    (define-key map "\C-m" 'reason-interactive-send-input)
    (define-key map [(shift return)]
      'reason-interactive-send-input-end-of-phrase)
    (define-key map [(ctrl return)]
      'reason-interactive-send-input-end-of-phrase)
    (define-key map [kp-enter] 'reason-interactive-send-input-end-of-phrase)
    map))

(defconst reason-interactive-buffer-name "*ReasonML*")

(defconst reason-interactive-error-range-regexp
  "[ \t]*Characters \\([0-9]+\\)-\\([1-9][0-9]*\\):\n"
  "Regexp matching the char numbers in ReasonML REPL's error messages.")

(defconst reason-interactive-error-regexp
  "\n\\(Error: [^#]*\\)")

(defconst reason-interactive-exception-regexp
  "\\(Exception: [^#]*\\)")

(defvar reason-interactive-last-phrase-pos-in-source 0)

(defvar reason-interactive-last-phrase-pos-in-repl 0)

(defun reason-interactive-filter (_text)
  (when (eq major-mode 'reason-interactive-mode)
    (save-excursion
      (when (>= comint-last-input-end comint-last-input-start)
        (when reason-interactive-read-only-input
          (add-text-properties
           comint-last-input-start comint-last-input-end
           (list 'read-only t)))
        (when (and font-lock-mode reason-interactive-input-font-lock)
          (font-lock-fontify-region comint-last-input-start
                                    comint-last-input-end))
        (when reason-interactive-output-font-lock
          (save-excursion
            (goto-char (point-max))
            (re-search-backward comint-prompt-regexp
                                comint-last-input-end t)
            (add-text-properties
             comint-last-input-end (point)
             '(font-lock-face reason-font-lock-interactive-output-face))))
        (when reason-interactive-error-font-lock
          (save-excursion
            (goto-char comint-last-input-end)
            (cond
             ((looking-at reason-interactive-error-range-regexp)
              (let ((beg (string-to-number (match-string-no-properties 1)))
                    (end (string-to-number (match-string-no-properties 2))))
                (put-text-property
                 (+ comint-last-input-start beg)
                 (+ comint-last-input-start end)
                 'font-lock-face 'reason-font-lock-error-face))
              (goto-char comint-last-input-end)
              (when (re-search-forward reason-interactive-error-regexp nil t)
                (let ((errbeg (match-beginning 1))
                      (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'reason-font-lock-interactive-error-face))))
             ((looking-at reason-interactive-exception-regexp)
              (let ((errbeg (match-beginning 1))
                    (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'reason-font-lock-interactive-error-face)))
             )))))))

(easy-menu-define
  reason-interactive-mode-menu reason-interactive-mode-map
  "Reason Interactive Mode Menu."
  '("Reason"
    ("Interactive Mode"
     ["Run ReasonML REPL" reason-run-reason t]
     ["Interrupt ReasonML REPL" reason-interrupt-reason
      :active (comint-check-proc reason-interactive-buffer-name)]
     ["Kill ReasonML REPL" reason-kill-reason
      :active (comint-check-proc reason-interactive-buffer-name)]
     ["Evaluate Region" reason-eval-region :active (region-active-p)]
     ["Evaluate Phrase" reason-eval-phrase t]
     ["Evaluate Buffer" reason-eval-buffer t])
    "---"
    ["Customize Reason Mode..." (customize-group 'reason) t]
    ("Reason Options" ["Dummy" nil t])
    ("Reason Interactive Options" ["Dummy" nil t])
    "---"
    ["About" reason-about t]
    ["Help" reason-interactive-help t]))

(define-derived-mode reason-interactive-mode comint-mode "Reason-Interactive"
  "Major mode for interacting with an ReasonML process.
Runs an ReasonML REPL as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in reason mode.

Short cuts for interactions with the REPL:
\\{reason-interactive-mode-map}"
  ;;;---
  ;; (add-hook 'comint-output-filter-functions #'reason-interactive-filter)
  (setq comint-prompt-regexp "^#  *")
  (setq comint-process-echoes nil)
  (setq comint-get-old-input 'reason-interactive-get-old-input)
  (setq comint-scroll-to-bottom-on-output
        reason-interactive-scroll-to-bottom-on-output)
  (set-syntax-table reason-mode-syntax-table)
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comint-prompt-read-only t)

  ;; (reason--common-mode-setup)
  ;; (reason--install-font-lock t)
  (when (or reason-interactive-input-font-lock
            reason-interactive-output-font-lock
            reason-interactive-error-font-lock)
    (font-lock-mode 1))

  (easy-menu-add reason-interactive-mode-menu)
  ;; (reason-update-options-menu)
  )

;;;###autoload
(defun reason-run-reason ()
  "Run an ReasonML REPL process.  I/O via buffer `*ReasonML*'."
  (interactive)
  (reason-run-process-if-needed)
  (display-buffer reason-interactive-buffer-name))

;;;###autoload
(defalias 'run-reason 'reason-run-reason)

;;;###autoload
(add-to-list 'interpreter-mode-alist '("reasonrun" . reason-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("reason" . reason-mode))

(defun reason-run-process-if-needed (&optional cmd)
  "Run an ReasonML REPL process if needed, with an optional command name.
I/O via buffer `*ReasonML*'."
  (if cmd
      (setq reason-interactive-program cmd)
    (unless (comint-check-proc reason-interactive-buffer-name)
      (setq reason-interactive-program
            (read-shell-command "ReasonML REPL to run: "
                                reason-interactive-program))))
  (unless (comint-check-proc reason-interactive-buffer-name)
    (let ((cmdlist (reason--split-args reason-interactive-program))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint) "ReasonML"
                         (car cmdlist) nil (cdr cmdlist)))
      (reason-interactive-mode)
      (sleep-for 1))))

(defun reason--split-args (args)
  (condition-case nil
      (split-string-and-unquote args)
      (error (progn
               (message "Arguments ‘%s’ ill quoted.  Ignored." args)
               nil))))

(defun reason-interactive-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (re-search-backward comint-prompt-regexp (point-min) t)
      (when (looking-at comint-prompt-regexp)
        (re-search-forward comint-prompt-regexp))
      (buffer-substring-no-properties (point) end))))

(defconst reason-interactive--send-warning
  "Note: REPL processing requires a terminating `;', or use S-return.")

(defun reason-interactive--indent-line ()
  (insert "\n")
  (indent-according-to-mode)
  (message reason-interactive--send-warning))

(defun reason-interactive-send-input ()
  "Send the current phrase to the ReasonML REPL or insert a newline.
If the point is next to \";\", the phrase is sent to the REPL,
otherwise a newline is inserted and the lines are indented."
  (interactive)
  (cond
   ((reason-in-literal-or-comment-p) (reason-interactive--indent-line))
   (;;;---
    ;; (or (equal ";" (save-excursion (nth 2 (smie-backward-sexp))))
    ;;     (looking-at-p "[ \t\n\r]*;"))
    ;;;+++
    (looking-at-p "[ \t\n\r]*;")
    (comint-send-input))
   (t
    ;;;---
    ;; (reason-interactive--indent-line)
    ;;;+++
    (reason-interactive-send-input-end-of-phrase))))

(defun reason-interactive-send-input-end-of-phrase ()
  (interactive)
  (goto-char (point-max))
  ;;;---
  ;; (unless (equal ";" (save-excursion (nth 2 (smie-backward-sexp))))
  ;;   (insert ";"))
  (comint-send-input))

(defun reason-interactive--send-region (start end)
  "Send the region between START and END to the ReasonML REPL.
It is assumed that the range START-END delimit valid ReasonML phrases."
  (save-excursion (reason-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (let* ((phrases (buffer-substring-no-properties start end))
         (phrases (replace-regexp-in-string "[ \t\n]*\\(;[ \t\n]*\\)?\\'" ""
                                            phrases))
         (phrases-colon (concat phrases ";")))
    (if (string= phrases "")
        (message "Cannot send empty commands to ReasonML REPL!")
      (with-current-buffer reason-interactive-buffer-name
        (goto-char (point-max))
        (setq reason-interactive-last-phrase-pos-in-repl (point))
        (comint-send-string reason-interactive-buffer-name phrases-colon)
        (let ((pos (point)))
          (comint-send-input)
          (when reason-interactive-echo-phrase
            (save-excursion
              (goto-char pos)
              (insert phrases-colon)))))))
  (when reason-display-buffer-on-eval
    (display-buffer reason-interactive-buffer-name)))

(defun reason-eval-region (start end)
  "Eval the current region in the ReasonML REPL."
  (interactive "r")
  (setq reason-interactive-last-phrase-pos-in-source start)
  (setq start (car (reason-discover-phrase start)))
  (if start
      (progn
        (setq end (cadr (reason-discover-phrase end)))
        (if end
            (reason-interactive--send-region start end)
          (message "The expression after the point is not well braced.")))
    (message "The expression after the point is not well braced.")))

(defun reason-narrow-to-phrase ()
  "Narrow the editting window to the surrounding ReasonML phrase (or block)."
  (interactive)
  (let ((phrase (reason-discover-phrase)))
    (if phrase
        (narrow-to-region (nth 0 phrase) (nth 1 phrase)))))

(defun reason--after-double-colon ()
  "Non nil if the current position is after or inside ';'.  In
this case, the returned value is the position before ';' (unless
it is the first position of the buffer)."
  (save-excursion
    (when (looking-at-p "[;[:blank:]]*$")
      (skip-chars-backward ";[:blank:]")
      (if (> (point) 1) (- (point) 1)))))

(defun reason-eval-phrase ()
  "Eval the surrounding ReasonML phrase (or block) in the ReasonML REPL."
  (interactive)
  (let* ((pos (reason--after-double-colon))
         (pos (if pos pos (point)))
         (phrase (reason-discover-phrase pos)))
    (if phrase
        (progn
          (reason-interactive--send-region (nth 0 phrase) (nth 1 phrase))
          (when reason-skip-after-eval-phrase
            (goto-char (nth 2 phrase))
            (reason--skip-double-colon)
            (reason-skip-blank-and-comments)))
      (message "The expression after the point is not well braced."))))

(defun reason-eval-buffer ()
  "Send the buffer to the Reason Interactive process."
  (interactive)
  (reason-interactive--send-region (point-min) (point-max)))

(defvar reason-interactive-next-error-olv (make-overlay 1 1))

(overlay-put reason-interactive-next-error-olv
             'face 'reason-font-lock-error-face)

(delete-overlay reason-interactive-next-error-olv)

(defun reason-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (with-current-buffer reason-interactive-buffer-name
      (goto-char reason-interactive-last-phrase-pos-in-repl)
      (setq error-pos
            (re-search-forward reason-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (match-string-no-properties 1))
              end (string-to-number (match-string-no-properties 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ reason-interactive-last-phrase-pos-in-source beg)
            end (+ reason-interactive-last-phrase-pos-in-source end))
      (goto-char beg)
      (move-overlay reason-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay reason-interactive-next-error-olv))
      )))

(defun reason-interactive-next-error-repl ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char reason-interactive-last-phrase-pos-in-repl)
      (setq error-pos
            (re-search-forward reason-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (match-string-no-properties 1))
              end (string-to-number (match-string-no-properties 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ reason-interactive-last-phrase-pos-in-repl beg)
            end (+ reason-interactive-last-phrase-pos-in-repl end))
      (move-overlay reason-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay reason-interactive-next-error-olv))
      (goto-char beg))))

(defun reason-interrupt-reason ()
  (interactive)
  (when (comint-check-proc reason-interactive-buffer-name)
    (with-current-buffer reason-interactive-buffer-name
      (comint-interrupt-subjob))))

(defun reason-kill-reason ()
  (interactive)
  (when (comint-check-proc reason-interactive-buffer-name)
    (with-current-buffer reason-interactive-buffer-name
      (comint-kill-subjob))))

(provide 'reason-interactive)
