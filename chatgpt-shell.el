;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez, Roni Kallio

;; Author: Alvaro Ramirez
;;     Roni Kallio <roni.jj.kallio@gmail.com>
;; Maintainer: Roni Kallio <roni.jj.kallio@gmail.com>
;; URL: https://github.com/rkallio/chatgpt-shell
;; Upstream-URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.4
;; Package-Requires: ((emacs "28.2"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Note: This is very much a proof of concept (and very rough!).  Much
;; of the code is based on `ielm'.
;;
;; You must set `chatgpt-shell-openai-key' to your key before using.
;;
;; Run `chatgpt-shell' to get a ChatGPT shell.

;;; Known issues:

;; * Sometimes the first token of a completion disappears on
;;   transport.

;;; Code:

(require 'comint)
(require 'map)
(require 'seq)

(eval-when-compile
  (require 'cl-lib))

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice string
                 function)
  :group 'chatgpt-shell)

(defcustom chatgpt-shell--request-timeout 60
  "Timeout request after this many seconds."
  :type 'number
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version "gpt-3.5-turbo"
  "The used OpenAI model.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-max-tokens nil
  "The maximum number of tokens to generate in the completion.

For chatgpt-3.5-turbo, the maximum token count is 4096.  Both the
prompt and the completion have to fit into that limit.  Note that
the cost for chatgpt-3.5-turbo is currently at $0.002 cents/1000
tokens, so a maximally long completion will cost about $0.008.

Value may be nil, in which case the API decides maximum token
length.

See https://platform.openai.com/docs/api-reference/completions\
/create#completions/create-max_tokens for details."
  :type '(choice integer
                 (const nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-temperature nil
  "What sampling temperature to use, between 0 and 2, or nil.

Higher values like 0.8 will make the output more random, while
lower values like 0.2 will make it more focused and
deterministic.  Value of nil will not pass this configuration to
the model.  Alter this or `chatgpt-shell-model-top-p', but not
both.

See
https://platform.openai.com/docs/api-reference/completions\
/create#completions/create-temperature
for details."
  :type '(choice number
                 (const nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-top-p nil
  "An alternative to sampling with temperature.

In nucleus sampling, the model considers the results of the
tokens with `chatgpt-shell-model-top-p' probability mass.  So 0.1
means only the tokens comprising the top 10% probability mass are
considered.  Alter this or `chatgpt-shell-model-temperature', but not
both.

See https://platform.openai.com/docs/api-reference/\
completions/create#completions/create-top_p for details."
  :type '(choice number
                 (const nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-stream t
  "If set, completion will be received and displayed as deltas.

See https://platform.openai.com/docs/api-reference/chat\
/create#chat/create-stream for details."
  :type 'boolean
  :group 'chatgpt-shell)

(defvar chatgpt-shell-model-stop nil
  "Not implemented.

See https://platform.openai.com/docs/api-reference/chat\
/create#chat/create-stop for details.")

(defcustom chatgpt-shell-model-presence-penalty nil
  "Number between -2.0 and 2.0.
Positive values penalize new tokens based on whether they appear
in the text so far, increasing the model's likelihood to talk
about new topics.

See https://platform.openai.com/docs/api-reference/chat\
/create#chat/create-presence_penalty for details."
  :type '(choice number
                 (const nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-frequency-penalty nil
  "Number between -2.0 and 2.0.
Positive values penalize new tokens based on their existing
frequency in the text so far, decreasing the model's likelihood
to repeat the same line verbatim."
  :type '(choice number
                 (const nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-transmitted-context-length nil
  "Controls the amount of context provided to chatGPT.

This context needs to be transmitted to the API on every request.
ChatGPT reads the provided context on every request, which will
consume more and more prompt tokens as your conversation grows.
Models do have a maximum token limit, however.

A value of nil will send full chat history (the full contents of
the comint buffer), to ChatGPT.

A value of 0 will not provide any context.  This is the cheapest
option, but ChatGPT can't look back on your conversation.

A value of 1 will send only the latest prompt-completion pair as
context.

A Value >1 will send that amount of prompt-completion pairs to
ChatGPT."
  :type '(choice integer
                 (const nil))
  :group 'chatgpt-shell)

(defvar chatgpt-shell--input)

(defvar-local chatgpt-shell--prompt "ChatGPT> ")

(defvar chatgpt-shell--api-endpoint "https://api.openai.com/v1/chat/completions")

(defvar chatgpt-shell--total-tokens 0)

(defvar chatgpt-shell--last-response nil)

(defvar chatgpt-shell--show-invisible-markers nil)

(defvaralias 'inferior-chatgpt-mode-map 'chatgpt-shell-map)

(defconst chatgpt-shell-font-lock-keywords
  '(;; Markdown triple backticks
    ("\\(^\\(```\\)[^`\n]*\n\\)\\(\\(?:.\\|\n\\)*?\\)\\(^\\(```\\)$\\)"
     (3 'shadow))
    ;; Markdown single backticks
    ("`\\([^`\n]+\\)`"
     (1 'shadow))))

(defvar chatgpt-shell-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'gpt-return)
    map)
  "Keymap for ChatGPT mode.")

(defalias 'chatgpt-shell-clear-buffer 'comint-clear-buffer)

;;;###autoload
(defun chatgpt-shell ()
  "Start a ChatGPT shell."
  (interactive)
  (let ((old-point)
        (buf-name "*chatgpt*"))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create "*chatgpt*")
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (inferior-chatgpt-mode)))
    (pop-to-buffer-same-window buf-name)
    (when old-point
      (push-mark old-point))))

(define-derived-mode inferior-chatgpt-mode comint-mode "CHATGPT"
  "Major mode for interactively evaluating ChatGPT prompts.
Uses the interface provided by `comint-mode'"
  (visual-line-mode)
  (setq comint-prompt-regexp (concat "^" (regexp-quote gpt--prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'gpt--input-sender)
  (setq comint-process-echoes nil)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'gpt--get-old-input)
  (setq-local comint-completion-addsuffix nil)

  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "chatgpt" (current-buffer) "hexl")
      (file-error (start-process "chatgpt" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (gpt--process) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (gpt--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (gpt--process) gpt--prompt)
    (set-marker comint-last-input-start (gpt--pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter))

  (font-lock-add-keywords nil gpt-font-lock-keywords))

(defun chatgpt-shell-return ()
  "RET binding."
  (interactive)
  (gpt--send-input))

(defun chatgpt-shell-send-to-buffer (text &optional submit save-excursion)
  "Send TEXT to *chatgpt* buffer.
Set SUBMIT to automatically submit to ChatGPT.
Set SAVE-EXCURSION to prevent point from moving."
  (chatgpt-shell)
  (switch-to-buffer (gpt--buffer))
  (with-current-buffer (gpt--buffer)
    (goto-char (point-max))
    (if save-excursion
        (save-excursion
          (insert text))
      (insert text))
    (when submit
      (gpt--send-input))))

(defun chatgpt-shell--eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  (cond
   ((string-equal "clear" (string-trim input-string))
    (call-interactively #'comint-clear-buffer)
    (comint-output-filter (gpt--process) gpt--prompt))
   ((string-empty-p (string-trim input-string))
    (comint-output-filter (gpt--process) gpt--prompt))
   (t
    ;; For viewing prompt delimiter (used to handle multiline prompts).
    ;; (comint-output-filter (chatgpt-shell--process) "<gpt-end-of-prompt>")
    (comint-output-filter (gpt--process)
                          (propertize "<gpt-end-of-prompt>"
                                      'invisible (not gpt--show-invisible-markers)))
    (gpt--request-completion))))

(defun chatgpt-shell--request-options ()
  "Create a request options alist.
This is data that will be sent in the request body of a HTTP
request."
  (let ((request-data))
    (when gpt-model-temperature
      (push (cons 'temperature gpt-model-temperature) request-data))
    (when gpt-model-top-p
      (push (cons 'top-p gpt-model-top-p) request-data))
    (when gpt-model-max-tokens
      (push (cons 'max-tokens gpt-model-max-tokens) request-data))
    (when gpt-model-presence-penalty
      (push (cons 'presence_penalty gpt-model-presence-penalty) request-data))
    (when gpt-model-frequency-penalty
      (push (cons 'frequency_penalty gpt-model-frequency-penalty) request-data))
    (push (cons 'stream gpt-stream) request-data)
    (push (cons 'model gpt-model-version) request-data)))

(defun chatgpt-shell--request-completion ()
  "Request a completion."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (gpt--openai-key)))
            ("Content-Type" . "application/json")))
         (messages
          (last (gpt--extract-prompts-and-completions)
                (if (null gpt-transmitted-context-length)
                    most-positive-fixnum
                  (1+ (* 2 gpt-transmitted-context-length)))))
         (request-data (append
                        (gpt--request-options)
                        `((messages . ,messages))))
         (url-request-data (encode-coding-string (json-encode request-data) 'utf-8 t)))
    (if (alist-get 'stream request-data)
        ;; streamed response
        (with-current-buffer (url-retrieve gpt--api-endpoint #'gpt--url-retrieve-stream-callback)
          (add-hook 'after-change-functions #'gpt--check-on-streamed-request nil t))
      ;; non-streamed response
      (run-with-timer gpt--request-timeout nil #'gpt--check-on-request
                      (url-retrieve gpt--api-endpoint #'gpt--url-retrieve-callback)))))

(defun chatgpt-shell--url-retrieve-callback (_status)
  (search-forward "\n\n")
  (let* ((response (json-parse-buffer :object-type 'alist))
         (completion (gpt--first-completion response))
         (tokens (gpt--read-tokens response)))
    (setq gpt--total-tokens (+ (caddr tokens) gpt--total-tokens))
    (with-current-buffer "*chatgpt*"
      (setq header-line-format
            (format " Tokens P %s C %s, Session %s ($%.2f)"
                    (car tokens)
                    (cadr tokens)
                    gpt--total-tokens
                    (* gpt--total-tokens 2e-06))))
    (let ((proc (gpt--process)))
      (comint-output-filter proc completion)
      (comint-output-filter proc (concat "\n" gpt--prompt)))))

(defun chatgpt-shell--check-on-request (url-process-buffer)
  "Check on the status of the HTTP request.

URL-PROCESS-BUFFER is the buffer that is associated with the
request process."
  (with-current-buffer url-process-buffer
    (let ((process (get-buffer-process (current-buffer))))
      (condition-case nil
          (delete-process process)
        (error nil)))))

(defun chatgpt-shell--url-retrieve-stream-callback (_status)
  (with-current-buffer (chatgpt-shell--buffer)
    (font-lock-update)))

(defun chatgpt-shell--check-on-streamed-request (begin end _pre-change-length)
  "Check on the status of the HTTP request.
Called whenever new data is provided.  BEGIN is the position of
the beginning of the changed text, END is the end position."
  (let ((delta (buffer-substring begin end)))
    (with-temp-buffer
      (insert delta)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (when (search-forward "data: {" nil t)
        (backward-char)
        (condition-case nil
            (let* ((body (json-parse-buffer :object-type 'alist))
                   (choices (alist-get 'choices body))
                   (first (elt choices 0))
                   (delta (alist-get 'delta first))
                   (finish-reason (alist-get 'finish_reason first))
                   (content (alist-get 'content delta)))
              (when content
                (comint-output-filter (gpt--process) content))
              (when (string= finish-reason "stop")
                (comint-output-filter (gpt--process) (concat "\n" gpt--prompt)))))))))

(defun chatgpt-shell--first-completion (completion-response)
  "Access the first completion in COMPLETION-RESPONSE."
  (let* ((choices (alist-get 'choices completion-response))
         (first-choice (elt choices 0))
         (message (alist-get 'message first-choice))
         (content (alist-get 'content message)))
    content))

(defun chatgpt-shell--read-tokens (completion-response)
  "Acess tokens in COMPLETION-RESPONSE.

Prompt tokens will be stored in `car', completion tokens in
`cadr', and total tokens in `caddr' of the returned list."
  (let* ((usage (alist-get 'usage completion-response)))
    (list
     (alist-get 'prompt_tokens usage)
     (alist-get 'completion_tokens usage)
     (alist-get 'total_tokens usage))))

(defun chatgpt-shell--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (gpt--process)) pos))

(defun chatgpt-shell--pm nil
  "Return the process mark of the current buffer."
  (process-mark (gpt--process)))

(defun chatgpt-shell--input-sender (_proc input)
  "Set the variable `chatgpt-shell--input' to INPUT.
Used by `chatgpt-shell--send-input's call."
  (setq gpt--input input))

(defun chatgpt-shell--send-input ()
  "Send text after the prompt."
  (interactive)
  (let (gpt--input)
    (comint-send-input)
    (gpt--eval-input gpt--input)))

(defun chatgpt-shell--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun chatgpt-shell--extract-prompts-and-completions ()
  "Extract all command and responses in buffer."
  (let ((result))
    (with-current-buffer (gpt--buffer)
      (mapc (lambda (item)
              (let* ((values (split-string item "<gpt-end-of-prompt>"))
                     (lines (split-string item "\n"))
                     (prompt (string-trim (nth 0 values)))
                     (response (string-trim (progn
                                              (if (> (length values) 1)
                                                  (nth 1 values)
                                                (string-join
                                                 (cdr lines) "\n"))))))
                (unless (string-match "<gpt-ignored-response>" response)
                  (when (not (string-empty-p prompt))
                    (push (list (cons 'role "user")
                                (cons 'content prompt)) result))
                  (when (not (string-empty-p response))
                    (push (list (cons 'role "system")
                                (cons 'content response)) result)))))
            (split-string (substring-no-properties (buffer-string))
                          gpt--prompt)))
    (nreverse result)))

(defun chatgpt-shell--buffer ()
  "Get *chatgpt* buffer."
  (get-buffer-create "*chatgpt*"))

(defun chatgpt-shell--process nil
  "Get *chatgpt* process."
  (get-buffer-process (gpt--buffer)))

(defun chatgpt-shell--openai-key ()
  "Extract key from `chatgpt-shell-openai-key'."
  (pcase chatgpt-shell-openai-key
    ((pred functionp) (funcall chatgpt-shell-openai-key))
    ((pred stringp) chatgpt-shell-openai-key)
    (_ (error "Set `chatgpt-shell-openai-key' to a string or a function"))))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("gpt-" . "chatgpt-shell-"))
;; End:
