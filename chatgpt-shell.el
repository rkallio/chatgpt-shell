;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))

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

;;; Code:

(require 'comint)
(require 'map)
(require 'seq)

(eval-when-compile
  (require 'cl-lib))

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell--request-timeout 30
  "Timeout request after this many seconds."
  :type 'integer
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

(defcustom chatgpt-shell-stream nil
  "If set, completion will be received and displayed as deltas.

Not implemented, so does not do anything.

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
    (define-key map "\C-m" 'chatgpt-shell-return)
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
  (setq comint-prompt-regexp (concat "^" (regexp-quote chatgpt-shell--prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'chatgpt-shell--input-sender)
  (setq comint-process-echoes nil)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'chatgpt-shell--get-old-input)
  (setq-local comint-completion-addsuffix nil)

  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "chatgpt" (current-buffer) "hexl")
      (file-error (start-process "chatgpt" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (chatgpt-shell--process) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (chatgpt-shell--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt)
    (set-marker comint-last-input-start (chatgpt-shell--pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter))

  (font-lock-add-keywords nil chatgpt-shell-font-lock-keywords))

(defun chatgpt-shell-return ()
  "RET binding."
  (interactive)
  (chatgpt-shell--send-input))

(defun chatgpt-shell-send-to-buffer (text &optional submit save-excursion)
  "Send TEXT to *chatgpt* buffer.
Set SUBMIT to automatically submit to ChatGPT.
Set SAVE-EXCURSION to prevent point from moving."
  (chatgpt-shell)
  (switch-to-buffer (chatgpt-shell--buffer))
  (with-current-buffer (chatgpt-shell--buffer)
    (goto-char (point-max))
    (if save-excursion
        (save-excursion
          (insert text))
      (insert text))
    (when submit
      (chatgpt-shell--send-input))))

(defun chatgpt-shell--eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  (cond
   ((string-equal "clear" (string-trim input-string))
    (call-interactively #'comint-clear-buffer)
    (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt))
   ((string-empty-p (string-trim input-string))
    (comint-output-filter (chatgpt-shell--process)
                          (concat "\n" chatgpt-shell--prompt)))
   (t
    ;; For viewing prompt delimiter (used to handle multiline prompts).
    ;; (comint-output-filter (chatgpt-shell--process) "<gpt-end-of-prompt>")
    (comint-output-filter (chatgpt-shell--process)
                          (propertize "<gpt-end-of-prompt>"
                                      'invisible (not chatgpt-shell--show-invisible-markers)))
    (chatgpt-shell--request-completion))))

(defun chatgpt-shell--request-options ()
  "Create a request options alist.
This is data that will be sent in the request body of a HTTP
request."
  (let ((request-data))
    (when chatgpt-shell-model-temperature
      (push (cons 'temperature chatgpt-shell-model-temperature) request-data))
    (when chatgpt-shell-model-top-p
      (push (cons 'top-p chatgpt-shell-model-top-p) request-data))
    (when chatgpt-shell-model-max-tokens
      (push (cons 'max-tokens chatgpt-shell-model-max-tokens) request-data))
    (when chatgpt-shell-model-presence-penalty
      (push (cons 'presence_penalty chatgpt-shell-model-presence-penalty) request-data))
    (when chatgpt-shell-model-frequency-penalty
      (push (cons 'frequency_penalty chatgpt-shell-model-frequency-penalty) request-data))
    (push (cons 'model chatgpt-shell-model-version) request-data)))

;; Maybe I should be a macro (get rid of callback), maybe not
(defun chatgpt-shell--request-completion ()
  "Request a completion.

KEY is API key.  CALLBACK is called with a parsed response body,
where objects are converted into alists."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " chatgpt-shell-openai-key))
            ("Content-Type" . "application/json")))
         (messages
          (vconcat
           (last (chatgpt-shell--extract-prompts-and-completions)
                 (if (null chatgpt-shell-transmitted-context-length)
                     ;; If variable above is nil, send "full" context.
                     ;; Arbitrarily chosen big number here to signify
                     ;; it
                     2048
                   ;; Send in pairs of prompt and completion by
                   ;; multiplying by 2
                   (1+ (* 2 chatgpt-shell-transmitted-context-length))))))
         (request-data (append
                            (chatgpt-shell--request-options)
                            `((messages . ,messages))))
         (url-request-data (concat (json-encode request-data) "\n")))
    (let ((processing-buffer
           (condition-case err
               (url-retrieve chatgpt-shell--api-endpoint
                             #'chatgpt-shell--url-retrieve-callback)
             (error (chatgpt-shell--write-reply (error-message-string err) t)))))
      (run-with-timer chatgpt-shell--request-timeout nil #'chatgpt-shell--check-on-request processing-buffer))))

(defun chatgpt-shell--check-on-request (url-process-buffer)
  "Check on the status of the HTTP request.

URL-PROCESS-BUFFER is the buffer that is associated with the
request process."
  (with-current-buffer url-process-buffer
    (let ((process (get-buffer-process (current-buffer))))
      (condition-case nil
          (delete-process process)
        (error nil)))))

(defun chatgpt-shell--url-retrieve-callback (status &optional cbargs)
  ""
  (let ((status (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer))))
    ;; Something went wrong in the request, either here or on the
    ;; server, but at least we got a response
    (search-forward "\n\n")
    (let ((response (json-parse-buffer :object-type 'alist)))
      (chatgpt-shell--write-reply
       (string-trim
        (map-elt
         (map-elt
          (seq-first
           (map-elt response 'choices))
          'message) 'content)))
    (with-current-buffer "*chatgpt*"
      (let* ((usage (alist-get 'usage response))
             (prompt-tokens (cdar usage))
             (completion-tokens (cdadr usage))
             (total-tokens (cdaddr usage)))
        (setq chatgpt-shell--total-tokens
              (+ total-tokens chatgpt-shell--total-tokens))
        (setq header-line-format
              (format " Tokens P %s + C %s = %s, Session %s ($%s)"
                      prompt-tokens
                      completion-tokens
                      total-tokens
                      chatgpt-shell--total-tokens
                      (* chatgpt-shell--total-tokens 2e-06))))))))

(defun chatgpt-shell--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (chatgpt-shell--buffer))) pos))

(defun chatgpt-shell--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (chatgpt-shell--buffer))))

(defun chatgpt-shell--input-sender (_proc input)
  "Set the variable `chatgpt-shell--input' to INPUT.
Used by `chatgpt-shell--send-input's call."
  (setq chatgpt-shell--input input))

(defun chatgpt-shell--send-input ()
  "Send text after the prompt."
  (interactive)
  (let (chatgpt-shell--input)
    (comint-send-input)
    (chatgpt-shell--eval-input chatgpt-shell--input)))

(defun chatgpt-shell--write-reply (reply &optional failed)
  "Write REPLY to prompt.  Set FAILED to record failure."
  (comint-output-filter (chatgpt-shell--process)
                        (concat "\n"
                                (string-trim reply)
                                (if failed
                                    (propertize "\n<gpt-ignored-response>"
                                                'invisible (not chatgpt-shell--show-invisible-markers))
                                  "")
                                "\n\n"
                                chatgpt-shell--prompt)))

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
    (with-current-buffer (chatgpt-shell--buffer)
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
                          chatgpt-shell--prompt)))
    (nreverse result)))

(defun chatgpt-shell--buffer ()
  "Get *chatgpt* buffer."
  (get-buffer-create "*chatgpt*"))

(defun chatgpt-shell--process nil
  "Get *chatgpt* process."
  (get-buffer-process (chatgpt-shell--buffer)))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
