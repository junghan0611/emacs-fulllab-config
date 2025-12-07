;;; +gptel.el -*- lexical-binding: t; -*-

;;;; gptel tips from agzam
;; /home/junghan/sync/man/dotsamples/doom/agzam-dot-doom/modules/custom/ai/autoload/gptel.el

(defvar +gptel-improve-text-prompt nil)

(defvar +gptel-improve-text-prompts-history
  (list
   (concat "You are a spelling corrector and text improver. "
           "Only correct mistakes, do not alter the text structure unless stylistic, "
           "orthographic, morphologic and other linguistic errors found. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are a fact-checker and text enhancer. "
           "Fix mistakes and flag factual inaccuracies, do not alter the text structure "
           "unless it is absolutely necessary. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are spelling corrector and text enhancer. "
           "Provide 3 different improved variations of the given text, "
           "separating each variant with: "
           "\n\n---\n\n"
           "Do not include any explanations, titles, headers or bullet points "
           "- ONLY plain text of variants, nothing else!")

   (concat "You are an experienced software developer. Explain the given code snippet, "
           "diving into technical details for better understanding. "
           "Suggest a better approach if necessary. "
           "Strive for concise code that is easy-to-reason about. "
           "Optionally, recommend libraries, tools and literature for better "
           "understanding the problem and improving upon it.")

   (concat "You are a great software developer. "
           "You strive for simplicity in your code "
           "that is both concise and easy-to-reason about. "
           "Add comments to the provide code snippet, without changing the code itself."
           "Do not include any headers, titles or explanations outside of the snippet, "
           "keep the three ticks with the language designator (markdown code markup).")))

(defun +replace-region-with-string (replacement buffer beg end)
  "Replace region or buffer content with REPLACEMENT."
  (with-current-buffer buffer
    (delete-region beg end)
    (insert replacement)
    (insert "\n")))

(transient-define-infix +gptel--improve-text-infix-prompt ()
  "Prompt selection for improving text."
  :description "Set prompt"
  :prompt "Prompt: "
  :variable '+gptel-improve-text-prompt
  :class 'transient-lisp-variable
  :key "RET"
  :format "%k %d"
  :reader (lambda (prompt &rest _)
            ;; usual bs to keep the preserve the list order
            (let* ((comp-table (lambda (cs)
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (display-sort-function . ,#'identity))
                                     (complete-with-action action cs str pred)))))
                   (sel (completing-read
                         prompt
                         (funcall
                          comp-table
                          +gptel-improve-text-prompts-history))))
              (add-to-list '+gptel-improve-text-prompts-history
                           sel)
              sel)))

(require 'gptel-transient)

;;;###autoload
(transient-define-prefix +gptel-improve-text-transient ()
  "Improve region with gptel."
  [:description
   (lambda ()
     (concat
      (or +gptel-improve-text-prompt
          (car +gptel-improve-text-prompts-history)) "\n"))
   [(gptel--infix-provider)
    (+gptel--improve-text-infix-prompt)]
   [""
    ("C-<return>" "Let's go" +gptel-improve-text)]])

;;;###autoload
(defun +gptel-improve-text (&optional arg)
  (interactive "P")
  (unless (region-active-p)
    (user-error "no selection"))
  (setq +gptel-improve-text-prompt (or +gptel-improve-text-prompt
                                       (car +gptel-improve-text-prompts-history)))
  (let* ((buffer (current-buffer))
         (beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end))
         (in-place? (string-match-p
                     "fix mistakes\\|correct mistakes"
                     +gptel-improve-text-prompt)))
    (message "beep-bop... checking your crap with %s" gptel-model)
    (gptel-request text
      :system +gptel-improve-text-prompt
      :buffer buffer
      :callback
      (lambda (resp info)
        (let* ((model (let-plist info .data.model)))
          (cond
           (in-place?
            (let* ((_ (+replace-region-with-string resp buffer beg end))
                   (_ (message "딱 그거야!")) ; ¡Ahí está!
                   (fst-buf (with-current-buffer (generate-new-buffer (format "* %s 1 *" model))
                              (insert text)
                              (current-buffer)))
                   (snd-buf (with-current-buffer (generate-new-buffer (format "* %s 2 *" model))
                              (insert resp)
                              (current-buffer)))
                   (diff-win (diff fst-buf snd-buf "--text" 'no-async)))

              ;; cleaner diff
              (with-current-buffer (window-buffer diff-win)
                (read-only-mode -1)
                (goto-char (point-min))
                (dolist (r '("^diff.*\n"
                             "^. No newline at end of file\n"
                             "^. No newline at end of file\n"
                             "^Diff finished.*$"))
                  (re-search-forward r nil :noerror)
                  (replace-match ""))
                (visual-line-mode))
              (kill-buffer fst-buf)
              (kill-buffer snd-buf)))

           (t
            (let ((buf (generate-new-buffer (format "* %s *" model))))
              (with-current-buffer buf
                (markdown-mode)
                (insert resp))
              (switch-to-buffer-other-window buf)))))))))

;;;###autoload
(defun gptel-clear-buffer+ ()
  (interactive)
  (let* ((beg-marker (concat "^" (alist-get gptel-default-mode gptel-prompt-prefix-alist)))
         (keep-line (save-excursion
                      (goto-char (point-max))
                      (when (re-search-backward beg-marker nil t)
                        (unless (save-excursion
                                  (forward-line)
                                  (re-search-forward beg-marker nil t))
                          (point))))))
    (delete-region (point-min) keep-line)
    (evil-insert-state)))

;;;###autoload
(defun gptel+ (&optional arg)
  (interactive "P")
  (let ((last-b (thread-last
                  (buffer-list)
                  (seq-filter
                   (lambda (buf)
                     (buffer-local-value 'gptel-mode buf)))
                  (seq-sort
                   (lambda (a b)
                     (string> (buffer-name a) (buffer-name b))))
                  (seq-first))))
    (if (or arg (null last-b))
        (call-interactively #'gptel)
      (if (get-buffer-window last-b 'visible)
          (switch-to-buffer-other-window last-b)
        (switch-to-buffer last-b)))))

;; (defun gptel-persist-history+ ()
;;   "Save buffer to disk when starting gptel"
;;   (interactive)
;;   (unless (buffer-file-name (current-buffer))
;;     (let ((suffix (format-time-string "%Y%m%dT%H%M%S"))
;;           (chat-dir (concat org-directory "/llmlog"))
;;           (ext (replace-regexp-in-string "-mode$" "" (symbol-name gptel-default-mode))))
;;       (unless (file-directory-p chat-dir)
;;         (make-directory chat-dir :parents))
;;       (write-file
;;        (expand-file-name (concat suffix "__llmlog" "." ext) chat-dir)))))

;;;; gptel model descriptions

;;;;; gptel openrouter models

;; ~/sync/man/dotsamples/dotall/yqrashawn-dot-doom-clj/.doom.d/models.el

(defconst gptel--openrouter-models
  '(
    ;; https://openrouter.ai/provider/deepseek
    ;; Created Jan 20, 2025 163,840 context $0.40/M input tokens $2/M output tokens
    (deepseek/deepseek-r1-0528
     :capabilities (tool reasoning)
     :context-window 164
     :input-cost 0.55
     :output-cost 2.19)

    (google/gemini-3-pro-preview
     :capabilities (media tool-use cache reasoning)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1048
     :input-cost 2
     :output-cost 12
     :cutoff-date "2025-11")

    ;; https://openrouter.ai/google/gemini-2.5-pro
    (google/gemini-2.5-pro
     :capabilities (media tool-use cache reasoning)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1048
     :input-cost 1.25
     :output-cost 10)

    (google/gemini-2.5-flash
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1048
     :input-cost 0.30
     :output-cost 2.5)

    (openai/gpt-5.1
     :description
     "GPT-5.1 is the latest frontier-grade model in the GPT-5 series, offering stronger general-purpose reasoning, improved instruction adherence, and a more natural conversational style compared to GPT-5. It uses adaptive reasoning to allocate computation dynamically, responding quickly to simple queries while spending more depth on complex tasks. The model produces clearer, more grounded explanations with reduced jargon, making it easier to follow even on technical or multi-step problems. "
     :capabilities (media json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2025-11")

    ;; (openai/gpt-5-mini
    ;;  :description "Faster, more cost-efficient version of GPT-5"
    ;;  :capabilities (media json url)
    ;;  :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
    ;;  :context-window 400
    ;;  :input-cost 0.25
    ;;  :output-cost 2.0
    ;;  :cutoff-date "2025-08")

    ;; (openai/gpt-oss-120b
    ;;  :description "gpt-oss-120b is an open-weight, 117B-parameter Mixture-of-Experts (MoE) language model from OpenAI designed for high-reasoning, agentic, and general-purpose production use cases."
    ;;  :capabilities (media tool-use json url)
    ;;  :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
    ;;  :context-window 131
    ;;  :input-cost 0.04
    ;;  :output-cost 0.4
    ;;  :cutoff-date "2025-08")

    ;; https://openrouter.ai/anthropic/claude-sonnet-4
    ;; (anthropic/claude-sonnet-4
    ;;  :description "Hybrid model capable of standard thinking and extended thinking modes"
    ;;  :capabilities (media tool-use cache)
    ;;  :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
    ;;  :context-window 200
    ;;  :input-cost 3
    ;;  :output-cost 15
    ;;  :cutoff-date "2025-05")

    ;; (anthropic/claude-opus-4.1
    ;;  :description "Hybrid model capable of standard thinking and extended thinking modes"
    ;;  :capabilities (media tool-use cache)
    ;;  :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
    ;;  :context-window 200
    ;;  :input-cost 15
    ;;  :output-cost 75
    ;;  :cutoff-date "2025-07")
    ))

;;;; openrouter

;; google/gemini-2.5-flash
;; google/gemini-2.5-pro
;; anthropic/claude-sonnet-4
;; anthropic/claude-3.5-haiku
;; deepseek/deepseek-chat
;; deepseek/deepseek-r1-0528

;; anthropic/claude-3.7-sonnet
;; openai/gpt-4.1
;; openai/gpt-4o-mini
;; qwen/qwen-2.5-7b-instruct

;;~/sync/man/dotsamples/doom/rajp152k-dot-all/doom/.config/doom/config.el
;; (openai/gpt-4.1
;;                     openai/gpt-4.1-nano
;;                     openai/gpt-4.1-mini
;;                     openai/o4-mini-high
;;                     openai/o4-mini

;;                     meta-llama/llama-4-maverick:free
;;                     meta-llama/llama-4-maverick
;;                     meta-llama/llama-4-scout:free
;;                     meta-llama/llama-4-scout

;;                     deepseek/deepseek-chat
;;                     deepseek/deepseek-chat-v3-0324
;;                     deepseek/deepseek-r1
;;                     deepseek/deepseek-r1-distill-llama-70b

;;                     mistralai/mixtral-8x7b-instruct
;;                     mistralai/codestral-2501
;;                     mistralai/codestral-mamba
;;                     mistralai/ministral-8b
;;                     mistralai/mistral-small-3.1-24b-instruct
;;                     mistralai/mistral-saba

;;                     anthropic/claude-3.7-sonnet:thinking
;;                     anthropic/claude-3.7-sonnet
;;                     anthropic/claude-3.5-haiku

;;                     google/gemini-2.5-flash-preview:thinking
;;                     google/gemini-2.5-flash-preview
;;                     google/gemini-2.5-pro-preview-03-25

;;                     qwen/qwen3-30b-a3b
;;                     qwen/qwen3-8b:free
;;                     qwen/qwen3-14b
;;                     qwen/qwen3-32b
;;                     qwen/qwen3-235b-a22b

;;                     x-ai/grok-3-mini-beta
;;                     x-ai/grok-3-beta)

;; ~/sync/man/dotsamples/vanilla/mgalgs-dotfiles-gptel/init.el
;; (anthropic/claude-3.7-sonnet
;;               anthropic/claude-3.7-sonnet:thinking
;;               anthropic/claude-opus-4
;;               anthropic/claude-sonnet-4
;;               deepseek/deepseek-chat-v3-0324
;;               deepseek/deepseek-chat-v3-0324:free
;;               deepseek/deepseek-r1-0528
;;               deepseek/deepseek-r1-0528:free
;;               google/gemini-2.0-flash-001
;;               google/gemini-2.5-flash-preview-05-20
;;               google/gemini-2.5-pro-preview-05-06
;;               openai/gpt-4.1
;;               openai/gpt-4.1-mini
;;               openai/gpt-4o
;;               openai/gpt-4o-2024-11-20
;;               openai/gpt-4o-mini
;;               openai/o1
;;               openai/o1-mini
;;               qwen/qwen3-235b-a22b
;;               x-ai/grok-3-mini-beta)

;;;; model descriptions

;; updated 2025-01-27

;; sonar, sonar-pro

;; (defconst gptel--gemini-models
;;   '((gemini-1.5-pro-latest
;;      :description "Google's latest model with enhanced capabilities across various tasks"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 2000
;;      ;; input & output price is halved for prompts of 128k tokens or less
;;      :input-cost 2.50
;;      :output-cost 10
;;      :cutoff-date "2024-05")
;;     (gemini-2.0-flash-exp
;;      :description "Next generation features, superior speed, native tool use"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 1000
;;      :cutoff-date "2024-12")
;;     (gemini-1.5-flash
;;      :description "A faster, more efficient version of Gemini 1.5 optimized for speed"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 1000
;;      ;; input & output price is halved for prompts of 128k tokens or less
;;      :input-cost 0.15
;;      :output-cost 0.60
;;      :cutoff-date "2024-05")
;;     (gemini-1.5-flash-8b
;;      :description "High volume and lower intelligence tasks"
;;      :capabilities (tool-use json media)
;;      :context-window 1000
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      ;; input & output price is halved for prompts of 128k tokens or less
;;      :input-cost 0.075
;;      :output-cost 0.30
;;      :cutoff-date "2024-10")
;;     (gemini-2.0-flash-thinking-exp
;;      :description "Stronger reasoning capabilities."
;;      :capabilities (tool-use media)
;;      :context-window 32
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "text/plain" "text/csv" "text/html")
;;      :cutoff-date "2024-08")
;;     (gemini-exp-1206
;;      :description "Improved coding, reasoning and vision capabilities"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :cutoff-date "2024-12")
;;     (gemini-pro
;;      :description "The previous generation of Google's multimodal AI model"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 32
;;      :input-cost 0.50
;;      :output-cost 1.50
;;      :cutoff-date "2023-02"))
;;   "List of available Gemini models and associated properties.
;; Keys:

;; - `:description': a brief description of the model.

;; - `:capabilities': a list of capabilities supported by the model.

;; - `:mime-types': a list of supported MIME types for media files.

;; - `:context-window': the context window size, in thousands of tokens.

;; - `:input-cost': the input cost, in US dollars per million tokens.

;; - `:output-cost': the output cost, in US dollars per million tokens.

;; - `:cutoff-date': the knowledge cutoff date.

;; - `:request-params': a plist of additional request parameters to
;;   include when using this model.

;; Information about the Gemini models was obtained from the following
;; source:

;; - <https://ai.google.dev/pricing>
;; - <https://cloud.google.com/vertex-ai/generative-ai/docs/learn/models>")
