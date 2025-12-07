;;; +completion.el --- Corfu and Vertico configuration -*- lexical-binding: t; -*-

;;;; :completion corfu vertico

;;;;; DONT vertico-posframe-mode remove-hook

;; (when (modulep! :completion vertico +childframe)
;;   (remove-hook 'vertico-mode-hook #'vertico-posframe-mode))

;;;;; vertico-buffer on TOP

;; vertico-buffer on-top
;; (progn
;;   (require 'vertico-buffer)
;;   (setq vertico-resize 'grow-only) ; doom nil

;;   ;; vertico on Top
;;   (setq vertico-buffer-display-action
;;         `(display-buffer-in-side-window
;;           (window-height . ,(+ 3 vertico-count)) (side . top)))
;;   (vertico-mode +1)
;;   (vertico-buffer-mode +1))

;;;;; vertico-multiform

;; sachac-dotfiles/Sacha.org
(with-eval-after-load 'vertico-multiform
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

;;;;; DONT vertico hangul

;; from ohyecloudy
;; vertico는 =post-command-hook= 을 사용해서 증분 완성(incremental completion)을
;; 수행한다. 영문 입력시 =post-command-hook= 이 잘 발동하지만 조합해서 입력하는
;; 한글은 =post-command-hook= 이 호출되지 않는다. helm 동작 방법을 참고해
;; timer를 돌려서 해결했다.

;; (after! vertico
;;   (defun my/vertico-setup-then-remove-post-command-hook (&rest args)
;;     "vertico--setup 함수에서 추가하는 post-command-hook을 제거한다.

;;      입력 조합으로 표현하는 한글 입력시 post-command-hook이 입력되지 않는다.
;;      한글 증분 완성을 위해 timer로 호출하기 때문에 제거한다"
;;     (remove-hook 'post-command-hook #'vertico--exhibit 'local))

;;   (defun my/vertico-exhibit-with-timer (&rest args)
;;     "타이머를 넣어 타이머 이벤트 발생시 vertico--exhibit을 호출해 미니버퍼 완성(completion) 후보 리스트를 갱신한다

;;      post-command-hook이 발동하지 않는 한글 입력시에도 한글 증분 완성을 하기 위해 timer를 사용한다"
;;     (let (timer)
;;       (unwind-protect
;;           (progn
;;             (setq timer
;;                   (run-with-idle-timer
;;                    0.01 'repeat
;;                    (lambda ()
;;                      (with-selected-window (or (active-minibuffer-window)
;;                                                (minibuffer-window))
;;                        (vertico--exhibit)))))
;;             (apply args))
;;         (when timer
;;           (cancel-timer timer)))))

;;   (advice-add
;;    #'vertico--setup
;;    :after #'my/vertico-setup-then-remove-post-command-hook)
;;   (advice-add #'vertico--advice :around #'my/vertico-exhibit-with-timer))

;;;;; marginalia with vertico-sort

;;;;;; marginalia for file

(after! vertico
  (require 'marginalia)
  (defun gr/marginalia--annotate-local-file (cand)
    "Annotate local file CAND.
Removes modes, which I've never needed or wanted."
    (marginalia--in-minibuffer
      (when-let (attrs (ignore-errors
                         ;; may throw permission denied errors
                         (file-attributes (substitute-in-file-name
                                           (marginalia--full-candidate cand))
                                          'integer)))
        (marginalia--fields
         ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
         ((marginalia--time (file-attribute-modification-time attrs))
          :face 'marginalia-date :width -12)
         ;; File owner at the right
         ((marginalia--file-owner attrs) :face 'marginalia-file-owner)))))

  (defun gr/marginalia-annotate-file (cand)
    "Annotate file CAND with its size, modification time and other attributes.
These annotations are skipped for remote paths."
    (if-let (remote (or (marginalia--remote-file-p cand)
                        (when-let (win (active-minibuffer-window))
                          (with-current-buffer (window-buffer win)
                            (marginalia--remote-file-p (minibuffer-contents-no-properties))))))
        (marginalia--fields (remote :format "*%s*" :face 'marginalia-documentation))
      (gr/marginalia--annotate-local-file cand)))

  ;; M-A 순서를 바꾸면 된다.
  (add-to-list 'marginalia-annotators
               '(file gr/marginalia-annotate-file marginalia-annotate-file builtin none))

;;;;;; vertico sort modified

  ;; (setq vertico-multiform-categories nil)
  ;; (setq vertico-multiform-categories
  ;;       '(
  ;;         ;; (file (vertico-sort-function . sort-directories-first))
  ;;         ;; (file (vertico-sort-function . gr/sort-modified))
  ;;         (file (+vertico-transform-functions . +vertico-highlight-directory)) ; doom default
  ;;         ))

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun gr/sort-modified (list)
    "Sort LIST of files for latest modified."
    (let ((ht (make-hash-table :test #'equal :size 5000)))
      (dolist (x list)
        (puthash x (file-attribute-modification-time (file-attributes x)) ht))
      (sort list
            (lambda (a b)
              (let ((one
                     (gethash a ht))
                    (two
                     (gethash b ht)))
                (time-less-p two one))))))

  (defun vertico-sort-modified ()
    (interactive)
    (setq-local vertico-sort-override-function
                (and (not vertico-sort-override-function)
                     #'gr/sort-modified)
                vertico--input t))

  (keymap-set vertico-map "M-," #'vertico-sort-modified))

;;;;; custom consult

(after! consult
  ;; replace "." search with consul-line in Evil normal state
  ;; use default "/" evil search
  ;; (evil-global-set-key 'normal (kbd ".") 'consult-line) ;; see doomkeys.el
  ;; (evil-global-set-key 'motion (kbd ".") 'consult-line)
  ;; (evil-global-set-key 'visual (kbd ".") 'consult-line)

  ;; (map! :leader
  ;;       :g "j i" #'consult-line
  ;;       :g "j I" #'consult-buffer)

  ;; +default/search-cwd
  (defun my/consult-find ()
    (interactive)
    (consult-find "."))

  (defun my/consult-fd ()
    (interactive)
    (consult-fd "."))

  ;; spacemacs/layers/+completion/compleseus/funcs.el
  ;;;;###autoload
  (defun my/compleseus-search (use-initial-input initial-directory)
    (let* ((initial-input
            (if use-initial-input
                (doom-pcre-quote ;; rxt-quote-pcre
                 (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (or (thing-at-point 'symbol t) ""))) ""))
           (default-directory
            (or initial-directory
                (read-directory-name "Start from directory: "))))
      (consult-ripgrep default-directory initial-input)))

  (defun my/search-cwd-symbol-at-point ()
    "Search current folder."
    (interactive)
    (my/compleseus-search t default-directory))

  (setq consult-preview-key "M-m")
  ;; (setq consult--customize-alist nil)
  (consult-customize
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd

   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark

   ;; custom below
   my/search-cwd-symbol-at-point
   +lookup/definition
   +lookup/implementations
   :preview-key '("M-m" :debounce 0.3 "<up>" "<down>" "C-j" "C-k")) ; M-j,k
  )

;;;;; corfu

;; 2024-11-06 back to default
;; 2024-09-13 기본 설정, jump-out-of-pair 추가
;; DEPRECATED Tab 이 자동 완성이면 괄호 점프랑 충돌 난다. C-j/k C-n/p 는 직관적인 기본 설정이므로 건들이지 않는다.

(after! corfu
  ;; (setq corfu-auto-delay 0.5) ; doom 0.24
  (setq corfu-auto-prefix 3) ; doom 2, default 3
  ;; (setq corfu-preselect 'valid) ; doom 'prompt
  ;; (setq tab-always-indent t) ; for jump-out-of-pair - doom 'complete
  ;; (setq +corfu-want-minibuffer-completion nil) ; doom t

  ;; (setq +corfu-want-tab-prefer-expand-snippets nil) ; 2024-11-06
  ;; (setq +corfu-want-tab-prefer-navigating-snippets nil)
  ;; (setq +corfu-want-tab-prefer-navigating-org-tables nil)

  ;; from minemacs
  ;; HACK: Prevent the annoting completion error when no `ispell' dictionary is set, prefer `cape-dict'
  (when (eq emacs-major-version 30)
    (setq text-mode-ispell-word-completion nil))

  ;; 2025-02-03 disable - use C-c d @org-mode
  ;; 2025-03-23 enable for english writing
  (add-hook! '(org-mode-hook markdown-mode-hook)
    (defun +corfu-add-cape-dict-h ()
      (add-hook 'completion-at-point-functions #'cape-dict 0 t)))

  ;; IMO, modern editors have trained a bad habit into us all: a burning need for
  ;; completion all the time -- as we type, as we breathe, as we pray to the
  ;; ancient ones -- but how often do you *really* need that information? I say
  ;; rarely. So opt for manual completion:
  ;; doom/hlissner-dot-doom/config.el
  ;; (setq corfu-auto nil)
  )

;;;;; DONT corfu-echo

;; 1) add '(package! corfu-popupinfo :disable t)' in packages.el

;; 2) turn on corfu-echo
;; (after! corfu
;;   (require 'corfu-echo)
;;   (add-hook 'corfu-mode-hook 'corfu-echo-mode)
;;   )

;;;;; cape with dict

(defun my/enable-cape-dict-on-completion ()
  (interactive)
  (add-to-list 'completion-at-point-functions #'cape-dict))

;; (map! :map some-mode-map
;;       "C-x e" #'cape-emoji)
;; (add-hook! some-mode (add-hook 'completion-at-point-functions #'some-capf depth t))
;; ;; OR, but note the different call signature
;; (add-hook 'some-mode-hook (lambda () (add-hook 'completion-at-point-functions #'some-capf depth t)))

;; ;; 2023-07-08 순서 때문에 따로 확실하게 점검한다.
;; (defun cape-markdown-mode-setup ()
;;   (interactive)
;;   (add-to-list 'completion-at-point-functions #'cape-dict)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-history)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; top
;;   )

;; (defun cape-org-mode-setup ()
;;   ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-history)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dict)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   ;; (add-to-list 'completion-at-point-functions #'zk-completion-at-point) ;; top
;;   )

;; ;; (defun cape-prog-mode-setup ()
;; ;;   ;; (add-to-list 'completion-at-point-functions #'cape-file)
;; ;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;; ;;   ;; (add-to-list 'completion-at-point-functions #'cape-history)
;; ;;   ;; (add-to-list 'completion-at-point-functions #'cape-keyword) ;; no.1
;; ;;   )

;; (add-hook 'markdown-mode-hook 'cape-markdown-mode-setup)
;; (add-hook 'org-mode-hook 'cape-org-mode-setup)
;; ;; (add-hook 'conf-mode-hook 'cape-prog-mode-setup)
;; ;; (add-hook 'prog-mode-hook 'cape-prog-mode-setup)

;;; +completion.el ends here
