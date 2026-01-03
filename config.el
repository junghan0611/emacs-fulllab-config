;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; 정말 설정 변경할 만한 녀석들만 짧게 넣어야 한다
;; 나머지는 모듈이나 사용자 설정으로 다 빼놔야 혼란이 적고 손댈일이 없다.

;; Check for missing external software
;;
;; - soffice (LibreOffice): View and create office documents
;; - zip: Unpack ePub documents
;; - pdftotext (poppler-utils): Convert PDF to text
;; - djvu (DjVuLibre): View DjVu files
;; - curl: Reading RSS feeds
;; - divpng: Part of LaTeX
;; - dot (GraphViz): Create note network diagrams
;; - convert (ImageMagick): Convert image files
;; - gm (GraphicsMagick): Convert image files
;; - latex (TexLive, MacTex or MikTeX): Preview LaTex and export Org to PDF
;; - hunspell: Spellcheck. Also requires a hunspell dictionary
;; - grep: Search inside files
;; - ripgrep: Faster alternative for grep
;; - gs (GhostScript): View PDF files
;; - mutool (MuPDF): View PDF files
;; - mpg321, ogg123 (vorbis-tools), mplayer, mpv, vlc: Media players

;;; Commentary:

;; ❶ :: U+2776 ==> 더원싱 태그로 활용
;; ㉽ :: U+327D
;; ㉼ :: U+327C

;;;; Load use-package org

(straight-use-package 'org)

;;;; Load 'Per-Machine' - User Configs

;; Most of my per-environment config done via =customize= and is in .custom.el.
;; However, some config is more involved, such as packages I just want in one
;; environment and not the others.  To that end, let's load a file that can contain
;; those customizations.
(let ((per-machine-filename (concat user-dotemacs-dir "per-machine.el")))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

;; (let ((user-keys-filename (concat user-dotemacs-dir "user-keys.el")))
;;   (when (file-exists-p user-keys-filename)
;;     (load-file user-keys-filename)))

;;;; Doom Common Configuration

;;;;; GENERAL SETTINGS

;; /doom/high-school-macos-emacs-dev-env/doom/init.el
(setq-default x-stretch-cursor t) ; make the cursor wide over tabs, etc.
(setq truncate-string-ellipsis "…") ; Unicode ellispis are nicer than "...", and also save /precious/ space

;;;;; startup and dashboard

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my notes). I can
;; do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode)

;; Set initial buffer to org
(setq initial-major-mode #'emacs-lisp-mode); text-mode

;;;;; gc-cons : gcmh

;; (setq gcmh-idle-delay 5) ; doom 'auto
;; (setq gcmh-high-cons-threshold (* 100 1024 1024)) ; doom 16m
;; (setq gc-cons-threshold gcmh-high-cons-threshold)
;; (setq garbage-collection-messages t)

;;;;; Leader key

;; Over-ride or add to Doom Emacs default key bindings
;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153
;; 'M-m', '\,' 'SPC m' for localleader
(setq
 doom-localleader-key ","
 doom-localleader-alt-key "C-,") ; emacs insert mode

;; persp-mode and projectile in different prefixes
;; (setq! persp-keymap-prefix (kbd "C-c w"))
;; (after! projectile
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(defun my/call-localleader ()
  (interactive)
  (setq unread-command-events (listify-key-sequence ",")))

(map! :leader (:desc "+major-mode" "m" #'my/call-localleader))

(after! evil
  ;; (global-set-key (kbd "M-m") #'my/call-localleader)
  (evil-define-key '(normal visual) prog-mode-map (kbd "C-,") 'my/call-localleader))

;;;;; Doom-Font

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(defun get-font-size-from-script ()
  "스크립트에서 폰트 크기를 float로 가져옴"
  (let ((output (string-trim (shell-command-to-string
                              "~/.local/bin/set-font-size.sh --default-font-size"))))
    (read (format "%s" output))))  ; read 함수는 17.0을 실수로 파싱

(when (display-graphic-p) ; gui
  (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size (get-font-size-from-script)) ; "GLG Nerd Font Mono"
        doom-big-font (font-spec :family "Sarasa Term K Nerd Font" :size 23.0))
  ;;       doom-big-font (font-spec :family "Sarasa Term K Nerd Font" :size 18.0))
  (setq doom-variable-pitch-font (font-spec :family "Pretendard Variable" :size (get-font-size-from-script)))
  (setq doom-unicode-font (font-spec :family "Symbola" :size (get-font-size-from-script)))
  )

(unless (display-graphic-p) ; terminal
  (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 15.1)))
;;;;; Doom's snippets-dir

(setq +snippets-dir (expand-file-name "snippets/" user-dotemacs-dir))

;;;;; global completion-at-point-functions should be nil

;; 'tags-completion-at-point-function' break ten-glossary
(setq-default completion-at-point-functions nil)

;;;;; Basics

;; (setq-default display-line-numbers-width-start t) ; doom's default t
(setq inhibit-compacting-font-caches t)

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; Disable .# lock files
(setq create-lockfiles nil)

;; t - Ridiculous path view is vanilla emacs. change truename!
;; nil - truename 을 원치 않고, 심볼링링크 사용
(setq find-file-visit-truename nil) ; doom t
;; Stop asking abount following symlinks to version controlled files
;; (setq vc-follow-symlinks nil) ; doom t

;;;;; Tab-width

;; ====== Buffer-local variables ======
;; (setq-default
;;  ;; Display long lines
;;  truncate-lines nil ; default t
;;  ;; Default fill column width
;;  fill-column 80
;;  ;; Never mix, use only spaces
;;  indent-tabs-mode nil ;; Width for line numbers display-line-numbers-width 4

;;  ;; 1) per major-mode config or hook
;;  ;; 2) editorconfig
;;  ;; 3) tab-width 4 (below)
;;  ;; tab-width 4 ;; 2024-03-11 org-mode element-cache 사용 시 무조건 8이다. 충돌난다. 끈다.

;;  display-line-numbers-width-start t ; 2024-06-26
;;  )

;;;;; Display-Line-Numbers-Mode

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)
(remove-hook! text-mode #'display-line-numbers-mode)

;; 2024-04-01 disable
;; (unless IS-TERMUX
;;   (add-hook 'org-mode-hook 'display-line-numbers-mode)
;;   (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

;;;;; Which-key

(after! which-key
  (setq which-key-idle-delay 0.4 ; important
        which-key-idle-secondary-delay 0.01)
  (setq which-key-use-C-h-commands t) ; paging key maps
  ;; (setq which-key-max-description-length 36) ; doom 27, spacemacs 36
  )

;;;;; evil

;; Key binding guide
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/
;; NOTE: use `map!' macro for convienience

;; ------------------------------------------------
;; Key binding vars
(after! evil
  ;; Implicit /g flag on evil ex substitution, because I use the default behavior less often.
  (setq evil-ex-substitute-global t) ; default nil

  ;; C-h is backspace in insert state
  ;; (setq evil-want-C-h-delete t) ; default nil
  (setq evil-want-C-w-delete t) ; default t
  (setq evil-want-C-u-scroll t) ; default t

  ;; use C-i / C-o  evil-jump-backward/forward
  (setq evil-want-C-i-jump t) ; default nil

  ;; mpereira-dotfiles-evil-clojure/configuration.org
  ;; FIXME: this correctly causes '*' to match on whole symbols (e.g., on a
  ;; Clojure file pressing '*' on 'foo.bar' matches the whole thing, instead of
  ;; just 'foo' or 'bar', BUT, it won't match 'foo.bar' in something like
  ;; '(foo.bar/baz)', which I don't like.
  ;; (setq-default evil-symbol-word-search t)
  ;; (setq evil-jumps-cross-buffers nil)
  (setq evil-want-Y-yank-to-eol t)

  ;; 'Important' Prevent the cursor from moving beyond the end of line.
  ;; Don't move the block cursor when toggling insert mode
  (setq evil-move-cursor-back nil) ; nil is better - default t
  (setq evil-move-beyond-eol nil) ; default nil

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil) ; default t

  ;; Change Doom's Default
  (setq +evil-want-o/O-to-continue-comments nil) ;; default t
  (setq +default-want-RET-continue-comments nil) ;; default t

  (setq evil-disable-insert-state-bindings t) ; 2024-10-25 default nil

  (setq evil-want-fine-undo t) ; doom 'nil

  ;; Don't create a kill entry on every visual movement.
  ;; More details: https://emacs.stackexchange.com/a/15054:
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; (setq evil-insert-state-cursor '(box "#F86155")) ;; better look
  ;; (setq evil-normal-state-cursor '(box "DarkGoldenrod2"))

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map "L" nil)
    (define-key evil-motion-state-map "M" nil)

    ;; (evil-global-set-key
    ;;  'normal (kbd "DEL") 'evil-switch-to-windows-last-buffer) ; Backspace

    ;; Replace Emacs Tabs key bindings with Workspace key bindings
    ;; replace "." search with consul-line in Evil normal state
    ;; use default "/" evil search
    ;; ;; (define-key evil-insert-state-map (kbd "C-k") 'kill-line) ; 2024-06-11 disable conflict with corfu-previous

    ;; evil macro
    (define-key evil-normal-state-map (kbd "q") 'nil) ; evil macro disable
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)

    ;; o :: ace-link-info 이거면 충분하다.
    ;; [[file:~/spacemacs/doc/DOCUMENTATION.org::*Binding keys][Binding keys]]
    (define-key evil-insert-state-map (kbd "C-]") 'forward-char) ; very useful

    ;; =C-w= 'insert 'evil-delete-backward-word
    ;; =C-w= 'visual 'evil-window-map
    ;; use evil bindings $ ^
    ;; (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
    ;; (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)
    ;; (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
    ;; (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)

    ;; M-d region delete and C-d char delete
    (define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)

    ;; evil-delete-char -> delete-forward-char
    (define-key evil-normal-state-map "x" 'delete-forward-char)
    (define-key evil-normal-state-map "X" 'delete-backward-char))
  ) ; end-of after evil

;;;;; evil cursor with toggle-input-method

(after! evil
  ;; keep evil insert cursor status per input-method
  ;; 2024-04-09 커서 상태 기반 한영 입력! 커서를 신뢰하라!
  ;; 2024-09-19 org 모드에서 동작이 영 안좋다. 끈다.
  ;; - 버퍼 전환 시 커서 상태 유지
  ;; - 커서를 보면 input-method 온오프를 알 수 있다.
  ;; - 한영 전환은 insert 모드에서만 가능

;;;;###autoload
  (defun block-toggle-input-method ()
    (interactive)
    (message (format "Input method is disabled in <%s> state." evil-state)))

  (mapc
   (lambda (mode)
     (let ((keymap (intern (format "evil-%s-state-map" mode))))
       (define-key (symbol-value keymap) (kbd "<Hangul>") #'block-toggle-input-method)
       (define-key (symbol-value keymap) (kbd "S-SPC") #'block-toggle-input-method)
       (define-key (symbol-value keymap) (kbd "<menu>") #'block-toggle-input-method)))
   '(motion normal visual))

  ;; ;;;;###autoload
  ;; (defun check-evil-cursor-state-between-window-switch ()
  ;;   (let ((type
  ;;          (pcase current-input-method ('nil 'bar) ("korean-hangul" 'hbar))))
  ;;     (setq-local evil-insert-state-cursor type)))

  ;; ;;;;###autoload
  ;; (defun toggle-input-method-with-evil-cursor-switch ()
  ;;   (interactive)
  ;;   (toggle-input-method)
  ;;   (check-evil-cursor-state-between-window-switch)
  ;;   ;; (message (format "Input method is disabled in <%s> state." evil-state))
  ;;   )

  ;; (mapc
  ;;  (lambda (mode)
  ;;    (let ((keymap (intern (format "evil-%s-state-map" mode))))
  ;;      (define-key
  ;;       (symbol-value keymap) (kbd "<Hangul>") #'toggle-input-method-with-evil-cursor-switch)
  ;;      (define-key
  ;;       (symbol-value keymap) (kbd "S-SPC") #'toggle-input-method-with-evil-cursor-switch)))
  ;;  '(insert))

  ;; (add-hook 'evil-insert-state-entry-hook 'check-evil-cursor-state-between-window-switch 90)

  ;; (defadvice! change-cursor-after-toggle-input (fn &optional arg interactive)
  ;;   :around #'toggle-input-method
  ;;   :around #'set-input-method (funcall fn arg interactive)
  ;;   (let ((type
  ;;          (pcase current-input-method
  ;;            ('nil 'bar)
  ;;            ("korean-hangul" 'hbar))))
  ;;     (setq-local evil-insert-state-cursor type)))
  )


;;;;; evil-escape

;; ,. as Esc key binding
;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59/7
(after! evil-escape
  (setq evil-escape-key-sequence ",.") ;; "jk"
  (setq evil-escape-unordered-key-sequence nil)
  (setq evil-escape-delay 1.0) ;; 0.5, default 0.1
  (evil-escape-mode 1))

;;;;; undo-fu

(after! undo-fu
  ;; undo-fu
  (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "\C-r") 'undo-fu-only-redo)
  (unbind-key "C-M-_" 'undo-fu-mode-map)
  (global-unset-key (kbd "C-M-_")))

;; 스페이스맥스와 다르네?! 아래는 스페이스맥스 설정
;;         ;; C-r 은 isearch-backward 가 기본
;;         (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
;;         (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

;;         ;; Undo-fu customization options
;;         ;; Undoing with a selection will use undo within that region.
;;         (setq undo-fu-allow-undo-in-region t)
;;         ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.
;;         (setq undo-fu-ignore-keyboard-quit t)
;;         ;; By default while in insert all changes are one big blob. Be more granular
;;         (setq evil-want-fine-undo t)

;;;;; set-popup-rules

(progn
  ;; Disabling hidden mode-line in popups
  ;; By default, the mode-line is hidden in popups. To disable this, you can either:
  ;; (plist-put +popup-defaults :modeline t)

  ;; Completely disable management of the mode-line in popups:
  (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h) ; important

  ;; (setq +popup--display-buffer-alist nil) ; reset all

  ;; default popup rules
  (set-popup-rules!
    '(
      ;; ("^\\*scratch*" :ignore t) ; for TEST
      ("^\\.doom.d/diary" :ignore t) ; for TEST
      ("^\\*Completions" :ignore t)
      ("*Org Preview LaTeX Output*" :ingnore t) ; 2025-01-24
      ("^\\*Local variables\\*$" :vslot -1 :slot 1 :size +popup-shrink-to-fit)
      ;; ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :vslot -2 :size 0.3 :autosave t :quit t :ttl nil) ; +default
      ("^\\*Compil\\(?:ation\\|e-Log\\)" :size 0.3 :ttl 0 :quit t)
      ("^\\*Messages\\*" :vslot -4 :side right :size 0.4 :quit t :ttl 0) ; jh
      ("^\\*\\(?:doom \\|Pp E\\)" ; transient buffers (no interaction required)
       :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
      ("^\\*doom:" :vslot -4 :size 0.35 :side right :autosave t :select t :modeline t :quit nil :ttl t) ; editing buffers (interaction required)
      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup" ; editing buffers (interaction required)
       :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
      ("^\\*eat*\\$" :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
      ("^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$" :vslot -5 :side right :size 0.35 :select t :modeline t :quit nil :ttl t) ; 2025-02-27 python
      ("^\\*\\(?:Wo\\)?Man " :vslot -6 :size 0.45 :select t :quit t :ttl 0)
      ("^\\*Calc" :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
      ("^\\*Customize" :slot 2 :side right :size 0.5 :select t :quit nil)
      ("^ \\*undo-tree\\*" :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*\\([Hh]elp\\|Apropos\\)" :slot 2 :vslot -8 :size 0.42 :select t)

      ;; ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
      ;;  :vslot -11 :size 0.35 :select t)
      ("^\\*xwidget" :vslot -11 :size 0.35 :select nil)
      ;; ("*Org Agenda(n)*" :size 0.5 :side left :select nil :quit nil :ttl 0)
      ("^\\*eww.*" :size 82 :side left :modeline t :select t :quit nil :ttl t) ; jh

      ;; =M-x eldoc-doc-buffer= 함수 호출로 표시하는 buffer 크기 조절
      ("^\\*eldoc for" :size 0.2 :vslot -1) ; "*eldoc*"
      ("*Ilist*" :size 45 :side right :modeline t :select nil :quit nil) ; imenu-list 45
      ;; ("^ ?\\*Treemacs" :slot 7 :size 45 :side left :modeline nil :select nil :quit nil)
      ;; ("^ ?\\*NeoTree" :slot 7 :size 45 :side left :modeline t :slect nil :quit nil)

      ;; ("^\\*gptel\\*" :size 84 :side right :modeline t :select t :quit nil :ttl t)
      ;; ("^\\*gptel-ask\\*" :size 80 :side right :modeline nil :select nil :quit nil)
      ;; ("^\\*gptel-quick\\*" :size 80 :side right :modeline nil :select nil :quit nil)
      ;; "*EKG Capture.*\\*" "*EKG Edit.*\\*"  "*[Ee][Kk][Gg] .*\\*"
      ;; ("*EKG Capture.*\\*" :slot 3 :side bottom :size 0.4 :select t :quit nil) ; jh
      ;; ("*EKG Edit.*\\*" :slot 3 :side bottom :size 0.4 :select t :quit nil) ; jh
      ("*Go-Translate*" :side bottom :size 0.4 :select t :quit t) ; jh
      ("\\`\\*evil-owl\\*\\'" :side bottom :ttl t :height 20 :quit t)
      ;; ("\\*Embark Actions\\*" :side bottom :ttl t :height 20 :quit t) ; mixed
      ;; ("^\\*info\\*$" :slot 2 :vslot 2 :size 0.45 :select t))
      ("^\\*info\\*$"
       :slot 2
       :vslot 2
       :size 82
       :side left
       :modeline t
       :select t
       :ttl nil)) ;; `Info-mode' jh
    )

  ;; 와우 이거다. 태그랑 쓰기랑 나눠야 한다.
  ;;(add-to-list
  ;;  'display-buffer-alist
  ;;  `("*ekg tag.*\\*"
  ;;    (display-buffer-reuse-window display-buffer-in-direction)
  ;;    (direction . left)
  ;;    (window . root)
  ;;    (window-width . 0.35)))

  ;; 2024-03-19 sdcv 에서 가져옴. 이게 괜찮은듯
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  `("^\\*eww.*"
  ;;    (display-buffer-reuse-window
  ;;     display-buffer-in-direction)
  ;;    (direction . right)
  ;;    (window . root)
  ;;    (window-width . 0.35)))

  (set-popup-rule! "^\\*eww.*" :size 84 :side 'right :modeline t :select t :ttl nil)
  ;; (set-popup-rule! "\\`\\*chatgpt\\* " :ttl t :side 'bottom :height 20 :quit t :select t)
  ;; ;; (set-popup-rule! "^\\*Messages\\*" :ttl t :side 'bottom :height 20 :quit t :ttl t)
  ;; (set-popup-rule! "^\\*doom:vterm*" :ttl t :side 'bottom :height 20 :quit t)

  ;; (set-popup-rule! "^\\*npm*" :ttl t :side 'bottom :height 20 :quit t)
  ;; (set-popup-rule! "^\\*Flycheck*" :ttl t :side 'bottom :height 20 :quit t)

  ;; from prot's dotfiles : important
  (add-to-list
   'display-buffer-alist
   `("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
     (display-buffer-no-window)
     (allow-no-window . t)))
  )

;;;;; bookmark

;; On each machine I use, I have different bookmarks, yet they all
;; point to the same location.
(setq bookmark-default-file "~/emacs-bookmarks.el")
;; (setq bookmark-default-file (concat user-dotemacs-dir "assets/bookmarks"))
(setq bookmark-use-annotations nil)
(setq bookmark-automatically-show-annotations t)
;; Save the `bookmark-file' each time I modify a bookmark.
(setq bookmark-save-flag 1)

;;;;; show-trainling-whitespace

;; https://idiomdrottning.org/show-trailing-whitespace
;; `show-trailing-whitespace' is my friend.
;; (setq-hook! (text-mode prog-mode conf-mode) show-trailing-whitespace t)
(setq show-trailing-whitespace t) ; globally on

;; White space cleanup, without obtrusive white space removal.
;; Whitespaces at EOL and EOF are trimmed upon file save, and only for lines modified by you.
;; Much better than globally removing EOL whitespace on save, especially when
;; editing collaboratively with others.
;; README/doomemacs-git/lisp/doom-editor.el

(defun my/save-hook-delete-trailing-whitespace ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'markdown-mode-hook #'my/save-hook-delete-trailing-whitespace)

;;;;; User Goto Functions

;; (defun goto-emacs-dotfiles.org ()
;;   "Open jh-emacs.org file."
;;   (interactive)
;;   (find-file (concat dotspacemacs-directory "jh-emacs.org")))

(defun goto-pandoc-config ()
  "Open pandoc metadata file."
  (interactive)
  (find-file "~/.config/pandoc/metadata.yml"))

;;;;; CJK Word Wrap

;; Emacs 28 adds better word wrap / line break support for CJK.
(setq word-wrap-by-category t) ; default nil

;; Normally when word-wrap is on, Emacs only breaks lines after
;; whitespace characters.  When this option is turned on, Emacs also
;; breaks lines after characters that have the "|" category (defined in
;; characters.el).  This is useful for allowing breaking after CJK
;; characters and improves the word-wrapping for CJK text mixed with
;; Latin text.

;; 일반적으로 단어 줄 바꿈이 켜져 있으면 Emac 은 공백 문자 뒤에 오는 줄만 줄
;; 바꿈합니다. 이 옵션을 켜면 Emac 은 "|" 범주(characters.el 에 정의됨)가 있는 문자
;; 뒤의 줄도 줄 바꿈합니다. 이 옵션은 한중일 문자 뒤에 줄 바꿈을 허용하는 데
;; 유용하며 라틴 텍스트와 혼합된 한중일 텍스트의 단어 줄 바꿈을 개선합니다.

;;;;; winner

(after! winner
  (setq winner-boring-buffers-regexp "\\*.*\\*")
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

;;;;; abbr

(setq abbrev-file-name (concat user-dotemacs-dir "var/abbrev_defs"))
(read-abbrev-file abbrev-file-name)
(setq save-abbrevs t)
(setq-default abbrev-mode t)

;;;;; fill-column-indicator-mode

;; 2023-04-16 Learn how-to use menu-bar for beginner on GUI mode
(when (display-graphic-p) ; gui
  (add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
  ;; (add-hook 'markdown-mode-hook 'display-fill-column-indicator-mode)
  )

;;;;; fortune

;; not work on termux
(unless IS-TERMUX
  (require 'fortune)
  (setq fortune-always-compile nil)
  (setq fortune-dir (concat root-path "usr/share/games/fortunes/advice"))
  (setq fortune-file (concat root-path "usr/share/games/fortunes/advice")))


;;;;; autorevert

;; ~/doomemacs-junghan0611/lisp/doom-editor.el
;; (after! autorevert
;; Ensure that files are reloaded from disk (when switching branches, or from dropbox sync)
(global-auto-revert-mode 1) ; doom nil
(setq auto-revert-interval 10)

;;;;; DONT help-mode-hook : visual-line-mode

;; (add-hook 'help-mode-hook #'visual-line-mode)

;;;;; Transparency

(if (eq system-type 'gnu/linux)
    (setq default-frame-alist
          (push '(alpha-background . 93) default-frame-alist)) ;; 93
  (setq default-frame-alist (push '(alpha . (95 90)) default-frame-alist)))

;; Emacs 29 ushers in a bold new era where a frame's background can be made
;; transparent without affecting the transparency of foreground text and other
;; elements. Who'd-a thunk? Use that feature when available

(defun toggle-transparency (alpha-level)
  (interactive "p")
  (message (format "%s" alpha-level))
  (when (< alpha-level 50)
    (setq alpha-level 90))
  (let ((myalpha (or (frame-parameter nil 'alpha) 100))
        (frame-param
         (if (< emacs-major-version 29)
             'alpha
           'alpha-background)))

    (set-frame-parameter nil frame-param myalpha)
    (message (format "Frame %s level is %d" frame-param myalpha))))

(defun set-transparency (alpha-level)
  ;; in Emacs 29+, set background opacity
  ;; before 29, we have no choice but to set frame opacity
  (interactive "p")
  (message (format "Alpha level passed in: %s" alpha-level))
  (let ((alpha-level
         (if (< alpha-level 2)
             (read-number "Opacity percentage: " 90)
           alpha-level))
        (frame-param
         (if (< emacs-major-version 29)
             'alpha
           'alpha-background)))
    (set-frame-parameter nil frame-param alpha-level)
    (message (format "Frame %s level is %d" frame-param alpha-level))))
(defalias 'set-opacity 'set-transparency)

(defun set-transparency-low ()
  (interactive)
  (set-transparency 70))

;;;;; golden-ratio

(use-package! golden-ratio)

;;;;; emms : music player

;; path
(setq
 emms-directory (concat doom-data-dir "emms")
 emms-cache-file (concat doom-cache-dir "emms"))

;; M-x emms-add-directory-tree


;;;;; winum

;; (use-package! winum
;;   :demand t
;;   :config
;;   (defun my/winum-assign-custom ()
;;     (cond
;;      ;; 0 minibuffer, 9 treemacs, 8 imenu-list
;;      ((equal (buffer-name) "*Ilist*") 8)
;;      ((equal (buffer-name) "*Calculator*") 7)
;;      ;; ((equal (buffer-name) "*Flycheck errors*") 6) ; use flymake
;;      )
;;     )

;;   (set-face-attribute 'winum-face nil :weight 'bold)
;;   (add-to-list 'winum-assign-functions #'my/winum-assign-custom)

;;   (setq winum-scope                      'frame-local
;;         winum-auto-assign-0-to-minibuffer t ; important
;;         winum-ignored-buffers '(" *LV*" " *which-key*")
;;         winum-auto-setup-mode-line nil
;;         winum-reverse-frame-list nil)
;;   (winum-mode +1)
;;   )

;;;; :emacs

;; move to modules/custom/emacs

;;;; :completion corfu vertico

(load! "+completion")

;;;; :checkers

;;;;; DONT Flycheck

;; (after! flycheck
;;   (setq flycheck-global-modes '(not emacs-lisp-mode org-mode markdown-mode gfm-mode))
;;   (setq flycheck-checker-error-threshold 1000) ; need more than default of 400
;;   )

;; (remove-hook 'doom-first-buffer-hook #'global-flycheck-mode)

;; (progn
;;   (setq flycheck-help-echo-function nil ; default 'flycheck-help-echo-all-error-messages
;;         flycheck-display-errors-function nil ; default 'flycheck-display-error-messages
;;         )

;;   (after! flycheck
;;     (ignore-errors
;;       (define-key flycheck-mode-map flycheck-keymap-prefix nil))
;;     (setq flycheck-keymap-prefix nil)

;;     (add-hook! flycheck-mode
;;       (defun disable-flycheck-popup-buffer ()
;;         (setq flycheck-display-errors-function #'ignore)))
;;     (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-package)
;;     )

;;   (after! elisp-mode
;;     (add-hook! 'doom-scratch-buffer-created-hook
;;       (defun flycheck-off ()
;;         (flycheck-mode -1))))
;;   )

;;;;; Flymake

;;;;;; remove flymake-mode default

(remove-hook! (prog-mode text-mode) #'flymake-mode)

;;;;;; DONT flymake-vale

;; flymake-vale-modes defaults to:
;;  => (text-mode latex-mode org-mode markdown-mode message-mode)
;; (add-to-list 'flymake-vale-modes 'adoc-mode)
;; (require 'flymake-vale)
;; (add-hook 'text-mode-hook #'flymake-vale-load)
;; (add-hook 'latex-mode-hook #'flymake-vale-load)
;; (add-hook 'org-mode-hook #'flymake-vale-load)
;; (add-hook 'markdown-mode-hook #'flymake-vale-load)
;; (add-hook 'message-mode-hook #'flymake-vale-load)

;;;; :editor

;; (evil +everywhere); come to the dark side, we have cookies
;; file-templates    ; auto-snippets for empty files
;; fold              ; (nigh) universal code folding
;; format            ; automated prettiness
;; multiple-cursors  ; editing in many places at once
;; rotate-text       ; cycle region at point between text candidates
;; snippets          ; my elves. They type so I don't have to
;; ;;word-wrap         ; soft wrapping with language-aware indent


;;;;; midnight mode - built-in

(use-package! midnight
  :defer-incrementally t
  :config
  (add-to-list 'clean-buffer-list-kill-buffer-names "*Native-compile-Log*")
  (add-to-list 'clean-buffer-list-kill-buffer-names "*Async-native-compile-log*"))

;;;;; NOTE :editor doom

;;;;; :editor titlecase

;; (use-package! titlecase
;;   :defer t
;;   :init
;;   (after! embark
;;     (define-key embark-region-map "T" #'titlecase-region)
;;     (define-key embark-heading-map "T" #'titlecase-line)
;;     (define-key embark-sentence-map "T" #'titlecase-sentence)))

;;;;; :editor string-inflection

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  )

;;;;; unfill

(use-package! unfill
  :bind
  (([remap fill-paragraph] . unfill-toggle)
   :map
   org-mode-map
   ("M-q" . unfill-toggle)))

;;;;; evil-matchit - symbol-overlay

;; % evilmi

;; 스페이스맥스 : 절대 글로벌로 켜지 말 것! 각 언어 레이어에 보면 이미 들어가 있다.
;; /mpereira-dotfiles-evil-clojure/configuration.org
;; vim matchit.vim is ported into emacs
;; (use-package! evil-matchit
;;   :config
;;   ;; https://github.com/redguardtoo/evil-matchit/pull/141
;;   (evilmi-load-plugin-rules '(web-mode) '(simple template html))
;;   (add-hook 'web-mode-hook 'turn-on-evil-matchit-mode))

(use-package! evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;;;;; evil-surround

;; @call-function
;; visual mode S- or gS-
;; normal mode ys- or yS-
;; change surround cs-
;; delete surround ds-
;; @select area
;; call-functionu- - ;현재부터 단어 끝까지
;; {call-function}-i- ;현재 단어
;; {call-function}-s- ;현재 줄
;; @wrap function
;; {select-area}-w
;; ${target}( 바꾸고싶은거 ), ${change}(바뀔거)
;; 감싸기:     => y-s-i-w-${change}( "(", "{", "[")
;; 전부 감싸기 => y-s-s-${change}
;; 바꾸기: => c-s-${target}( "(", "{", "["), ${change}
;; 벗기기: => d-s-${target}( "(", "{", "[")

;;;;;;  evil-traces

;; move: m +{n}, delete: +{n},+{n}d, join: .,+{n}j glboal: g/{target}/{change}

;;;;; evil-owl

;; gl ${operator}
;; evil-owl-mode                   A minor mode to preview marks and registers before using them.
;; evil-owl-goto-mark            (`)   Wrapper function for ‘evil-goto-mark’ that shows a preview popup.
;; evil-owl-set-marker           (m)   Wrapper function for ‘evil-set-marker’ that shows a preview popup.
;; evil-owl-record-macro         (q)   Wrapper function for ‘evil-record-macro’ that shows a preview popup.
;; evil-owl-use-register         (")   Wrapper function for ‘evil-use-register’ that shows a preview popup.
;; evil-owl-execute-macro        (@)   Wrapper function for ‘evil-execute-macro’ that shows a preview popup.
;; evil-owl-goto-mark-line       (')   Wrapper function for ‘evil-goto-mark-line’ that shows a preview popup.
;; evil-owl-scroll-popup-up        Scroll the popup up one page.
;; evil-owl-scroll-popup-down      Scroll the popup down one page.
;; evil-owl-paste-from-register    Wrapper function for ‘evil-paste-from-register’ that shows a preview popup.

;; Not sure what is in a register? Have it show you when you hit ~”~ or ~@~
;; (use-package! evil-owl
;;   :hook (doom-first-input . evil-owl-mode)
;;   :config
;;   (setq evil-owl-display-method 'window)
;;   (setq evil-owl-idle-delay 0.5) ; default 1
;;   (setq evil-owl-max-string-length 500))

;;;;; smartparens

;; Smartparens - Practicalli config and key bindings
;; A Spacemacs like Lisp state menu (without the transient state)

(after! smartparens
  ;; 2023-09-14 global 로 사용하다보니 거슬린다. 잠시만. 글로벌을 빼면 어떤가?
  ;; ("\\\\(" . "\\\\)") ;; emacs regexp parens
  ;; ("\\{"   . "\\}")   ;; latex literal braces in math mode
  ;; ("\\("   . "\\)")   ;; capture parens in regexp in various languages
  ;; ("\\\""  . "\\\"")  ;; escaped quotes in strings
  ;; ("/*"    . "*/")    ;; C-like multi-line comment
  ;; ("\""    . "\"")    ;; string double quotes
  ;; ("'"     . "'")     ;; string single quotes/character quotes
  ;; ("("     . ")")     ;; parens (yay lisp)
  ;; ("["     . "]")     ;; brackets
  ;; ("{"     . "}")     ;; braces (a.k.a. curly brackets)
  ;; ("`"     . "`")     ;; latex strings. tap twice for latex double quotes

  ;; Unbind `M-s' (set by paredit keybindings above) because it's bound
  ;; to some handy occur related functions
  ;; (define-key sp-keymap (kbd "M-s") nil)

  ;; org 모드에서 거슬린다. 제거. 굳.
  (sp-local-pair 'org-mode "(" ")" :actions '(rem)) ; for denote completion
  (sp-local-pair 'org-mode "[" "]" :actions '(rem)) ; temporarly
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "`" "`" :actions '(rem))
  (sp-local-pair 'org-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'org-mode "/" "/" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "~" "~" :actions '(rem))

  ;; markdown 에서도 삭제
  (sp-local-pair 'markdown-mode "(" ")" :actions '(rem))
  (sp-local-pair 'markdown-mode "'" "'" :actions '(rem))
  (sp-local-pair 'markdown-mode "`" "`" :actions '(rem))
  (sp-local-pair 'markdown-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'markdown-mode "/" "/" :actions '(rem))

  (sp-with-modes
      '(minibuffer-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "(" nil :wrap "C-("))
  (sp-local-pair 'minibuffer-mode "[" "]" :actions '(rem)) ; 2025-07-13 remove pair

  (sp-with-modes 'markdown-mode (sp-local-pair "**" "***"))

  ;; for latex math
  (sp-with-modes
      'org-mode
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "$$" "$$"))

  (sp-with-modes
      'web-mode
    (sp-local-pair "{{#if" "{{/if")
    (sp-local-pair "{{#unless" "{{/unless"))
  ) ;; end-of smartparens

;; lisp modes
;; (sp-with-modes sp--lisp-modes
;;   (sp-local-pair "(" nil
;;                  :wrap "C-("
;;                  :pre-handlers '(my-add-space-before-sexp-insertion)
;;                  :post-handlers '(my-add-space-after-sexp-insertion)))

;; (defun my-add-space-after-sexp-insertion (id action _context)
;;   (when (eq action 'insert)
;;     (save-excursion
;;       (forward-char (sp-get-pair id :cl-l))
;;       (when (or (eq (char-syntax (following-char)) ?w)
;;                 (looking-at (sp--get-opening-regexp)))
;;         (insert " ")))))

;; (defun my-add-space-before-sexp-insertion (id action _context)
;;   (when (eq action 'insert)
;;     (save-excursion
;;       (backward-char (length id))
;;       (when (or (eq (char-syntax (preceding-char)) ?w)
;;                 (and (looking-back (sp--get-closing-regexp))
;;                      (not (eq (char-syntax (preceding-char)) ?'))))
;;         (insert " ")))))

;; ;; SP config for other modes. (from vedang)
;; (eval-after-load 'cider-repl
;;   '(progn
;;      (define-key cider-repl-mode-map (kbd ")") 'sp-up-sexp)
;;      (define-key cider-repl-mode-map (kbd "]") 'sp-up-sexp)
;;      (define-key cider-repl-mode-map (kbd "}") 'sp-up-sexp)))

;; (eval-after-load 'clojure-mode
;;   '(progn
;;      (define-key clojure-mode-map (kbd ")") 'sp-up-sexp)
;;      (define-key clojure-mode-map (kbd "]") 'sp-up-sexp)
;;      (define-key clojure-mode-map (kbd "}") 'sp-up-sexp)))

;; indent after inserting any kinds of parens
;; (defun my/smartparens-pair-newline-and-indent (id action context)
;;   (save-excursion
;;     (newline)
;;     (indent-according-to-mode))
;;   (indent-according-to-mode))
;; (sp-pair "(" nil :post-handlers
;;          '(:add (my/smartparens-pair-newline-and-indent "RET")))
;; (sp-pair "{" nil :post-handlers
;;          '(:add (my/smartparens-pair-newline-and-indent "RET")))
;; (sp-pair "[" nil :post-handlers
;;          '(:add (my/smartparens-pair-newline-and-indent "RET")))

;;;;; tempel

(defvar jf/denote-base-dir
  (file-truename
   (if (file-exists-p (expand-file-name "~/.my-computer"))
       "~/sync/org/"
     "~/Documents/denote/"))
  "Where I put my notes; I need to provision differently for personal and
work computers.")

;;;;###autoload
(cl-defun jf/org-macro-value-list (macro-name
                                   &key (dir jf/denote-base-dir))
  "List the unique inner text of all uses of MACRO-NAME in given DIR."
  (let ((path
         (if current-prefix-arg
             dir
           (or (buffer-file-name (current-buffer)) dir))))
    (s-split
     "\n"
     (s-trim
      (shell-command-to-string
       (concat
        "rg \"\\{\\{\\{"
        macro-name
        "\\((.+?)\\)\\}\\}\\}"
        "\" --only-matching --no-filename -r '$1' "
        path
        " | sort | uniq"))))))

;; (cl-defun my/org-tag-value-list (macro-name
;;                                  &key (dir jf/denote-base-dir))
;;   "list the unique inner text of all uses of macro-name in given dir."
;;   (let ((path
;;          (if current-prefix-arg
;;              dir
;;            (or (buffer-file-name (current-buffer)) dir))))
;;     (s-split
;;      "\n"
;;      (s-trim
;;       (shell-command-to-string
;;        (concat
;;         "fd "
;;         macro-name
;;         path))))))

;; Template-based in-buffer completion (tempel.el)
;; NOTE 2023-01-19: Check the `templates'
(use-package! tempel
  :bind
  (;; ("M-+" . tempel-complete) ;; Alternative tempel-expand
   ("M-*" . tempel-insert))
  :bind (:map tempel-map (([backtab] . tempel-previous)
                          ("TAB" . tempel-next)))
  :init
  (setq tempel-path (expand-file-name "var/tempel-templates.eld" user-dotemacs-dir))
  :config
  ;; (global-tempel-abbrev-mode)
  ;; (setq tempel-trigger-prefix "<") ; conflits with evil-shift

  ;; Setup completion at point
  ;; (defun tempel-setup-capf ()
  ;;   ;; Add the Tempel Capf to
  ;;   ;; `completion-at-point-functions'. `tempel-expand' only triggers on
  ;;   ;; exact matches. Alternatively use `tempel-complete' if you want to
  ;;   ;; see all matches, but then Tempel will probably trigger too often
  ;;   ;; when you don't expect it.  NOTE: We add `tempel-expand' *before*
  ;;   ;; the main programming mode Capf, such that it will be tried first.
  ;;   (setq-local completion-at-point-functions
  ;;               (cons #'tempel-expand
  ;;                     completion-at-point-functions)))
  ;; (add-hook 'prog-mode-hook 'tempel-setup-capf)
  ;; (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Use concrete keys because of org mode
  ;; "M-RET" #'tempel-done
  ;; "M-{" #'tempel-previous
  ;; "M-}" #'tempel-next
  ;; "M-<up>" #'tempel-previous
  ;; "M-<down>" #'tempel-next

  ;; 2023-10-19 disable my custom
  (define-key tempel-map (kbd "RET") #'tempel-done)
  (define-key tempel-map (kbd "M-n") #'tempel-next)
  (define-key tempel-map (kbd "M-p") #'tempel-previous)

  (use-package! tempel-collection))

;;;;; imenu-list

;;;;###autoload
(defun my/imenu-list-tuncates-without-tab-line ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    ;; (tab-line-mode -1)
    (toggle-truncate-lines t)))

;; Show an outline summary of the current buffer.
(use-package! imenu-list
  :init
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-auto-update t)
  (setq imenu-list-idle-update-delay 1.0)
  (add-hook 'imenu-list-major-mode-hook #'my/imenu-list-tuncates-without-tab-line)
  :config

  ;;;;###autoload
  (defun spacemacs/imenu-list-smart-focus ()
    "Focus the `imenu-list' buffer, creating as necessary.
If the imenu-list buffer is displayed in any window, focus it, otherwise create and focus.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
    (interactive)
    (if (get-buffer-window imenu-list-buffer-name t)
        (imenu-list-show)
      (imenu-list-smart-toggle)))

  (progn ; imenu-list--set-mode-line
    (setq imenu-list-mode-line-format
          (eval
           '(progn
              (require 'doom-modeline)
              (doom-modeline-def-segment imenu-workspace-name
                "Display imenu"
                (propertize (format "%s" (buffer-name imenu-list--displayed-buffer))
                            'face (doom-modeline-face 'doom-modeline-buffer-minor-mode)))
              (doom-modeline-def-modeline 'imenu '(bar window-number " " major-mode) '(imenu-workspace-name))
              (doom-modeline 'imenu))))
    )
  )

;; (after! winum
;;   (define-key
;;    winum-keymap
;;    [remap winum-select-window-8]
;;    #'spacemacs/imenu-list-smart-focus))

;;;;; ace-link

(use-package! ace-link
  :config
  (with-eval-after-load 'info
    (define-key Info-mode-map "o" 'ace-link-info))
  (with-eval-after-load 'help-mode
    (define-key help-mode-map "o" 'ace-link-help))
  (with-eval-after-load 'woman
    (define-key woman-mode-map "o" 'link-hint-open-link)))

;;;;; deadgrep

(use-package! deadgrep
  :defer t
  :after consult
  :commands deadgrep
  :custom (deadgrep-project-root-function 'projectile-project-root))

;;;;; rg ripgrep

(use-package! rg
  :defer t
  :config
  ;; (rg-enable-default-bindings) ;; use =C-c s=
  (setq rg-executable "rg") ; defaults to (executable-find "rg") which can be wrong on Windows
  (rg-enable-menu)          ; start w/ C-c s p, "rg-project"

  ;; (setq rg-command-line-flags '("--hidden" "--follow"))

  ;; rg-mode binds C-n and C-p to go to next/prev file rather than by line
  ;; which is a bit jarring.
  ;; (define-key rg-mode-map (kbd "C-n") nil)
  ;; (define-key rg-mode-map (kbd "C-p") nil)

  ;; 버퍼가 열리면 포커스를 그쪽으로 이동시킨다.
  ;; 이거 없으면 생각보다 귀찮아진다.
  (add-hook 'rg-mode-hook (lambda () (switch-to-buffer-other-window "*rg*")))

  (rg-define-search rg-files-without-match
    :format literal
    :flags ("--files-without-match")
    :menu ("Custom" "@" "Files without matches"))

  (rg-define-search rg-search-all       ; C-c s a: search all in project
    "Search all files in project with rg"
    :files "everything"
    :dir project
    :menu ("Search" "a" "All in project")
    )
  (rg-define-search rg-search-dir       ; C-c s d: search in current dir
    "Search in current dir with rg"
    :files "everything"
    :dir current
    :menu ("Search" "C" "All in current dir")
    )
  )

;;;;; affe

;; affe-grep: Filters the content of all text files in the current directory
;; affe-find: Filters the file paths of all files in the current directory
;; (use-package affe!
;;   :defer 5
;;   :config
;;   ;; (consult-customize affe-grep :preview-key (kbd "M-."))
;;   (defvar affe-orderless-regexp "")
;;   (defun affe-orderless-regexp-compiler (input _type)
;;     (setq affe-orderless-regexp (orderless-pattern-compiler input))
;;     (cons affe-orderless-regexp
;;           (lambda (str) (orderless--highlight affe-orderless-regexp str))))
;;   (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
;;   )

;;;;; fzf fuzzy find

;; git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
(use-package! fzf
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;;fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        ;; fzf/position-bottom t
        fzf/window-height 15))

;;;;; expand-region

(use-package! expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  ;; Easily navigate sillycased words
  (global-subword-mode +1)
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"
        expand-region-subword-enabled t))

;;;;; separedit

;; ~/sync/man/dotsamples/doom/agzam-dot-doom/modules/custom/writing/config.el
(use-package! separedit
  :defer t
  :commands (separedit separedit-dwim)
  :init
  (map! :map prog-mode-map :inv "C-c '"
        (cmd! () (cond
                  ((bound-and-true-p org-src-mode) (org-edit-src-exit))
                  ((eq major-mode 'separedit-double-quote-string-mode) (separedit-commit))
                  (t (separedit-dwim)))))
  (map! :map (separedit-double-quote-string-mode-map
              separedit-single-quote-string-mode-map)
        :inv "C-c '" #'separedit-commit)
  (map! :map minibuffer-local-map "C-c '" #'separedit)
  :config
  (setq separedit-default-mode 'markdown-mode))

;;;;; youtube-sub-extractor

;; agzam
(use-package! youtube-sub-extractor
  :commands (youtube-sub-extractor-extract-subs)
  :config
  (map! :map youtube-sub-extractor-subtitles-mode-map
        :desc "copy timestamp URL" :n "RET" #'youtube-sub-extractor-copy-ts-link
        :desc "browse at timestamp" :n "C-c C-o" #'youtube-sub-extractor-browse-ts-link
        :n "q" #'kill-buffer-and-window))

;;;; proced process monitor htop - built-in

(use-package! proced
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

;;;; :term

;;;;; vterm

(after! vterm
  (setq vterm-max-scrollback 10000)

  ;; 빠른 원격 접속 함수
  (defun my/connect-storage ()
    "Connect to storage server"
    (interactive)
    (vterm)
    (vterm-send-string "mosh goqual@storage-01")
    (vterm-send-return))

  ;; 키바인딩
  (global-set-key (kbd "C-c -") 'my/connect-storage)
  ;; (setq x-gtk-use-native-input nil) ;; 2025-08-10 Important with ibus korean input

  ;; kime 환경변수 설정 (기존 코드 유지)
  (add-to-list 'vterm-environment "GTK_IM_MODULE=fcitx5")
  (add-to-list 'vterm-environment "QT_IM_MODULE=fcitx5")
  (add-to-list 'vterm-environment "XMODIFIERS=@im=fcitx5")
  (setq vterm-shell "/usr/bin/bash")

  (defun my/vterm-setup-terminal-font ()
    "Setup terminal font for vterm using fontaine"
    (when (and (eq major-mode 'vterm-mode)
               (featurep 'fontaine))
      (setq-local nobreak-char-display nil)
      (setq-local line-number-mode nil)
      (setq-local column-number-mode nil)
      (setq-local scroll-margin 3
                  line-spacing nil)
      (setq-local x-gtk-use-native-input t)
      ;; vterm의 default face에 터미널 폰트 적용
      (face-remap-add-relative 'default
                               :family (fontaine--get-preset-property
                                        fontaine-current-preset :term-family))))
  (add-hook 'vterm-mode-hook #'my/vterm-setup-terminal-font)
  )

;;;;; DONT eat

;; (use-package! eat
;;   :commands (eat)
;;   :init
;;   ;; Runs not compatible eshell term stuff with eat on the same buffer
;;   ;; (add-hook 'eshell-mode-hook #'eat-eshell-mode)
;;   ;; Runs listed 'visual-mode' eshell stuff with eat on separated buffer (takes precedence over the above setting)
;;   ;; (add-hook 'eshell-mode-hook #'eat-eshell-visual-command-mode)

;;   (setq eat-term-name "xterm-256color")
;;   (setq eat-term-scrollback-size 131072)
;;   (setq eat-enable-mouse nil) ; default t
;;   (setq eat-kill-buffer-on-exit t)
;;   (setq eat-shell "/usr/bin/bash")
;;   ;; (advice-add 'eat-semi-char-mode :after 'eat-line-mode)
;;   (add-hook! 'eat-mode-hook
;;     (defun cae-eat-mode-setup-h ()
;;       ;; (auto-fill-mode -1)
;;       (doom-mark-buffer-as-real-h)))
;;   :config

;;   (defun my/eat-send-return ()
;;     "Send <return> to eat."
;;     (interactive)
;;     (eat-term-send-string eat-terminal (kbd "RET")))

;;   ;; 2025-04-05: This resolves the continuation lines issue in EAT terminal
;;   ;; (including eat-shell in `eat-eshell-visual-command-mode').  The
;;   ;; continuation line issue results in, I think, the default font being too
;;   ;; wide, causing the width of the characters to exceed the width of the
;;   ;; window, resulting in ugly continuation lines that ruin the wrapping of the
;;   ;; output.
;;   (defun krisb-eat--setup ()
;;     "Set up an EAT terminal shell."
;;     (interactive)
;;     (when (eq major-mode 'eat-mode)
;;       (setq-local x-gtk-use-native-input t))
;;     (when (featurep 'fontaine)
;;       (setq-local nobreak-char-display nil)
;;       (setq-local line-number-mode nil)
;;       (setq-local column-number-mode nil)
;;       (setq-local scroll-margin 3
;;                   line-spacing nil)
;;       (set-face-attribute 'eat-term-font-0 nil
;;                           ;; This returns the default-family of the current
;;                           ;; preset, whether explicitly or implicitly set
;;                           :family (fontaine--get-preset-property fontaine-current-preset :term-family))))
;;   ;; (add-hook 'eat-mode-hook #'krisb-eat--setup)
;;   )

;;;; :tools writing


;;;;; markdown-mode

;; agzam
(after! markdown-mode ; chatgpt-shell-mode
  (load! "+markdown")

  (setq-default markdown-enable-math t)

  (after! evil
    ;; (advice-add #'evil-ex-start-word-search :around #'evil-ex-visual-star-search-a)
    (advice-add 'evil-yank :around #'maybe-yank-and-convert-a))

  ;; (map! :map (markdown-mode-map
  ;;             chatgpt-shell-mode-map)
  ;;       (:localleader
  ;;        (:prefix ("s" . "wrap")
  ;;                 "<" #'markdown-wrap-collapsible
  ;;                 "C" #'markdown-wrap-code-clojure
  ;;                 "c" #'markdown-wrap-code-generic)))
  )

;;;;; palimpsest

;; M-x palimpsest-move-region-to-bottom
;; M-x palimpsest-move-region-to-top
;; M-x palimpsest-move-region-to-trash
(use-package! palimpsest
  :after org
  :hook (org-mode . palimpsest-mode))

;;;;; org-web-tools

(use-package! org-web-tools)

;;;;; corg

(use-package! corg
  :after org
  :config
  (add-hook 'org-mode-hook #'corg-setup))

;;;;; edit-indirect

;; agzam
(after! edit-indirect
  ;; I want indirect buffers to always appear on the right side of current window
  (add-to-list
   'display-buffer-alist
   `("\\*edit-indirect .*\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right))))

;;;;; quarto-mode : jupyter alternates with polymode

;; polymode
;; (use-package! quarto-mode
;;   :mode (("\\.Rmd" . poly-quarto-mode)))
;(use-package!  quarto-mode
;  :mode (("\\.[qQ][mM][dD]" . poly-quarto-mode)))

;;;;; checkers : spelling

;;;;;:checkers (spell +flyspell)

;; Default spelling dictionary is English
(unless IS-TERMUX
  ;; (after! flyspell
  (require 'ispell)
  (setq ispell-dictionary "english")
  (setq ispell-personal-dictionary (concat user-dotemacs-dir "var/aspell.en.pws"))

  (remove-hook! '(org-mode-hook
                  markdown-mode-hook
                  TeX-mode-hook
                  rst-mode-hook
                  ;; mu4e-compose-mode-hook
                  message-mode-hook
                  ;; git-commit-mode-hook
                  )
    #'flyspell-mode)
  )

;;;;; jinx for hangul with hunspell

;; 2024-05-21 성능이 빠르다면 한글을 이걸로 써야 한다.
(use-package! jinx
  :config
  (setq jinx-delay 0.5) ; default 0.2
  ;; (dolist (hook '(text-mode-hook conf-mode-hook)) ; prog-mode-hook
  ;;   (add-hook hook #'jinx-mode))

  ;; (add-hook 'org-mode-hook #'jinx-mode)
  ;; (add-hook 'prog-mode-hook #'jinx-mode) ; 주석

  ;; 1) 영어 제외 : 한글만 검사
  ;; 2) 한글 영어 선택하도록 제공
  ;; 한글 일 경우는 ko.dic 을 ~/.hunspell_ko_personal 으로 심볼링 링크 해줄 것
  ;; enchant.ordering -> ../../mydotfiles/config-common/.config/enchant/enchant.ordering
  ;; hunspell -> /usr/share/hunspell/
  ;; ~/.config/enchant/ko.dic -> /home/junghan/dotemacs/var/hunspell_ko_personal
  ;; 2) enchant.ordering hunspell 변경 ko_KR:hunspell
  (setq jinx-languages "ko")
  ;; (setq jinx-exclude-regexps '((t "[A-Za-z]" "[']")))
  (setq jinx-exclude-regexps
        '((emacs-lisp-mode "Package-Requires:.*$")
          (t "[A-Za-z]" "[']" "[A-Z]+\\>" "-+\\>" "\\w*?[0-9]\\w*\\>" "[a-z]+://\\S-+" "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" "\\(?:Local Variables\\|End\\):\\s-*$" "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")))

  ;; 아래는 기본인데 일단 해보면서 보자.
  ;; "[A-Z]+\\>"         ;; Uppercase words
  ;; "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
  ;; "[a-z]+://\\S-+"    ;; URI
  ;; "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" ;; Email
  ;; "\\(?:Local Variables\\|End\\):\\s-*$" ;; Local variable indicator
  ;; "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")) ;; Local variables

  ;; C-; embark-dwim
  ;; C-: 점 앞의 철자가 틀린 단어에 대한 수정을 트리거합니다.
  ;; C-u M-$전체 버퍼에 대한 수정을 트리거합니다.
  (keymap-global-set "C-:" #'jinx-correct)
  (keymap-global-set "C-M-$" #'jinx-languages)

  ;; 'z =' ispell-word
  (map! :map (org-mode-map
              markdown-mode-map
              text-mode-map
              chatgpt-shell-mode-map)
        :n ", SPC" #'jinx-correct
        ;; :n ", 4" #'jinx-autocorrect-last+
        ;; :i ", ," #'insert-comma
        ;; :i ", m" #'jinx-autocorrect-last+
        ;; :i ", SPC" (cmd! (jinx-autocorrect-last+ :prompt))
        )

  ;; /tecosaur-dot-doom/config.org
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
  ;; Take over the relevant bindings.
  (after! ispell
    (global-set-key [remap ispell-word] #'jinx-correct))
  (after! evil-commands
    (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
    (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous))
  ;; I prefer for `point' to end up at the start of the word,
  ;; not just after the end.
  ;; (advice-add 'jinx-next :after (lambda (_) (left-word)))
  )

;;;;;; jinx for english

;; 영어만 검사
;; (setenv "LANG" "en_US.UTF-8")
;; (setq jinx-languages "en")

;; (add-hook 'jinx-mode-hook (lambda () (setq-local jinx-languages "en")))
;; (setq jinx-exclude-regexps
;;       '((emacs-lisp-mode "Package-Requires:.*$")
;;         (t "[가-힣]" "[A-Z]+\\>" "-+\\>" "\\w*?[0-9]\\w*\\>" "[a-z]+://\\S-+" "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" "\\(?:Local Variables\\|End\\):\\s-*$" "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")))

;; 1) 개인사전 연결해줄 것
;; ~/.config/enchant/en.dic -> /home/junghan/.aspell_en_personal
;; ko.dic -> /home/junghan/.aspell_en_personal 뭐지? 이렇게 들어가네?!
;; 한글 일 경우는 ko.dic 을 ~/.hunspell_ko_personal 으로 심볼링 링크 해줄 것
;; 되는데로 하자
;; 2) enchant.ordering aspell 로 변경할 것
;; 	*:nuspell,aspell,hunspell
;; en_AU:aspell,hunspell,nuspell
;; en_CA:aspell,hunspell,nuspell
;; en_GB:aspell,hunspell,nuspell
;; en_US:aspell,hunspell,nuspell
;; en:aspell,hunspell,nuspell

;;;;; d2 / mermaid / plantuml

(after! org
  ;; M-x plantuml-download-jar
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0)

  ;; (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
  ;;       org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

  ;; sudo apt-get install ditaa
  (require 'ob-ditaa)
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

;; Mermaid is a tool for drawing systems diagrams.
;; *NOTE*: The variable =ob-mermaid-cli-path= needs to be set in the config (because it will change from system to system).
;; - npm install -g @mermaid-js/mermaid-cli
;; - mmdc -i input.mmd -o output.svg

(use-package! mermaid-mode
  :config
  (map! :localleader
        :map (mermaid-mode-map)
        "c" 'mermaid-compile
        "f" 'mermaid-compile-file
        "b" 'mermaid-compile-buffer
        "r" 'mermaid-compile-region
        "b" 'mermaid-open-browser
        "d" 'mermaid-open-doc))

;; (use-package! d2-mode
;;   :mode "\\.d2\\'"
;;   :config
;;   (setq d2-output-format ".png")
;;   (map! :localleader
;;         :map (d2-mode-map)
;;         "h" #'d2-open-doc
;;         "v" #'d2-view-current-svg
;;         "o" #'d2-open-browser
;;         "c" #'d2-compile
;;         "f" #'d2-compile-file
;;         "b" #'d2-compile-buffer
;;         "r" #'d2-compile-region
;;         "F" #'d2-compile-file-and-browse
;;         "B" #'d2-compile-buffer-and-browse
;;         "R" #'d2-compile-region-and-browse
;;         "o"  #'d2-open-browser
;;         "v"  #'d2-view-current-svg
;;         "h"  #'d2-open-doc))

;;;;; orgabilize

(use-package! orgabilize
  :after org
  :defer 10
  :config
  (setq orgabilize-org-archive-directory (concat org-directory "import/")))

;;;;; org-pandoc-import

(use-package! org-pandoc-import
  :after org
  :defer 10
  :commands (org-pandoc-import-as-org)
  :config
  (require 'org-pandoc-import))

;; ("C" org-pandoc-import-csv-as-org "Import CSV")

;;;;; redacted

(use-package! redacted
  :defer t
  :commands (redacted-mode))

;;;;; hypothesis

;; M-x hypothesis-to-org downloads the 200 newest notations and inserts
;; them into a temporary org-mode buffer. M-x hypothesis-to-archive
;; imports notations into hypothesis-archive. It will import up to 200
;; notations but will only import notations made after the last import.
;; (use-package! hypothesis
;;   :defer 5
;;   :commands hypothesis-to-org hypothesis-to-archive
;;   :config
;;   (setq hypothesis-username user-hypothesis-username)
;;   (setq hypothesis-token user-hypothesis-token)
;;   (setq hypothesis-quote-prefix "#+begin_example")
;;   (setq hypothesis-quote-sufix "#+end_example")
;;   )

;; (after! hypothesis
;;   (setq hypothesis-archive (my/org-links-file)))

;;;;; guess-language

(use-package! guess-language
  :demand t
  :init
  (setq guess-language-langcodes
        '((en . ("en" "English" "🇬🇧" "English"))
          (ko . ("ko" "Korean" "🇰🇷" "Korean"))))
  (setq guess-language-languages '(ko en))
  (setq guess-language-min-paragraph-length 35)
  )

;;;;; immersive-translate

(use-package! immersive-translate
  :defer 5
  :init
  (setq immersive-translate-backend 'trans)
  ;; (setq immersive-translate-backend 'lmstudio)
  ;; (setq immersive-translate-lmstudio-model "llama-3-korean-bllossom-8b")

  (setq immersive-translate-failed-message "💢")
  (setq immersive-translate-trans-target-language "ko")
  ;; (setq immersive-translate-auto-idle 2.0) ; default 0.5
  ;; wget git.io/trans; chmod +x trans; sudo mv trans /usr/local/bin
  ;; ko         Korean                         한국어
  :config
  (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
  (add-hook 'Info-mode-hook #'immersive-translate-setup)
  (add-hook 'help-mode-hook #'immersive-translate-setup)
  (add-hook 'helpful-mode-hook #'immersive-translate-setup)
  ;; ;; (add-hook 'nov-mode-hook #'immersive-translate-setup)
  ;; (setq immersive-translate-backend 'chatgpt) ; 2025-04-08 not working
  ;; (setq immersive-translate-chatgpt-host "api.x.ai")
  ;; (setq immersive-translate-chatgpt-model "grok-2-latest")
  )

;;;; :workspace

;;;;; outli

(use-package! outli
  :init (setq outli-speed-commands nil)
  :hook (prog-mode . outli-mode)
  :config
  (add-to-list 'outli-heading-config '(c-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(bibtex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(yaml-mode "##" ?# t))

  (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
  (add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))

  (add-hook 'bibtex-mode-hook 'outli-mode) ; not markdown-mode!
  ;; (add-hook 'org-mode-hook 'outli-mode)

  ;; Add h as narrow prefix for headings in consult-imenu
  (require 'consult-imenu)
  (push '(?h "Section") (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types))
  )

;;;;; treemacs

(setq treemacs-position 'left)
(setq +treemacs-git-mode 'deferred)

(after! treemacs
  ;; (setq treemacs-follow-mode t)
  ;; (setq treemacs-git-mode nil)
  ;; (setq treemacs-move-files-by-mouse-dragging nil)
  (setq
   treemacs-width 45
   treemacs-imenu-scope 'current-project
   treemacs-indentation 1
   treemacs-space-between-root-nodes nil ; spacing in treemacs views
   treemacs-fringe-indicator-mode nil ; default t
   treemacs-show-cursor nil ; default nil
   )

  ;; check treemacs-compatibility.el
  (progn
    (setq treemacs-follow-after-init t
          ;; treemacs-is-never-other-window t
          treemacs-sorting 'alphabetic-case-insensitive-asc
          ;; treemacs-persist-file (concat doom-cache-dir "treemacs-persist")
          ;; treemacs-last-error-persist-file (concat doom-cache-dir "treemacs-last-error-persist")
          )

    (after! dired (treemacs-resize-icons 16))
    ;; (treemacs-follow-mode 1)

    (add-hook! 'treemacs-mode-hook #'hl-line-mode
      (defun treemacs--dont-ignore-winum-h ()
        (setq winum-ignored-buffers-regexp
              (remove (regexp-quote
                       (format "%sScoped-Buffer-"
                               treemacs--buffer-name-prefix))
                      winum-ignored-buffers-regexp))))

    (after! winum
      (setq winum-ignored-buffers-regexp
            (remove ".*Treemacs.*" winum-ignored-buffers-regexp)))

    (setq treemacs-user-mode-line-format
          (eval
           '(progn
              (require 'doom-modeline)
              (doom-modeline-def-segment treemacs-workspace-name
                "Display treemacs."
                (propertize (format "%s" (treemacs-workspace->name (treemacs-current-workspace)))
                            'face (doom-modeline-face 'doom-modeline-buffer-minor-mode)))
              (doom-modeline-def-modeline 'treemacs '(bar window-number " " major-mode) '(treemacs-workspace-name))
              (doom-modeline 'treemacs))))
    )
  )

;;;;; tabgo

(use-package! tabgo)

;;;; :tools magit vc

;;;;; magit

;; hilsner
(setq magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t)

(setq evil-collection-magit-want-horizontal-movement t) ; default nil

;; Location of Git repositories
;; define paths and level of sub-directories to search
(setq magit-repository-directories
      '( ;; ("~/doomemacs/" . 0)
        ("~/dotemacs/" . 0) ("~/office/" . 2) ("~/git/" . 2)
        ;; ("~/mydotfiles/" . 0)
        ;; ("~/sync/code/" . 2)
        ))

(after! magit
  ;; Use Emacs as $EDITOR (or $GIT_EDITOR) for git commits messages
  ;; when using git commit on the command line
  ;; (global-git-commit-mode t)

  ;; Commit message checks
  ;; ~/.config/emacs/modules/emacs/vc/config.el
  ;; - checks for overlong-summary-line non-empty-line
  ;; (setq git-commit-summary-max-length 50
  ;;       git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))

  ;; Enforce git commit conventions.
  ;; See: http://chris.beams.io/posts/git-commit
  (require 'git-commit)
  (setq git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (evil-set-initial-state 'git-commit-mode 'insert)

  (setq git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (setq
   ;; Highlight specific characters changed
   magit-diff-refine-hunk 'all
   ;; Show Libravatar of commit author
   magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  ;; Number of topics displayed (issues, pull requests)
  ;; open & closed, negative number for closed topics
  ;; or `forge-toggle-closed-visibility'
  ;; set closed to 0 to never show closed issues
  ;; (setq  forge-topic-list-limit '(100 . 0))
  (setq forge-topic-list-limit '(100 . -10))
  ;; GitHub user and organization accounts owned
  ;; used by @ c f  to create a fork
  (setq forge-owned-accounts '(("junghan0611" "junghanacs")))

  ;; Blacklist specific accounts, over-riding forge-owned-accounts
  ;; (setq forge-owned-blacklist
  ;;       '(("bad-hacks" "really-bad-hacks")))
  ;; End of Version Control configuration

  ;; 2025-01-27 reset transient-display-buffer-action for gptel-menu
  (setq transient-display-buffer-action
        '(display-buffer-in-side-window
          (side . bottom)
          (dedicated . t)
          (inhibit-same-window . t)))

  ;; MattheZMD-dotfiles-aidermacs-eaf/README.md
  (defun my/magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t))
  )

;;;;; DONT git-commit : categories

;; from /doom/cashpw-dotfiles-node/config/doom/config-personal.org

;; (progn
;;   (require 'git-commit)

;;   (defgroup cashpw/source-control nil
;;     "Source control."
;;     :group 'cashpw)

;;   (defcustom cashpw/source-control--commit-categories
;;     '(("Fix" . (:symbol "🐛"
;;                 :shortcode ":bug:"))
;;       ("UI" . (:symbol "💄"
;;                :shortcode ":lipstick:"))
;;       ("UX" . (:symbol "💄"
;;                :shortcode ":lipstick:"))
;;       ("Add" . (:symbol "✨"
;;                 :shortcode ":sparkles:"))
;;       ("Feature" . (:symbol "✨"
;;                     :shortcode ":sparkles:"))
;;       ("Document" . (:symbol "📝"
;;                      :shortcode ":memo:"))
;;       ("Typo" . (:symbol "✏️"
;;                  :shortcode ":pencil2:"))
;;       ("Refactor" . (:symbol "♻"
;;                      :shortcode ":recycle:"))
;;       ("Rollout" . (:symbol "🚀"
;;                     :shortcode ":rocket:"))
;;       ("Launch" . (:symbol "🚀"
;;                    :shortcode ":rocket:"))
;;       ("Version" . (:symbol "🔖"
;;                     :shortcode ":bookmark:"))
;;       ("Release" . (:symbol "🔖"
;;                     :shortcode ":bookmark:"))
;;       ("Deploy" . (:symbol "🚀"
;;                    :shortcode ":rocket:"))
;;       ("Delete" . (:symbol "🔥"
;;                    :shortcode ":fire:"))
;;       ("Remove" . (:symbol "🔥"
;;                    :shortcode ":fire:"))
;;       ("Test" . (:symbol "✅"
;;                  :shortcode ":white_check_mark:")))
;;     "Alist of commit categories and extras."
;;     :group 'cashpw/source-control
;;     :type 'string)

;;   (defun cashpw/source-control--read-commit-category ()
;;     "Return commit noun as selected by user."
;;     (let ((category (completing-read "Category: "
;;                                      cashpw/source-control--commit-categories
;;                                      ;; predicate
;;                                      nil
;;                                      ;; require-match
;;                                      t)))
;;       (assoc category
;;              cashpw/source-control--commit-categories)))

;;   (defun cashpw/source-control--commit--section (title content)
;;     "Return formatted section for a commit message."
;;     (s-lex-format "## ${title}

;; ${content}"))

;;   (defun cashpw/source-control--commit--build-message ()
;;     "Return commit message template."
;;     (let* ((category (cashpw/source-control--read-commit-category))
;;            (emoji (plist-get (cdr category) :symbol))
;;            ;; (what-section (cashpw/source-control--commit--section "What does this change?"
;;            ;;                                                       "1. TODO"))
;;            ;; (why-section (cashpw/source-control--commit--section "Why make these changes?"
;;            ;;                                                      "TODO"))
;;            )
;;       (s-lex-format "${emoji} ")))

;;   (defun cashpw/source-control--commit--insert-message ()
;;     "Insert my commit message template."
;;     (insert (cashpw/source-control--commit--build-message)))

;;   (add-hook! 'git-commit-setup-hook
;;              'cashpw/source-control--commit--insert-message)
;;   )

;;;;; magit-todos

;; Show project TODO lines in Magit Status
(use-package! magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

;;;;; git-cliff

;; [[denote:20230612T070000]]
(use-package! git-cliff
  :defer t
  :after (magit transient)
  :custom (git-cliff-enable-examples t)
  :config
  ;; git-cliff-extra-path : directory storing user defined presets and templates.
  ;; Integrate to `magit-tag'
  (with-eval-after-load 'magit-tag
    (transient-append-suffix
      'magit-tag '(1 0 -1) '("c" "changelog" git-cliff-menu))))

;;;;; consult-git-log-grep

(use-package! consult-git-log-grep
  :after magit
  :defer t
  :custom (consult-git-log-grep-open-function #'magit-show-commit)
  :bind (("C-c K" . consult-git-log-grep)))

;;;;; gist

(use-package! gist
  :defer t
  :config
  ;; view your Gist using `browse-url` after it is created
  (setq gist-view-gist t))

;;;;; consult-gh

(use-package! consult-gh
  :after consult
  :commands (consult-gh-search-repos
             consult-gh-search-code
             consult-gh-search-prs
             consult-gh-search-issues
             consult-gh-pr-list
             consult-gh-issue-list
             consult-gh-default-repos
             consult-gh-find-file
             consult-gh-repo-clone
             consult-gh-repo-fork)
  :custom
  (consult-gh-repo-maxnum 30) ;;set max number of repos to 30
  (consult-gh-issues-maxnum 100) ;;set max number of issues to 100
  (consult-gh-show-preview t)
  (consult-gh-preview-key "M-m")
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-prioritize-local-folder 'suggest)
  (consult-gh-preview-buffer-mode 'org-mode) ;; show previews in org-mode

  (consult-gh-repo-action #'consult-gh--repo-browse-files-action) ;;open file tree of repo on selection
  (consult-gh-issue-action #'consult-gh--issue-view-action) ;;open issues in an emacs buffer
  (consult-gh-pr-action #'consult-gh--pr-view-action) ;;open pull requests in an emacs buffer
  (consult-gh-code-action #'consult-gh--code-view-action) ;;open files that contain code snippet in an emacs buffer
  (consult-gh-file-action #'consult-gh--files-view-action) ;;open files in an emacs buffer
  :config

  (require 'consult-gh-transient)

  ;; set the default folder for cloning repositories, By default Consult-GH will confirm this before cloning
  (setq consult-gh-default-clone-directory "~/git/clone/")
  (setq consult-gh-default-save-directory "~/Downloads")

  (dolist (repo '("junghan0611" "junghanacs" "agzam" "minad" "alphapapa"
                  "LemonBreezes" "protesilaos" "armindarvish"
                  "doomemacs" "tecosaur"))
    (add-to-list 'consult-gh-favorite-orgs-list repo))

  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)

  ;; Install `consult-gh-embark' for embark actions
  (with-eval-after-load 'embark
    (require 'consult-gh-embark)
    (consult-gh-embark-mode +1))

  ;; Install `consult-gh-forge' for forge actions
  ;; (with-eval-after-load 'forge
  ;;    (require 'consult-gh-forge)
  ;;    (consult-gh-forge-mode +1)
  ;;    (setq consult-gh-forge-timeout-seconds 20))
  )

;;;;; magit-blame-color-by-age

;; (require 'magit-blame-color-by-age)
(use-package! magit-blame-color-by-age
  :defer t :after-call magit-blame-mode-hook :config
  ;; Double-check that nothing else has modified the Git Blame header before
  ;; modifying it.
  (when (string= "%-20a %C %s\n"
                 (alist-get 'heading-format
                            (alist-get 'headings magit-blame-styles)))
    (setf (alist-get 'heading-format (alist-get 'headings magit-blame-styles))
          "%C %-20a %s\n"))
  (setq magit-blame-color-by-age-full-heading nil)
  (magit-blame-color-by-age-mode +1))

;;;; :lang org

;;;;; doom packages
;;;;;; org-noter

;; (require 'org-noter)
(setq org-noter-notes-search-path (list (concat user-org-directory "notes/")))
(setq org-noter-default-notes-file-names "20240902T165404--org-noter.org")

;;;;;; format-on-save-disabled-modes

(setq +format-on-save-disabled-modes
      '( ;; emacs-lisp-mode  ; elisp's mechanisms are good enough
        ;; org-mode
        org-msg-edit-mode
        sql-mode ; sqlformat is currently broken
        tex-mode ; latexindent is broken
        latex-mode))

;;;;;; org-mode-hook for ispell

(add-hook 'org-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'ispell-completion-at-point t)))

;;;;;; org-contacts-files

(setq org-contacts-files org-user-contacts-files)

;;;;;; TODO org-cliplink

(progn
  (require 'org-cliplink)
  (setq org-cliplink-max-length 72)
  (setq org-cliplink-ellipsis "-")

  ;; from ohyecloudy
  (defun my/org-cliplink ()
    (interactive)
    (org-cliplink-insert-transformed-title
     (org-cliplink-clipboard-content) ;take the URL from the CLIPBOARD
     #'my-org-link-transformer))

  (defun my-org-link-transformer (url title)
    (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
           (host-url
            (replace-regexp-in-string "^www\\." "" (url-host parsed-url)))
           (clean-title
            (cond
             ;; if the host is github.com, cleanup the title
             ((string= (url-host parsed-url) "github.com")
              (replace-regexp-in-string
               "^/" "" (car (url-path-and-query parsed-url))))
             ;; otherwise keep the original title
             (t
              (my-org-cliplink--cleansing-site-title title))))
           (title-with-url (format "%s - %s" clean-title host-url)))
      ;; forward the title to the default org-cliplink transformer
      (org-cliplink-org-mode-link-transformer url title-with-url)))

  (defun my-org-cliplink--cleansing-site-title (title)
    (let ((result title)
          (target-site-titles
           '(" - 위키백과"
             " - Wikipedia"
             " - PUBLY"
             " - YES24"
             "알라딘: "
             " : 클리앙"
             " - YouTube")))
      (dolist (elem target-site-titles)
        (if (string-match elem result)
            (setq result (string-replace elem "" result))
          result))
      result))

  ;; 마지막에 host 를 붙이고 싶어서 link transformer 함수를 짰다. =title -
  ;; ohyecloudy.com= 식으로 org link 를 만든다.
  (define-key org-mode-map [remap org-cliplink] 'my/org-cliplink)
  )

;;;;; additional packages

;;;;;; org-download

(use-package! org-download
  :after org
  :hook (;; (dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :commands (org-download-enable
             org-download-yank
             org-download-screenshot)
  :config
  (setq-default org-download-heading-lvl nil)
  (setq org-download-method 'directory) ; doom 'attach
  (setq-default org-download-image-dir "~/screenshot" ) ;; share all devieces
  (setq org-download-display-inline-images nil)
  (setq org-download-timestamp"%Y%m%dT%H%M%S-") ;; denote id

  ;; #+caption: "
  ;; #+name: fig-"
  ;; #+attr_html: :width 40% :align center"
  ;; #+attr_latex: :width \\textwidth"
  (setq org-download-image-attr-list
        '("#+attr_html: :width 80% :align center"
          "#+attr_latex: :width \\textwidth"
          "#+attr_org: :width 800px"))

  ;; (defun kimim/org-download-annotate (link)
  ;;   "Annotate LINK with the time of download."
  ;;   (format "#+name: fig:%s\n#+caption: %s\n"
  ;;           (file-name-base link) (file-name-base link)))
  ;; (setq org-download-annotate-function #'kimim/org-download-annotate)
  )

;;;;;; org-appear

(use-package! org-appear
  :after org
  :if window-system
  :init
  ;; (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autolinks nil ;; default nil
        org-appear-autoemphasis t
        org-appear-autosubmarkers t)
  )

;;;;;; org-rich-yank

(use-package! org-rich-yank
  :defer t
  :commands (org-rich-yank))

;;;;;; org-transclusion

;; (use-package! org-transclusion
;;   :after org
;;   :defer 2
;;   :commands org-transclusion-mode
;;   :config
;;   (set-face-attribute 'org-transclusion-fringe nil :foreground "light green" :background "lime green")
;;   )

;; (after! org-transclusion
;;   (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
;;   (require 'org-transclusion-indent-mode))

;;;;;; org-remark

;; (use-package! org-remark
;;   :after org
;;   :config (setq org-remark-notes-file-name (my/org-remark-file))
;;   (org-remark-create "red-line"
;;                      '(:underline (:color "magenta" :style wave))
;;                      '(CATEGORY "review" help-echo "Review this"))
;;   (org-remark-create "yellow"
;;                      '(:underline "gold")
;;                      '(CATEGORY "important"))

;;   ;; It is recommended that `org-remark-global-tracking-mode' be enabled when
;;   ;; Emacs initializes. Alternatively, you can put it to `after-init-hook' as in
;;   ;; the comment above
;;   ;; (require 'org-remark-global-tracking)
;;   ;; (org-remark-global-tracking-mode +1)

;;   ;; Optional if you would like to highlight websites via eww-mode
;;   ;; (with-eval-after-load 'eww (org-remark-eww-mode +1))
;;   ;; Optional if you would like to highlight EPUB books via nov.el
;;   ;; (with-eval-after-load 'nov (org-remark-nov-mode +1))
;;   ;; Optional if you would like to highlight Info documentation via Info-mode
;;   ;; (with-eval-after-load 'info (org-remark-info-mode +1))
;;   )

;;;;;; remember

(use-package! remember
  :commands remember
  :init
  (setq
   remember-notes-initial-major-mode 'org-mode
   remember-notes-auto-save-visited-file-name t)
  :config (setq remember-data-file (my/org-remember-file)))

;;;;;; Repetation : org-drill

(use-package! org-drill
  :defer t
  :init
  ;; save buffers after drill sessions without prompt.
  (setq org-drill-save-buffers-after-drill-sessions-p nil)
  ;; reduce from the default 30 to make it to become a habit.
  (setq org-drill-maximum-items-per-session 10))

;;;;;; ox-reveal vs. org-re-reveal

(use-package! ox-reveal
  :defer t
  :commands org-reveal-export-to-html) ; org-re-reveal better

;;;;;; presentations setup

(defun my/presentation-setup ()
  (hide-mode-line-mode 1)

  ;; (org-display-inline-images 1) ;; Can also use org-startup-with-inline-images

  (display-fill-column-indicator-mode -1)
  (display-line-numbers-mode -1)

  ;; A) Scale the text.  The next line is for basic scaling:
  ;; (setq text-scale-mode-amount 2)
  ;; (text-scale-mode 1)

  ;; B) This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch) ; variable-pitch
  ;;                                    (header-line (:height 2.0) variable-pitch) ; variable-pitch
  ;;                                    (org-document-title (:height 1.5) org-document-title)
  ;;                                    (org-code (:height 1.55) org-code)
  ;;                                    (org-verbatim (:height 1.55) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block)
  ;;                                    (org-block-begin-line (:height 0.85) org-block)))
  )

(defun my/presentation-end ()
  (hide-mode-line-mode 0)

  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1)

  ;; A) Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  ;; (text-scale-mode 0)

  ;; B) If you use face-remapping-alist, this clears the scaling:
  ;; (setq-local face-remapping-alist '((default fixed-pitch default)))
  )

;;;;;; DONT org-excalidraw

;; (use-package! org-excalidraw
;;   :after org
;;   :defer t
;;   :commands (org-excalidraw-create-drawing)
;;   :config
;;   (setq org-excalidraw-directory (concat user-org-directory "resources/excalidraw")))

;;;;;; ews: emacs writing studio

(use-package! ox-epub
  :defer t
  :after org)

;; (after! ox-latex
;;   ;; Multiple LaTeX passes for bibliographies
;;   (setq org-latex-pdf-process
;;         '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;           "bibtex %b"
;;           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;   ;; Clean temporary files after export
;;   (setq org-latex-logfiles-extensions
;;         (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
;;                 "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
;;                 "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
;;                 "tex" "bcf")))

;;   ;; LaTeX templates
;;   (add-to-list
;;    'org-latex-classes
;;    '("crc"
;;      "\\documentclass[krantz2]{krantz}
;;         \\usepackage{lmodern}
;;         \\usepackage[authoryear]{natbib}
;;         \\usepackage{nicefrac}
;;         \\usepackage[bf,singlelinecheck=off]{caption}
;;         \\captionsetup[table]{labelsep=space}
;;         \\captionsetup[figure]{labelsep=space}
;;         \\usepackage{Alegreya}
;;         \\usepackage[scale=.8]{sourcecodepro}
;;         \\usepackage[breaklines=true]{minted}
;;         \\usepackage{rotating}
;;         \\usepackage[notbib, nottoc,notlot,notlof]{tocbibind}
;;         \\usepackage{amsfonts, tikz, tikz-layers}
;;         \\usetikzlibrary{fadings, quotes, shapes, calc, decorations.markings}
;;         \\usetikzlibrary{patterns, shadows.blur}
;;         \\usetikzlibrary{shapes,shapes.geometric,positioning}
;;         \\usetikzlibrary{arrows, arrows.meta, backgrounds}
;;         \\usepackage{imakeidx} \\makeindex[intoc]
;;         \\renewcommand{\\textfraction}{0.05}
;;         \\renewcommand{\\topfraction}{0.8}
;;         \\renewcommand{\\bottomfraction}{0.8}
;;         \\renewcommand{\\floatpagefraction}{0.75}
;;         \\renewcommand{\\eqref}[1]{(Equation \\ref{#1})}
;;         \\renewcommand{\\LaTeX}{LaTeX}"
;;      ("\\chapter{%s}" . "\\chapter*{%s}")
;;      ("\\section{%s}" . "\\section*{%s}")
;;      ("\\subsection{%s}" . "\\subsection*{%s}")
;;      ("\\subsubsection{%s}" . "\\paragraph*{%s}")))
;;   )

;;;;;; tmr

(use-package! tmr
  :after embark
  :config
  (unless IS-TERMUX
    (setq tmr-sound-file
          "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"))
  (setq
   tmr-notification-urgency 'normal
   tmr-description-list 'tmr-description-history)

  (defvar tmr-action-map
    (let ((map (make-sparse-keymap)))
      (define-key map "k" #'tmr-remove)
      (define-key map "r" #'tmr-remove)
      (define-key map "R" #'tmr-remove-finished)
      (define-key map "c" #'tmr-clone)
      (define-key map "e" #'tmr-edit-description)
      (define-key map "s" #'tmr-reschedule)
      map))
  ;; (define-key global-map (kbd "M-g M-t") 'tmr-action-map)

  ;; (with-eval-after-load 'embark
  ;;   (add-to-list 'embark-keymap-alist '(tmr-timer . tmr-action-map))
  ;;   (cl-loop
  ;;    for
  ;;    cmd
  ;;    the
  ;;    key-bindings
  ;;    of
  ;;    tmr-action-map
  ;;    if
  ;;    (commandp cmd)
  ;;    do
  ;;    (add-to-list 'embark-post-action-hooks (list cmd 'embark--restart))))
  )

;;;;;; ox-leanpub

;; By default, the ox-leanpub module sets things up for exporting books in
;; Markua format. If you want to export your books in LFM format, you need to
;; additionally load the ox-leanpub-markdown exporter and tell ox-leanpub-book
;; to set up the corresponding menu entries, as follows:
(use-package! ox-leanpub
  :defer t
  :after org
  :config
  (require 'ox-leanpub-markdown)
  (org-leanpub-book-setup-menu-markdown))

;;;;;; ox-quarto

(use-package! ox-quarto
  :after org)

;;;;;; org-bookmarks with link

;; (use-package! org-bookmarks
;;   :defer t
;;   :after org
;;   :commands (org-bookmarks)
;;   :init (setq org-bookmarks-file (my/org-links-file))
;;   ;; :config
;;   ;; (org-bookmarks-add-org-capture-template t)
;;   ;; (org-bookmarks-add-org-capture-template)
;;   )

;;;;;; org-ql

(use-package! org-ql
  :after org
  :commands org-ql-search)

;;;;;; org-marked-text-overview

(use-package! org-marked-text-overview
  :defer 5
  :after org
  :bind ("M-g l" . org-marked-text-overview-mode))

;;;;;; org-glossary

(use-package! org-glossary
  :after org
  :init
  (setq org-glossary-idle-update-period 1.0) ; 0.5
  ;; (setq org-glossary-autodetect-in-headings t) ; 2024-06-13 new
  ;; :hook (org-mode . org-glossary-mode)
  :config
  (setq org-glossary-collection-root (concat org-directory "dict/"))
  ;; (setq org-glossary-global-terms "global")

  (define-key org-mode-map (kbd "C-}") 'org-glossary-insert-term-reference)
  (define-key org-mode-map (kbd "C-{") 'org-glossary-create-definition)
  (define-key org-mode-map (kbd "C-\"") 'org-glossary-create-definition)
  ;; (setq org-glossary-automatic nil) ;; disable auto-export
  )

;; sample from tecosaur/org-glossary
;; (defun +org-glossary--latex-cdef (backend info term-entry form &optional ref-index plural-p capitalized-p extra-parameters)
;;   (org-glossary--export-template
;;    (if (plist-get term-entry :uses)
;;        "*%d*\\emsp{}%v\\ensp{}@@latex:\\labelcpageref{@@%b@@latex:}@@\n"
;;      "*%d*\\emsp{}%v\n")
;;    backend info term-entry ref-index
;;    plural-p capitalized-p extra-parameters))
;; (org-glossary-set-export-spec
;;  'latex t
;;  :backref "gls-%K-use-%r"
;;  :backref-seperator ","
;;  :definition-structure #'+org-glossary--latex-cdef)

;;;; :tools biblio : citar

(progn
  (require 'citar)
  (require 'bibtex)
  ;; HUGO for quartz
  ;; Setup export processor; default csl/citeproc-el, with biblatex for latex
  ;; (setq citar-notes-paths '("~/sync/org/bib/"))

  (when (boundp 'config-bibfiles)
    (setq citar-notes-paths (list (concat org-directory "bib/")))
    (setq citar-bibliography config-bibfiles)
    (setq bibtex-files config-bibfiles)
    (setq org-cite-global-bibliography config-bibfiles))

  ;; CSL styles directory
  (when (boundp 'org-directory)
    (setq org-cite-csl-styles-dir (concat org-directory ".csl"))
    (setq citar-citeproc-csl-styles-dir (concat org-directory ".csl")))

  ;; Setup export processor; default csl/citeproc-el, with biblatex for latex
  (after! oc
    (require 'citar-citeproc)
    (setq bibtex-files config-bibfiles)
    (setq citar-format-reference-function 'citar-citeproc-format-reference)
    (setq citar-citeproc-csl-style "apa.csl")
    ;; org-cite-csl-link-cites t = generate #citeproc_bib_item_N anchor links
    (setq org-cite-csl-link-cites t)
    (setq org-cite-export-processors '((latex biblatex) (t csl))))

  ;; Managing Bibliographies
  ;; (bibtex-user-optional-fields
  ;;  '(("keywords" "Keywords to describe the entry" "")
  ;;    ("file" "Link to a document file." "" )))
  (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
  (add-hook 'bibtex-mode-hook 'visual-line-mode)
  (add-hook 'bibtex-mode-hook 'smartparens-mode)

  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'citar-history))
  )

;;;; :custom PKM
;;;;; EKG

;; (use-package! ekg
;;   :defer 2
;;   ;; :init (setq ekg-db-file (concat user-org-directory "ekg/ekg.db"))
;;   :commands (ekg-dispatch ekg-capture ekg-capture-url ekg-show-notes-with-all-tags)
;;   :bind
;;   (
;;    ;; ("C-c n u" . ekg-show-notes-with-all-tags)
;;    ;; ("C-c n U" . ekg-capture)
;;    (:map
;;     ekg-notes-mode-map
;;     (("<return>" . ekg-notes-open) ("C-c C-o" . org-open-at-point))))
;;   :config
;;   (setq ekg-db-file (concat org-directory "ekg/ekg.db"))
;;   (require 'ekg-auto-save)
;;   (require 'ekg-embedding)

;;   ;; (ekg-embedding-generate-on-save)
;;   ;; (require 'ekg-llm)

;;   (setq llm-warn-on-nonfree nil)
;;   (require 'llm-openai) ;; The specific provider you are using must be loaded.
;;   (let ((my-provider (make-llm-openai :key user-openai-api-key)))
;;     (setq
;;      ekg-llm-provider my-provider
;;      ekg-embedding-provider my-provider))

;;   ;; (add-to-list 'display-buffer-alist '("*EKG Capture.*\\*"
;;   ;;                                      (display-buffer-in-side-window)
;;   ;;                                      (side . right)
;;   ;;                                      (slot . 0)
;;   ;;                                      (window-width . 80)
;;   ;;                                      ))

;;   ;; (require 'llm-gemini)
;;   ;; (let ((my-provider (make-llm-gemini :key user-gemini-api-key)))
;;   ;;   (setq ekg-llm-provider my-provider
;;   ;;         ekg-embedding-provider my-provider))

;;   ;; (defun ash/capture-literature-note ()
;;   ;;   (interactive)
;;   ;;   (ekg-capture-url (ash/get-current-url) (ash/get-current-title)))

;;   ;; org-store-link 를 해야 한다. org-mode 를 사용하라!
;;   ;; 2024-04-01 아니다. 간단하게 생각해보라. 찾을 수 있게 정보 다루는게 좋을 것 같다. ekg-denote 가보자. 그냥 조직 모드로 간다.
;;   ;; (setq ekg-capture-default-mode 'markdown-mode) ; default 'org-mode

;;   ;; (setq ekg-metadata-separator-text
;;   ;;       "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
;;   ;; (setq ekg-display-note-template
;;   ;;       "%n(id)%n(tagged)%n(titled)%n(text 50)%n(other)")
;;   (setq ekg-notes-display-images nil)

;;   ;; (setq ekg-inline-custom-tag-completion-symbols
;;   ;;       '((?@ . "person") ; default
;;   ;;         (?! . "idea") ; default
;;   ;;         ;; (?$ . "meta")
;;   ;;         ;; (?% . "docs")
;;   ;;         ;; (?\& . "project")
;;   ;;         ))

;;   (unless IS-TERMUX
;;     ;; gleek-dotfiles-ekg/core/lang/core-org.el:802
;;     ;; (setq ekg-logseq-dir (concat +ekg-directory "logseq/"))
;;     ;; (setq ekg-logseq-dir "~/sync/markdown/ekglogseq/")

;;     ;; RESET
;;     ;; (ekg-logseq-set-last-export 0)
;;     ;; (ekg-logseq-set-last-import 0)

;;     (defun +ekg-logseq-sync (&rest args)
;;       (interactive)
;;       (require 'ekg-logseq)
;;       (setq ekg-logseq-dir "~/sync/logseq/logseqfiles/")
;;       (ekg-logseq-sync))
;;     ;; (add-hook 'ekg-note-save-hook '+ekg-logseq-sync)
;;     )

;;   (defun ash/log-to-ekg (text &optional org-mode)
;;     "Log TEXT as a note to EKG's date, appending if possible."
;;     (let ((notes (ekg-get-notes-with-tags (list (ekg-tag-for-date) "log"))))
;;       (if notes
;;           (progn
;;             (setf (ekg-note-text (car notes)) (concat (ekg-note-text (car notes)) "\n" text))
;;             (ekg-save-note (car notes)))
;;         (ekg-save-note (ekg-note-create :text text :mode (if org-mode 'org-mode 'text-mode)
;;                                         :tags `(,(ekg-tag-for-date) "log"))))))
;;   )

;;;;; denote

;;;;;; denote confuguration

(use-package! denote
  :demand t
  :commands
  (denote denote-create-note denote-insert-link denote-show-backlinks-buffer denote-link-ol-store)
  :hook (dired-mode . denote-dired-mode)
  :init
  (require 'denote-silo)
  (require 'denote-sequence)
  (require 'denote-org)
  (require 'denote-markdown) ; markdown-obsidian
  (setq denote-file-type 'org)
  (setq denote-sort-components '(signature title keywords identifier))
  (setq denote-sort-keywords nil) ; denote-sort-keywords-comparison-function - default string-collate-lessp
  (setq denote-infer-keywords t) ; important
  ;; TODO denote-excluded-keywords-regexp
  (setq denote-excluded-directories-regexp "archives")
  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  ;; (setq denote-rename-buffer-format "Denote: %t (%k)")

  ;; The default sequence scheme is `numeric'.
  (setq denote-sequence-scheme 'alphanumeric)

  (setq denote-org-front-matter
        "#+title:      %1$s
#+filetags:   %3$s
#+hugo_lastmod: %2$s
#+date:       %2$s
#+identifier: %4$s
#+export_file_name: %4$s.md
#+description: %1$s
#+hugo_categories: Noname
#+OPTIONS: toc:1

* 히스토리
- %2$s

* 관련메타
#+print_bibliography:

\n")

  ;; (setq denote-modules '(project xref ffap)) ; Enable integration with Emacs modules
  (setq denote-prompts '(subdirectory title keywords)) ; These are the minimum viable prompts for notes
  (setq denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech

  ;; More functionality
  (setq denote-org-store-link-to-heading nil ; default t
        denote-rename-confirmations nil ; default '(rewrite-front-matter modify-file-name)
        denote-save-buffers t) ; default nil

  (add-hook 'org-mode-hook (lambda ()
                             (setq denote-rename-buffer-backlinks-indicator "¶")
                             ;; (setq denote-rename-buffer-format "%t%b")
                             (setq denote-rename-buffer-format "%b %s %t")
                             (denote-rename-buffer-mode +1)))

  (setq denote-directory (expand-file-name user-org-directory))
  ;; (setq denote-dired-directories
  ;;       (list denote-directory
  ;;       (thread-last denote-directory (expand-file-name "bib"))
  ;;       (thread-last denote-directory (expand-file-name "elisp"))
  ;;       (thread-last denote-directory (expand-file-name "docs"))
  ;;       (thread-last denote-directory (expand-file-name "meta"))
  ;;       ;; (thread-last denote-directory (expand-file-name "topic"))
  ;;       (thread-last denote-directory (expand-file-name "notes"))
  ;;       (thread-last denote-directory (expand-file-name "private"))
  ;;       (thread-last denote-directory (expand-file-name "posts"))
  ;;       (thread-last denote-directory (expand-file-name "llmlog"))
  ;;       ;; (thread-last denote-directory (expand-file-name "ekg"))
  ;;       ))
  :config
  (set-register ?n (cons 'file (concat org-directory "notes")))

  ;; (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe) ; from 3.0
  ;; (add-hook 'markdown-mode-hook #'denote-fontify-links-mode-maybe) ; from 3.0

  ;; (progn ;; vedangs tips
  ;;   (unless IS-TERMUX
  ;;     (add-to-list
  ;;      'denote-silo-directories
  ;;      (expand-file-name "~/git/jh-blogookpub/org"))
  ;;     ;; (add-to-list
  ;;     ;;  'denote-silo-directories
  ;;     ;;  (expand-file-name "~/Documents/org")) ; book
  ;;     ;; (add-to-list
  ;;     ;;  'denote-silo-directories (expand-file-name "~/sync/winmacs/org"))
  ;;     )

  ;;   ;; I use Yasnippet to expand these into a better template.
  ;;   (add-to-list 'denote-templates '(reference-note . "reference"))
  ;;   (add-to-list 'denote-templates '(morning . "morningpage"))
  ;;   (add-to-list 'denote-templates '(emotion . "emotion"))
  ;;   (add-to-list 'denote-templates '(insight . "insight"))
  ;;   (add-to-list 'denote-templates '(weekly_intentions . "weekint"))
  ;;   (add-to-list 'denote-templates '(weekly_report . "weekrpt"))

  ;;   (setq denote-dired-directories-include-subdirectories t)
  ;;   ;; If you want to have Denote commands available via a right click
  ;;   ;; context menu, use the following and then enable
  ;;   ;; `context-menu-mode'.
  ;;   ;; (add-hook 'context-menu-functions #'denote-context-menu)
  ;;   )
  ;; end-of progn from vedang's custom

;;;;;; DONT add denote-file-types for quarto - qmd

  ;; (after! denote
  ;;   (let ((quarto (cdr (assoc 'markdown-yaml denote-file-types))))
  ;;     (setf (plist-get quarto :extension) ".qmd")
  ;;     (add-to-list 'denote-file-types (cons 'quarto quarto)))
  ;;   )

;;;;;; docsim

  (use-package! docsim
    :defer 3
    ;; :bind (("C-c n s" . docsim-search)
    ;;        ("C-c n d" . docsim-search-buffer))
    :init
    (setq docsim-assume-english nil) ; default t
    :config
    (setq docsim-search-paths (list org-directory)))

;;;;;; consult-denote

  (use-package! consult-denote
    :after denote
    :hook (org-mode . consult-denote-mode)
    :config
    ;; Prefer `ripgrep' and `fd' variants when available
    (when (executable-find "fd")
      (setopt consult-denote-find-command #'consult-fd))
    (when (executable-find "rg")
      (setopt consult-denote-grep-command #'consult-ripgrep))
    (consult-customize
     consult-denote-find
     consult-denote-grep
     :preview-key '("M-m" :debounce 0.3 "<up>" "<down>" "C-j" "C-k"))
    )

;;;;;; consult-notes

  (use-package! consult-notes
    :defer 2
    :commands (consult-notes consult-notes-search-in-all-notes)
    :init
    (setq consult-notes-denote-display-id t)
    (setq consult-notes-denote-dir t)
    (setq consult-notes-denote-title-margin 2) ; 24
    :config

    (consult-customize
     consult-notes
     :preview-key '("M-m" :debounce 0.3 "<up>" "<down>" "C-j" "C-k"))

    ;; (unless IS-TERMUX
    ;;   (setq consult-notes-file-dir-sources
    ;;         '(("Clone-notes"  ?c  "~/nosync/clone-notes/"))))

    ;; '(("Denote"  ?d  "~/org/denotes")
    ;;  ("Fleeting"  ?f  "~/org/denotes/fleeting")
    ;;  ("Literature"  ?l  "~/org/denotes/literature")
    ;;  ("Permanent"  ?p  "~/org/denotes/permanent")
    ;;  ("Personal"  ?e  "~/org/denotes/personal")
    ;;  ("Journal"  ?j  "~/org/denotes/journal")
    ;;  ("Hub"  ?h  "~/org/denotes/hubs"))

    ;; 1) denote
    (progn
      (defun my/consult-notes-denote--display-keywords (keywords)
        (format "%30s" (if keywords (concat "#" (mapconcat 'identity keywords " ")) ""))) ; default 18

      (defun my/consult-notes-denote--display-dir (dirs)
        (format "%10s" (concat "/" dirs))) ; default 18

      (setq consult-notes-denote-display-keywords-function #'my/consult-notes-denote--display-keywords)
      (setq consult-notes-denote-display-dir-function #'my/consult-notes-denote--display-dir)

      ;; (vertico-sort-function 'vertico-sort-history-alpha)
      ;; https://github.com/mclear-tools/consult-notes/issues/16
      ;; (after! vertico-multiform
      ;; ;;   ;; /doomemacs-junghan0611/modules/completion/vertico/config.el
      ;; ;;   (setq vertico-multiform-categories nil) ; reset nil
      ;; ;;   (setq vertico-multiform-commands nil) ; reset nil
      ;;   (add-to-list 'vertico-multiform-commands
      ;;                '(consult-denote-open (vertico-sort-function . vertico-sort-history-alpha))
      ;;                '(consult-notes (vertico-sort-function . vertico-sort-history-alpha))))
; vertico-sort-alpha

      (consult-notes-denote-mode 1)

      (defun +consult-notes--unbound-org-roam ()
        (fmakunbound 'consult-notes-org-roam-mode)
        (fmakunbound 'consult-notes-org-roam-find-node-relation))
      (+consult-notes--unbound-org-roam)
      )

    ;; 2) heading
    ;; (setq consult-notes-org-headings-files
    ;;       (list
    ;;        (my/org-inbox-file)
    ;;        (my/org-life-file)
    ;;        (my/org-tasks-file)
    ;;        ;; (my/org-diary-file)
    ;;        (my/org-drill-file)
    ;;        (my/org-quote-file)
    ;;        (my/org-mobile-file)
    ;;        (my/org-contacts-file)
    ;;        (my/org-links-file)))
    ;; (consult-notes-org-headings-mode 1)
    )

;;;;;; denote-explore

  ;; 읽어볼 것 https://github.com/pprevos/denote-explore
  (use-package! denote-explore
    :defer 5
    :config
    (setq denote-explore-random-regex-ignore "archive")
    ;; :custom
    ;; Location of graph files
    ;; (denote-explore-network-directory "~/documents/notes/graphs/")
    ;; (denote-explore-network-filename "denote-network")
    ;; Output format
    ;; (denote-explore-network-format 'graphviz)
    ;; (denote-explore-network-graphviz-filetype "svg")
    ;; Exlude keywords or regex
    ;; (denote-explore-network-keywords-ignore '("bib"))
    )

;;;;;; citar-denote

  (use-package! citar-denote
    :after citar
    :demand t ;; Ensure minor mode is loaded
    :bind (:map org-mode-map ("C-c B" . citar-insert-citation)) ;; ("M-B" . citar-insert-preset)
    :commands
    (citar-create-note citar-open-notes citar-denote-open citar-denote-add-citekey)
    :init
    (require 'bibtex)
    (require 'citar)
    :custom
    ;; (citar-open-always-create-notes t)
    ;; (citar-denote-signature t)
    (citar-denote-file-type 'org)
    (citar-denote-subdir t)
    (citar-denote-keyword "bib")
    (citar-denote-title-format "author-year-title") ; default title
    (citar-denote-use-bib-keywords nil)
    (citar-denote-title-format-authors 1)
    (citar-denote-title-format-andstr "and")
    :config
    (setq citar-file-open-functions '(("html" . citar-file-open-external)
                                      ;; ("pdf" . citar-file-open-external)
                                      (t . find-file)))

    ;; FIXME for denote-obsidian
    (setq citar-denote-file-types
          `((org
             :reference-format "#+reference:  %s\n"
             :reference-regex "^#\\+reference\\s-*:")
            (markdown-obsidian ;; 2025-02-03
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-yaml
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-toml
             :reference-format "reference  = %s\n"
             :reference-regex "^reference\\s-*=")
            (text
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")))
    (citar-denote-mode)

    (require 'citar-org-mode)
    (setq citar-org-mode-directory (concat org-directory "bib/"))
    )

;;;;;; denote-search

  (use-package! denote-search
    :commands (denote-search denote-search-marked-dired-files denote-search-files-referenced-in-region)
    ;; :bind
    ;; Customize keybindings to your liking
    ;; (("C-c s s" . denote-search)
    ;;  ("C-c s d" . denote-search-marked-dired-files)
    ;;  ("C-c s r" . denote-search-files-referenced-in-region))
    :custom
    ;; Disable help string (set it once you learn the commands)
    ;; (denote-search-help-string "")
    ;; Display keywords in results buffer
    (denote-search-format-heading-function #'denote-search-format-heading-with-keywords))

;;;;;; denote-regexp

  (use-package! denote-regexp)

;;;;;; end-of denote
  ) ;; end-of denote

;;;;; DONT Obsidian

;; (use-package! obsidian
;;   :defer t
;;   :init
;;   ;; (require 'hydra)
;;   ;; (bind-key (kbd "M-g O") 'obsidian-hydra/body 'obsidian-mode-map)
;;   (setq obsidian-include-hidden-files nil)
;;   (obsidian-specify-path (concat org-directory "md/"))
;;   :config
;;   ;; (global-obsidian-mode t)
;;   )

;;;;; binder

(use-package! binder
  :defer t
  :commands binder-toggle-sidebar
  :config
  (require 'binder-tutorial)  ;; optional
  )

;;;;; adoc-mode

(use-package! adoc-mode
  :custom-face
  (adoc-title-0-face ((t (:height 1.0 :weight bold)))))

;;;; :custom AI

;;;;; llmclient: gptel - llmclient

;;;;;; evil-collection-gptel

(setq evil-collection-gptel-want-ret-to-send nil) ; default t
(setq evil-collection-gptel-want-shift-ret-to-send nil) ; default t
(setq evil-collection-gptel-want-shift-ret-menu t)

;;;;;; use-package gptel

;;;;;;; 01 - gptel

;; (use-package! gptel
;;   :commands (gptel gptel-send)
;;   :init
;;   ;; (add-to-list 'yank-excluded-properties 'gptel)
;;   (setq gptel-default-mode 'org-mode)
;;   (setq gptel-temperature 0.5) ; gptel 1.0, Perplexity 0.2

;;   ;; "^\\*gptel-ask\\*"
;;   ;; ("^\\*ChatGPT\\*" :size 84 :side right :modeline t :select t :quit nil :ttl t)
;;   ;; (set-popup-rule! "^\\*ChatGPT\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4
;;   ;; (set-popup-rule! "^\\*gptel\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4
;;   ;; (set-popup-rule! "^\\*Deepseek\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4
;;   ;; (set-popup-rule! "^\\*xAI\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4
;;   ;; :config
;;   )

(after! gptel

  (setq gptel-default-mode 'org-mode)

  ;; ~/sync/man/dotsamples/vanilla/gregoryg-dotfiles-gpt/README.org
  (setq gptel-include-reasoning 'ignore)
  (setq gptel-expert-commands t)
  (setq gptel-temperature 0.2) ; gptel 1.0, Perplexity 0.2

  ;; (setq gptel-include-reasoning nil)
  ;; (setq gptel-include-reasoning "*reasoning*")

;;;;;;; 02 - default prompt

  ;; (progn
  ;;   (setq gptel-model 'gpt-4o-mini) ; default 'gpt-4o-mini
  ;;   (setq gptel-api-key user-openai-api-key)
  ;;   )

  (setf
   (cdr (assoc 'default gptel-directives))
   "You are a large language model living in Emacs and a helpful assistant. Respond concisely using Korean language.")
  (setq gptel--system-message (alist-get 'default gptel-directives))

;;;;;;; 04 - gptel-org-toggle-branching-context

  ;; (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** @user ")
  ;; (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  (require 'gptel-org)

;;;###autoload
  (defun gptel-org-toggle-branching-context ()
    "Toggle gptel context between doc and subheading."
    (interactive)
    (if gptel-org-branching-context
        (progn
          (setq-local gptel-org-branching-context nil)
          (message "Context: whole doc"))
      (setq-local gptel-org-branching-context t)
      (message "Context: subheading")))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user "
        (alist-get 'org-mode gptel-response-prefix-alist) "@assistant "
        (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")

  (with-eval-after-load 'gptel-org
    (setq-default gptel-org-branching-context t)) ; default nil

;;;;;;; 05 - gptel backend configurations

  ;; ~/sync/man/dotsamples/doom/agzam-dot-doom/modules/custom/ai/config.el
  (require 'gptel-integrations)

  ;; (setq gptel-model 'claude-sonnet-4)
  ;; (setq gptel-copilot-backend (gptel-make-gh-copilot "Copilot"))
  ;; (setq gptel-backend gptel-copilot-backend)

  (load! "+gptel") ; agzam-dot-doom

  ;; OpenRouter offers an OpenAI compatible API
  ;; https://openrouter.ai/
  (setq gptel-openrouter-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key #'gptel-api-key
          :models gptel--openrouter-models))

  (setq gptel-backend gptel-openrouter-backend)
  (setq gptel-model 'openai/gpt-5.1)

  ;; xAI offers an OpenAI compatible API
  ;; (gptel-make-openai "xAI"
  ;;   :host "api.x.ai"
  ;;   :key #'gptel-api-key
  ;;   :endpoint "/v1/chat/completions"
  ;;   :stream t
  ;;   :request-params '(:temperature 0.2)
  ;;   :models '(grok-3 ; grok-3-fast
  ;;             grok-3-mini ; grok-3-mini-fast
  ;;             grok-2-image-1212))

  ;; Google - Gemini
  ;; (gptel-make-gemini "Gemini"
  ;;   :key #'gptel-api-key
  ;;   :stream t)

  ;; Anthropic - Claude
  ;; (gptel-make-anthropic "Claude"
  ;;   :key #'gptel-api-key
  ;;   :stream t)

  ;; https://perplexity.mintlify.app/guides/pricing
  ;; Model	Context Length	Model Type
  ;; sonar-reasoning	127k	Chat Completion
  ;; sonar-pro	200k	Chat Completion
  ;; sonar	127k	Chat Completion
  (gptel-make-perplexity "Perplexity"
    :host "api.perplexity.ai"
    :key #'gptel-api-key
    :endpoint "/chat/completions"
    :stream t
    :request-params '(:temperature 0.2) ; sonar's default 0.2
    :models '(sonar sonar-pro sonar-reasoning))

  ;; Upstage: solar
  ;; https://developers.upstage.ai/docs/apis/chat
  ;; (gptel-make-openai "Upstage"
  ;;   :host "api.upstage.ai/v1/solar"
  ;;   :key #'gptel-api-key
  ;;   :endpoint "/chat/completions"
  ;;   :stream t
  ;;   :request-params '(:temperature 0.5)
  ;;   :models '(solar-pro
  ;;             solar-mini))

  ;; DeepSeek offers an OpenAI compatible API
  ;; The deepseek-chat model has been upgraded to DeepSeek-V3. deepseek-reasoner points to the new model DeepSeek-R1.
  ;; USE CASE	TEMPERATURE
  ;; Coding / Math	0.0
  ;; Data Cleaning / Data Analysis	1.0
  ;; General Conversation	1.3
  ;; Translation	1.3
  ;; Creative Writing / Poetry	1.5
  ;; https://api-docs.deepseek.com/quick_start/parameter_settings
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key #'gptel-api-key
    ;; :request-params '(:temperature 0.0) ; 1.0 default
    )

  ;; TODO 2025-07-04 테스트 필요
  ;; (gptel-make-openai "Github Copilot"
  ;;   :protocol "http"
  ;;   :host "localhost:4141"
  ;;   :endpoint "/chat/completions"
  ;;   :stream t
  ;;   :key "no-key-required"
  ;;   :models gptel--gh-copilot-models)

  ;; Kagi’s FastGPT model and the Universal Summarizer are both supported. A couple of notes:
  ;; (gptel-make-kagi "Kagi"
  ;; :stream t
  ;; :key #'gptel-api-key)

  ;; Together.ai offers an OpenAI compatible API
  ;; (gptel-make-openai "TogetherAI"
  ;;   :host "api.together.xyz"
  ;;   :key #'gptel-api-key
  ;;   :stream t
  ;;   :models '(;; has many more, check together.ai
  ;;             "meta-llama/Llama-3.2-11B-Vision-Instruct-Turbo" ;; Meta Llama 3.2 11B Vision Instruct Turbo $0.18
  ;;             "meta-llama/Llama-3.2-3B-Instruct-Turbo" ;; Meta Llama 3.2 3B Instruct Turbo $0.06
  ;;             ))

  ;; Github Models offers an OpenAI compatible API
  ;; https://docs.github.com/en/github-models/prototyping-with-ai-models
  ;; (gptel-make-openai "GithubModels" ; Any name you want
  ;;   :host "models.inference.ai.azure.com"
  ;;   :endpoint "/chat/completions"
  ;;   :stream t
  ;;   :key #'gptel-api-key
  ;;   :models '(gpt-4o-mini)) ;; low tier

  ;; gptel-transient

  ;; 2025-07-13 [[denote:20250713T154805][#LLM: 20250713T154805]]
  (require 'gptel-transient)
  (transient-suffix-put 'gptel-tools 'gptel--suffix-mcp-connect :key "a")
  (transient-suffix-put 'gptel-tools 'gptel--suffix-mcp-disconnect :key "d")

  ;; (after! gptel
  ;;   (transient-append-suffix 'gptel-menu "k"
  ;;     '("q" "quit" transient-quit-one))
  ;;   ;; Doom binds ~RET~ in Org mode to =+org/dwim-at-point=, which appears to conflict with gptel's transient menu bindings for some reason.
  ;;   ;; Two solutions:
  ;;   ;; - Press ~C-m~ instead of the return key. evil-ret
  ;;   ;; - Change the send key from return to a key of your choice:
  ;;   ;; (transient-suffix-put 'gptel-menu (kbd "RET") :key "M-RET") ;; 2025-05-13 FIXME
  ;;   )

;;;;;;; 06 - gptel-magit

  (setq gptel-magit-backend gptel-openrouter-backend)
  (setq gptel-magit-model "google/gemini-2.5-flash");

  ) ; end-of gptel

;;;;;; TODO cashpwd - gptel-send with prompt

(after! gptel
  ;; /home/junghan/sync/man/dotsamples/doom/cashpw-dotfiles-node/config/doom/config-personal.org
  (defvar cashpw/llm--default-prompt
    "You are a large language model living in Emacs and a helpful assistant. Respond concisely using Korean language.")

  (defvar cashpw/llm--chain-of-thought-prompt
    "You are a large language model living and a helpful assistant. First, enumerate a list of steps one should follow to find an appropriate answer. Second, follow those steps and show your work. Respond concisely using Korean language")

  (defvar cashpw/llm--follow-up-prompt
    "Assume the persona of a peer and colleague who is working with me to understand and expand on an idea or question. Respond with between three and ten follow-up questions or considerations. Format your response in markdown using Korean language.")

  (defvar cashpw/llm--writing-prompt
    "You are a large language model and a writing assistant. Respond concisely using Korean language.")

  (defvar cashpw/llm--programming-prompt
    "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt, or note.")

  (defvar cashpw/llm--chat-prompt
    "You are a large language model and a conversation partner. Respond concisely using Korean language")

  ;; cashpw
  (setq gptel-directives `((default . ,cashpw/llm--default-prompt)
                           (chain-of-thought . ,cashpw/llm--chain-of-thought-prompt)
                           (follow-up . ,cashpw/llm--follow-up-prompt)
                           (writing . ,cashpw/llm--writing-prompt)
                           (programming . ,cashpw/llm--programming-prompt)
                           (chat . ,cashpw/llm--chat-prompt)))

;; ;;;###autoload
  (defun cashpw/gptel-send (prompt)
    "Invoke `gptel-send' with specific PROMPT."
    (let ((gptel--system-message prompt))
      (gptel-send)))
  )

;;;;;; TODO gptel-make-preset

;; (after! gptel
;;   (gptel-make-preset 'python-coder
;;     :description "Full prompt for a python coder partner."
;;     :system (gjg/build-system-prompt
;;              '("roles" "use-org-mode" "project-context" "documentation" "task-management" "tool-usage" "code-structure" "coding-behavior-rules" "human-coding-partner" "python-style-conventions" "python-tests")))
;;   )

;;;;;; agzam

(after! gptel

  (add-hook! 'gptel-mode-hook
    (defun gptel-mode-set-local-keys ()
      (map! :map gptel-mode-map
            :iv "M-<return>" #'gptel-send
            :iv "M-RET" #'gptel-send
            (:localleader
             :desc "gptel/default" "5" #'gptel-menu ;; TODO fixme
             ;; "M-s" #'gptel-save-as-org-with-denote-metadata
             "0" #'cashpw/gptel-send
             :desc "gptel/default" "1" (cmd! (cashpw/gptel-send (alist-get 'default gptel-directives)))
             :desc "gptel/chain of thought" "2" (cmd! (cashpw/gptel-send (alist-get 'chain-of-thought gptel-directives)))
             :desc "gptel/follow up" "3" (cmd! (cashpw/gptel-send (alist-get 'follow-up gptel-directives)))
             (:prefix ("s" . "session")
              :desc "clear" "l" #'gptel-clear-buffer+
              ;; "p" #'gptel-save-as-org-with-denote-metadata
              )))))

  (add-hook! 'gptel-mode-hook
    (defun cae-gptel-mode-setup-h ()
      ;; (setq-local nobreak-char-display nil) ; 2025-07-26 보는게 좋아
      (auto-fill-mode -1)
      (doom-mark-buffer-as-real-h)))

  ;; 2024-12-12 disable
  ;; (add-hook! 'kill-emacs-hook
  ;;   (defun persist-gptel-model ()
  ;;     (customize-save-variable 'gptel-backend gptel-backend)
  ;;     (customize-save-variable 'gptel-model gptel-model)))
  ;; (add-hook! 'gptel-post-stream-hook #'gptel-save-as-org-with-denote-metadata) ;; manually
  )

;;;;;; TODO gptel with gregory prompt

;; (after! gptel
;;   ;; classic gptel configuration
;;   (setq
;;    gptel-model 'claude-3-opus-20240229
;;    gptel-backend (gptel-make-anthropic "Claude"
;;                    :stream t :key "sk-..."))
;;   ;; set gptel-directives as AIPIHKAL system-prompts
;;   (let ((build-directives-fun "~/projects/ai/AIPIHKAL/gptel-build-directives.el"))
;;     (when (file-exists-p build-directives-fun)
;;       (load build-directives-fun)
;;       (setq gptel-directives (gjg/gptel-build-directives "~/projects/ai/AIPIHKAL/system-prompts/")
;;             gptel-system-message (alist-get 'default gptel-directives)))))

;;;;;; gptel-quick

(use-package! gptel-quick
  :after gptel
  :commands gptel-quick
  :config
  (progn
    (require 'gptel-quick)
    (setq gptel-quick-word-count 30)
    (setq gptel-quick-timeout 20)
    (setq gptel-quick-backend gptel-openrouter-backend
          gptel-quick-model 'openai/gpt-4.1-nano) ; 2025-07-26

    ;;;;###autoload
    (defun gptel-quick (query-text &optional count)
      "Explain or summarize region or thing at point with an LLM.
      QUERY-TEXT is the text being explained.  COUNT is the approximate
      word count of the response."
      (interactive (list
                    (cond
                     ((use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end)))
                     ((and (derived-mode-p 'pdf-view-mode)
                           (pdf-view-active-region-p))
                      (mapconcat #'identity (pdf-view-active-region-text)
                                 "\n\n"))
                     (t
                      (thing-at-point 'sexp)))
                    current-prefix-arg))
      (let* ((count (or count gptel-quick-word-count))
             (gptel-max-tokens
              (floor (+ (sqrt (length query-text)) (* count 2.5))))
             (gptel-use-curl)
             (gptel-use-context (and gptel-quick-use-context 'system)))
        (gptel-request
         query-text
         :system (format "1) Translate the question to English. 2) Respond in %d words or fewer using Korean." count)
         :context
         (list
          query-text count
          (posn-at-point (and (use-region-p) (region-beginning))))
         :callback #'gptel-quick--callback-posframe)))

    ;; keymap
    ;; (map! :n "C-k" #'gptel-quick)
    ;; (map! :map visual-line-mode-map "C-k" #'gptel-quick)
    ) ; progn gptel-quick
  )

;;;;;;  gpt-babel

; (use-package! gpt-babel
;   :after gptel org
;   :init
;   (setq gpt-babel/error-action 'nil)  ; Options: nil, 'send, or 'fix
;   :defer 2)
;
;;;;;; elysium for pair programming

(use-package! elysium
  :after gptel
  :defer 2
  :commands (elysium-toggle-window)
  :bind (("C-c e q" . elysium-query)
         ("C-c e t" . elysium-toggle-window)
         ("C-c e c" . elysium-clear-buffer)
         ("C-c e a" . elysium-add-context)
         ("C-c e k" . elysium-keep-all-suggested-changes)
         ("C-c e d" . elysium-discard-all-suggested-changes))
  :init
  ;; Below are the default values
  (setq elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (setq elysium-window-style 'vertical) ; Can be customized to horizontal
  ;; Use `smerge-mode` to then merge in the changes
  (require 'smerge-mode)
  (add-hook 'prog-mode-hook 'smerge-mode)

  :config
  ;; OpenRouter를 통한 Anthropic 설정
  (setq gptel-elysium-backend
        (gptel-make-openai "Elysium-OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key #'gptel-api-key
          :models gptel--openrouter-models))
  (setq gptel-elysium-model "anthropic/claude-sonnet-4")
  (setq gptel-elysium-temperature 0.1) ; 코딩용 설정
  )

;;;;;; ob-gptel

(after! gptel
  (require 'ob-gptel)
  ;; (add-to-list 'org-babel-load-languages '(gptel . t))
  ;; (add-hook 'completion-at-point-functions 'ob-gptel-capf nil t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((gptel . t))))
  )

;;;;;; gptel-prompt

(after! gptel
  (require 'gptel-prompts)
  (setq gptel-prompts-directory (concat org-directory "resources/prompts/"))

  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers)

  (use-package! uuidgen)

  (require 'gptel-litellm)
  (gptel-litellm-install-sessions)
  )

;;;;; llmclient: emigo

;; (use-package! emigo
;;   :config
;;   (emigo-enable) ;; Starts the background process automatically
;;   ;; :custom
;;   ;; Encourage using OpenRouter with Deepseek
;;   ;; openrouter/quasar-alpha
;;   ;; (emigo-model "openrouter/deepseek/deepseek-chat-v3-0324")
;;   ;; (emigo-base-url "https://openrouter.ai/api/v1")
;;   ;; (emigo-api-key (getenv "OPENROUTER_API_KEY"))
;;   )

;;;;; llmclient: github copilot

;; (use-package! copilot
;;   :defer 5
;;   :commands (copilot-login copilot-diagnose)
;;   :init
;;   ;; Sometimes the copilot agent doesn't start. Restarting fixes the issue.
;;   (setq copilot-indent-offset-warning-disable t
;;         copilot-max-char 10000) ; default 100000
;;   (setq copilot-version "1.282.0") ;; 2025-06-03 use stable version
;;   (setq copilot-idle-delay 2) ; nil
;;   :bind (:map copilot-completion-map
;;               ("C-g" . 'copilot-clear-overlay)
;;               ("M-P" . 'copilot-previous-completion)
;;               ("M-N" . 'copilot-next-completion)
;;               ("M-<tab>" . 'copilot-accept-completion) ; vscode
;;               ;; ("TAB" . 'copilot-accept-completion) ; vscode
;;               ("M-f" . 'copilot-accept-completion-by-word)
;;               ("M-<return>" . 'copilot-accept-completion-by-line)
;;               ("M-]" . 'copilot-next-completion) ; vscode
;;               ("M-[" . 'copilot-next-completion) ; vscode
;;               ;; ("C-'" . 'copilot-accept-completion)
;;               ;; ("C-;" . 'copilot-accept-completion)
;;                    )
;;   ;; :hook ((prog-mode . copilot-mode))
;;   ;; (org-mode . copilot-mode)
;;   ;; (markdown-mode . copilot-mode)
;;   )

;;;;; llmclient: github copilot-chat

;; 2025-03-19 v2.0
;; (use-package! copilot-chat
;;   :defer 6
;;   :after request
;;   :bind (:map global-map
;;               ("C-c C-y" . copilot-chat-yank)
;;               ("C-c M-y" . copilot-chat-yank-pop)
;;               ("C-c C-M-y" . (lambda () (interactive) (copilot-chat-yank-pop -1))))
;;   :init
;;   (setq copilot-chat-frontend 'markdown)

;;   ;; (setq copilot-chat-backend 'request)
;;   ;; (setq! copilot-chat-model "claude-3.5-sonnet"
;;   ;;        copilot-chat-frontend 'org)
;;   ;; (set-popup-rules!
;;   ;;   '(("^\\*Copilot-chat-prompt\\*$" :vslot -2 :size 0.15 :select t :quit t)
;;   ;;     ("^\\*Copilot-chat-list\\*$" :slot 10 :side bottom :size 0.1 :select nil :quit t)
;;   ;;     ("^\\*Copilot-chat\\*$" :slot 2 :side right :size 0.45 :select nil :quit t)))
;;   :config
;;   ;; From https://github.com/chep/copilot-chat.el/issues/24
;;   (defun my/copilot-chat-display (prefix)
;;     "Opens the Copilot chat window, adding the current buffer to the context.
;; Called with a PREFIX, resets the context buffer list before opening"
;;     (interactive "P")

;;     (require 'copilot-chat)
;;     (let ((buf (current-buffer)))

;;       ;; Explicit reset before doing anything, avoid it resetting later on
;;       ;; target-fn and ignoring the added buffers
;;       (unless (copilot-chat--ready-p)
;;         (copilot-chat-reset))

;;       (when prefix (copilot-chat--clear-buffers))

;;       (copilot-chat--add-buffer buf)
;;       (copilot-chat-display)))
;;   )

;;;;; DONT llmclient: aider.el

;; (use-package! aider
;;   :commands (aider-transient-menu)
;;   :config
;;   (require 'aider-doom)
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
;;   ;; (setq aider-args '("--model"  "deepseek/deepseek-chat"))
;;   (setq aider-args '("--model" "openrouter/deepseek/deepseek-r1" )) ;; add --no-auto-commits if you don't want it
;;   ;; - openrouter/deepseek/deepseek-coder

;;   (setenv "DEEPSEEK_API_KEY" user-deepseek-api-key)
;;   (setenv "OPENROUTERAPI_KEY" user-openrouter-api-key)
;;   (add-hook 'aider-comint-mode-hook #'visual-line-mode)
;;   )

;; aider --list-models deepseek
;; deepseek/deepseek-chat, deepseek/deepseek-coder, deepseek/deepseek-reasoner

;; Or use chatgpt model since it is most well known
;; (setq aider-args '("--model" "gpt-4o-mini"))
;; Or use gemini v2 model since it is very good and free
;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
;; Optional: Set a key binding for the transient menu
;; (global-set-key (kbd "C-c a") 'aider-transient-menu)

;;;;; DONT llmclient: codeium

;; (use-package! codeium
;;   :after cape
;;   :commands (codeium-install)
;;   :config
;;   ;; codeium-completion-at-point is autoloaded, but you can
;;   ;; optionally set a timer, which might speed up things as the
;;   ;; codeium local language server takes ~0.2s to start up
;;   ;; (add-hook 'emacs-startup-hook
;;   ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;   ;; if you don't want to use customize to save the api-key
;;   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

;;   ;; alternatively for a more extensive mode-line
;;   ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

;;   ;; you can also set a config for a single buffer like this:
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local codeium/editor_options/tab_size 4)))

;;   ;; You can overwrite all the codeium configs!
;;   ;; for example, we recommend limiting the string sent to codeium for better performance
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;   ;; if you change the text, you should also change the cursor_offset
;;   ;; warning: this is measured by UTF-8 encoded bytes
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text)
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
;;

;;;;; DONT llmclient: wolframalpha

;; ziova/wolfram.el
;; (use-package! wolfram
;;   :config (setq wolfram-alpha-app-id user-wolfram-alpha-app-id))

;;;;; DONT llmclient: kagi
;; cecil
;; agnes
;; daphne
;; muriel
;; (use-package! kagi
;;   :defer 3
;;   :custom
;;   (kagi-api-token user-kagi-api-key)
;;   ;; (kagi-api-token (lambda () (password-store-get "Kagi/API")))
;;   ;; Universal Summarizer settings
;;   (kagi-summarizer-engine "cecil") ;; Formal, technical, analytical summary.
;;   (kagi-summarizer-default-language "KO")
;;   (kagi-summarizer-cache t))

;;;;; DONT elisa

;; ELISA (Emacs Lisp Information System Assistant)
;; (use-package! elisa
;;   :if (not IS-TERMUX)
;;   :init
;;   (setopt elisa-limit 5)
;;   (require 'llm-ollama)
;;   (setopt elisa-embeddings-provider (make-llm-ollama :embedding-model "nomic-embed-text"))
;;   (setopt elisa-chat-provider (make-llm-ollama
;; 			       :chat-model "sskostyaev/openchat:8k-rag"
;; 			       :embedding-model "nomic-embed-text")))


;;;;; DONT ellama - offline local llm

;; https://github.com/s-kostyaev/ellama
(use-package! ellama
  :defer 3
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; (setopt ellama-language "Korean")
  ;; (setopt ellama-provider emacs-llm-default-provider)

  ;; I've looked for this option for 1.5 hours
  (setq ellama-long-lines-length 100000)
  )

;;;;; DONT whisper

;; 데스크탑에서 사용해야 할 듯
;; (use-package! whisper
;;   :defer t
;;   :config
;;   ;; (setq whisper-language "ko") ; "en"
;;   (setq
;;    ;; whisper-install-directory "~/.config/emacs/.local/cache/"
;;    ;; whisper-model "large-v3"
;;    ;; whisper-model "medium"
;;    ;; whisper-model "small"
;;    whisper-model "base"
;;    whisper-language "en"
;;    whisper-translate nil
;;    ;; whisper--ffmpeg-input-device "hw:0"
;;    ;; whisper-return-cursor-to-start nil)
;;    )
;;   )

;;;;; DONT DALL-E

;; (setq dall-e-n 1)
;; (setq dall-e-spinner-type 'flipping-line)
;; (setq dall-e-display-width 256)

;;;; :lang coding

;;;;; treesit - tree-sitter

(when (treesit-available-p)
  (setq treesit-extra-load-path (list (concat doom-profile-data-dir "tree-sitter"))))

;;;;; c/c++ clangd with eglot

(after! cc-mode
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy")))

;; (setq flycheck-clang-language-standard "c++17")
;; (after! lsp-clangd
;;   (setq lsp-clients-clangd-executable "clangd")
;;   (setq lsp-clients-clangd-args
;;         '("-j=2"
;;           "--background-index"
;;           "--clang-tidy"
;;           "--completion-style=detailed"
;;           "--header-insertion=never"
;;           "--header-insertion-decorators=0"))
;;   (set-lsp-priority! 'clangd 2)
;; )

;; (after! ccls
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 1))

;;;;; eglot configuration

(progn
  (map! (:map eglot-mode-map
         :after eglot
         "C-c r" 'eglot-rename
         "C-c d" 'eldoc
         "C-c f" 'flymake-show-buffer-diagnostics
         "C-c 0" 'eglot-inlay-hints-mode
         ;; "C-RET" 'eglot-code-actions
         )

        ;; FIXME need new keybindings
        ;; (:map 'flymake-mode-map
        ;;       "C-n" #'flymake-goto-next-error
        ;;       "C-p" #'flymake-goto-prev-error)
        )

  ;; (setq eglot-send-changes-idle-time 0.5)
  (setq flymake-no-changes-timeout nil)

  ;; Allow edits without confirmation?
  (setopt eglot-confirm-server-initiated-edits nil)
  ;; Show code action indicators?
  (setopt eglot-code-action-indications nil)

  (add-hook! 'eglot-managed-mode-hook
    (eglot-inlay-hints-mode -1))
  )

;;;;; eglot-booster

;; install lsp-booster binary first
;; (use-package! eglot-booster
;;   :after eglot
;;   :config
;;   ;; (setq eglot-confirm-server-initiated-edits nil)
;;   ;; (setq eglot-extend-to-xref t)
;;   (eglot-booster-mode +1))

;;;;; indent-bars

(progn
  (remove-hook 'text-mode-hook #'+indent-guides-init-maybe-h)
  (remove-hook 'prog-mode-hook #'+indent-guides-init-maybe-h)
  ;; use indent-bar-mode
  )

;;;;; DONT lsp-mode - lsp-ui-mode - lsp-treemacs

;; lsp 관련 설정 메뉴들. 느리게 만드는 범인중 십중팔구 LSP가 관련되어져 있다고 함.
;; 해당 튜닝도 구글링을 통해서 찾았다.
;; (setq lsp-file-watch-threshold (* 1024 1024))
;; (setq read-process-output-max (* 1024 1024))

;; (progn
;;   (after! lsp-mode
;;     (setq
;;      ;; https://emacs-lsp.github.io/lsp-mode/page/settings/headerline/
;;      lsp-headerline-breadcrumb-enable t ; doom nil
;;      lsp-headerline-breadcrumb-icons-enable nil
;;      ;; lsp-headerline-breadcrumb-segments '(symbols) ; namespace & symbols, no file path

;;      lsp-imenu-index-function #'lsp-imenu-create-categorized-index ;; 2025-03-26 doom 'lsp-imenu-create-uncategorized-index

;;      lsp-idle-delay 0.2  ; smooth LSP features response
;;      ;; lsp-eldoc-enable-hover nil ; default t - disable all hover actions
;;      ;; lsp-modeline-code-actions-segments '(count icon)
;;      ;; lsp-navigation 'both ; default 'both ; 'simple or 'peek
;;      ;; lsp-modeline-diagnostics-enable nil
;;      ;; lsp-modeline-code-actions-enable nil
;;      )
;;     )

;;   (after! lsp-ui
;;     (setq
;;      ;; lsp-ui-doc-use-webkit nil ; default nil
;;      ;; lsp-ui-doc-winum-ignore t ; default t
;;      lsp-ui-sideline-enable nil ; doom t - disable sideline for less distraction
;;      lsp-ui-sideline-diagnostic-max-line-length 20 ; default 100
;;      ;; lsp-ui-doc-enable nil ;; doom t - disable all doc popups
;;      treemacs-space-between-root-nodes nil  ;; doom nil
;;      ;; lsp-log-io t  ; default nil - Log client-server json communication
;;      lsp-ui-peek-enable t ; doom t
;;      ))

;;   (when (modulep! :ui treemacs +lsp)
;;     (setq lsp-treemacs-error-list-current-project-only t)
;;     (lsp-treemacs-sync-mode +1))
;;   )

;;;;; devdocs-browser

;; 한글 번역 문서 지원
;; devdocs-browser-install-doc - index 파일만 저장
;; devdocs-browser-download-offline-data - 오프라인 전체 데이터 저장 (기본 영어)
;; 2024-01-31 Python 3.11, NumPy 1.23 pandas 1.5.0, Elixir 1.13
;; 2024-06-26 common-lisp
(use-package! devdocs-browser
  :defer 2
  :commands (devdocs-browser-open devdocs-browser-open-in)
  :bind (("M-s-," . devdocs-browser-open) ;; M-s-/ yas-next-field
         ("M-s-." . devdocs-browser-open-in))
  :config
  (set-popup-rule! "*devdocs-.*\\*" :width 84 :side 'right :select nil :quit nil :ttl 0)
  (setq devdocs-browser-data-directory (concat doom-emacs-dir "devdocs-browser"))
  (add-to-list 'devdocs-browser-major-mode-docs-alist '(js2-mode "javascript" "node"))
  (add-to-list 'devdocs-browser-major-mode-docs-alist '(python-mode "Python" "NumPy" "pandas"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(python-ts-mode "Python" "NumPy" "pandas"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(elixir-ts-mode "Elixir"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(rjsx-mode "react" "javascript" "node"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(typescript-ts-mode "typescript"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(js-ts-mode "javascript" "node"))
  )

;;;;; DONT symbol-overlay

;; (use-package! symbol-overlay
;;   :commands (symbol-overlay-mode symbol-overlay-put)
;;   :bind
;;   (:map symbol-overlay-mode-map
;;         ("C-c C-n" . symbol-overlay-jump-next)
;;         ("C-c C-p" . symbol-overlay-jump-prev))
;;   :hook
;;   (prog-mode . symbol-overlay-mode)
;;   ;; (emacs-lisp-mode . symbol-overlay-mode)
;;   ;; (python-mode . symbol-overlay-mode)
;;   )

;;;;; sideline-blame

(use-package! sideline-blame
  :defer 5
  :init
  (setq sideline-backends-left '((sideline-blame . down))))

;;;;; git-messenger

;; git-messenger.el provides function that popup commit message at current line.
;; This is useful when you want to know why this line was changed.
(use-package! git-messenger
  :defer t
  :commands git-messenger:popup-message
  :config (setq git-messenger:use-magit-popup t)
  )

;;;;; yasnippet Navigation M-n/M-p and hippie-expand M-/

;; use Meta-n and Meta-p to jump between fields
;; <backspace>    +snippets/delete-backward-char
;; <delete>       +snippets/delete-forward-char-or-field
;; C-a            +snippets/goto-start-of-field
;; C-e            +snippets/goto-end-of-field
;; M-<backspace>  +snippets/delete-to-start-of-field
;; M-<left>       +snippets/goto-start-of-field
;; M-<right>      +snippets/goto-end-of-field
;; yas/expand

(remove-hook! 'yas-minor-mode-hook
  (defun +corfu-add-yasnippet-capf-h ()
    (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t)))

(after! yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all)
  (yas-global-mode +1)
  (define-key yas-keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "M-p") 'yas-prev-field)
  (define-key yas/keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
  (define-key yas/keymap (kbd "M-p") 'yas-prev-field)
  )

;;;;; :lang python

;;;;;; conda

;; set env ANACONDA_HOME and M-x conda-env-activate
;; (after! python
;;   (setq conda-anaconda-home (expand-file-name  "~/miniconda3/"))
;;   )

;;;;;; ipython default

;; (after! python
;;   ;; use ipython for interpreter if it exists
;;   (if (executable-find "ipython")
;;       (progn (setq python-shell-interpreter "ipython")
;;              (setq python-shell-interpreter-args "-i --simple-prompt")))
;;   )

;;;;;; indent-guides-init-maybe-h

(add-hook 'python-mode-hook #'+indent-guides-init-maybe-h)

;;;;;; DONT custom emacs-jupyter/jupyter ob-jupyter

;; (require 'my-python-jupyter)
;; (require 'my-org-literate)

;; (require 'jupyter-tramp)
;; (setq jupyter-eval-use-overlays t)

;;;;;; python with eglot - basedpyright

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   `(python-mode .
     ,(eglot-alternatives
       '(("basedpyright-langserver" "--stdio")))))
  ;; (add-hook 'after-save-hook 'eglot-format)
  )


;;;;;; uv : uv-mode and uv-menu

;; .venv
;; (use-package! uv-mode
;;   :hook (python-mode . uv-mode-auto-activate-hook))

;;;;;; disable lsp! default on python-mode

;; 2025-02-18 python-mode-hook for uv
;; ;; (remove-hook 'python-mode-hook 'pipenv-mode)

;; 2025-02-18 python-mode-local-vars-hook
;; lsp! - /modules/tools/lsp/autoload/common.el
;; python-mode-local-vars-hook - (pyvenv-track-virtualenv lsp!)

;; (when (modulep! :tools lsp -eglot)
;;   (remove-hook 'python-mode-local-vars-hook 'lsp!))

;;;;;; TODO python-pytest

;; pytest.ini
;; [pytest]
;; markers =
;;     task: A concept exercise task.

;; (after! python-pytest
;; (add-to-list 'pytest-project-root-files "setup.cfg")
;; (add-to-list 'pytest-project-root-files "pytest.ini")
;; )

;;;;;; DONT disable +format-with-lsp

;; use apheleia
;; (setq-hook! 'python-mode-hook +format-with-lsp nil)

;;;;;; hy : hylang

;; 0.28 hy-mode, hyuga
;; (use-package! hy-mode
;;   :mode "\\.hy\\'"
;;   :interpreter "hy"
;;   ;; :hook ((hy-mode . eglot-ensure))
;;   :config
;;   (set-repl-handler! 'hy-mode #'hy-shell-start-or-switch-to-shell)
;;   (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(hy-mode))
;;   (when (executable-find "hyuga") ; it's works!
;;     (require 'eglot)
;;     (set-eglot-client! 'hy-mode '("hyuga")))
;;   )

;;;;; docker-compose-mode

(use-package! docker-compose-mode
  :mode "docker-compose.*\\.ya?ml\\'"
  :config
  (after! outli
    (add-to-list 'outli-heading-config '(docker-compose-mode "##" ?# t))
    (add-hook 'docker-compose-mode-hook 'outli-mode))
  )

;;;; :format

;;;;; elisp-autofmt

(use-package! elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  ;; :hook (emacs-lisp-mode . elisp-autofmt-mode)
  )

;;;;; apheleia + format-with-lsp

;; +onsave -- global

;; (setq +format-on-save-disabled-modes
;;       '(emacs-lisp-mode  ; elisp's mechanisms are good enough
;;         sql-mode         ; sqlformat is currently broken
;;         tex-mode         ; latexindent is broken
;;         latex-mode
;;         org-msg-edit-mode
;;         ))

;; Disabling the LSP formatter
;; 1) To disable this behavior universally use:
;; (setq +format-with-lsp nil)

;; 2) To disable this behavior in one mode:
;; ;; (setq-hook! 'python-mode-hook +format-with-lsp nil) ; python


;;;;; nix

(after! nix-mode
  ;; alejandra를 기본 포맷터로 설정
  (setq nix-nixfmt-bin "~/.local/bin/alejandra"))

(add-hook 'nix-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'nix-format-buffer nil t)))

;;;; :ui

;;;;; doom-dashboard - splash

(progn
  (defun emacs-dashboard-draw-ascii-banner-fn ()
    (let* ((banner
            '("Welcome to                                 "
              "███████╗███╗   ███╗ █████╗  ██████╗███████╗"
              "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
              "█████╗  ██╔████╔██║███████║██║     ███████╗"
              "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
              "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
              "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"))
           (longest-line (apply #'max (mapcar #'length banner))))
      (put-text-property
       (point)
       (dolist (line banner (point))
         (insert
          (+doom-dashboard--center
           +doom-dashboard--width
           (concat line (make-string (max 0 (- longest-line (length line))) 32)))
          "\n"))
       'face 'bold)))
  (setq +doom-dashboard-ascii-banner-fn 'emacs-dashboard-draw-ascii-banner-fn)

  (setq fancy-splash-image (concat user-dotemacs-dir "var/logo.png"))

  (setq +doom-dashboard-functions
        '(doom-dashboard-widget-banner
          doom-dashboard-widget-shortmenu
          my/dashboard-widget-fortune ;; fortune
          doom-dashboard-widget-loaded
          doom-dashboard-widget-footer))

  (defun my/dashboard-widget-fortune ()
    (let* ((quotestring
            (if (executable-find "fortune")
                (string-join
                 (mapcar
                  (lambda (l) (concat "\n " (string-fill l 72)))
                  (if IS-TERMUX
                      (string-lines (shell-command-to-string "fortune"))
                    (string-lines
                     (shell-command-to-string
                      "fortune -c 90% advice 10% .")))))))) ;; 10% samples
      (+doom-dashboard--center
       (- +doom-dashboard--width 2)
       (insert quotestring "\n"))))
  )

;;;;; Font Test:

;; Font test: " & ' ∀ ∃ ∅ ∈ ∉ ∏ ∑ √ ∞ ∧ ∨ ∩ ∪ ∫ ² ³ µ · × ∴ ∼
;; ≅ ≈ ≠ ≡ ≤ ≥ < > ⊂ ⊃ ⊄ ⊆ ⊇ ⊥ ∂ ∇ ∈ ∝ ⊕ ⊗ ← → ↑ ↓ ↔ ⇐ ⇒ ⇔
;; □ ■ | © ¬ ± ° · ˜ Γ Δ α β γ δ ε φ ∀, ∃, ￢(~), ∨, ∧,⊂, ∈,
;; ⇒, ⇔ 𝑀＜1
;; 𝑻𝑼𝑽𝗔𝗕𝗖𝗗 𝞉𝞩𝟃 ϑϕϰ ⊰⊱⊲⊳⊴⊵⫕ 𝚢𝚣𝚤𝖿𝗀𝗁𝗂
;; § † ‡ № ¶

;;;;; Math Symbol

;; vanilla/garyo-dotfiles-ekg/lisp/init-fonts-and-frames.el
;; to display Unicode math chars, like math A to z (𝐴 .. 𝑧, #x1D434 .. #x1D467)
;; and pi: #1D70B = 𝜋 Cambria and Segoe UI Symbol should both work on Windows, but Emacs may pick up some other inappropriate font.

;; (when-windows
;;  (set-fontset-font t 'mathematical "Segoe UI Symbol"))

;; Useful things for chars, fonts and fontsets:
;;  M-x describe-fontset
;;  C-u C-x = ; char details at point
;;  C-x 8 RET ; insert char by name or unicode (hex)
;;  var script-representative-chars: list of all (most?) Unicode script ranges with "representative" chars
;;  See https://lists.gnu.org/archive/html/help-gnu-emacs/2021-09/txtRLYx8BDBtJ.txt for useful math fontset test code
;;  As of 2024, Emacs 30 on Windows does not support color emojis, just black & white.
;;  To set or adjust text scale: C-x C-= to enlarge, C-x C-- to shrink, C-x C-0 to reset.
;;    To modify interactively, S-0 followed by +, -, 0, etc.

;;;;; Fontaine

;; Read the manual: <https://protesilaos.com/emacs/fontaine>

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+

;; terminal-mode is nil
;; A narrow focus package for naming font configurations and then selecting them.
(use-package! fontaine
  :if window-system
  :init
  ;; This is defined in Emacs C code: it belongs to font settings.
  ;; (setq x-underline-at-descent-line nil)
  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; Weights :: Thin ExtraLight Light Regular Medium SemiBold Bold ExtraBold Heavy
  ;; Slopes :: Upright Oblique Italic
  ;; Width :: Normal Extended

  :config
  (setq
   fontaine-presets
   ;; 80 120, 136, 151, 180, 211 ; sarasa mono / term
   ;; 120, 140, 170, 190, 210, 230 ; monoplex kr nerd
   '(
     (small12 :default-height 120)
     (regular14 :default-height 140)
     (regular17 :default-height 170)
     (regular19 :default-height 190)
     (large21 :default-height 210)
     (present23
      :default-height 230
      ;; :fixed-pitch-family "Sarasa Term Slab K"
      ;; :fixed-pitch-serif-family "Sarasa Term Slab K"
      :bold-weight extrabold)
     (t
      ;; Following Prot’s example, keeping these for for didactic purposes.
      ;; :line-spacing 2
      ;; :default-family "Sarasa Term K Nerd Font"
      ;; :default-height 151
      :default-family "GLG Nerd Font Mono"
      ;; :default-height (get-font-size-from-script)

      ;; :default-family "Sarasa Term K Nerd Font"
      ;; :default-height 136
      :default-weight regular
      :term-family "Sarasa Term K Nerd Font"
      ;; :fixed-pitch-family "Sarasa Term K Nerd Font"
      ;; :fixed-pitch-height 151
      ;; :fixed-pitch-weight nil
      ;; :fixed-piath-serif-family nil
      ;; :fixed-pitch-serif-weight nil
      ;; :fixed-pitch-serif-height nil
      :variable-pitch-family "Pretendard Variable"
      ;; :variable-pitch-height 1.0
      ;; :variable-pitch-family nil
      ;; :variable-pitch-weight nil
      :bold-family nil
      :bold-weight bold
      ;; :bold-width extended
      :italic-family nil
      :italic-slant italic)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  ;; (fontaine-set-preset 'regular)
  ;; (set-fontset-font t 'hangul (font-spec :family (face-attribute 'default :family))) ; t or nil ?

  ;; store current preset
  (defun my/fontaine-store-preset ()
    (interactive)
    ;; (message "my/fontaine-store-preset")
    (fontaine-store-latest-preset))

  ;; 한글 사용 위해서 필수!
  (defun my/load-font-cjk ()
    (interactive)
    (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family))) ; default face
    ;; (set-fontset-font "fontset-default" 'hangul (font-spec :family "GLG Nerd Font Mono")) ;  "Sarasa Term K"
    ;; (set-fontset-font "fontset-default" 'cjk-misc (font-spec :family "Sarasa Term SC" )) ; default face
    ;; (set-fontset-font "fontset-default" 'bopomofo (font-spec :family "Sarasa Term SC" )) ; default face
    ;; (set-fontset-font "fontset-default" 'kana (font-spec :family "Sarasa Term J")) ; default face
    ;; (set-fontset-font "fontset-default" 'han (font-spec :family "Sarasa Term SC")) ; default face
    )

  ;; load @ start-up
  (defun my/fontaine-load-preset ()
    (interactive)

    ;; The other side of `fontaine-restore-latest-preset'.
    ;; (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
    (my/load-font-cjk))

  (progn
    (add-hook 'after-setting-font-hook #'my/fontaine-load-preset 90)
    ;; (add-hook 'doom-first-input-hook #'my/fontaine-load-preset) ;; 2025-06-25
    (add-hook 'doom-load-theme-hook #'my/fontaine-load-preset) ;; 2025-06-25
    )

  ;; load @ theme change
  ;; (set-fontset-font "fontset-default" 'hangul (font-spec :family "BHGoo") nil 'append) ; 구본형체 테스트
  ;; (defun my/fontaine-apply-current-preset ()
  ;;   (interactive)
  ;;   ;; (fontaine-apply-current-preset)
  ;;   (my/load-font-cjk))
  ;; (add-hook 'doom-load-theme-hook 'my/fontaine-apply-current-preset 80)
  )

;;;;; Show Font (preview fonts)

;; Read the manual: <https://protesilaos.com/emacs/show-font>
(use-package! show-font
  :if (display-graphic-p)
  :commands (show-font-select-preview show-font-list)
  :config
  ;; (setq show-font-pangram 'fox)
  (setq show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
"))

;;;;; info-colors Info & Help

;; Info 모드 Node 이동
(use-package! info+
  :commands (info info-display-manual)
  :config
  (setq Info-fontify-angle-bracketed-flag nil)
  (add-hook 'Info-mode-hook (lambda () (require 'info+)))
  ;; (define-key Info-mode-map (kbd "C-p") 'Info-prev)
  ;; (define-key Info-mode-map (kbd "C-n") 'Info-next)

  )

(after! info
  (add-to-list 'Info-directory-list
               (expand-file-name "~/git/junghan0611/sicp-info/")))

;;;;; custom eww

(after! eww
  (after! evil
    (add-to-list 'evil-buffer-regexps '("\\*eww\\*" . normal)))

  (require 'eww-load) ; custom module

  ;; Shr group: Simple HTML Renderer를 의미한다. 여기 설정을 바꾸면 faces를 수정할 수 있음
  ;; Make EWW look like the rest of Emacs
  ;; (setq shr-max-width fill-column)

  (setq eww-browse-url-new-window-is-tab nil ; doom tab-bar
        shr-max-image-proportion 0.6 ; 0.8
        shr-discard-aria-hidden t ; nil
        shr-bullet "* " ; default "* "
        shr-image-animate nil ; default nil
        ;; shr-inhibit-images t
        eww-header-line-format " %u"
        eww-buffer-name-length 80 ; 40
        eww-readable-urls '("yes24"
                            "hada"
                            "junghanacs"
                            "naver"
                            "daum"))

  (setq eww-bookmarks-directory (concat org-directory "resources/"))
  (setq eww-form-checkbox-selected-symbol "[X]") ;; "☒"
  (setq eww-form-checkbox-symbol "[ ]")

  ;; Sometimes EWW makes web pages unreadable by adding a bright background.
  ;; Do not colorize backgrounds at all.
  ;; (advice-add #'shr-colorize-region :around #'ignore)

  ;; Allow switching to these buffers with `C-x b'
  (add-hook 'eww-mode-hook #'doom-mark-buffer-as-real-h) ; add

  ;; Default Browser
  ;; (setq browse-url-browser-function 'eww-browse-url
  ;;       browse-url-secondary-browser-function 'browse-url-default-browser)

  ;; change default browser as eww
  ;; (setq +lookup-open-url-fn #'eww) ; doom browse-url
  (setq browse-url-default-browser 'browse-url-firefox)

  ;; This function allows Imenu to offer HTML headings in EWW buffers,
  ;; helpful for navigating long, technical documents.
  ;; https://github.com/alphapapa/unpackaged.el
  (defun +eww/imenu-eww-headings ()
    "Return alist of HTML headings in current EWW buffer for Imenu.
Suitable for `imenu-create-index-function'."
    (let ((faces '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-heading)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (cl-loop
           for
           next-pos
           =
           (next-single-property-change (point) 'face)
           while
           next-pos
           do
           (goto-char next-pos)
           for
           face
           =
           (get-text-property (point) 'face)
           when
           (cl-typecase
               face
             (list (cl-intersection face faces))
             (symbol (member face faces)))
           collect
           (cons (buffer-substring (point-at-bol) (point-at-eol)) (point))
           and
           do
           (forward-line 1))))))

  ;; https://github.com/alphapapa/unpackaged.el
  (add-hook
   'eww-mode-hook
   (lambda ()
     (setq-local imenu-create-index-function #'+eww/imenu-eww-headings)))

  (evil-define-key 'normal eww-link-keymap "f" 'ace-link-eww)
  (evil-define-key 'normal eww-mode-map "f" 'ace-link-eww)
  )

;;;;;; eww image-slicing : not working

;; (after! eww
;;   (require 'image-slicing)
;;   (add-to-list 'shr-external-rendering-functions '(img . image-slicing-tag-img))
;;   (push #'image-slicing-mode eww-after-render-hook)
;;   )

;;;;; mode-minder

(use-package! mode-minder :defer t :commands mode-minder)

;;;;; inhibit-double-buffering

;; prevents some cases of emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;;;; hl-todo

;; TODO FIXME BUG FAIL REVIEW THEM HACK KLUDGE HOLD
;; NEXT
;; DEPRECATED DONT NOTE DONE OKAY XXX
;; TODO: test

;; Use full hl-todo keywords
(after! hl-todo
  (setq hl-todo-highlight-punctuation "") ; back to default
  (setq hl-todo-keyword-faces
        '( ;; For reminders to change or add something at a later date.
          ("TODO" error bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ("BUG" error bold)

          ("NEXT" warning bold)
          ("PROG" warning bold)

          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ("THEM" font-lock-keyword-face bold)

          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ("TEMP" font-lock-constant-face bold)
          ("KLUDGE" font-lock-constant-face bold)
          ("HOLD" font-lock-constant-face bold)

          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ("FAIL" font-lock-doc-face bold)
          ("DONT" font-lock-doc-face bold)

          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("DONE" success bold)
          ("OKAY" success bold)

          ("XXXX*" font-lock-constant-face bold) ; XXX
          )))

;;;;; DONT nerd-icons-dired

;; (use-package! nerd-icons-dired
;;   :if window-system
;;   :hook (dired-mode . nerd-icons-dired-mode))

;; (use-package! oneko-macs
;;   :if window-system)

;;;;; DONT auto-highlight-symbol

;; (use-package! auto-highlight-symbol
;;   :commands (auto-highlight-symbol-mode)
;;   :init
;;   (setq ahs-idle-interval 1.0) ; default 1.0
;;   (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode))

;;;;; breadcrumb - with eglot

(use-package! breadcrumb
  :defer 2
  :init
  ;; (add-hook 'prog-mode-hook 'breadcrumb-local-mode)
  ;; (add-hook 'clojure-mode-hook 'breadcrumb-local-mode)
  ;; (add-hook 'python-mode-hook 'breadcrumb-local-mode)
  (add-hook 'emacs-lisp-mode-hook 'breadcrumb-local-mode)
  )

;;   (add-hook 'markdown-mode-hook 'breadcrumb-local-mode)
;;   ;; (add-hook 'org-mode-hook 'breadcrumb-local-mode)
;;   ;; (add-hook 'text-mode-hook 'breadcrumb-local-mode)
;;   (setq breadcrumb-idle-time 5) ; 1
;;   ;; (setq breadcrumb-imenu-crumb-separator "/")

;;   (setq breadcrumb-project-max-length 0.1) ; 0.3
;;   (setq breadcrumb-imenu-max-length 0.2) ; 0.3

;;   ;; Make Org heading style the same.
;;   ;; https://github.com/joaotavora/breadcrumb/issues/35
;;   (defun breadcrumb-org-crumbs ()
;;     "Get the chain from the top level heading down to current heading."
;;     (org-format-outline-path
;;      (org-get-outline-path t) (1- (frame-width)) nil " > "))
;;   (defun breadcrumb--header-line ()
;;     "Helper for `breadcrumb-headerline-mode'."
;;     (let* ((imenu-crumbs
;;             (if (eq major-mode 'org-mode)
;;                 'breadcrumb-org-crumbs
;;               'breadcrumb-imenu-crumbs))
;;            (x
;;             (cl-remove-if
;;              #'seq-empty-p
;;              (mapcar #'funcall `(breadcrumb-project-crumbs ,imenu-crumbs)))))
;;       (mapconcat #'identity x (propertize " : " 'face 'breadcrumb-face)))))

;;;; :app


;;;;; consult-jq

(use-package! consult-jq)
;; :config
;; (setq consult-jq-filter-alist
;;       '(("items" . "keys[]")
;;         ("types" . "map_values(type)")
;;         ("count" . "length")))  ;; Added custom shorthand

;;;;; DONT :app wiki-summary

;; (require 'wiki-summary)
;; (setq wiki-summary-language-string "ko")

;;;;; :app calendar

;; calendar

;;;;; :app @yeetube

(use-package! yeetube
  :defer t
  :after emms
  :init (define-prefix-command 'my/yeetube-map)
  :config
  (setf yeetube-mpv-disable-video t) ;; Disable video output
  (setf yeetube-play-function #'emms-play-url)
  :bind (("C-c y" . 'my/yeetube-map)
         :map my/yeetube-map
         ("s" . 'yeetube-search)
         ("b" . 'yeetube-play-saved-video)
         ("d" . 'yeetube-download-videos)
         ("p" . 'yeetube-mpv-toggle-pause)
         ("v" . 'yeetube-mpv-toggle-video)
         ("V" . 'yeetube-mpv-toggle-no-video-flag)
         ("k" . 'yeetube-remove-saved-video)))


;;;;; :app rss

;; (rss +org +youtube)        ; emacs as an RSS reader

;; gc copy-link
(after! elfeed
  ;; +rss-enable-sliced-images ;  default t
  (setq rmh-elfeed-org-files (list (my/org-elfeed-file))) ; default ~/org/elfeed.org
  ;; org-directory
  (setq elfeed-search-filter "") ; "@6-months-ago") ;;  "@1-month-ago +unread"
  (setq elfeed-search-title-max-width 90) ; default 70
  ;; (add-hook 'elfeed-search-mode-hook #'elfeed-update)
  )

(after! elfeed-tube
  (require 'elfeed-tube)
  ;; (setq elfeed-tube-invidious-url "https://vid.puffyan.us")
  (setq elfeed-tube-captions-languages '("en" "ko" "englsh (auto generated)")))

;;;;; DONT :app @mastodon

;; (use-package! tp)

(use-package! mastodon)

(after! mastodon
  (require 'mastodon-toot)
  (setq mastodon-tl--horiz-bar "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (setq mastodon-tl--highlight-current-toot t
        mastodon-tl--tag-timeline-tags t
        mastodon-tl--show-avatars t)
  ;; The default emojis take two characters for me
  (setq mastodon-tl--symbols
        '((reply "" . "R")
          (boost "" . "B")
          (favourite "" . "F")
          (bookmark "" . "K")
          (media "" . "[media]")
          (verified "" . "V")
          (locked "" . "[locked]")
          (private "" . "[followers]")
          (direct "" . "[direct]")
          (edited "" . "[edited]")))
  (mastodon-discover) ; context-mode
  (add-hook 'mastodon-toot-mode-hook
            (lambda ()
              (auto-fill-mode -1) ; default
              (display-line-numbers-mode -1)
              (jinx-mode 1)))
  )

;;;;;; my/dired-attach-to-mastodon

;; (defun my/dired-attach-to-mastodon (files mastodon-buffer)
;;   (interactive
;;    (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
;;          (or (cl-loop for buf being the buffers
;;                       if (eq (buffer-local-value 'mastodon-toot-mode buf) t)
;;                       return buf)
;;              (user-error "No buffer found!"))))
;;   (unless files
;;     (user-error "No (non-directory) files selected"))
;;   (with-current-buffer mastodon-buffer
;;     (dolist (file files)
;;       (mastodon-toot--attach-media
;;        file
;;        (read-from-minibuffer (format "Description for %s: " file))))))

;;;;;; HACK mastodon-toot--attach-media

;; (after! mastodon
;;   ;; string-to-unibyte 함수가 문제인데. 대체 하면 된다. 왜 이걸 쓰는 것인가?
;;   (defun mastodon-http--read-file-as-string (filename &optional url)
;;     "Read a file FILENAME as a string.
;; Used to generate image preview.
;; URL means FILENAME is a URL."
;;     (with-temp-buffer
;;       (if url
;;           (url-insert-file-contents filename)
;;         (insert-file-contents filename))
;;       (string-to-multibyte (buffer-string))
;;       ;; (string-to-unibyte (buffer-string))
;;       ))
;;   )

;;;;; :app @ebooks

;;;;; :app @osm OpenStreetMaps

;; Very cool and the nice thing is it integrates itself with the built-in
;; bookmarking system. So you can bookmark places (or store them as org links)
;; and jump to them whenever needed.

(use-package! osm
  :defer t
  :bind ("C-c M" . osm-prefix-map) ;; Alternatives: `osm-home' or `osm'
  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information
  ;; :config
  ;; Add custom servers, see also https://github.com/minad/osm/wiki
  ;; (osm-add-server 'myserver
  ;;   :name "My tile server"
  ;;   :group "Custom"
  ;;   :description "Tiles based on aerial images"
  ;;   :url "https://myserver/tiles/%z/%x/%y.png?apikey=%k")
  )
;;;;;; calibredb

;; apt-get install calibre -y
(use-package! calibredb
  :defer t
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Documents/calibre/"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  ;; (setq calibredb-library-alist '(("~/Documents/calibre/")
  ;;                                 ("~/sync/markdown/epub/")))
  (map! :map calibredb-show-mode-map
        :ne "?" #'calibredb-entry-dispatch
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "q" #'calibredb-entry-quit
        :ne "." #'calibredb-open-dired
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments)
  (map! :map calibredb-search-mode-map
        :ne [mouse-3] #'calibredb-search-mouse
        :ne "RET" #'calibredb-find-file
        :ne "?" #'calibredb-dispatch
        :ne "a" #'calibredb-add
        :ne "A" #'calibredb-add-dir
        :ne "c" #'calibredb-clone
        :ne "d" #'calibredb-remove
        :ne "D" #'calibredb-remove-marked-items
        :ne "j" #'calibredb-next-entry
        :ne "k" #'calibredb-previous-entry
        :ne "l" #'calibredb-virtual-library-list
        :ne "L" #'calibredb-library-list
        :ne "n" #'calibredb-virtual-library-next
        :ne "N" #'calibredb-library-next
        :ne "p" #'calibredb-virtual-library-previous
        :ne "P" #'calibredb-library-previous
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "S" #'calibredb-switch-library
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "v" #'calibredb-view
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "." #'calibredb-open-dired
        :ne "b" #'calibredb-catalog-bib-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "r" #'calibredb-search-refresh-and-clear-filter
        :ne "R" #'calibredb-search-clear-filter
        :ne "q" #'calibredb-search-quit
        :ne "m" #'calibredb-mark-and-forward
        :ne "f" #'calibredb-toggle-favorite-at-point
        :ne "x" #'calibredb-toggle-archive-at-point
        :ne "h" #'calibredb-toggle-highlight-at-point
        :ne "u" #'calibredb-unmark-and-forward
        :ne "i" #'calibredb-edit-annotation
        :ne "DEL" #'calibredb-unmark-and-backward
        :ne [backtab] #'calibredb-toggle-view
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-n" #'calibredb-show-next-entry
        :ne "M-p" #'calibredb-show-previous-entry
        :ne "/" #'calibredb-search-live-filter
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments))

;;;;;; nov

;; evil-collection/modes/nov/evil-collection-nov.el
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :commands (nov-org-link-follow nov-org-link-store)
  :init
  (with-eval-after-load 'org
    (org-link-set-parameters "nov"
                             :follow 'nov-org-link-follow
                             :store 'nov-org-link-store))
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up
        :n "d" 'nov-scroll-up
        :n "u" 'nov-scroll-down)

  (defun +nov-mode-setup ()
    "Tweak nov-mode to our liking."
    (face-remap-add-relative 'variable-pitch
                             :family "Pretendard Variable"
                             :height 1.1
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.0)
    (variable-pitch-mode 1)
    (setq-local line-spacing 0.2
                ;; next-screen-context-lines 4
                shr-use-colors nil)
    (when (featurep 'hl-line-mode)
      (hl-line-mode -1))
    (when (featurep 'font-lock-mode)
      (font-lock-mode -1))
    ;; Re-render with new display settings
    (nov-render-document)
    ;; Look up words with the dictionary.
    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition))
  (add-hook 'nov-mode-hook #'+nov-mode-setup 80)
  (setq font-lock-global-modes '(not nov-mode))
  )

;;;;; DONT :app @anddo for todo

;; (progn
;;   (require 'anddo)
;;   ;; fix path
;;   (defun anddo--create-tables ()
;;     (unless anddo--db
;;       (setq-local anddo--db
;;                   (sqlite-open
;;                    (expand-file-name "resources/anddo.sqlite" org-directory)))
;;       (sqlite-execute anddo--db "create table if not exists item (id integer primary key, status text, subject text, body text, entry_time text, modification_time text)")))
;;   )

;;;;; :app @gif-screencast

(use-package! gif-screencast
  :defer 5
  :commands gif-screencast
  :bind (:map gif-screencast-mode-map
              ("<f8>". gif-screencast-toggle-pause)
              ("<f9>". gif-screencast-stop))
  :init
  (setq gif-screencast-output-directory org-screenshot-path)
  ;; :init
  ;; (setq gif-screencast-args '("-x")
  ;;       gif-screencast-cropping-program ""
  ;;       gif-screencast-capture-format "ppm")
  :config
  (defun gif-screencast--generate-gif (process event)
    "Generate GIF file."
    (when process
      (gif-screencast-print-status process event))
    (message "Compiling GIF with %s..." gif-screencast-convert-program)
    (let* ((output-filename (expand-file-name
                             (format-time-string
                              (concat "%Y%m%dT%H%M%S--screencast." gif-screencast-output-format) ; "output-%F-%T."
                              (current-time))
                             (or (and (file-writable-p gif-screencast-output-directory)
                                      gif-screencast-output-directory)
                                 (read-directory-name "Save output to directory: "))))
           (delays (cl-loop for (this-frame next-frame . _)
                            on gif-screencast--frames
                            by #'cdr
                            ;; Converters delays are expressed in centiseconds.
                            for delay = (when next-frame
                                          (format "%d" (* 100 (float-time
                                                               (time-subtract (gif-screencast-frame-timestamp next-frame)
                                                                              (gif-screencast-frame-timestamp this-frame))))))
                            when next-frame
                            collect delay))
           (delays (cons gif-screencast-first-delay delays))
           (files-args (cl-loop for frame in gif-screencast--frames
                                for delay in delays
                                append (list "-delay" delay (gif-screencast-frame-filename frame))))
           (convert-args (append gif-screencast-convert-args
                                 files-args
                                 (list output-filename)))
           (convert-process (gif-screencast--start-process
                             gif-screencast-convert-program
                             convert-args)))
      (set-process-sentinel convert-process (lambda (process event)
                                              (gif-screencast-print-status process event)
                                              (when (and gif-screencast-want-optimized
                                                         (eq (process-status process) 'exit)
                                                         (= (process-exit-status process) 0))
                                                (gif-screencast-optimize output-filename))
                                              (when (and gif-screencast-autoremove-screenshots
                                                         (eq (process-status process) 'exit)
                                                         (= (process-exit-status process) 0))
                                                (dolist (f gif-screencast--frames)
                                                  (delete-file (gif-screencast-frame-filename f))))))))
  )



;;;;; :app go-translate v3

;; M-x gt-do-translate
;; /vanilla/douo-dotfiles-kitty/init.el
;; (use-package! go-translate
;;   :defer t
;;   :after pdf-tools
;;   :if window-system
;;   :commands (douo/go-do-translate douo/pdf-view-translate)
;;   :init
;;   (setq gt-langs '(en ko))
;;   ;; Translate by paragraph and insert each result at the end of source paragraph
;;   ;; This configuration is suitable for translation work. That is: Translate -> Modify -> Save
;;   (defun douo/go-do-translate (text-property-string)
;;     (gt-start (gt-translator
;;                :taker (gt-taker
;;                        ;; 单个换行替换为空格
;;                        :text (replace-regexp-in-string
;;                               "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2"
;;                               text-property-string))
;;                :engines (gt-google-engine)
;;                :render (gt-posframe-pop-render))))
;;   :custom
;;   (gt-cache-p t)
;;   (gt-default-translator
;;    (gt-translator
;;     :taker (gt-taker :langs '(en ko) :text (lambda () (replace-regexp-in-string
;;                                                        "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2"
;;                                                        (thing-at-point 'paragraph)))
;;                      :prompt t
;;                      )
;;     :engines (gt-google-engine)
;;     :render (gt-buffer-render)))
;;   :bind
;;   (:map pdf-view-mode-map
;;         ;; consult 不支持与 pdf-tools 的交互
;;         ;; ("C-s" . isearch-forward)
;;         ;; ("C-r" . isearch-backward)
;;         ("C-t" . douo/pdf-view-translate))
;;   (:map embark-prose-map ;; 覆盖 transpose-xxx
;;         ("t" . douo/go-do-translate))
;;   (:map embark-region-map ;; 覆盖 transpose-regions
;;         ("t" . douo/go-do-translate))
;;   :config
;;   (setq gt-chatgpt-key
;;         (auth-info-password
;;          (car (auth-source-search
;;                :host "api.openai.com"
;;                :user "apikey"))))
;;   (setq gt-chatgpt-model "gpt-4o-mini")

;;   (require 'pdf-tools)
;;   ;; 自定义 pdf 翻译文本提取器
;;   ;; 如果有高亮返回高亮文本，无则返回整页文本
;;   (defun douo/gts-pdf-view-selection-texter ()
;;     (unless (pdf-view-active-region-p)
;;       (pdf-view-mark-whole-page))
;;     ;; remove-newline-characters-if-not-at-the-end-of-sentence
;;     ;; ::HACK:: 解决 pdf 提取文本不能正确断行的问题
;;     ;; 移除不是处于句尾[.!?]的换行符
;;     (replace-regexp-in-string "\\([^.!?]\\)\n\\([^ ]\\)" "\\1 \\2"
;;                               (car (pdf-view-active-region-text))))
;;   (defvar douo/pdf-translater
;;     (gt-translator
;;      :taker (gt-taker :text 'douo/gts-pdf-view-selection-texter)
;;      :engines (list (gt-google-engine))
;;      :render (gt-buffer-render)
;;      ;; :splitter (gts-paragraph-splitter)
;;      ))
;;   (defun douo/pdf-view-translate ()
;;     (interactive)
;;     (gt-start douo/pdf-translater)
;;     ;;  cancel selection in emacs
;;     (deactivate-mark))
;;   )
; end-of go-translate

;; (setq gt-default-translator
;;       (gt-translator
;;        :taker (gt-taker :text 'buffer :pick 'paragraph)
;;        :engines (gt-google-engine)
;;        :render (gt-insert-render :type 'after)))

;; Translate the current paragraph and replace it with the translation result
;; This configuration is suitable for scenes such as live chat. Type some text, translate it, and send it
;; (setq gt-default-translator
;;       (gt-translator
;;        :taker (gt-taker :text 'paragraph :pick nil)
;;        :engines (gt-google-engine)
;;        :render (gt-insert-render :type 'replace)))

;; Translate specific words in current paragraph and insert the result after each word
;; This configuration can help in reading articles with some words you don't know
;; (setq gt-default-translator
;;       (gt-translator
;;        :taker (gt-taker :text 'paragraph
;;                         :pick 'word
;;                         :pick-pred (lambda (w) (length> w 6)))
;;        :engines (gt-google-engine)
;;        :render (gt-insert-render :type 'after
;;                                  :rfmt " (%s)"
;;                                  :rface '(:foreground "grey"))))

;; (setq gt-default-translator
;;       (gt-translator :taker (gt-taker :pick nil :prompt t)
;;                      :engines (gt-chatgpt-engine :stream t)
;;                      :render (gt-insert-render)))

;;;;; TODO notmuch - email

;; karthink-dotfiles-popper/lisp/setup-email.el
;; Use corfu in notmuch buffers
;; (use-package notmuch-address
;;   :when (or (daemonp) (display-graphic-p))
;;   :defer
;;   :config
;;   (setq notmuch-address-use-company nil
;;         notmuch-address-selection-function #'ignore)
;;   (define-advice notmuch-address-setup (:after () use-corfu)
;;     (add-hook 'completion-at-point-functions
;;               (cape-company-to-capf 'notmuch-company)
;;               nil t)))

;;;;; DONT :app consult-omni

;; (use-package! consult-omni
;;   :after (consult gptel)
;;   :custom
;;   ;; General settings that apply to all sources
;;   (consult-omni-show-preview t) ;;; show previews
;;   (consult-omni-preview-key "M-m") ;;; set the preview key to C-o
;;   (consult-omni-highlight-matches-in-minibuffer t) ;;; highlight matches in minibuffer
;;   (consult-omni-highlight-matches-in-file t) ;;; highlight matches in files
;;   (consult-omni-default-count 5) ;;; set default count
;;   (consult-omni-default-page 0) ;;; set the default page (default is 0 for the first page)

;;   ;; optionally change the consult-omni debounce, throttle and delay.
;;   ;; Adjust these (e.g. increase to avoid hiting a source (e.g. an API) too frequently)
;;   (consult-omni-dynamic-input-debounce 0.8)
;;   (consult-omni-dynamic-input-throttle 1.6)
;;   (consult-omni-dynamic-refresh-delay 0.8)

;;   ;; Optionally set backend for http request (either 'url, 'request, or 'plz)
;;   (consult-omni-http-retrieve-backend 'url)
;;   :config
;;   ;; Load Sources Core code
;;   (require 'consult-omni-sources)

;;   ;; Load Embark Actions
;;   (require 'consult-omni-embark)

;;   ;; Either load all source modules or a selected list
;;   ;; Select a list of modules you want to aload, otherwise all sources all laoded
;;   (setq consult-omni-sources-modules-to-load
;;         '(consult-omni-man
;;           consult-omni-wikipedia
;;           consult-omni-notes
;;           ;; consult-omni-numi ; calculator
;;           consult-omni-locate
;;           consult-omni-line-multi
;;           consult-omni-youtube
;;           consult-omni-invidious
;;           ;; consult-omni-gptel
;;           ;; consult-omni-google-autosuggest
;;           ;; consult-omni-google
;;           consult-omni-git-grep
;;           consult-omni-grep
;;           consult-omni-ripgrep
;;           consult-omni-gh
;;           consult-omni-find
;;           consult-omni-fd
;;           consult-omni-elfeed
;;           consult-omni-duckduckgo
;;           ;; consult-omni-doi
;;           consult-omni-dict
;;           consult-omni-notes
;;           ;; consult-omni-chatgpt
;;           consult-omni-calc
;;           ;; consult-omni-buffer
;;           ;; consult-omni-browser-history
;;           ;; consult-omni-brave
;;           ;; consult-omni-brave-autosuggest
;;           ;; consult-omni-bing
;;           consult-omni-apps))
;;   (consult-omni-sources-load-modules)

;;   ;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
;;   (setq consult-omni-multi-sources '("calc"
;;                                      ;; "File"
;;                                      ;; "Buffer"
;;                                      ;; "Bookmark"
;;                                      ;; "Apps"
;;                                      ;; "gptel"
;;                                      "Brave"
;;                                      "Dictionary"
;;                                      ;; "Google"
;;                                      "Wikipedia"
;;                                      "elfeed"
;;                                      ;; "mu4e"
;;                                      ;; "buffers text search"
;;                                      ;; "Notes Search"
;;                                      ;; "Org Agenda"
;;                                      "GitHub"
;;                                      ;; "YouTube"
;;                                      ;; "Invidious"
;;                                      ))

;; ;;;;;; consult-omni - Per source customization

;;   (require 'auth-source)
;;   (require 'gptel)

;;   ;; Set API KEYs. It is recommended to use a function that returns the string for better security.
;;   ;; (setq consult-omni-google-customsearch-key "YOUR-GOOGLE-API-KEY-OR-FUNCTION")
;;   ;; (setq consult-omni-google-customsearch-cx "YOUR-GOOGLE-CX-NUMBER-OR-FUNCTION")
;;   ;; (setq consult-omni-brave-api-key (auth-info-password (car (auth-source-search :host "api.search.brave.com" :user "apikey"))))
;;   ;; (setq consult-omni-stackexchange-api-key "YOUR-STACKEXCHANGE-API-KEY-OR-FUNCTION")
;;   ;; (setq consult-omni-pubmed-api-key "YOUR-PUBMED-API-KEY-OR-FUNCTION")
;;   ;; (setq consult-omni-openai-api-key (auth-info-password (car (auth-source-search :host "api.openai.com" :user "apikey"))))

;;   ;; add more keys as needed here.
;;   ;; gptel settings
;;   (setq consult-omni-gptel-cand-title #'consult-omni--gptel-make-title-short-answer)

;;   ;; default terminal
;;   (setq consult-omni-embark-default-term #'vterm)

;;   ;; default video player
;;   (setq consult-omni-embark-video-default-player  #'mpv-play-url)

;;   ;; pretty prompt for launcher
;;   (setq consult-omni-open-with-prompt "  ")

;;   ;; Pick your favorite autosuggest command.
;;   (setq consult-omni-default-autosuggest-command #'consult-omni-dynamic-brave-autosuggest) ;;or any other autosuggest source you define

;;   ;; Set your shorthand favorite interactive command
;;   (setq consult-omni-default-interactive-command #'consult-omni-multi)

;;   ;; Optionally Set back-end for notes search to ripgrep-all (requires ripgrep-all)
;;   ;; (setq consult-omni-notes-backend-command "rga")

;; ;;;;;; Optionally add more interactive commands

;; ;;;;;;; my/consult-omni-web

;;   (progn
;;     (defvar consult-omni-web-sources (list "gptel"
;;                                            "Brave"
;;                                            "elfeed"
;;                                            ;; "mu4e"
;;                                            "Wikipedia"
;;                                            ;; "GitHub"
;;                                            ))
;;     (defun my/consult-omni-web (&optional initial prompt sources no-callback &rest args)
;;       "Interactive web search”

;; This is similar to `consult-omni-multi', but runs the search on
;; web sources defined in `consult-omni-web-sources'.
;; See `consult-omni-multi' for more details.
;; "
;;       (interactive "P")
;;       (let ((prompt (or prompt (concat "[" (propertize "my/consult-omni-web" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
;;             (sources (or sources consult-omni-web-sources)))
;;         (consult-omni-multi initial prompt sources no-callback args)))
;;     )

;; ;;;;;;; my/consult-omni-video

;;   (progn
;;     (defvar consult-omni-video-sources (list "gptel"
;;                                              "Invidious"
;;                                              ))
;;     (defun my/consult-omni-video (&optional initial prompt sources no-callback &rest args)
;;       "Interactive video search”

;; This is similar to `consult-omni-multi', but runs the search on
;; web sources defined in `consult-omni-video-sources'.
;; See `consult-omni-multi' for more details.
;; "
;;       (interactive "P")
;;       (let ((prompt (or prompt (concat "[" (propertize "my/consult-omni-video" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
;;             (sources (or sources consult-omni-video-sources)))
;;         (consult-omni-multi initial prompt sources no-callback args)))
;;     )

;; ;;;;;;; my/consult-omni-local

;;   (defvar consult-omni-local-sources (list "ripgrep"
;;                                            "Notes Search"
;;                                            ;; "Apps"
;;                                            "Org Agenda"
;;                                            ))
;;   (defun my/consult-omni-local (&optional initial prompt sources no-callback &rest args)
;;     "Interactive local search”

;; This is similar to `consult-omni-multi', but runs the search on
;; local sources defined in `consult-omni-local-sources'.
;; See `consult-omni-multi' for more details.
;; "
;;     (interactive "P")
;;     (let ((prompt (or prompt (concat "[" (propertize "my/consult-omni-local" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
;;           (sources (or sources consult-omni-local-sources)))
;;       (consult-omni-multi initial prompt sources no-callback args)))

;; ;;;;;;; DONT my/consult-omni-scholar

;;   ;; (progn
;;   ;;     (setq consult-omni-scholar-sources (list "PubMed" "Scopus" "Notes Search" "gptel"))

;;   ;;     (defun consult-omni-scholar (&optional initial prompt sources no-callback &rest args)
;;   ;;       "Interactive “multi-source acadmic literature” search

;;   ;; This is similar to `consult-omni-multi', but runs the search on
;;   ;; academic literature sources defined in `consult-omni-scholar-sources'.
;;   ;; See `consult-omni-multi' for more details.
;;   ;; "
;;   ;;       (interactive "P")
;;   ;;       (let ((prompt (or prompt (concat "[" (propertize "consult-omni-multi" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
;;   ;;             (sources (or sources consult-omni-scholar-sources)))
;;   ;;         (consult-omni-multi initial prompt sources no-callback args)))
;;   ;;     )

;; ;;;;;;; my/consult-omni-autosuggest-at-point

;;   ;; AutoSuggest at point
;;   (defun my/consult-omni-autosuggest-at-point ()
;;     (interactive)
;;     (let ((input (or (thing-at-point 'url) (thing-at-point 'filename) (thing-at-point 'symbol) (thing-at-point 'sexp) (thing-at-point 'word))))
;;       (when (and (minibuffer-window-active-p (selected-window))
;;                  (equal (substring input 0 1) (consult--async-split-initial nil)))
;;         (setq input (substring input 1)))
;;       (consult-omni-brave-autosuggest input)))
;;   )
; end-of consult-omni

;;;;; DONT :app wakatime-mode

;; python3 -c "$(wget -q -O - https://raw.githubusercontent.com/wakatime/vim-wakatime/master/scripts/install_cli.py)"

; (use-package! wakatime-mode
;;   ;; :if (and (or
;;   ;;              (string= (system-name) "jhnuc")
;;   ;;              (string= (system-name) "junghan-laptop")
;;   ;;              )
;;   ;;         (not my/slow-ssh)
;;   ;;         (not my/remote-server))
;;   :init
;;   (add-hook 'prog-mode-hook 'wakatime-mode)
;;   (add-hook 'org-mode-hook 'wakatime-mode)
;;   (add-hook 'markdown-mode-hook 'wakatime-mode)
;;   :defer 5
;;   :config
;;   (advice-add 'wakatime-init :after (lambda () (setq wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli"))))

;;   ;; wakatime-api-key  "your-api-key" in permachine.el
;;   (defun my/wakatime-dashboard ()
;;     (interactive)
;;     (browse-url "https://wakatime.com/dashboard"))
;;   )

;;;; :os tty

;;;;; DONT term-keys

;; (use-package! term-keys
;;   :unless window-system
;;   :config
;;   (unless (display-graphic-p) ; terminal
;;     (term-keys-mode t)))

;;;;;; usage

;; (progn
;;   (require 'term-keys-kitty)
;;   (with-temp-buffer
;;     (insert (term-keys/kitty-conf))
;;     (write-region (point-min) (point-max) "~/kitty-for-term-keys.conf")))

;;;;; DONT kkp

;; (unless (display-graphic-p) ; gui
;;   (require 'kkp)
;;   (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
;;   )

;;;; end-of user-configs

;;;; LOAD DOOMEMACS-CONFIG/LISP

;;;; :lang org

;;;;; org configs

(setq org-id-locations-file (file-name-concat org-directory (concat "." system-name "-orgids"))) ; ".org-id-locations"))

;;;;; org reloading

(after! org
  (message "after org - config")

  (load-file (concat user-dotemacs-dir "lisp/org-funcs.el"))
  (load-file (concat user-dotemacs-dir "lisp/org-config.el"))
  ;; (+org-init-keybinds-h) -> 2024-06-01 여기 키바인딩 관련 부분 뒤에서 다시 잡아줌
  ;; (setq org-attach-use-inheritance nil) ; selective

  ;; overide here! important
  (setq org-insert-heading-respect-content nil) ; doom t

  (progn
    ;; [[denote:20250309T142131][#LLM: Tab width in Org files must be 8, not 4]]
    (add-hook 'org-mode-hook (lambda () (setq-local tab-width 8)))

    (defun my/org-tab-width-warning-filter (orig-fun type message &rest args)
      (unless (and (eq type 'emacs)
                   (string-match-p "Tab width in Org files must be 8" message))
        (apply orig-fun type message args)))
    (advice-add 'display-warning :around #'my/org-tab-width-warning-filter))

  ;; (progn
  ;;   ;; 2024-06-04 file - id - http/https
  ;;   (org-link-set-parameters "file" :face `(:inherit link :weight semibold :slant italic :underline t)) ;; italic
  ;;   (org-link-set-parameters "http" :face `(:inherit warning :weight semibold :underline t))
  ;;   (org-link-set-parameters "info" :face `(:inherit info-file :weight semibold :underline t))
  ;;   (org-link-set-parameters "https" :face `(:inherit warning :weight semibold :underline t))
  ;;   )
  (org-link-set-parameters "denote" :face `(:inherit success :weight semibold :underline t)) ; id

  ;; 2024-06-24 performance issue
  ;; (remove-hook 'org-mode-hook 'org-eldoc-load)
  )

;;;;; org-modern

(after! org
  (when (locate-library "org-modern")
    (require 'org-modern)
    (progn
      ;; configurtaion
      (setq
       ;; Edit settings
       ;; org-auto-align-tags nil ; default t
       org-tags-column 0 ; doom 0
       org-catch-invisible-edits 'show-and-error ; smart
       org-special-ctrl-a/e t
       ;; org-insert-heading-respect-content t ; prefer nil

       ;; Org styling, hide markup etc.
       ;; org-ellipsis "…"
       org-hide-emphasis-markers nil ; nil
       org-pretty-entities t ; nil
       org-agenda-tags-column 0)

      (setq org-modern-tag nil)
      (setq org-modern-timestamp nil)
      (setq org-modern-table nil) ; org-modern-indent
      ;;  org-modern-todo t
      ;;  org-modern-priority t
      ;;  org-modern-checkbox t
      ;;  org-modern-block-name t
      ;;  org-modern-footnote nil
      ;;  org-modern-internal-target nil
      ;;  org-modern-radio-target nil
      ;;  org-modern-progress nil)

      (setq org-modern-star nil) ; org-modern-indent
      (setq org-modern-hide-stars nil) ; adds extra indentation
      (setq org-modern-list
            '((?+ . "•") ; ◦
              (?- . "–") ; – endash
              (?* . "➤"))) ; ➤ ‣

      ;; (setq org-modern-block-fringe 0) ; default 2
      (setq org-modern-block-name nil)
      ;; (setq org-modern-block-name
      ;;       '((t . t)
      ;;         ("src" "»" "«")
      ;;         ("example" "»–" "–«")
      ;;         ("quote" "❝" "❞")
      ;;         ("export" "⏩" "⏪")))

      (setq org-modern-progress nil)

      ;; https://github.com/tecosaur/emacs-config/blob/master/config.org?plain=1#L7886
      (setq org-modern-keyword nil)
      (setq org-modern-priority t)
      (setq org-modern-priority-faces
            '((?A :inverse-video t :inherit +org-todo-todo)
              (?B :inverse-video t :inherit +org-todo-next)
              (?C :inverse-video t :inherit +org-todo-dont)
              (?D :inverse-video t :inherit +org-todo-done)
              ))

      (setq org-modern-todo-faces
            '(("TODO" :inverse-video t :inherit +org-todo-todo)
              ("DONE" :inverse-video t :inherit +org-todo-done)
              ("NEXT"  :inverse-video t :inherit +org-todo-next)
              ("DONT" :inverse-video t :inherit +org-todo-dont)
              ))
      )

    ;; (remove-hook! org-mode-hook #'org-modern-mode)
    ;; (remove-hook! org-agenda-finalize . org-modern-agenda)
    ;; (require 'org-modern-indent)
    ;; (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
    )
  )

;;;;; TODO org-src-mode-map

;; (with-eval-after-load 'org-src
;;   ;; "c" 'org-edit-src-exit
;;   ;; "a" 'org-edit-src-abort
;;   ;; "k" 'org-edit-src-abort

;; C-c C-c geiser-eval-definition
;;   ;; I prefer C-c C-c over C-c ' (more consistent)
;;   (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)
;;   )

;;;;; org-capture templates

;;;;;; org-contacts

(after! org
  (require 'org-contacts)
  (add-to-list
   'org-capture-templates
   `("c" "Contacts" entry (file ,(my/org-contacts-file))
     "* %(org-contacts-template-name)\n:PROPERTIES:\n:GITHUB:\n:EMAIL: a@a.com\n:URL:\n:NOTE:\n:END:\n%U\n%T\n%a\n")) ;; :CUSTOM_ID: %(prot-org--id-get)\n
  )

;;;;;; org-bookmarks

;; (after! org
;;   (require 'org-bookmarks)
;;   (add-to-list 'org-capture-templates
;;                `("b" ,(format "%s\tAdd a new bookmark to %s"
;;                               (when (fboundp 'nerd-icons-mdicon)
;;                                 (nerd-icons-mdicon
;;                                  "nf-md-bookmark_plus_outline"
;;                                  :face 'nerd-icons-blue))
;;                               (file-name-nondirectory org-bookmarks-file))
;;                  entry (file ,(expand-file-name org-bookmarks-file))
;;                  ,(concat
;;                    "* %^{bookmark title}\t\t\t\t"
;;                    (format ":%s:" org-bookmarks-tag)
;;                    "
;; :PROPERTIES:
;; :URL:  %^C
;; :DATE: %t
;; :END:")
;;                  :empty-lines 1
;;                  :jump-to-captured t
;;                  :refile-targets ((,org-bookmarks-file :maxlevel 3)))
;;                :append))

;;;;; org-journal

;; (require 'side-journal)
(progn
  (require 'org-journal)

  (defun my-old-carryover (old_carryover)
    (save-excursion
      (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
        (dolist (entry (reverse old_carryover))
          (save-restriction
            (narrow-to-region (car entry) (cadr entry))
            (goto-char (point-min))
            (org-scan-tags '(lambda ()
                              (org-todo "DONT")
                              (org-set-tags ":ARCHIVE:"))
                           matcher org--matcher-tags-todo-only))))))

  (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"NEXT\"")
  (setq org-journal-handle-old-carryover-fn 'my-old-carryover)

  (setq org-journal-dir (concat user-org-directory "journal"))
  (setq org-journal-file-format "%Y%m%dT000000--%Y-%m-%d__journal_week%W.org")
  (setq org-journal-date-format "%Y-%m-%d %a") ; Week%W:

  ;; (setq org-journal-date-prefix "#+title: ")
  ;; (setq org-journal-time-prefix "** ") ; default **
  ;; (setq org-journal-time-format "%R ") ; "[%<%Y-%m-%d %a %H:%M>]" ; default "%R "

  (setq org-journal-enable-agenda-integration t) ; default nil
  (setq org-journal-file-type 'weekly) ; default 'daily

  (setq org-journal-tag-alist '(("meet" . ?m) ("dev" . ?d) ("idea" . ?i) ("emacs" . ?e) ("discuss" . ?c) ("1on1" . ?o))) ; default nil

  (add-hook 'org-journal-mode-hook (lambda () (setq-local tab-width 8)))

  (defun my/org-journal-add-custom-id ()
    ;;  :CUSTOM_ID: 2025-03-21 Mon
    (unless (org-journal--daily-p)
      (org-set-property "CUSTOM_ID"
                        (downcase (format-time-string "%Y-%m-%d-%a")))))

  (add-hook 'org-journal-after-header-create-hook #'my/org-journal-add-custom-id)

  )

;;;;; TODO om-dash org-based dashboards

;; [cite:@gavvomdash24] Building blocks for org-based dashboards.
;; (use-package! om-dash
;;   :defer 5
;;   :config
;;   (require 'parse-csv)
;;   )

;;;; :ui

(defvar user-imenu-list-height 0.90)

;;;;; savehist-auto-save-interval

(setq savehist-autosave-interval 300)

;;;;; lin - hl-line

;;  “LIN locally remaps the hl-line face to a style that is optimal
;;  for major modes where line selection is the primary mode of
;;  interaction.”  In otherwords, ~lin.el~ improves the highlighted
;;  line behavior for the competing contexts.
;; :init (global-hl-line-mode) ; doom default

(use-package! lin
  :init
  (global-hl-line-mode +1)
  :config
  ;; You can use this to live update the face:
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-blue)
  (lin-global-mode 1))

;;;;; spacious-padding

(use-package! spacious-padding
  :if window-system ; important
  :hook (server-after-make-frame . spacious-padding-mode)
  :init
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line-active
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive))
  (setq spacious-padding-widths
        '(:header-line-width 4
          :mode-line-width 4 ; 6
          :tab-width 4 ; sync mode-line-width for keycast-tab-bar
          :internal-border-width 20 ; 15
          :right-divider-width 30 ; 30
          :scroll-bar-width 8
          :fringe-width 8
          ))
  (add-hook! 'doom-load-theme-hook #'spacious-padding-mode)
  :config
  ;; (when (fboundp 'tooltip-mode) (tooltip-mode 1))
  ;; (when (fboundp 'tool-bar-mode) (tool-bar-mode 1))
  ;; (when (display-graphic-p) ; gui
  ;;   (menu-bar-mode +1)) ; disable <f10>
  (spacious-padding-mode +1))

;;;;; pulse : built-in - visual feedback

(progn
  ;; add visual pulse when changing focus, like beacon but built-in
  ;; from from https://karthinks.com/software/batteries-included-with-emacs/
  (require 'pulse)
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command
           '(scroll-up-command scroll-down-command recenter-top-bottom ace-window other-window))
    (advice-add command :after #'pulse-line)))

;;;;; DONT pulsar - performance issue

;; LionyxML-lemacs/lemacs-init.org
;; The `pulsar' package enhances the user experience in Emacs by providing
;; visual feedback through pulsating highlights. This feature is especially
;; useful in programming modes, where it can help users easily track
;; actions such as scrolling, error navigation, yanking, deleting, and
;; jumping to definitions.

;; (use-package! pulsar
;;   :hook (doom-first-input . pulsar-global-mode)
;;   :config
;;   (progn
;;     (setq pulsar-pulse t)
;;     (setq pulsar-delay 0.025)
;;     (setq pulsar-iterations 10)
;;     ;; (setq pulsar-face 'evil-ex-lazy-highlight)
;;     (setq pulsar-face 'pulsar-magenta)
;;     ;; (setq pulsar-highlight-face 'pulsar-yellow)

;;     ;; reset
;;     (setq pulsar-pulse-functions nil)

;;     (dolist
;;         (built-in-function
;;          '(recenter-top-bottom
;;            move-to-window-line-top-bottom
;;            reposition-window bookmark-jump other-window
;;            delete-window delete-other-windows
;;            forward-page backward-page scroll-up-command
;;            scroll-down-command tab-new tab-close tab-next
;;            org-next-visible-heading
;;            org-previous-visible-heading
;;            org-forward-heading-same-level
;;            org-backward-heading-same-level
;;            outline-backward-same-level
;;            outline-forward-same-level
;;            outline-next-visible-heading
;;            outline-previous-visible-heading
;;            outline-up-heading))
;;       (add-to-list 'pulsar-pulse-functions built-in-function))

;;     (when (fboundp 'winner-undo)
;;       (add-to-list 'pulsar-pulse-functions 'winner-undo)
;;       (add-to-list 'pulsar-pulse-functions 'winner-redo))

;;     (when (fboundp 'winum-select-window-1)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-1)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-2)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-3)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-4)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-5)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-6)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-7)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-8)
;;       (add-to-list 'pulsar-pulse-functions 'winum-select-window-9))

;;     (when (fboundp 'evil-window-right)
;;       (add-to-list 'pulsar-pulse-functions 'evil-window-right)
;;       (add-to-list 'pulsar-pulse-functions 'evil-window-left)
;;       (add-to-list 'pulsar-pulse-functions 'evil-window-up)
;;       (add-to-list 'pulsar-pulse-functions 'evil-window-next)
;;       (add-to-list 'pulsar-pulse-functions 'evil-window-prev)
;;       (add-to-list 'pulsar-pulse-functions 'evil-window-down))

;;     (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
;;     (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
;;     (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
;;     (add-to-list 'pulsar-pulse-functions 'flycheck-next-error)
;;     (add-to-list 'pulsar-pulse-functions 'flycheck-previous-error)
;;     ;; (add-to-list 'pulsar-pulse-functions 'evil-yank)
;;     ;; (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
;;     (add-to-list 'pulsar-pulse-functions 'evil-delete)
;;     (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
;;     ;; (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
;;     (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
;;     (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk)
;;     ))

;;;; :lang pkm

;;;;; after denotes : Load custom denote

(after! denote
  (message "Load: custom denote")
  (require 'denote-config)
  (require 'denote-funcs)
  (require 'denote-hugo) ; for publish
  ;; (add-hook 'doom-first-input-hook #'my/refresh-agenda-files)
  )

;;;;; check consult-denotes

;; (consult-customize
;;  my/denote-grep  my/denote-find-file
;;  :preview-key '("M-m" :debounce 0.3 "<up>" "<down>" "C-j" "C-k"))

;;;;; DONT consult-notes-file-dir-sources

(after! consult-notes
  (setq consult-notes-file-dir-sources
        '(
          ;; ("root"  ?r  "~/sync/org")
          ("meta/Hub" ?h "~/sync/org/meta")
          ;; ("bib/Literature" ?b "~/sync/org/bib")
          ;; ("notes/Fleeting" ?n "~/sync/org/notes")
          ;; ("posts/Permanent" ?p "~/sync/org/posts")
          ;; ("llmlog/AI" ?l "~/sync/org/llmlog")
          ;; ("docs/Zettels" ?d "~/sync/org/docs")
          ;; ("07.Journal" ?j "~/sync/org/journal")
          ;; ("09.Ekg" ?e "~/sync/org/ekg")
          ;; ("10.MD" ?m "~/sync/org/md")
          ;; ("11.Import" ?i "~/sync/org/import")
          ;; ("12.Talks" ?t  "~/sync/org/talks")
          )))

;;;; Waiting

;;;;; atomic-chrome

(use-package! atomic-chrome
  :if window-system
  :defer 4
  :commands (atomic-chrome-start-server)
  :config
  (atomic-chrome-start-server)
  )

;;;;; google-translte

(use-package! google-translate
  :defer 3
  :init
  (require 'google-translate)
  :init
  (autoload 'google-translate-translate "google-translate-core-ui" "google-translate-translate" nil nil)

  :config
  ;; load +google-translate
  (load! "+google-translate")

  (defadvice! google-translate-at-point--set-lang-auto (fn &optional override-p)
    :around #'google-translate-at-point
    (pcase-let ((`(,src ,tgt)
                 (alist-get current-input-method
                            '((nil . (en ko))
                              ("korean-hangul" . (ko en)))
                            nil nil #'string-equal)))
      (let ((google-translate-default-source-language (symbol-name src))
            (google-translate-default-target-language (symbol-name tgt)))
        (funcall-interactively fn override-p))))

  (defun google-translate-to-korean (&optional str)
    "Translate given string automatically without language selection prompt."
    (let ((lang
           (cond
            ((string-match "[가-힣]" str)
             "ko")
            ((or (string-match "[ァ-ヶー]" str)
                 (string-match "[ぁ-んー]" str)
                 ;; (string-match "[亜-瑤]" str)
                 )
             "ja")
            ((string-match "[一-龥]" str)
             "zh-CN")
            (t
             "en"))))
      (google-translate-translate
       lang
       (if (string= "ko" lang)
           "en"
         "ko")
       str)))

  (setq google-translate-input-method-auto-toggling t
        google-translate-preferable-input-methods-alist
        '((nil . ("en"))
          (korean-hangul . ("ko"))))

  (setq google-translate-show-phonetic t)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "ko")

  ;; it doesn't pop to the buffer automatically
  (defun google-translate--find-buffer (x)
    (pop-to-buffer "*Google Translate*"))

  (advice-add 'google-translate-buffer-output-translation :after #'google-translate--find-buffer)

  (add-to-list
   'display-buffer-alist
   '("\\*Google Translate\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.25)))
  )


;;;;; TODO translate-mode

(use-package! translate-mode
  :defer 5
  ;; :hook (translate-mode . translate//set-translate-mode-paragraph-functions)
  )

;; (progn
;;   (defun translate/translate-current-reference-paragraph ()
;;     "Show all available translations of the reference paragraph at point in a pop-up frame."
;;     (interactive)
;;     (gt-translate translate//paragraph-translator))

;;   (defun translate/translate-word-at-point ()
;;     "Pop-up translations of the word at point."
;;     (interactive)
;;     (gt-translate translate//word-translator))

;;   (defun translate//set-translate-mode-paragraph-functions ()
;;     (cond ((eq major-mode 'markdown-mode)
;;            (setq translate-forward-paragraph-function 'markdown-forward-paragraph
;;                  translate-backward-paragraph-function 'markdown-backward-paragraph))
;;           ((eq major-mode 'org-mode)
;;            (setq translate-forward-paragraph-function 'org-forward-paragraph
;;                  translate-backward-paragraph-function 'org-backward-paragraph))))
;;   )

;;;;; TODO IDE Layout with Side Windows

;; https://whhone.com/emacs-config/#ide-layout-with-side-windows

;;;;; browse-hist

(use-package! browser-hist
  :defer t
  :commands (browser-hist-search)
  :init
  (require 'embark) ; load Embark before the command (if you're using it)
  :config
  (setq browser-hist-db-paths
        '((edge . "/home/junghan/.config/microsoft-edge/Default/History")
          (whale . "/home/junghan/.config/naver-whale/Default/History")
          (chrome . "$HOME/.config/google-chrome/Default/History")
          (brave . "$HOME/.config/BraveSoftware/Brave-Browser/Default/History")
          (firefox . "$HOME/.mozilla/firefox/*.default-release-*/places.sqlite")
          (qutebrowser . "$HOME/.local/share/qutebrowser/history.sqlite")))
  (setq browser-hist--db-fields
        '((chrome      "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
          (edge    "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
          (whale    "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
          (qutebrowser "title"    "url"    "History"       "ORDER BY atime           desc")
          (brave       "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
          (firefox     "title"    "url"    "moz_places"    "ORDER BY last_visit_date desc")
          ))
  (setq browser-hist-default-browser 'firefox) ; edge
  )

;;;;; DONT dictionary-overlay

;; 수정이 필요할 듯
;; https://github.com/ginqi7/dictionary-overlay
;; (use-package! dictionary-overlay)

;;;;; TODO paw

;; https://emacs-china.org/t/paw-el-emacs-lingq/27331/71
;; (use-package! paw
;;   :defer t)

;; (set-popup-rules! '(("^\\*paw-view-note*" :size 0.35 :side right :quit t :modeline t :select nil :ttl nil :vslot 2 :slot 1)
;;                     ("^\\*paw-sub-note*" :height 0.5 :side right :quit t :modeline t :select t :ttl nil :vslot 2 :slot 2)))

;;;; DONT persp-mode with tab-bar for open-workspaces

;; 2025-08-19 disabled
;; (after! persp-mode
;;   ;; shares a common set of buffers between perspectives
;; (setq persp-shared-buffers
;;       '("*scratch*" "*Org Agenda(n)*" "*Messages*" "*doom:scratch*"))
;; (add-hook 'persp-activated-functions
;;           (lambda (_) (persp-add-buffer persp-shared-buffers)))
;;   )

;;;;; my/workspaces

;;;;###autoload
(defun my/open-workspaces ()
  (interactive)

  ;; (message "my/open-workspaces")
  (+workspace/new-named "work")
  (find-file "~/repos/work")

  (+workspace/new-named "repos")
  (find-file user-project-directory)

  (+workspace/new-named "dots")
  (find-file doom-user-dir)

  (+workspace/new-named "feed")
  (elfeed)
  ;; (bh/switch-to-scratch)

  (+workspace/switch-to-0) ;; main
  ;; (find-file (concat denote-directory "notes")) ; for denote-dired excerpt
  ;; (evil-window-vsplit)
  (my/denote-random-note-from-directory (concat denote-directory "meta"))

  (setq org-agenda-files org-user-agenda-files) ; reset
  ;; (my/add-today-journal-to-agenda) -- use org-journal-enable-agenda-integration
  (my/refresh-agenda-files)
  ;; (ash-goto-org-agenda) ; tab-bar

  (setq tab-bar-close-button nil)
  (tab-bar-new-tab)
  (bh/switch-to-scratch)
  (tab-bar-select-tab 1)
  (modus-themes-toggle)
  )

;; (when (display-graphic-p) ; gui
;;   (add-hook 'doom-first-input-hook #'my/open-workspaces))

;;;; tab-line-mode on emacs-30

(use-package! tab-line
  :if window-system
  :demand t
  :config
  (setq tab-line-exclude-modes '(completion-list-mode reb-mode reb-lisp-mode calc-mode calc-trail-mode)) ; 2025-02-09
  (setq tab-line-close-tab-function #'kill-buffer)
  (setq tab-line-tab-name-truncated-max 26) ; default 20
  (setq tab-line-tab-name-ellipsis "…")
  (setq tab-line-tab-name-function
        #'tab-line-tab-name-truncated-buffer)
  (setq
   tab-line-new-button-show nil
   tab-line-close-button-show nil)

  (global-tab-line-mode 1)
  )

;;;; copy-screenshot-markdown

;; https://tristancacqueray.github.io/blog/emacs-30

(defun get-newest-file-from-dir  (path)
  "Return the latest file in PATH."
  (car (directory-files path 'full nil #'file-newer-than-file-p)))

(defun copy-screenshot-markdown (name)
  "Move the latest screenshot and insert markdown link with NAME."
  (interactive "Mname: ")
  (let* ((infile (expand-file-name (get-newest-file-from-dir org-screenshot-path)))
         (outdir (concat (file-name-directory (buffer-file-name)) (concat org-directory "images")))
         (outfile (expand-file-name (concat name ".png") outdir)))
    (unless (file-directory-p outdir) (make-directory outdir t))
    (rename-file infile outfile)
    (insert (concat "![" name "]( ../images/" (file-name-nondirectory outfile) ")"))
    (newline)
    (newline)))

;;;; Latex Preview for math symbol

;;;;; math-preview

(use-package! math-preview)

;;;;; org-fragtog

;; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  ;; :hook (markdown-mode . org-fragtog-mode)
  :init
  (progn ;; for org-fragtog-mode for markdown-mode
    ;; 2025-01-24 disable for markdown-mode, 2024-06-27 안쓰는게 나은듯
    ;; The new org-data element provides properties from top-level property drawer,
    (setq org-element-use-cache nil) ; default t
    ;; Element cache persists across Emacs sessions
    (setq org-element-cache-persistent nil) ; default t
    ;; org-element-with-disabled-cache
    (add-to-list 'warning-suppress-types '(org-element))
    )

  ;; (setq org-fragtog-preview-delay 0.2)
  ;; (setq org-startup-with-latex-preview t) ; doom nil
  ;; (setq org-highlight-latex-and-related '(native)) ; doom nil
  ;; (setq org-highlight-latex-and-related '(native script entities)) ; doom org +pretty
  )

;;;;; DONT xelatex and dvisvgm

;; from tshu
;; (after! ox-latex
;;   (setq org-latex-compiler "xelatex"
;;         org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"
;;                                 "latexmk -c -bibtex")
;;         org-latex-prefer-user-labels t
;;         org-preview-latex-default-process 'dvisvgm
;;         ;; org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/")
;;         org-preview-latex-process-alist
;;         '((dvisvgm :programs ("xelatex" "dvisvgm")
;;            :description "xdv > svg"
;;            :message "you need to install the programs: xelatex and dvisvgm."
;;            :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
;;            :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
;;            :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))
;;   )

;;;;; DONT org-latex-preview

;; /home/junghan/sync/man/dotsamples/doom/tecosaur-dot-doom/config.org

;; Setup LaTeX previews in =org-mode=.
;; See https://abode.karthinks.com/org-latex-preview/ for configuration.

;; (after! org
;;   (add-to-list 'org-latex-packages-alist '("" "amsmath" t))
;;   (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
;;   (add-to-list 'org-latex-packages-alist '("" "mathtools" t))
;;   (add-to-list 'org-latex-packages-alist '("" "mathrsfs" t)))

;; (use-package! org-latex-preview
;;   :after org
;;   :config
;;   (setq org-startup-with-latex-preview t) ; doom nil
;;   (setq org-highlight-latex-and-related '(native script entities)) ; doom org +pretty
;;   ;; (setq org-highlight-latex-and-related '(native)) ; doom nil
;;   ;; Increase preview width
;;   (plist-put org-latex-preview-appearance-options
;;              :page-width 0.8)

;;   ;; Use dvisvgm to generate previews
;;   ;; You don't need this, it's the default:
;;   ;; (setq org-latex-preview-process-default 'dvisvgm)

;;   ;; Turn on auto-mode, it's built into Org and much faster/more featured than org-fragtog.
;;   ;; (Remember to turn off/uninstall org-fragtog.)
;;   (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
;;   (add-hook 'markdown-mode-hook 'org-latex-preview-auto-mode)

;;   ;; Block C-n and C-p from opening up previews when using auto-mode
;;   (setq org-latex-preview-auto-ignored-commands
;;         '(next-line previous-line mwheel-scroll
;;           scroll-up-command scroll-down-command))

;;   ;; Enable consistent equation numbering
;;   (setq org-latex-preview-numbered t)

;;   ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
;;   ;; fragment and updates the preview in real-time as you edit it.
;;   ;; To preview only environments, set it to '(block edit-special) instead
;;   (setq org-latex-preview-live t)

;;   ;; More immediate live-previews -- the default delay is 1 second
;;   (setq org-latex-preview-live-debounce 0.25)
;;   )

;;;;; DONT cdlatex

;; rather use doom's config

;; (use-package! cdlatex
;;   :bind
;;   (("C-\"" . (lambda () (interactive)
;;                (cdlatex-ensure-math)
;;                (cdlatex-math-symbol))))
;;   :init
;;   (setq cdlatex-math-symbol-prefix ?\;)
;;   :config

;;   (defun lauremacs-cdlatex-add-math-symbols ()
;;     (add-multiple-into-list
;;      'cdlatex-math-symbol-alist-comb
;;      '(
;;        (?.  "\\cdot"   "\\dots")
;;        (?\; "\\;")
;;        (?C  ""         "\\mathbb{C}"   "\\arccos")
;;        (?N  "\\nabla"  "\\mathbb{N}"   "\\exp")
;;        (?Q  "\\Theta"  "\\mathbb{Q}")
;;        (?R  "\\Re"     "\\mathbb{R}")
;;        (?Z  ""         "\\mathbb{Z}")
;;        )))

;;   (define-minor-mode org-math-mode
;;     "Some config to write math on `org-mode'."
;;     :lighter "org-math-mode"
;;     (org-fragtog-mode 1)
;;     (org-cdlatex-mode 1)
;;     (lauremacs-cdlatex-add-math-symbols))
;;   )

;;;;; TODO aas : auto-activating-snippets

;; https://github.com/ymarco/auto-activating-snippets
;; ~/sync/man/dotsamples/doom/lemon-dot-doom/config.el

;; (use-package! aas
;;   ;; can't defer loading of this as we need it in every single spawned
;;   ;; buffer including scratch
;;   :init (add-hook 'find-file-hook #'aas-activate-for-major-mode)
;;   :config
;;   (aas-global-mode)
;;   (aas-set-snippets 'global
;;     ":-)" "🙂"
;;     ";--" "—"
;;     ";-." "→"
;;     ";=." "⇒"
;;     ";!=" "≠"
;;     "-." "->"
;;     "=." "=>"
;;     "j9" "("))

;;;;; laas : latex-auto-activating-snippets

;; https://github.com/tecosaur/LaTeX-auto-activating-snippets
;; (browse-url-emacs "https://raw.githubusercontent.com/tecosaur/LaTeX-auto-activating-snippets/master/README.org")

(use-package! laas
  :hook ((LaTeX-mode . laas-mode)
	 (org-mode . laas-mode)))

;;   (defun laas-tex-fold-maybe ()
;;     (unless (equal "/" aas-transient-snippet-key)
;;       (+latex-fold-last-macro-a)))
;;   (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe)

;;;;; DONT literate-calc-mode

;; (use-package! literate-calc-mode)

;;;; TODO Load +llm summarize-buffer

(load! "+llm")
(global-set-key (kbd "M-g SPC") '+gpt-dwim-current-buffer)

;;;; TODO Scheme Clojure Racket with SICP/SICM

;;;;; scheme with geiser-mit

(use-package! geiser-mit
  :config
  (setenv "MITSCHEME_HEAP_SIZE" "100000") ; 16384
  (setenv "MITSCHEME_LIBRARY_PATH" "/usr/lib/x86_64-linux-gnu/mit-scheme")
  (setenv "MITSCHEME_BAND" "mechanics.com")

  ;; (setenv "DISPLAY" ":0")
  (setq geiser-active-implementations '(mit))
  (setq geiser-mit-binary "/usr/bin/mit-scheme")
  )

;;;;; DONT built-in scheme not geiser

;; /home/junghan/doomemacs-git/modules/lang/scheme/
;; (progn
;;   (defvar calculate-lisp-indent-last-sexp)
;;   ;; Adapted from https://github.com/alezost/emacs-config/blob/master/utils/al-scheme.el#L76-L123
;; ;;;;###autoload
;;   (defun +scheme-indent-function-a (indent-point state)
;;     "Advice to replace `scheme-indent-function'.

;; This function is the same as `scheme-indent-function' except it properly indents
;; property lists and names starting with 'default'."
;;     (let ((normal-indent (current-column)))
;;       (goto-char (1+ (elt state 1)))
;;       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
;;       (if (and (elt state 2)
;;                ;; NOTE looking-at -> looking-at-p
;;                (not (looking-at-p "\\sw\\|\\s_")))
;;           (progn
;;             ;; NOTE (if (not ...) (progn ...)) -> (unless ... ...)
;;             (unless (> (save-excursion (forward-line 1) (point))
;;                        calculate-lisp-indent-last-sexp)
;;               (goto-char calculate-lisp-indent-last-sexp)
;;               (beginning-of-line)
;;               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
;;             (backward-prefix-chars)
;;             (current-column))
;;         ;; NOTE let -> let* & moved `method' def into let bindings
;;         (let* ((function (buffer-substring
;;                           (point) (progn (forward-sexp 1) (point))))
;;                (method (or (get (intern-soft function) 'scheme-indent-function)
;;                            (get (intern-soft function) 'scheme-indent-hook))))
;;           (cond ((or (eq method 'defun)
;;                      (and (null method)
;;                           (> (length function) 3)
;;                           ;; NOTE string-match -> string-match-p
;;                           ;; NOTE The original regexp is "\\`def" but it will mess
;;                           ;;      up indentation with such names as 'default-...'.
;;                           (string-match-p "\\`def" function)))
;;                  (lisp-indent-defform state indent-point))
;;                 ;; NOTE Added this clause to handle alignment of keyword symbols
;;                 ((and (null method)
;;                       (> (length function) 1)
;;                       ;; NOTE string-match -> string-match-p
;;                       (string-match-p "\\`:" function))
;;                  (let ((lisp-body-indent 1))
;;                    (lisp-indent-defform state indent-point)))
;;                 ((integerp method)
;;                  (lisp-indent-specform method state indent-point normal-indent))
;;                 (method
;;                  (funcall method state indent-point normal-indent)))))))

;;   (use-package! scheme
;;     :interpreter ("scsh" . scheme-mode)
;;     :config
;;     (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(scheme-mode))
;;     (advice-add #'scheme-indent-function :override #'+scheme-indent-function-a))

;;   (require 'scheme)
;;   (after! scheme
;;     (add-hook 'scheme-mode-hook #'aggressive-indent-mode)

;;     ;; /home/junghan/sync/man/dotsamples/spacemacs/sritchie-spacemacs-scheme/init.el
;;     ;; required to get org-mode exporting the goodies.
;;     (require 'ob-mit-scheme) ; site-lisp

;;     ;; ;; this is used by xscheme now.
;;     (setq scheme-program-name "mechanics")

;;     (add-to-list 'auto-mode-alist '("\\.vlad\\'" . scheme-mode))
;;     (add-to-list 'auto-mode-alist '("\\.dvl\\'" . scheme-mode))
;;     (add-to-list 'auto-mode-alist '("\\.sc\\'" . scheme-mode))

;;     (defun mechanics-local ()
;;       (interactive)
;;       (run-scheme "mechanics"))

;;     (defun mechanics ()
;;       (interactive)
;;       (let ((default-directory (or (projectile-project-root)
;;                                    default-directory)))
;;         (call-interactively #'mechanics-local)))

;; ;;;;###autoload
;;     (defun +scheme/open-repl ()
;;       "Open the Scheme REPL."
;;       (interactive)
;;       ;; (call-interactively #'geiser-repl-switch)
;;       (call-interactively #'mechanics-local)
;;       (current-buffer))

;;     )
;;   )

;;;;; DONT clojure

(when (modulep! :lang clojure)

;;;;;; clerk-show

  (defun clerk-show ()
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;;;;;; emmy

  ;; TODO emmy

;;;;;; clojure-mode - extra-font-locking

  ;; Do not indent single ; comment characters
  (add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))

  (after! clojure-mode
    (require 'clojure-mode-extra-font-locking)
    ;; (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)
    ;; (define-key clojure-mode-map (kbd "C-c v") 'vega-view)
    )

;;;;;; indent-bar

  (add-hook 'clojure-mode-hook #'+indent-guides-init-maybe-h)

;;;;;; DONT cider

  ;; [2025-04-02 Wed 10:33] jack-in하면 문제 발생. 확인바람.
  ;; (after! cider
  ;;   ;; In recent versions, an option has been introduced that attempts to improve
  ;;   ;; the experience of CIDER by accessing java source & javadocs, though this
  ;;   ;; option is still currently considered beta.
  ;;   (setq cider-enrich-classpath t)

  ;;   (if (package-installed-p 'corfu)
  ;;       (evil-define-key 'insert cider-repl-mode-map
  ;;         (kbd "C-j") 'corfu-next
  ;;         (kbd "C-k") 'corfu-previous))

  ;;   ;; (add-to-list 'auto-mode-alist '("\\.clj_kondo\\'" . clojure-mode))
  ;;   ;; (add-to-list 'auto-mode-alist '("\\.endl$" . clojure-mode))
  ;;   ;; (add-to-list 'magic-mode-alist '("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode))

  ;;   ;; Because of CIDER's insistence to send forms to all linked REPLs, we
  ;;   ;; *have* to be able to switch cljc buffer to clj/cljs mode without
  ;;   ;; cider complaining.
  ;;   ;; (setq clojure-verify-major-mode nil) ; 나중에 해보고

  ;;   ;; john's style
  ;;   ;; Vertically align s-expressions
  ;;   ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
  ;;   ;; (setq clojure-indent-style 'align-arguments ; default always-align
  ;;   ;;       clojure-align-forms-automatically t ; default nil
  ;;   ;;       clojure-toplevel-inside-comment-form t  ; nil - evaluate expressions in comment as top level
  ;;   ;;       )

  ;;   ;; manually use on lsp mode
  ;;   (defun my/cider-repl-prompt (namespace)
  ;;     "Return a prompt string that mentions NAMESPACE."
  ;;     (format "%s🦄 " (cider-abbreviate-ns namespace)))

  ;;   ;; NOTE 2022-11-21: for the linter (clj-kondo), refer to the Flymake
  ;;   ;; NOTE 2022-11-23: This is not final.  I will iterate on it over
  ;;   ;; time as I become more familiar with the requirements.
  ;;   (setq cider-repl-result-prefix ";; => "
  ;;         cider-eval-result-prefix ""
  ;;         cider-connection-message-fn t ; cute, but no!
  ;;         cider-repl-prompt-function #'my/cider-repl-prompt
  ;;         ;; cider-use-overlays nil ; echo area is fine
  ;;         )

  ;;   (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  ;;   (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)

  ;;   ;; (setq cider-preferred-build-tool 'clojure-cli)
  ;;   (setq
  ;;    cider-prompt-for-symbol nil ; default nil
  ;;    cider-repl-buffer-size-limit 100          ; limit lines shown in REPL buffer
  ;;    ;; cider-repl-display-help-banner t ; default nil enable help banner
  ;;    ;; cider-print-fn 'puget                   ;; pretty printing with sorted keys / set values
  ;;    ;; cider-result-overlay-position 'at-point   ; results shown right after expression
  ;;    ;; nrepl-use-ssh-fallback-for-remote-hosts t ; connect via ssh to remote hosts

  ;;    ;; cider-repl-use-clojure-font-lock nil ; default t
  ;;    ;; cider-repl-use-pretty-printing nil ; default t
  ;;    )
  ;;   )

;;;;;; TODO imenu for clojure

  ;; 2025-03-26 확인 나중에
  ;; (progn
  ;;     (require 'consult-imenu)

  ;; ;;;###autoload
  ;;     (defun add-clojure-imenu-regexp-h ()
  ;;       "Hacky way to get imenu for root-level keywords. Useful in edn files."
  ;;       (setq-local imenu-generic-expression
  ;;                   `(
  ;;                     ("Section" "^[ \t]*;;;+\\**[ \t]+\\([^\n]+\\)" 1)
  ;;                     ("Functions" "^\\s-*\\(defn +\\([^ )\n]+\\)" 1)
  ;;                     ("Macros" "^\\s-*\\(defmacro +\\([^ )\n]+\\)" 1)
  ;;                     ("Structs" "^\\s-*\\(defstruct +\\([^ )\n]+\\)" 1)
  ;;                     ("Namespaces" "^\\s-*\\(ns +\\([^ )\n]+\\)" 1)
  ;;                     ("Protocols" "^\\s-*\\(defprotocol +\\([^ )\n]+\\)" 1)
  ;;                     ("Records" "^\\s-*\\(defrecord +\\([^ )\n]+\\)" 1)
  ;;                     ("Types" "^\\s-*\\(deftype +\\([^ )\n]+\\)" 1)
  ;;                     ("Vars" "^\\s-*\\(def +\\([^ )\n]+\\)" 1)
  ;;                     ("Special Forms" "^\\s-*\\(def\\(?:\\w+\\) +\\([^ )\n]+\\)" 1)
  ;;                     ))
  ;;       (when (string= "edn" (file-name-extension (or (buffer-file-name) "")))
  ;;         (add-to-list 'imenu-generic-expression '(nil "^.?.?\\(:[^ ]+\\).*$" 1) t)))

  ;;     (add-hook! (clojure-mode clojure-ts-mode) #'add-clojure-imenu-regexp-h)

  ;;     (dolist (clojure '(clojure-mode clojure-ts-mode))
  ;;       (add-to-list 'consult-imenu-config
  ;;                    `(,clojure
  ;;                      :toplevel "Functions"
  ;;                      :types (
  ;;                              (?h "Section")
  ;;                              (?f "Functions" font-lock-function-name-face)
  ;;                              (?m "Macros" font-lock-type-face)
  ;;                              (?p "Protocols" font-lock-constant-face)
  ;;                              (?t "Types" font-lock-type-face)
  ;;                              (?v "Vars" font-lock-variable-name-face)
  ;;                              ))))
  ;;     )

;;;;;; clj-deps-new

  (use-package! clj-deps-new
    :defer 5
    :commands clj-deps-new)

;;;;;; TODO clay

  (use-package! clay
    :after cider
    :config
    (require 'clay))

;;;;;; kaocha-runner

  ;; Kaocha test runner from Emacs
  ;; - provides rich test reports
  (use-package! kaocha-runner
    :after cider
    ;; :config
    ;; enable Kaocha test runner
    ;; (setq clojure-enable-kaocha-runner t)
    )

;;;;;; cloure-essential-ref-nov

  ;; https://github.com/p3r7/clojure-essential-ref
  (use-package! clojure-essential-ref-nov
    :after cider
    :init
    (setq clojure-essential-ref-default-browse-fn #'clojure-essential-ref-nov-browse)
    (setq clojure-essential-ref-nov-epub-path "~/git/default/clj-essential-ref-v31.epub")
    :config
    (with-eval-after-load 'cider
      (evil-define-key '(insert normal) cider-mode-map (kbd "M-<f1>") 'clojure-essential-ref)
      (evil-define-key '(insert normal) cider-repl-mode-map (kbd "M-<f1>") 'clojure-essential-ref))
    )

;;;;;; TODO Clojure helper functions

  ;; Toggle reader comment #_ at beginnig of an expression
  (defun my/clojure-toggle-reader-comment-sexp ()
    (interactive)
    (let* ((point-pos1 (point)))
      (evil-insert-line 0)
      (let* ((point-pos2 (point))
             (cmtstr "#_")
             (cmtstr-len (length cmtstr))
             (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len)))
             (point-movement (if (string= cmtstr line-start) -2 2))
             (ending-point-pos (+ point-pos1 point-movement 1)))
        (if (string= cmtstr line-start)
            (delete-char cmtstr-len)
          (insert cmtstr))
        (goto-char ending-point-pos)))
    (evil-normal-state))

  ;; Assign keybinding to the toggle-reader-comment-sexp function
  ;; (define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)

;;;;;; DONT portal integration

  ;; def portal to the dev namespace to allow dereferencing via @dev/portal
  ;; (defun portal.api/open ()
  ;;   (interactive)
  ;;   (cider-nrepl-sync-request:eval
  ;;    "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

  ;; (defun portal.api/clear ()
  ;;   (interactive)
  ;;   (cider-nrepl-sync-request:eval "(portal.api/clear)"))

  ;; (defun portal.api/close ()
  ;;   (interactive)
  ;;   (cider-nrepl-sync-request:eval "(portal.api/close)"))

  ;; Key bindings added to Debug Clojure section
  ;; - , d p p - portal open
  ;; - , d p c - portal clear
  ;; - , d p D - portal clear

;;;;;; DONT clojure-cookbook with adoc-mode

  ;; (use-package! adoc-mode
  ;;   :mode (("\\.adoc$" . adoc-mode)
  ;;          ("\\.asciidoc$" . adoc-mode)))

  ;; (defun increment-clojure-cookbook ()
  ;;     "When reading the Clojure cookbook, find the next section, and
  ;; close the buffer. If the next section is a sub-directory or in
  ;; the next chapter, open Dired so you can find it manually."
  ;;     (interactive)
  ;;     (let* ((cur (buffer-name))
  ;; 	   (split-cur (split-string cur "[-_]"))
  ;; 	   (chap (car split-cur))
  ;; 	   (rec (car (cdr split-cur)))
  ;; 	   (rec-num (string-to-number rec))
  ;; 	   (next-rec-num (1+ rec-num))
  ;; 	   (next-rec-s (number-to-string next-rec-num))
  ;; 	   (next-rec (if (< next-rec-num 10)
  ;; 		         (concat "0" next-rec-s)
  ;; 		       next-rec-s))
  ;; 	   (target (file-name-completion (concat chap "-" next-rec) "")))
  ;;       (progn
  ;;         (if (equal target nil)
  ;; 	    (dired (file-name-directory (buffer-file-name)))
  ;; 	  (find-file target))
  ;;         (kill-buffer cur))))

  ;; (after! adoc-mode
  ;;   (define-key adoc-mode-map (kbd "M-+") 'increment-clojure-cookbook)
  ;;   (add-hook 'adoc-mode-hook 'cider-mode))

;;;;;; DONT ob-clojure with babashka

  ;; doom's default use cider
  ;; (require 'ob-clojure)
  ;; (setq! org-babel-clojure-backend 'babashka)

;;;;;; end-of-clojure

  ) ;; end-of clojure

;;;;; DONT racket-review with flycheck

;; (when (modulep! :lang racket)
;;   (require 'flycheck)
;;   ;; https://github.com/Bogdanp/racket-review
;;   (flycheck-define-checker racket-review
;;     "check racket source code using racket-review"
;;     :command ("raco" "review" source)
;;     :error-patterns
;;     ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
;;      (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
;;     :modes racket-mode)
;;   (add-to-list 'flycheck-checkers 'racket-review)
;;   )

;;;; Load ccmenu

;;;;; with ccmenu

(use-package! webpaste
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region)))

(use-package! fireplace :defer t)
(use-package! snow :defer t)

;;;;; Terminal Mode - (unless (display-graphic-p)

;; README /doomemacs-junghan0611/lisp/doom-ui.el

;; Terminal Mode
(unless (display-graphic-p) ; terminal
  (setq visible-cursor nil)
  (xterm-mouse-mode -1) ; important
  (setq fast-but-imprecise-scrolling nil)
  (setq hscroll-step 0)

  ;; Make vertical window separators look nicer in terminal Emacs
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  (show-paren-mode -1)
  (remove-hook 'dired-mode-hook 'nerd-icons-dired-mode)
  ;; (remove-hook 'marginalia-mode-hook 'nerd-icons-completion-marginalia-setup)
  )

;;;;; recent-rgrep - search

(use-package! recent-rgrep
  :defer t
  :commands (recent-rgrep))

;;;;; emacs-everywhere

;; (use-package! emacs-everywhere
;;   :config
;;   ;; emacs-everywhere-frame-name-format "Edit ∷ %s — %s"
;;   (setq emacs-everywhere-major-mode-function #'org-mode))

;;;;; DONT command-log-mode - keycast alternative

;; For showing which keys I'm pressing during screencasts, presentations, or pairing sessions.
;; - [[https://gitlab.com/screenkey/screenkey][screenkey]]: "A screencast tool to display your keys inspired by Screenflick"
;; (use-package! command-log-mode
;;   :config
;;   (setq
;;    command-log-mode-open-log-turns-on-mode t
;;    command-log-mode-window-size 80
;;    command-log-mode-is-global t))

;;;;; DONT code-cells for python jupyter

;; (use-package! code-cells
;;   :commands (code-cells-mode)
;;   :init (add-hook 'python-mode-hook 'code-cells-mode)
;;   )

;;;;; TODO embark-indicators

;; (progn
;;   ;; `embark-minimal-indicator', which does not display any
;;   (setq embark-indicators
;;         '(embark-mixed-indicator
;;           embark-highlight-indicator
;;           embark-isearch-highlight-indicator))

;;   (set-popup-rule! "\\*Embark Actions\\*" :side 'bottom :size 0.5 :select t :quit t) ; jh
;;   )

;;;;;; org-cv

;; [2023-08-14 Mon 14:12] http://ohyecloudy.com/emacsian/2022/10/29/org-mode-cv/
;; 종빈님 버전을 바로 사용.
;; M-x org-export-dispatch 함수를 호출하면 moderncv 메뉴가 보인다.
(use-package! ox-moderncv
  :init (require 'ox-moderncv)
  )

;;;; Load Unified Configuration

;; unified config for spacemacs and doom emacs
(require 'uniconfig)

;;;; Load Transient & Hydra Menu

(require 'hydrakeys)

;;;; Load Keys

(require 'keys)

;;;; Load 'Doom' Keybindings

;; override and add doom keybindings
(load! "+doomkeys")

;;;; macher

(use-package! macher
  :after gptel magit
  :commands (macher-implement macher-revise macher-discuss macher-abort)
  :custom
  ;; 'org' UI는 코드 블록과 설명을 구조화하기에 매우 좋습니다.
  (macher-action-buffer-ui 'org)
  :config
  (macher-install)
  (load! "+magit"))

;; (progn
;;   ;; [[denote:20250724T220236][#LLM: ◊diff-apply-unk Git 스타일 패치에서 새 파일 디렉토리 생성]]
;;   (defun my/diff-apply-hunk-with-file-creation ()
;;     "현재 hunk를 적용하면서, 필요한 디렉터리·파일을 자동으로 생성한다."
;;     (interactive)
;;     (save-excursion
;;       (pcase-let* ((`(,old ,new) (my/diff--old-new-files))
;;                    (target (if (string= old "/dev/null") new old))
;;                    ;; b/ 접두사 제거 후 절대경로
;;                    (target-path (expand-file-name target)))
;;         (cond
;;          ;; 1) 새 파일 추가(/dev/null → 실제 파일)
;;          ((string= old "/dev/null")
;;           (unless (file-directory-p (file-name-directory target-path))
;;             (when (y-or-n-p (format "Directory `%s' does not exist! Create it? "
;;                                     (file-name-directory target-path)))
;;               (make-directory (file-name-directory target-path) t)))
;;           (unless (file-exists-p target-path)
;;             (write-region "" nil target-path))
;;           (diff-apply-hunk))           ; 실제 패치 적용

;;          ;; 2) 파일 삭제(실제 파일 → /dev/null)
;;          ((string= new "/dev/null")
;;           (when (y-or-n-p (format "Delete file `%s'? " old))
;;             (diff-apply-hunk)))

;;          ;; 3) 기존 파일 수정
;;          (t
;;           (diff-apply-hunk))))))

;; ;; gemini 2.5 pro
;; (defun my/diff-apply-hunk-create-new-file (orig-fun &rest args)
;;   "Create directory and file for new files in Git-style patches.

;; This function is intended to be used as an :around advice for
;; `diff-apply-hunk`.

;; When applying a hunk for a new file (e.g., a diff from
;; /dev/null to b/path/to/new-file), this function first
;; creates the necessary parent directories and an empty file at the
;; target path. It then calls the original `diff-apply-hunk`
;; function to apply the changes."
;;   ;; In a diff buffer, `default-directory` is usually set to the
;;   ;; project root, so relative paths should work correctly.
;;   (let ((file-names (diff-hunk-file-names)))
;;     (when (and (listp file-names)
;;                (stringp (car file-names))
;;                (string-equal "/dev/null" (car file-names))
;;                (stringp (cadr file-names)))
;;       (let* ((new-file-git-path (cadr file-names))
;;              ;; Strip the "b/" prefix from git diff paths.
;;              (target-file (if (string-prefix-p "b/" new-file-git-path)
;;                               (substring new-file-git-path 2)
;;                             new-file-git-path))
;;              (target-dir (file-name-directory target-file)))

;;         ;; Create parent directories if they don't exist.
;;         (when (and target-dir (not (file-directory-p target-dir)))
;;           (make-directory target-dir 'parents))

;;         ;; Create an empty file to apply the hunk to.
;;         (unless (file-exists-p target-file)
;;           (with-temp-file target-file
;;             ;; This creates an empty file at the path `target-file`.
;;             nil)))))

;;   ;; Always call the original function to perform the actual patch application.
;;   ;; For new files, it will now find the created empty file.
;;   ;; For existing files or deletions, it will work as before.
;;   (apply orig-fun args))

;; ;; Activate the custom behavior by advising `diff-apply-hunk`.
;; (advice-add 'diff-apply-hunk :around #'my/diff-apply-hunk-create-new-file)

;; ;; To disable this behavior, you can run:
;; ;; (advice-remove 'diff-apply-hunk #'my/diff-apply-hunk-create-new-file)
;;   )

;;;; linenote

(use-package! org-linenote
  :init
  (setq org-linenote-default-extension ".md"))

;;;; org-books

(require 'org-books)
(setq org-books-file (my/org-reading-file))

;;; org-todo

(progn
  ;; /home/junghan/sync/man/dotsamples/dotall/yqrashawn-dot-doom-clj/.doom.d/org.el

  ;; (use-package! org-todoist
  ;;   :defer 5
  ;;   :init
  ;;   (setq org-todoist-priority-default 67)
  ;;   (setq org-todoist-deleted-keyword "DONT")
  ;;   :config
  ;;   (setq org-todoist-api-token user-todoist-token)
  ;;   (setq org-todoist-storage-dir (concat org-directory ".cache")) ; for cache
  ;;   (setq org-todoist-file "private/20250327T064848--org-todoist__aprj.org")
  ;;   )

  ;; (use-package! todoist
  ;;   :commands (todoist)
  ;;   :init
  ;;   (setq! todoist-token user-todoist-token
  ;;          ;; todoist-use-scheduled-instead-of-deadline
  ;;          todoist-backing-buffer (concat org-directory ".cache/todoist.org")))

  ;; (use-package! orgbox
  ;;   :after org
  ;;   :commands (orgbox orgbox-schedule)
  ;;   :init
  ;;   (setq! orgbox-start-time-of-day "9:30"
  ;;          orgbox-start-time-of-weekends "11:00"
  ;;          orgbox-start-time-of-evening "20:00"))

  (defun ar/org-insert-link-dwim ()
    "Like `org-insert-link' but with personal dwim preferences."
    (interactive)
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
           (clipboard-url (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0)))
           (region-content (when (region-active-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
             (delete-region (region-beginning) (region-end))
             (insert (org-make-link-string clipboard-url region-content)))
            ((and clipboard-url (not point-in-link))
             (insert (org-make-link-string
                      clipboard-url
                      (read-string "title: "
                                   (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                     (dom-text (car
                                                (dom-by-tag (libxml-parse-html-region
                                                             (point-min)
                                                             (point-max))
                                                            'title))))))))
            (t
             (call-interactively 'org-insert-link)))))

  (defun +denote-daily-note-file-name ()
    (concat (org-journal--get-entry-path
             (time-subtract (current-time)
                            (* 3600 org-extend-today-until)))))
  ;; (concat denote-directory "/" (format-time-string "%Y%m%dT000000" (current-time)) "==dailynote--daily-note__dailynote.md")

  (defun +denote-daily-note ()
    (interactive)
    (let ((daily-note-file (+denote-daily-note-file-name)))
      (if (f-exists? daily-note-file)
          (find-file daily-note-file)
        (progn
          ;; TODO add denote commands
          (org-journal-open-current-journal-file)
          ;; (format-time-string "[%Y-%m-%d %a %H:%M]")
          ;; (format-time-string "%Y%m%dT000000"
          ;;                     (org-journal--convert-time-to-file-type-time
          ;;                      (time-subtract (current-time)
          ;;                                     (* 3600 org-extend-today-until))))
          ))
      daily-note-file))

  ;; M-g j
  ;; (defun my/side-notes-toggle-daily-note ()
  ;;   (interactive)
  ;;   (let ((daily-note-file (+denote-daily-note-file-name)))
  ;;     (unless (f-exists? daily-note-file)
  ;;       (+denote-daily-note)
  ;;       (bury-buffer))
  ;;     (setq! side-notes-file daily-note-file)
  ;;     (call-interactively #'side-notes-toggle-notes)))
  )

;;;; mb-depth

;; from prot
;; (use-package! mb-depth
;;   :hook (after-init . minibuffer-depth-indicate-mode)
;;   :config
;;   (setq read-minibuffer-restore-windows nil) ; doom t
;;   ;; (setq enable-recursive-minibuffers t) ; conflict vertico-multiform
;;   )

;;;; DONT Emacs Application Framework (EAF)

;; check  'C-h v' eaf-var-list

;; (progn
;;   ;;  (setq eaf-python-command "/usr/bin/python")
;;   (require 'eaf)
;;   (require 'eaf-browser)
;;   ;; (require 'eaf-terminal)
;;   (require 'eaf-pyqterminal)
;;   ;; (require 'eaf-pdf-viewer)
;;   ;; (require 'eaf-mind-elixir)

;;   (add-hook 'eaf-mode-hook #'doom-mark-buffer-as-real-h)

;;   (defun my/eaf-setup-gtk-use-native-input ()
;;     (interactive)
;;     (when (eq major-mode 'eaf-mode)
;;       (setq-local x-gtk-use-native-input t)))
;;   (add-hook 'eaf-mode-hook #'my/eaf-setup-gtk-use-native-input)

;;   (progn
;;     ;; https://github.com/emacs-eaf/emacs-application-framework/wiki/Evil
;;     (require 'eaf-evil)
;;     (define-key key-translation-map (kbd "SPC")
;;                 (lambda (prompt)
;;                   (if (derived-mode-p 'eaf-mode)
;;                       (pcase eaf--buffer-app-name
;;                         ("browser" (if  eaf-buffer-input-focus
;;                                        (kbd "SPC")
;;                                      (kbd eaf-evil-leader-key)))
;;                         ("pdf-viewer" (kbd eaf-evil-leader-key))
;;                         ("mind-elixir" (kbd eaf-evil-leader-key))
;;                         ;; ("image-viewer" (kbd eaf-evil-leader-key))
;;                         (_  (kbd "SPC")))
;;                     (kbd "SPC"))))
;;     )

;;   ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (progn
;;     (setq eaf-browser-translate-language "ko")

;;     ;; make default browser
;;     ;; (setq browse-url-browser-function 'eaf-open-browser)
;;     ;; (defalias 'browse-web #'eaf-open-browser)
;;     (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind, see more in the Wiki

;;     ;; /home/junghan/sync/man/dotsamples/vanilla/gavinok-dotfiles/lisp/eaf-config.el
;;     (defun slurp (f)
;;       (with-temp-buffer
;;         (insert-file-contents f)
;;         (buffer-substring-no-properties
;;          (point-min)
;;          (point-max))))

;;     ;; https://www.abc.com abc
;;     (defun my/bm ()
;;       (interactive)
;;       (require 'eaf-browser)
;;       (let ((selected (completing-read
;;                        "Select URL: " (split-string
;;                                        (slurp "~/url-bookmarks.el") "\n" t))))
;;         (let ((url (car (split-string
;;                          selected
;;                          " " t))))
;;           (if (string-match-p "\\http.*\\'" url)
;;               ;; Open selected url
;;               (eaf-open-browser url)
;;             ;; Search entered text
;;             (eaf-search-it selected)))))
;;     (setq eaf-browser-continue-where-left-off t)
;;     (setq eaf-browser-default-search-engine "duckduckgo")
;;     (setq eaf-browser-enable-adblocker "true")
;;     ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;     ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;     )
;;   )
;; end-of eaf

;;;; :custom 'Local' Packages

;;;;; TODO elot : literate ontology tools

;; (add-to-list 'load-path "~/sync/emacs/forked-pkgs/elot")
;; (use-package! elot ; better
;;   :defer t)

;;;;; TODO emacs-bluesky

;; (add-to-list 'load-path "~/sync/emacs/git/junghan0611/emacs-bluesky/")
;; (load-file "~/sync/emacs/git/junghan0611/emacs-bluesky/bluesky.el")

;;;;; pylookup

(use-package! pylookup
  :commands (pylookup-lookup pylookup-update pylookup-update-all)
  :config
  (setq pylookup-dir (concat user-dotemacs-dir "local/pylookup/")
        pylookup-program (concat pylookup-dir "pylookup.py")
        pylookup-db-file (concat pylookup-dir "pylookup.db"))
  (setq pylookup-html-locations '("http://docs.python.org/ko/3.12")))

;;;;; txl.el

;; 2025-04-08 use local branch
(use-package! txl
  :defer 2
  :config
  ;; (setq txl-deepl-split-sentences nil)
  (setq txl-languages '(EN-US . KO)) ; using guess-language
  (setq txl-deepl-api-url "https://api-free.deepl.com/v2/translate")
  (setq txl-deepl-api-key (auth-info-password
                           (car (auth-source-search
                                 :host "api-free.deepl.com"
                                 :user "apikey"))))
  (with-eval-after-load 'evil-org
    (evil-define-key 'normal 'evil-org-mode-map (kbd "M-t") 'txl-translate-region-or-paragraph)))

;;;;; org-zettel

;; (use-package! org-zettel-ref-mode
;;   ;; :init
;;   ;; (setq org-zettel-ref-overview-directory "~/Documents/notes/source-note/")
;;   :config
;;   (setq org-zettel-ref-mode-type 'denote)
;;   ;; (setq org-zettel-ref-mode-type 'org-roam)
;;   ;; (setq org-zettel-ref-mode-type 'normal)
;;   (setq org-zettel-ref-python-file "~/sync/emacs/git/junghan0611/org-zettel-ref-mode/convert-to-org.py")
;;   ;; (setq org-zettel-ref-temp-folder "~/Documents/temp_convert/")
;;   ;; (setq org-zettel-ref-reference-folder "~/Documents/ref/")
;;   ;; (setq org-zettel-ref-archive-folder "/Volumes/Collect/archives/")
;;   (setq org-zettel-ref-debug t)
;;   )

;;;;; org-supertag

;(use-package! org-supertag
;  :after org
;  ;; :config (org-supertag-setup)
;  )

;;;;; org-ref

;(use-package! org-ref
;  :after (org)
;  :commands (org-ref-insert-link-hydra/body
;             org-ref-bibtex-hydra/body)
;  :config
;  (setq org-ref-insert-cite-function
;        (lambda ()
;          (call-interactively #'citar-insert-citation))))

;;;;; org-cv - ox-awesomecv

(after! org
  (add-to-list 'load-path "~/sync/emacs/git/default/org-cv/")
  (require 'ox-awesomecv))

;;; ccmenu: context-menu with casual

;; (when (display-graphic-p) ;; 2025-06-18 disable
;;   (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; (when (display-graphic-p) ;; gui
;;   (require 'ccmenu))

;;; Load office

(load! "+office")

;;; Additional Packages

;;;; Notmuch 이메일 설정

(after! notmuch
  ;; 다중 계정 설정
  (setq notmuch-identities
        '("jhkim2@goqual.com"
          "junghanacs@gmail.com"))

  ;; FCC (보낸 메일 저장 위치)
  (setq notmuch-fcc-dirs
        '(("jhkim2@goqual.com" . "work/[Gmail]/&yVwwYA-")
          ("junghanacs@gmail.com" . "personal/[Gmail]/Sent Mail")))

  ;; 메일 발송 설정
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil t)

  ;; 동기화 명령
  (setq +notmuch-sync-backend "mbsync -a")

  ;; 저장된 검색
  (setq notmuch-saved-searches
        '((:name "📧 Work Inbox"
           :query "tag:inbox AND to:jhkim2@goqual.com"
           :key "w")
          (:name "📧 Personal Inbox"
           :query "tag:inbox AND to:junghanacs@gmail.com"
           :key "p")
          (:name "📬 Unread"
           :query "tag:unread"
           :key "u")
          (:name "📤 Sent"
           :query "tag:sent"
           :key "s")
          (:name "🗓️ Today"
           :query "date:today"
           :key "t"))))

;;;; pass + auth

(after! pass
  (setq pass-username-field "login"
        password-store-password-length 24))

(use-package! password-store-menu
  :defer 2
  :commands (password-store-menu-enable)
  :custom (password-store-menu-key "C-c p")
  :config
  (password-store-menu-enable))

(setq auth-sources '(password-store "~/.authinfo.gpg"))

;;; TODO khoj

;; (
;;  ;; ~/.emacs.d/init.el 또는 ~/.emacs에 추가
;;  (add-to-list 'load-path "/home/goqual/git/clone/khoj/src/interface/emacs/")
;;  (require 'khoj)

;;  ;; 로컬 서버 설정
;;  (setq khoj-server-url "http://localhost:42110"
;;        khoj-auto-setup t)
;;  ;; Install Khoj client from MELPA Stable
;;  (setq khoj-index-directories '("~/sync/org/"))
;;  ;; khoj-index-files '("~/docs/todo.org" "~/docs/work.org")))

;;  ;; 키바인딩 설정 (선택사항)
;;  (global-set-key (kbd "C-c s") 'khoj)
;;  (global-set-key (kbd "C-c c") 'khoj-chat)
;;  )

;;; TODO VTERM

(after! vterm
  (setq vterm-max-scrollback 10000)

  ;; 빠른 원격 접속 함수
  (defun my/connect-storage ()
    "Connect to storage server"
    (interactive)
    (vterm)
    (vterm-send-string "mosh goqual@storage-01")
    (vterm-send-return))

  ;; 키바인딩
  (global-set-key (kbd "C-c -") 'my/connect-storage)
  ;; (setq x-gtk-use-native-input nil) ;; 2025-08-10 Important with ibus korean input

  ;; kime 환경변수 설정 (기존 코드 유지)
  (add-to-list 'vterm-environment "GTK_IM_MODULE=fcitx5")
  (add-to-list 'vterm-environment "QT_IM_MODULE=fcitx5")
  (add-to-list 'vterm-environment "XMODIFIERS=@im=fcitx5")

  (defun my/vterm-setup-terminal-font ()
    "Setup terminal font for vterm using fontaine"
    (when (and (eq major-mode 'vterm-mode)
               (featurep 'fontaine))
      (setq-local nobreak-char-display nil)
      (setq-local line-number-mode nil)
      (setq-local column-number-mode nil)
      (setq-local scroll-margin 3
                  line-spacing nil)
      (setq-local x-gtk-use-native-input t)
      ;; vterm의 default face에 터미널 폰트 적용
      (face-remap-add-relative 'default
                               :family (fontaine--get-preset-property
                                        fontaine-current-preset :term-family))))
  (add-hook 'vterm-mode-hook #'my/vterm-setup-terminal-font)
  )

;;; Load libraries via require (prevents duplicate loading)

(progn
  (add-to-list 'load-path (concat "~/repos/gh/doomemacs-config/lisp"))

  (require 'ui-config)
  (require 'evil-config)
  (require 'korean-input-config)
  (require 'time-config)
  (require 'completion-config)

  (require 'module-emacs-config)

  (require 'org-config)
  (require 'denote-config)
  (require 'denote-silo-config)
  (require 'denote-export-config)
  (require 'org-functions)
  (require 'denote-functions)
  (require 'unicode-config)
  (require 'editing-config)
  (require 'ai-gptel)
  (require 'ai-agent-shell)            ; acp 설정
  ;; (require 'ai-gptel-acp)           ; gptel + ACP 통합 (doom-md7)
  (require 'ai-stt-eca-whisper)
  (require 'ai-tts-edge)

  (require 'modeline-config)
  (require 'tab-bar-config)

  (require 'prog-mode-config)
  (require 'utils-config)
  (require 'project-config)
  (require 'eaf-config)                ; EAF (조건부 로딩)
  (require 'elfeed-config)             ; elfeed + elfeed-tube
  (require 'ai-orchestration)          ; efrit/beads (조건부 로딩)
  (require 'tmux-config)               ; tmux + claude code orchestration
  (require 'zellij-config)             ; zellij terminal multiplexer
  (require 'search-config)             ; recent-rgrep 등 검색 도구
  (require 'keybindings-config)
  (require 'keybindings-denote-config)
  (require 'termux-config)

  )

;;; left blank on purpose
