;;; lisp/uniconfig.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ❶ :: U+2776 ==> 더원싱 태그로 활용
;; ㉽ :: U+327D
;; ㉼ :: U+327C

;; 이맥스 배포판에 호환 하는 통합 설정
;; 목적 : 둠 스페이스맥스 등 관계 없이 패키지가 있다면
;; 여기 설정에 따라서 동작하도록 함.

;; 2024-04-20 일단 둠 이맥스 설정을 싹 비우는게 목표

;;; Configs

;;;; DONT custom scrolling

;; (setq
;;  ;; Do not adjust window-vscroll to view tall lines
;;  auto-window-vscroll nil ; doom nil
;;  ;; Keep the point in the same position while scrolling
;;  scroll-preserve-screen-position t ; doom t
;;  ;; Do not move cursor to the center when scrolling
;;  scroll-conservatively 10 ; doom 10
;;  ;; Scroll at a margin of one line
;;  ;; scroll-margin 1 ; doom 0
;;  )

;; (when (fboundp 'pixel-scroll-precision-mode)
;;   ;; Better scrolling on Emacs29+, specially on a touchpad
;;   (setq pixel-scroll-precision-use-momentum t) ; doom nil
;;   (pixel-scroll-precision-mode +1))

;;;; backtrace-mode-hook

(add-hook 'backtrace-mode-hook 'display-line-numbers-mode)
(add-hook 'backtrace-mode-hook 'visual-line-mode)

;;;; custom dabbrev

;; doomemacs-junghan0611/modules/completion/corfu/config.el
(progn
  (require 'dabbrev)
  (setq dabbrev-abbrev-char-regexp "[가-힣A-Za-z-_]")
  (setq dabbrev-ignored-buffer-regexps
        '("\\` "
          "\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
          "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-upcase-means-case-search nil) ; default t

  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  ;; (setq dabbrev-check-all-buffers t) ;; default t
  ;; (setq cape-dabbrev-check-other-buffers t) ; enable when dabbrev on init.el
  )

(with-eval-after-load 'cape
  ;; /gopar-dotfiles-youtuber/README.org:1371
  (setq cape-dabbrev-min-length 5) ; default 4
  (setq cape-dict-file user-dict-file)
  ;; (setq cape-dabbrev-check-other-buffers 'some)
  )

;;;; visual-line-mode

;; /home/junghan/sync/man/dotsamples/vanilla/localauthor-dotfiles-zk/init.el:119
;; (with-current-buffer "*Messages*"
;;   (visual-line-mode))
;; (with-current-buffer "*scratch*"
;;   (visual-line-mode))

(add-hook 'compilation-mode-hook 'visual-line-mode)
;; (add-hook 'fundamental-mode-hook 'visual-line-mode)

;;;; Xref

;; Denote 23.9. Speed up backlinks’ buffer creation?
;; Prefer ripgrep, then ugrep, and fall back to regular grep.

(setq xref-search-program
      (cond
       ((or (executable-find "ripgrep")
            (executable-find "rg"))
        'ripgrep)
       ((executable-find "ugrep")
        'ugrep)
       (t
        'grep)))

;; check below
;; (setq xref-file-name-display 'project-relative)
;; Use completing-read interface instead of definitions buffer (needs xref 1.1.0)
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; (setq xref-show-definitions-function #'consult-xref) ;; default xref-show-definitions-buffer

;;;; Hangul Korean

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
;; (setq default-transient-input-method "TeX") ; C-u set-input-method
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; 멀티바이트 모드 활성화 (필요시)
(set-buffer-multibyte t)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(set-clipboard-coding-system 'utf-8)

(setq-default line-spacing 3) ; use fontaine

(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_ALL" "ko_KR.UTF-8")

;; 날짜 표시를 영어로한다. org mode 에서 time stamp 날짜에 영향을 준다.
(setq system-time-locale "C")

(setq
 input-method-verbose-flag nil
 input-method-highlight-flag nil)

;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(global-set-key (kbd "<S-SPC>") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
(global-set-key (kbd "<menu>") 'toggle-input-method) ;; caps lock as <menu>
(add-hook 'context-menu-mode-hook '(lambda () (define-key context-menu-mode-map (kbd "<menu>") #'toggle-input-method)))
;; (global-unset-key (kbd "S-SPC"))

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

;;;; Emoji and Fonts with Hangul

(unless (string-equal system-type "android")
;;;###autoload
  (defun my/set-emoji-symbol-font ()
    (interactive)

    (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family)))

    (when (display-graphic-p) ; gui
      (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
      (set-fontset-font t 'mathematical (font-spec :family "Symbola") nil 'prepend) ; best
      (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
      (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top
      )
    (unless (display-graphic-p) ; terminal
      ;; 터미널에서는 Noto Color Emoji 사용 (컬러 이모지 지원시)
      (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil)
      ;; 터미널에서 폰트 스케일 조정 (이모지 크기 일정하게)
      (setq face-font-rescale-alist
            '(("Noto Color Emoji" . 0.9)
              ("Noto Emoji" . 0.9)
              ("Symbola" . 0.9)))

      ;; 이모지 문자의 너비를 2로 고정 (double-width)
      ;; 주요 이모지 범위들
      (dolist (range '((#x1F300 . #x1F6FF)  ; Misc Symbols and Pictographs
                       (#x1F700 . #x1F77F)  ; Alchemical Symbols
                       (#x1F780 . #x1F7FF)  ; Geometric Shapes Extended
                       (#x1F900 . #x1F9FF)  ; Supplemental Symbols and Pictographs
                       (#x1FA00 . #x1FA6F)  ; Chess Symbols
                       (#x1FA70 . #x1FAFF)  ; Symbols and Pictographs Extended-A
                       (#x2600 . #x26FF)    ; Miscellaneous Symbols
                       (#x2700 . #x27BF)    ; Dingbats
                       (#xFE00 . #xFE0F)    ; Variation Selectors
                       (#x1F000 . #x1F02F)  ; Mahjong Tiles
                       (#x1F030 . #x1F09F)  ; Domino Tiles
                       (#x1F0A0 . #x1F0FF))) ; Playing Cards
        (set-char-table-range char-width-table range 2))
      ;; 특정 이모지들을 유니코드 코드포인트로 너비 설정
      (dolist (codepoint '(#x1F600 #x1F603 #x1F604 #x1F601 #x1F606 #x1F605 #x1F602 #x1F923 #x1F60A #x1F607
                           #x1F642 #x1F643 #x1F609 #x1F60C #x1F60D #x1F970 #x1F618 #x1F617 #x1F619 #x1F61A
                           #x1F60B #x1F61B #x1F61C #x1F92A #x1F61D #x1F911 #x1F917 #x1F92D #x1F92B #x1F914
                           #x1F525 #x1F4AF #x2728 #x2B50 #x1F31F #x1F4AB #x1F308 #x2600 #x1F31E #x1F31D
                           #x2764 #x1F9E1 #x1F49B #x1F49A #x1F499 #x1F49C #x1F5A4 #x1F90D #x1F90E #x1F494
                           #x2705 #x274C #x2B55 #x1F534 #x1F7E0 #x1F7E1 #x1F7E2 #x1F535 #x1F7E3 #x26AB
                           #x26AA #x1F7E4 #x1F536 #x1F537 #x1F538 #x1F539 #x1F53A #x1F53B #x1F4A0 #x1F532))
        (set-char-table-range char-width-table codepoint 2)))

    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend))

  (add-hook 'after-setting-font-hook #'my/set-emoji-symbol-font))

;; (progn
;;   (defun my/set-fonts-scale (
;;                              ;; default-font-name
;;                              ;; default-font-height
;;                              ;; cjk-font-name
;;                              ;; cjk-font-scale
;;                              unicode-font-name
;;                              unicode-font-scale
;;                              emoji-font-name
;;                              emoji-font-scale
;;                              )
;;     "Helper function to set the default, CJK and Emoji fonts."
;;     ;; Set the default font
;;     ;; (when (member default-font-name (font-family-list))
;;     ;;   (set-face-attribute 'default nil
;;     ;;                       :family default-font-name
;;     ;;                       :height default-font-height)
;;     ;;   (set-frame-font default-font-name nil t))

;;     ;; Set the CJK font in the default fontset.
;;     ;; (when (member cjk-font-name (font-family-list))
;;     ;;   (dolist (script (list 'han 'kana 'cjk-misc))
;;     ;;     (set-fontset-font t script cjk-font-name)))

;;     (when (member unicode-font-name (font-family-list))
;;       (set-fontset-font t 'unicode unicode-font-name nil 'prepend)
;;       (set-fontset-font t 'mathematical unicode-font-name nil 'prepend)
;;       (set-fontset-font t 'symbol unicode-font-name nil 'prepend)
;;       )

;;     ;; Set the Emoji font in the default fontset.
;;     (when (member emoji-font-name (font-family-list))
;;       (set-fontset-font t 'emoji emoji-font-name nil 'prepend))

;;     ;; Rescale the CJK and emoji fonts.
;;     (setq face-font-rescale-alist
;;           `(;; (,(format ".*%s.*" cjk-font-name) . ,cjk-font-scale)
;;             (,(format ".*%s.*" unicode-font-name) . ,unicode-font-scale)
;;             (,(format ".*%s.*" emoji-font-name) . ,emoji-font-scale)
;;             )))

;; ;;;###autoload
;;   (defun my/emoji-set-font ()
;;     (interactive)

;;     (when (display-graphic-p) ; gui
;;       ;; (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
;;       ;; (set-fontset-font t 'mathematical (font-spec :family "Symbola") nil 'prepend) ; best
;;       ;; ;; https://fonts.google.com/noto/specimen/Noto+Sans+Math
;;       ;; ;; (set-fontset-font t 'mathematical (font-spec :family "Noto Sans Math") nil 'prepend) ;; 2024-10-26 테스트 DONT
;;       ;; ;; (set-fontset-font t 'mathematical "DejaVu Math TeX Gyre" nil 'prepend) ; DONT for test
;;       ;; (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
;;       (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top - my choice

;;       ;; Noto Emoji, Noto Color Emoji,
;;       ;; Different computers might need different scaling factors with the same fonts.
;;       (my/set-fonts-scale
;;        "Symbola" 0.90 ; unicode
;;        "Noto Color Emoji" 0.95)

;;       (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family))) ; default face
;;       (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'variable-pitch :family)) nil 'append) ; important 2025-03-14 가변 한글 필수!
;;       )

;;     (unless (display-graphic-p) ; terminal
;;       ;; 터미널에서 폰트 스케일 조정 (이모지 크기 일정하게)
;;       (setq face-font-rescale-alist
;;             '(("Noto Color Emoji" . 0.9)
;;               ("Noto Emoji" . 0.9)
;;               ("Symbola" . 0.9)))
;;       ;; 터미널에서는 Noto Color Emoji 사용 (컬러 이모지 지원시)
;;       (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil)
;;       (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Color Emoji") 'append)
;;       ;; 폴백 폰트 설정 (Noto Emoji가 없는 경우)
;;       (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans Mono") nil 'append)
;;       ;; 이모지 문자의 너비를 2로 고정 (double-width)
;;       ;; 주요 이모지 범위들
;;       (dolist (range '((#x1F300 . #x1F6FF)  ; Misc Symbols and Pictographs
;;                        (#x1F700 . #x1F77F)  ; Alchemical Symbols
;;                        (#x1F780 . #x1F7FF)  ; Geometric Shapes Extended
;;                        (#x1F900 . #x1F9FF)  ; Supplemental Symbols and Pictographs
;;                        (#x1FA00 . #x1FA6F)  ; Chess Symbols
;;                        (#x1FA70 . #x1FAFF)  ; Symbols and Pictographs Extended-A
;;                        (#x2600 . #x26FF)    ; Miscellaneous Symbols
;;                        (#x2700 . #x27BF)    ; Dingbats
;;                        (#xFE00 . #xFE0F)    ; Variation Selectors
;;                        (#x1F000 . #x1F02F)  ; Mahjong Tiles
;;                        (#x1F030 . #x1F09F)  ; Domino Tiles
;;                        (#x1F0A0 . #x1F0FF))) ; Playing Cards
;;         (set-char-table-range char-width-table range 2))
;;       ;; 특정 이모지들을 유니코드 코드포인트로 너비 설정
;;       (dolist (codepoint '(#x1F600 #x1F603 #x1F604 #x1F601 #x1F606 #x1F605 #x1F602 #x1F923 #x1F60A #x1F607
;;                            #x1F642 #x1F643 #x1F609 #x1F60C #x1F60D #x1F970 #x1F618 #x1F617 #x1F619 #x1F61A
;;                            #x1F60B #x1F61B #x1F61C #x1F92A #x1F61D #x1F911 #x1F917 #x1F92D #x1F92B #x1F914
;;                            #x1F525 #x1F4AF #x2728 #x2B50 #x1F31F #x1F4AB #x1F308 #x2600 #x1F31E #x1F31D
;;                            #x2764 #x1F9E1 #x1F49B #x1F49A #x1F499 #x1F49C #x1F5A4 #x1F90D #x1F90E #x1F494
;;                            #x2705 #x274C #x2B55 #x1F534 #x1F7E0 #x1F7E1 #x1F7E2 #x1F535 #x1F7E3 #x26AB
;;                            #x26AA #x1F7E4 #x1F536 #x1F537 #x1F538 #x1F539 #x1F53A #x1F53B #x1F4A0 #x1F532))
;;         (set-char-table-range char-width-table codepoint 2)))

;;     (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
;;     (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
;;     (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend))

;;   ;; (my/emoji-set-font)
;;   (add-hook 'after-setting-font-hook #'my/emoji-set-font)
;;   )


;;;; show-paren-delay 0

;; 괄호 강조를 즉시 보여준다
(setq show-paren-delay 0) ; 0.125

;;;; jit-lock-defer-time 0

;; NOTE: setting this to `0' like it was recommended in the article above seems
;; to cause fontification to happen in real time, which can be pretty slow in
;; large buffers. Giving it a delay seems to be better.
;; (setq jit-lock-defer-time 0.05) ;; better
(setq jit-lock-defer-time 0) ; important

;; My guess for how big this number should be for my setup. Call
;; `cae-set-jit-lock-chunk-size-to-optimal' on a few different files to get an idea.
(setq jit-lock-chunk-size 2500) ; default 1500

;;;; prog-mode-hooks

(progn
  ;; Color the string of whatever color code they are holding
  (add-hook 'prog-mode-hook 'rainbow-mode) ; 2023-11-23 on
  ;; (add-hook 'prog-mode-hook 'prettify-symbols-mode)

  ;; (ws-butler-keep-whitespace-before-point nil)
  ;; (ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode diff-mode markdown-mode))
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'prog-mode-hook 'visual-line-mode) ; 2024-10-24
  )

;;;; make Script Files Executable Automatically

;; Make script files (with shebang like #!/bin/bash, #!/bin/sh) executable automatically. See this blog post from Emacs Redux.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Terminal

;;;; Ghostty Integration

;; 터미널에서 테마 색상 충돌 방지
(unless (display-graphic-p)
  ;; 터미널에서 배경색 투명도 유지
  (setq-default frame-background-mode 'dark)

  ;; Ghostty 터미널 전용 설정
  (cond
   ;; xterm-ghostty terminfo 사용시
   ((string-match "ghostty" (or (getenv "TERM") ""))
    ;; Ghostty는 24비트 트루컬러 지원 (이미 terminfo에 정의됨)
    (setenv "COLORTERM" "truecolor")
    ;; 배경 투명도 유지
    (set-face-background 'default "unspecified-bg" nil)
    ;; 터미널 자체 색상 테마 우선
    (setq-default terminal-ansi-color-vector
                  [unspecified "#282a36" "#ff5555" "#50fa7b" "#f1fa8c"
                               "#6272a4" "#ff79c6" "#8be9fd" "#f8f8f2"])
    ;; Ghostty는 256색상 이상 지원 (terminfo pairs=0x7fff)
    (setq xterm-color-use-bold-for-bright nil)
    (add-to-list 'term-file-aliases '(("xterm-ghostty" . "xterm-direct")))
    )

   ;; 일반 256color 터미널
   ((string-match "256color" (or (getenv "TERM") ""))
    (setq xterm-color-names-bright
          ["#3B4252" "#BF616A" "#A3BE8C" "#EBCB8B"
           "#81A1C1" "#B48EAD" "#88C0D0" "#E5E9F0"])
    (setq xterm-color-names
          ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B"
           "#81A1C1" "#B48EAD" "#88C0D0" "#D8DEE9"]))))

;;; functions

;;;; Corfu and electric-Pair and Jump In/Out Parens

;; Linux GUI : <tab> TAB
;; Linux Terminal : TAB
;; Linux GUI : S-<iso-lefttab>
;; Linux Terminal : <backtab>

(progn
;;;###autoload
  (defun jump-out-of-pair ()
    (interactive)
    (let ((found (search-forward-regexp "[])}\"'`*=]" nil t)))
      (when found
        (cond
         ((or (looking-back "\\*\\*" 2)
              (looking-back "``" 2)
              (looking-back "\"\"" 2) ; 2023-10-02 added
              (looking-back "''" 2) (looking-back "==" 2))
          (forward-char))
         (t
          (forward-char 0))))))
  ;; 절대 하지 말것! (global-set-key [remap indent-for-tab-command] #'jump-out-of-pair)

;;;###autoload
  (defun jump-backward-pair ()
    (interactive)
    (let ((found (search-backward-regexp "[])}\"'`*=]" nil t)))
      (when found
        (cond
         ((or (looking-back "\\*\\*" 2)
              (looking-back "``" 2)
              (looking-back "\"\"" 2) ; 2023-10-02 added
              (looking-back "''" 2) (looking-back "==" 2))
          (backward-char))
         (t
          (backward-char 0)))))))

;;;; my/backward-kill-word-or-region

;; no kill-ring propagate

;;;###autoload
(defun my/delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

;;;###autoload
(defun my/backward-delete-word-or-region (&optional arg)
  "Calls `delete-region' when a region is active and doesn't affect the kill-ring"
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'delete-region)
    (my/delete-backward-word arg)))

;;;; DONT my/write-window-setup

;; Writing Window Setup on Dired
;; 괜찮다. 화면 버퍼 구성이 여러모로 집중하기 좋다.
;; (defun my/write-window-setup ()
;;   (interactive)
;;   (split-window-right)
;;   (windmove-right)
;;   (split-window-below)
;;   (windmove-left)
;;   (find-file "*draft.org" t)
;;   (windmove-right)
;;   (find-file "*notes.txt" t) ; txt
;;   (windmove-left))
;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map [f3] #'my/write-window-setup))


;;;; my/convert-hangul-jamo-to-syllable

(progn
  (require 'ucs-normalize)

  (defun my/convert-hangul-jamo-to-syllable ()
    "Convert conjoining jamo to precomposed syllables in the current buffer."
    (interactive)
    (let* ((choseong "[\u1100-\u115F\uA960-\uA97C]")
           (jungseong "[\u1160-\u11A7\uD7B0-\uD7C6]")
           (jongseong "[\u11A8-\u11FF\uD7CB-\uD7FB]?")
           (pattern (concat choseong jungseong jongseong)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (ucs-normalize-NFC-region start end))))))

  ;; 버퍼 전체에 적용하려면 다음 함수를 호출하세요:
  ;; (my/convert-hangul-jamo-to-syllable)

  (defun my/process-files-by-extension (directory extension process-func)
    "주어진 디렉토리 에서 특정 확장자를 가진 파일들에 대해 처리 함수를 적용합니다."
    (interactive
     (list (read-directory-name "처리할 디렉토리: ")
           (read-string "파일 확장자 (예: txt): ")
           (intern (completing-read "처리 함수: " obarray 'functionp t))))
    (dolist (file (directory-files-recursively directory (concat "\\." extension "$")))
      (with-current-buffer (find-file-noselect file)
        (funcall process-func)
        (save-buffer)
        (kill-buffer)))))


;;;; my/replace-latex-delimiters-with-dollar

(defun my/replace-latex-delimiters-with-dollar ()
  "현재 버퍼 에서 \\[와 \\]를 $로 바꿉니다."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\\[" nil t)
    (replace-match "$$" nil nil))
  (goto-char (point-min))
  (while (search-forward "\\]" nil t)
    (replace-match "$$" nil nil)))

;;; kmacro

;;;; mirror-buffer
(defalias 'ash/mirror-buffer
  (kmacro "C-x 1 C-x 3 C-x o"))
;; (general-define-key "s-'" 'ash/mirror-buffer)

;;; Utils
;;;; text-font-scale-up

(defun my/text-font-scale-up ()
  (interactive)
  (text-scale-increase 1))
(defun my/text-font-scale-down ()
  (interactive)
  (text-scale-decrease 1))

;; TODO Evil Keymaap
;; (global-set-key (kbd "C-=") 'my/text-font-scale-up)
;; (global-set-key (kbd "C--") 'my/text-font-scale-down)
;;;; clear-kill-ring

(defun clear-kill-ring ()
  "Clear the results on the kill ring."
  (interactive) (setq kill-ring nil))
(global-set-key (kbd "M-Y") 'clear-kill-ring)

;;;; disable enable easy ui

(defun disable-easy-ui ()
  (interactive)
  (setq use-dialog-box nil)

  ;; (blink-cursor-mode nil)

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'tooltip-mode)
    (tooltip-mode -1))
  (menu-bar-mode -1))

;; On Emacs without X, tool-bar-mode and scroll-bar-mode are not defined.
(defun enable-easy-ui ()
  (interactive)
  (setq tool-bar-style 'image)
  (setq use-dialog-box t) ; default t
  (blink-cursor-mode t)
  (when (fboundp 'tooltip-mode)
    (tooltip-mode 1))
  (menu-bar-mode 1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 1))
  )

;;;; goto-addr

;; Actionable URLs in Emacs buffers via [[http://xenodium.com/#actionable-urls-in-emacs-buffers][Álvaro Ramírez]].

(progn
  (require 'goto-addr)
  ;; :hook
  ;; ((compilation-mode . goto-address-mode)
  ;;  (prog-mode . goto-address-prog-mode)
  ;;  (eshell-mode . goto-address-mode)
  ;;  (shell-mode . goto-address-mode))
  ;; :bind (:map goto-address-highlight-keymap ("C-c C-o" . goto-address-at-point))
  ;; :config
  (global-goto-address-mode +1))

;;;; eldoc

(progn
  (require 'eldoc)
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p nil) ;  important - default 'truncate-sym-name-if-fit
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer t)) ; default nil - alway show echo-area

;; eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer)


;;;###autoload
(defun eldoc-toggle ()
  "Toggle eldoc's documentation buffer."
  (interactive)
  (let ((buffer (eldoc-doc-buffer)))
    (if-let (w (and buffer (get-buffer-window buffer)))
        (delete-window w)
      (eldoc-doc-buffer t))))

;;;; Run commands in a popup frame

;; i3config : bindsym $mod+$wm.binding.orgcapture exec --no-startup-id emacsclient -e '(progn (select-frame-set-input-focus (selected-frame)) (prot-window-popup-org-capture))'
;; myai.sh : emacsclient -e '(progn (select-frame-set-input-focus (selected-frame)) (ad/ai-from-anywhere))'

;;;;; prot's emacs-command-popup-frame

;; https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/

;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)
;; The emacsclient calls that need ot be bound to system-wide keys
;; emacsclient -e '(prot-window-popup-org-capture)'
;; emacsclient -e '(prot-window-popup-tmr)'

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture)

(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

(when (locate-library "tmr")
  (require 'tmr)
  (declare-function tmr "tmr" (time &optional description acknowledgep))
  (defvar tmr-timer-created-functions)

;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
  (prot-window-define-with-popup-frame tmr)
  (add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame))

;;;;; gptel: ai-from-anywhare

;; https://www.armindarvish.com/post/use_emacs_as_a_chatgpt_client/
(when (locate-library "gptel")

;;;###autoload
  (defun my/ai-from-anywhere ()
    (interactive)
    (let* ((screen-width (display-pixel-width))
           (screen-height (display-pixel-height))
           (frame-width (/ screen-width 3))
           (frame-height screen-height)
           (frame-left (- screen-width frame-width))
           (frame-top 0)
           (chat-frame (make-frame `(
                                     ;; (window-system . x) ; macOS use "ns"
                                     (top . ,frame-top)
                                     (left . ,frame-left)
                                     (width . (text-pixels . ,frame-width))
                                     (heigth . (text-pixels . ,frame-height))
                                     (minibuffer . t)))))
      (select-frame chat-frame))

    (gptel "My:AI Chat" gptel-api-key nil)
    (switch-to-buffer "My:AI Chat")
    (delete-other-windows))
  )

;;;;; use gptel citations utf8

(defun my/url-decode-to-korean (url)
  "URL 을 디코딩하고 한글로 표시합니다."
  (decode-coding-string (url-unhex-string url t) 'utf-8))

;; (when (locate-library "gptel")
;;   (with-eval-after-load 'gptel
;;     (require 'url)
;;     (fmakunbound 'gptel--perplexity-parse-citations)
;;     (defun gptel--perplexity-parse-citations (citations)
;;       (let ((counter 0))
;;         (concat "\n\nCitations:\n"
;;                 (mapconcat (lambda (url)
;;                              (setq counter (1+ counter))
;;                              (format "- [%d] %s" counter
;;                                      (decode-coding-string (url-unhex-string url t) 'utf-8)
;;                                      ;; (url-decode-to-korean url)
;;                                      ))
;;                            citations "\n"))))
;;     )
;;   )

;;; Packages with functions
;;;; buffer management

;; check ibuffer-list
(defun my/kill-all-buffers-except-toolbox ()
  "현재 버퍼와 기본 도구(*Messages*, *scratch*)를 제외한 모든 버퍼를 제거합니다."
  (interactive)
  (mapc 'kill-buffer
        (remove-if
         (lambda (x)
           (or
            (eq x (current-buffer))
            (member (buffer-name x) '("*doom:scratch*" "*Messages*" "*scratch*"))))
         (buffer-list)))
  (delete-other-windows))

;;;; consult - swiper style check wiki

;; https://github.com/minad/consult/wiki
;; (progn
;;   (defcustom my/consult-ripgrep-or-line-limit 300000
;;     "Buffer size threshold for `my/consult-ripgrep-or-line'.
;; When the number of characters in a buffer exceeds this threshold,
;; `consult-ripgrep' will be used instead of `consult-line'."
;;     :type 'integer)

;;   (defun my/consult-ripgrep-or-line ()
;;     "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
;;     (interactive)
;;     (if (or (not buffer-file-name)
;;             (buffer-narrowed-p)
;;             (ignore-errors
;;               (file-remote-p buffer-file-name))
;;             (jka-compr-get-compression-info buffer-file-name)
;;             (<= (buffer-size)
;;                 (/ my/consult-ripgrep-or-line-limit
;;                    (if (eq major-mode 'org-mode) 4 1))))
;;         (consult-line)
;;       (when (file-writable-p buffer-file-name)
;;         (save-buffer))
;;       (let ((consult-ripgrep-args
;;              (concat consult-ripgrep-args
;;                      ;; filter to desired filename
;;                      " -g "
;;                      (shell-quote-argument (file-name-nondirectory buffer-file-name))
;;                      " ")))
;;         (consult-ripgrep))))
;;   )


;;;; consult-denote and howmish

;; (let ((tab (make-hash-table :test 'equal)))
;;   (puthash "Foo" "foo.org" tab)
;;   (gethash (completing-read "Yes: "  tab) tab))

(when (locate-library "consult-denote")
  (progn
    ;; https://systemcrafters.net/live-streams/march-7-2025/
    ;; - Searching for notes in most-recent-edit order
    ;; - Searching for notes from today, yesterday, specific date
    ;; - Following search query links

    ;; (consult-denote-mode +1)

    (defun my/denote-howmish-find-file ()
      (declare (interactive-only t))
      (interactive)
      (let* ((sorted-files (sort (mapcar (lambda (file)
                                           (cons (file-attribute-modification-time
                                                  (file-attributes file))
                                                 file))
                                         (denote-directory-files))
                                 (lambda (left right)
                                   (not
                                    (time-less-p (car left)
                                                 (car right))))))
             (table (make-hash-table :test 'equal))
             (options
              (mapcar (lambda (file)
                        (puthash (denote-retrieve-title-or-filename (cdr file) 'org)
                                 (cdr file)
                                 table))
                      sorted-files))
             (result
              (consult--read table
                             :prompt "Note: "
                             :sort nil
                             :require-match t
                             :add-history (thing-at-point 'filename)
                             :state (consult--file-preview)
                             :category 'file
                             :history '(:input consult--find-history))))
        (when result
          (find-file (gethash result table)))))
    )
  )

;;;; DONT fontaine

;; ;; This is defined in Emacs C code: it belongs to font settings.
;; (setq x-underline-at-descent-line t) ; default nil
;; ;; And this is for Emacs28.
(setq-default text-scale-remap-header-line t) ; default nil

;; Weights :: Thin ExtraLight Light Regular Medium SemiBold Bold ExtraBold Heavy
;; Slopes :: Upright Oblique Italic
;; Width :: Normal Extended

;; (when (locate-library "fontaine")
;;   (when (display-graphic-p) ; gui
;;     ;; (setq fontaine-latest-state-file
;;     ;;       (locate-user-emacs-file "fontaine-latest-state.eld"))
;;     (setq fontaine-presets
;;           ;; 80 120, 136, 151, 180, 211 ; sarasa mono / term
;;           ;; 120, 140, 170, 190, 210, 230 ; monoplex kr nerd
;;           '(
;;             (demo14 :variable-pitch-family "Hahmlet")
;;             (small12 :default-height 120)
;;             (regular :default-height 140)
;;             (regular14 :default-height 140)
;;             (regular17 :default-height 170
;;                        :variable-pitch-height 1.0)
;;             (logosfocus :default-height 170)
;;             (large19 :default-height 190)
;;             (large21 :default-height 210)
;;             (present23
;;              :default-height 230
;;              ;; :fixed-pitch-family "Sarasa Term Slab K"
;;              ;; :fixed-pitch-serif-family "Sarasa Term Slab K"
;;              :bold-weight extrabold)
;;             (t
;;              ;; Following Prot’s example, keeping these for for didactic purposes.
;;              :line-spacing 3
;;              ;; :default-family "Sarasa Term K Nerd Font"
;;              ;; :default-height 151
;;              :default-family "Monoplex Nerd"
;;              :default-height 140
;;              :default-weight regular
;;              ;; :fixed-pitch-family "Sarasa Term K Nerd Font"
;;              ;; :fixed-pitch-height 151
;;              ;; :fixed-pitch-weight nil
;;              ;; :fixed-piath-serif-family nil
;;              ;; :fixed-pitch-serif-weight nil
;;              ;; :fixed-pitch-serif-height nil
;;              ;; :variable-pitch-family "Pretendard Variable"
;;              :variable-pitch-family "Hahmlet"
;;              :variable-pitch-height 1.0
;;              ;; :variable-pitch-family nil
;;              ;; :variable-pitch-weight nil
;;              :bold-family nil
;;              :bold-weight bold
;;              ;; :bold-width extended
;;              :italic-family nil
;;              :italic-slant italic)))

;;     ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular14))
;;     (fontaine-set-preset 'regular)

;;     ;; 한글 사용 위해서 필수!
;;     (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family))) ; default face
;;     (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'variable-pitch :family)  nil 'append)) ; for variable-pitch
;;     )

;;   ;; Persist the latest font preset when closing/starting Emacs and while switching between themes.
;;   (fontaine-mode 1)
;;   (add-hook 'fontaine-mode-hook (lambda ()
;;                                   (display-time-mode +1))) ; for tab-bar update
;;   )

;;;; Ten with etags

;; gavinok-dotfiles/init.el
;; Getting added in emacs 30 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67687
;; (use-package etags-regen
;;   :when (executable-find "etags")
;;   :custom (etags-regen-tags-file "/tmp/TAGS")
;;   :commands etags-regen-mode
;;   :bind (("C-c t" . complete-tag)
;;          ("C-c M-." . my/goto-etags))
;;   :init
;;   (defvar etags-regen-mode-map (make-sparse-keymap))
;;   (add-to-list 'minor-mode-map-alist (cons 'etags-regen-mode etags-regen-mode-map)))

;; (defun my/goto-etags ()
;;   (interactive)
;;   (let ((xref-backend-functions '(etags--xref-backend t)))
;;     (call-interactively 'xref-find-definitions)))

;; ;; eww-mode nov-mode -- conflict face 켜면 안된다.

(when (locate-library "ten")
  (require 'ten)
  (setq ten-glossary-file-extensions '("org" "md" "txt"))
  (setq ten-glossary-exclude-regexps '("/\\."))
  (setq ten-tags-file-default user-ten-tags-file)
  ;;   ;; :bind (("M-c t" . complete-tag)
  ;;   ;;        ("C-c M-." . my/goto-etags))
  (add-hook 'org-mode-hook 'ten-font-lock-mode) ;; never! for all text-mode
  (add-hook 'Info-mode-hook 'ten-font-lock-mode)
  (with-eval-after-load 'consult
    (require 'consult-ten)
    (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
    )
  )

;;;; markdown-mode

(with-eval-after-load 'markdown-mode
  (setq markdown-hide-urls nil) ; must
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-display-remote-images t)
  (setq markdown-list-item-bullets '("◦" "-" "•" "–"))

  ;; (advice-add
  ;;  'markdown-fontify-list-items :override
  ;;  (lambda (last)
  ;;    (when (markdown-match-list-items last)
  ;;      (when (not (markdown-code-block-at-point-p (match-beginning 2)))
  ;;        (let* ((indent (length (match-string-no-properties 1)))
  ;;               (level (/ indent markdown-list-indent-width))
  ;;               ;; level = 0, 1, 2, ...
  ;;               (bullet (nth (mod level (length markdown-list-item-bullets))
  ;;                            markdown-list-item-bullets)))
  ;;          (add-text-properties
  ;;           (match-beginning 2) (match-end 2) '(face markdown-list-face))
  ;;          (cond
  ;;           ;; Unordered lists
  ;;           ((string-match-p "[\\*\\+-]" (match-string 2))
  ;;            (add-text-properties
  ;;             (match-beginning 2) (match-end 2) `(display ,bullet)))
  ;;           ;; Definition lists
  ;;           ((string-equal ":" (match-string 2))
  ;;            (let ((display-string
  ;;                   (char-to-string (markdown--first-displayable
  ;;                                    markdown-definition-display-char))))
  ;;              (add-text-properties (match-beginning 2) (match-end 2)
  ;;                                   `(display ,display-string)))))))
  ;;      t)))

  (add-hook 'markdown-mode-hook #'visual-line-mode)
  ;; (add-hook 'markdown-mode-hook #'toggle-text-mode-auto-fill)

  ;; (add-hook
  ;;  'markdown-mode-hook
  ;;  (lambda ()
  ;;    "Beautify Markdown em-dash and checkbox Symbol"
  ;;    ;; (push '("--" . "—") prettify-symbols-alist)
  ;;    (push '("->" . "→" ) prettify-symbols-alist)
  ;;    (push '("=>" . "⟹") prettify-symbols-alist)
  ;;    (prettify-symbols-mode)))

  ;; Plain text (text-mode)
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))

  (progn
    (define-key markdown-mode-map (kbd "<f3>") 'markdown-toggle-markup-hiding)
    (define-key markdown-mode-map (kbd "<f4>") 'markdown-toggle-inline-images)

    (evil-define-key '(insert) markdown-mode-map (kbd "C-u") 'undo-fu-only-undo)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-r") 'undo-fu-only-redo)

    (evil-define-key '(insert normal visual) markdown-mode-map (kbd "C-<up>") 'markdown-backward-paragraph) ;; same as org-mode
    (evil-define-key '(insert normal visual) markdown-mode-map (kbd "C-<down>") 'markdown-forward-paragraph)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-n") 'markdown-outline-next)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-p") 'markdown-outline-previous)

    (evil-define-key '(insert) markdown-mode-map (kbd "C-n") 'next-line)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-p") 'previous-line)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-j") 'markdown-outline-next-same-level)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-k") 'markdown-outline-previous-same-level)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-j") 'markdown-outline-next-same-level)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-k") 'markdown-outline-previous-same-level)
    (evil-define-key '(normal visual) evil-markdown-mode-map (kbd "M-j") 'markdown-outline-next-same-level)
    (evil-define-key '(normal visual) evil-markdown-mode-map (kbd "M-k") 'markdown-outline-previous-same-level)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-n") 'markdown-outline-next) ;; markdown-mode-down
    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-p") 'markdown-outline-previous)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-S-p") 'markdown-up-heading)
    (evil-define-key '(normal visual) markdown-mode-map "zu" 'markdown-up-heading) ;; same with evil-collection outline
    )
  )

;; (when (locate-library "markdown-mode")
;;   (progn
;;     ;; https://www.reddit.com/r/emacs/comments/10h9jf0/beautify_markdown_on_emacs/

;;     (defvar nb/current-line '(0 . 0)
;;       "(start . end) of current line in current buffer")
;;     (make-variable-buffer-local 'nb/current-line)

;;     (defun nb/unhide-current-line (limit)
;;       "Font-lock function"
;;       (let ((start (max (point) (car nb/current-line)))
;;             (end (min limit (cdr nb/current-line))))
;;         (when (< start end)
;;           (remove-text-properties start end
;;                                   '(invisible t display "" composition ""))
;;           (goto-char limit)
;;           t)))

;;     (defun nb/refontify-on-linemove ()
;;       "Post-command-hook"
;;       (let* ((start (line-beginning-position))
;;              (end (line-beginning-position 2))
;;              (needs-update (not (equal start (car nb/current-line)))))
;;         (setq nb/current-line (cons start end))
;;         (when needs-update
;;           (font-lock-fontify-block 3))))

;;     (defun nb/markdown-unhighlight ()
;;       "Enable markdown concealling"
;;       (interactive)
;;       (markdown-toggle-markup-hiding 'toggle)
;;       (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
;;       (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

;;     (add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)
;;     )
;;   )

;;;; org-rainbow-tags

(when (locate-library "org-rainbow-tags")
  (with-eval-after-load 'org
    (require 'org-rainbow-tags)
    ;; (setq org-rainbow-tags-hash-start-index 8)
    (setq org-rainbow-tags-extra-face-attributes
          '(:inverse-video t :box t :weight 'bold :height user-imenu-list-height))
    ;; (add-hook 'org-mode-hook #'org-rainbow-tags-mode)
    ))

;;;; oneko-macs eyecandy

(when (locate-library "oneko-macs")
  (require 'oneko-macs))

;;;; ob-mermaid

;; (when (locate-library "ob-mermaid")
;;   (with-eval-after-load 'org
;;     (require 'ob-mermaid)
;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      (append org-babel-load-languages '((mermaid . t))))
;;     ))

;; (use-package! ob-d2
;;   :after org
;;   :defer 3
;;   :config
;;   (add-to-list 'org-babel-load-languages '(d2 . t)))

;;;; goto-last-change

(when (locate-library "goto-last-change")
  (defun my/goto-last-change ()
    (interactive)
    (outline-show-all) ; 전체를 펼치고 찾아라!
    (goto-last-change))
  (global-set-key (kbd "C-x ,") 'my/goto-last-change))

;;;; side-notes

(when (locate-library "side-notes")
  (require 'side-notes)
  (setq side-notes-display-alist '((side . right) (window-width . 40))) ; 84
  (add-hook 'side-notes-hook #'visual-line-mode)
  )

;;;; custom citar-org

(when (locate-library "citar")
  (with-eval-after-load 'citar
;;;;; citar-templates
    (setq citar-templates
          '((main
             .
             ;; [${urldate:10}]
             "'${dateadded:10} ${author editor:19} ${title:49} ${date year issued:4} ${translator:7} №${=key= id:17}")  ; 2024-09-12 김정한
            (suffix
             . "#${datemodified:10} ${=type=:10} ${shorttitle:19} ${namea:16} ${url:19} ${tags keywords:*}") ; 2024-11-17 add url
            (preview
             .
             "*** ${title}\n${shorttitle}\n${author} ${translator} ${namea} ${year issued date:4}\n\n${abstract}\n") ; citar-copy-reference
            (note . "#+title: ${author translator:10}, ${title}")))

    ;; (note . "Notes on ${author:10 editor:%etal}, ${title}")

;;;;; my/citar-org-to-reading-list

    (progn
      (require 'ol-bibtex)

      ;; use embark with at-point
      ;; (setq citar-at-point-function 'embark-act) ; citar-dwim
      ;; add beref entry for bookends
      ;; (setq citar-additional-fields '("url"))

      (defun my/citar-org-to-reading-list (citekeys)
        "Insert bibliographic entry associated with the CITEKEYS."
        (interactive (list (citar-select-refs)))
        (dolist (citekey citekeys)
          (my/citar--org-to-reading-list
           citekey)))

      (defun my/citar--org-to-reading-list (citekey)
        "Insert the bibtex entry for CITEKEY at point."

        (let* ((key citekey)
               (reading-list-file (my/org-reading-file)))
          (when key
            (with-temp-buffer
              ;; BibTeX 엔트리를 버퍼에 삽입
              (citar--insert-bibtex citekey)
              ;; org-bibtex 형식 으로 변환
              (org-bibtex-read)
              ;; 변환된 내용을 reading list 파일에 추가
              (with-current-buffer (find-file-noselect reading-list-file)
                (goto-char (point-max))
                (insert "\n")
                (org-bibtex-write)
                (insert "\n")
                (insert (format "[cite:@%s]" citekey))
                (insert "\n")

                (save-buffer)))
            (message "Entry added to reading list: %s" key))))
      )

;;;;; citar-indicator icons

    ;; (progn
    ;;   (defvar citar-indicator-files-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-faicon
    ;;               "nf-fa-file_o"
    ;;               :face 'nerd-icons-green
    ;;               :v-adjust 0.01)
    ;;      :function #'citar-has-files
    ;;      :padding "  " ; need this because the default padding is too low for these icons
    ;;      :tag "has:files"))
    ;;   (defvar citar-indicator-links-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-faicon
    ;;               "nf-fa-link"
    ;;               :face 'nerd-icons-orange
    ;;               :v-adjust 0.0)
    ;;      :function #'citar-has-links
    ;;      :padding "  "
    ;;      :tag "has:links"))
    ;;   (defvar citar-indicator-notes-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-codicon
    ;;               "nf-cod-note"
    ;;               :face 'nerd-icons-blue
    ;;               :v-adjust 0.0)
    ;;      :function #'citar-has-notes
    ;;      :padding "    "
    ;;      :tag "has:notes"))
    ;;   (defvar citar-indicator-cited-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-faicon
    ;;               "nf-fa-circle_o"
    ;;               :face 'nerd-icon-green
    ;;               :v-adjust 0.0)
    ;;      :function #'citar-is-cited
    ;;      :padding "  "
    ;;      :tag "is:cited"))
    ;;   (setq citar-indicators
    ;;         (list citar-indicator-files-icons
    ;;               citar-indicator-links-icons
    ;;               citar-indicator-notes-icons
    ;;               citar-indicator-cited-icons)))

    )
  )

;;;; ox-hugo

;; 2024-04-26 move here, Important
(with-eval-after-load 'ox-hugo
  ;; (setq org-hugo-base-dir (file-truename "~/git/blog/"))
  (setq org-hugo-base-dir user-hugo-notes-dir) ;; 2024-10-07 fix quartz
  (setq org-hugo-export-with-toc nil) ; default nil

  (progn
    ;; Append and update time-stamps for
    ;; #+hugo_lastmod: Time-stamp: <>
    ;; org-hugo-auto-set-lastmod should be nil
    (require 'time-stamp)
    ;; (add-hook 'write-file-functions 'time-stamp)
    ;; M-x time-stamp
    ;; Update last modified date for ox-hugo export
    ;; (add-hook 'before-save-hook 'time-stamp)
    (setq time-stamp-active t
          time-stamp-start "#\\+hugo_lastmod:[ \t]*"
          time-stamp-end "$"
          time-stamp-format "\[%Y-%m-%d\]"))

  (setq org-hugo-front-matter-format 'yaml)

  ;; My blog is created using Hugo and ox-hugo. It generates better markdown than what you would get using org-md-export!
  ;; It works well out-of-the-box. However, extra configuration is required to embed video.
  ;; In ox-hugo, uses #+begin_video to generate the <video> HTML5 tag (details in ox-hugo/issues/274).
  ;; In Hugo config, set markup.goldmark.renderer.unsafe to true (details in discourse.gohugo.io).
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webm")

  (setq org-hugo-section "notes") ; 2024-04-26 change
  (setq org-hugo-paired-shortcodes
        "mermaid callout cards details tabs sidenote") ; hint

  ;; https://ox-hugo.scripter.co/doc/formatting/
  ;; if org-hugo-use-code-for-kbd is non-nil
  ;; Requires CSS to render the <kbd> tag as something special.
  ;; eg: ~kbd~
  (setq org-hugo-use-code-for-kbd t)

  ;; https://ox-hugo.scripter.co/doc/linking-numbered-elements/

  ;; org-export-dictionary 에 Figure, Table 에 한글 번역을 넣으면
  ;; 한글로 바뀌어 export 될 것이다.
  ;; (setq org-hugo-link-desc-insert-type t)

  ;; 내보낼 때는 fill-column 끈다.
  ;; 개행문자는 조직모드 에서 \\ 를 뒤에 넣으라
  (setq org-hugo-preserve-filling nil) ; important
  (setq org-hugo-delete-trailing-ws nil) ; default t for quartz

  (setq org-hugo-allow-spaces-in-tags t) ; default t
  (setq org-hugo-prefer-hyphen-in-tags t) ; default t

  ;; Assume all static files are images for now otherwise this
  ;; defaults to /ox-hugo/mypicture.png which is ugly
  (setq org-hugo-default-static-subdirectory-for-externals "images") ; imgs
  ;; (setq org-hugo-default-static-subdirectory-for-externals "~/git/temp/notes.junghanacs.com/quartz/static/images") ; imgs

  ;; Override the default `org-hugo-export-creator-string' so that this
  ;; string is consistent in all ox-hugo tests.
  (setq org-hugo-export-creator-string "Emacs + Org-mode + ox-hugo")

  ;; In that normal example of the sidenote, ox-hugo trims the whitespace around
  ;; the sidenote block. That is configured by customizing the
  ;; org-hugo-special-block-type-properties variable:
  (progn
    ;; (add-to-list 'org-hugo-special-block-type-properties '("mermaid" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("callout" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("cards" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("details" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t))))

  ;; If this property is set to an empty string, this heading will not be auto-inserted.
  ;; default value is 'References'
  ;; https://ox-hugo.scripter.co/doc/org-cite-citations/
  (plist-put org-hugo-citations-plist :bibliography-section-heading "BIBLIOGRAPHY")

  (defun +org-export-remove-white-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string " " "" text)))
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-white-space t)
  )

;;;; TODO Org-Hugo Links

;; lambda-setup/lem-setup-org-extensions.el
;; (progn
;;   ;; New link type for Org-Hugo internal links
;;   (defun org-hugo-link-complete ()
;;     "Create link with Hugo ref shortcode"
;;     (concat "{{% ref " (file-relative-name (read-file-name "File: ")) " %}}"))

;;   (defun org-hugo-follow (link)
;;     (find-file (expand-file-name link)))

;;   (with-eval-after-load 'org
;;     (org-link-set-parameters "hugo"
;;                              :complete 'org-hugo-link-complete
;;                              :follow 'org-hugo-follow))
;;   )

;;;; my/add snippets on project

(progn
  (defun my/add-setup-env-sh-snippet ()
    (interactive)
    (with-current-buffer
        (find-file (expand-file-name "setup-env.sh" (project-root (project-current))))
      (insert
       "
mkdir -p .linenote
mkdir -p .vscode

cd .vscode
ln -s ../.linenote linenote
cd -
"
       )))


  (defun my/add-projectile-snippet ()
    (interactive)
    (with-current-buffer
        (find-file (expand-file-name ".projectile" (project-root (project-current))))
      (insert
       "- /*~
- /*.elc
- /modes/*.elc
- /git/"
       )))

  (defun my/add-dir-locals-snippet ()
    (interactive)
    (with-current-buffer
        (find-file (expand-file-name ".dir-locals.el" (project-root (project-current))))
      (insert
       "((nil .
      ( ;; (cider-clojure-cli-aliases . \"dev\")
       (+evil-want-o/O-to-continue-comments . nil)
       (+default-want-RET-continue-comments . nil)
       (cider-preferred-build-tool . clojure-cli)))
 (clojure-mode . (
                  (eval . (indent-bars-mode 1))
                  (eval . (copilot-mode 1))
                  ))
 )"
       )))
  )

;;;; DONT clojure/cider utils

;; benjamin-asdf-dotall/mememacs/.emacs-mememacs.d/lisp/init-cider.el
;; (when (locate-library "cider")
;;   (with-eval-after-load 'cider
;;     (defun my/clojure-add-libs-snippet ()
;;       (interactive)
;;       (insert
;;        "(comment
;;  (require '[clojure.tools.deps.alpha.repl :refer [add-libs]]))
;;   (add-libs
;;    '{org.sg.get-currency-conversions/get-currency-conversions
;;      {:local/root \"../get-currency-conversions/\"}})"))

;;     ))


;;;; Dired with ultra lightweight icons

; ;2025-08-13 disable on terminal
;; 2025-06-21 disable nerd-icons-dired first
;; https://emacs.dyerdwelling.family/emacs/20250612223745-emacs--emacs-dired-with-ultra-lightweight-visual-icons/
(progn
  (defvar dired-icons-map
    '(("el" . "λ") ("rb" . "◆") ("js" . "○") ("ts" . "●") ("json" . "◎") ("md" . "■")
      ("txt" . "□") ("html" . "▲") ("css" . "▼") ("png" . "◉") ("jpg" . "◉")
      ("pdf" . "▣") ("zip" . "▢") ("py" . "∆") ("c" . "◇") ("sql" . "▦")
      ("mp3" . "♪") ("mp4" . "▶") ("exe" . "▪")))

  (defun dired-add-icons ()
    (when (derived-mode-p 'dired-mode)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (and (not (eobp)) (< (line-number-at-pos) 200))
            (condition-case nil
                (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                  (when (and (> (length line) 10)
                             (string-match "\\([rwxd-]\\{10\\}\\)" line)
                             (dired-move-to-filename t)
                             (not (looking-at "[▶◦λ◆○●◎■□▲▼◉▣▢◇∆▦♪▪] ")))
                    (let* ((is-dir (eq (aref line (match-beginning 1)) ?d))
                           (filename (and (string-match "\\([^ ]+\\)$" line) (match-string 1 line)))
                           (icon (cond (is-dir "▶")
                                       ((and filename (string-match "\\.\\([^.]+\\)$" filename))
                                        (or (cdr (assoc (downcase (match-string 1 filename)) dired-icons-map)) "◦"))
                                       (t "◦"))))
                      (insert icon " "))))
              (error nil))
            (forward-line))))))

  (when (display-graphic-p) ; gui
    (add-hook 'dired-after-readin-hook 'dired-add-icons)))

;;;; my/diff-hl-dired-mark-modified

(with-eval-after-load 'diff-hl
  ;; [[denote:20250707T093026][#디레드]]

  (defun my/diff-mark-toggle-vc-modified ()
    "Dired 버퍼에서 git 등 VC로 변경된 파일에 마크를 토글합니다."
    (interactive)
    (require 'vc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((file (dired-get-filename nil t)))
          (when file
            (let ((backend (vc-backend file)))
              (when backend
                (let ((state (vc-state file backend)))
                  (when (memq state '(edited added removed conflict))
                    (dired-mark 1))))))
          (forward-line 1)))))

;; (defun my/diff-hl-dired-mark-untracked ()
  ;; "Mark all untracked files in current dired buffer."
(defun my/diff-hl-dired-mark-modified ()
  "Mark all modified (edited/added/removed/untracked) files in current dired buffer."
  (interactive)
  (let ((backend (ignore-errors (vc-responsible-backend default-directory)))
        (def-dir default-directory)
        (current-buffer (current-buffer)))
    (when (and backend (not (memq backend diff-hl-dired-ignored-backends)))
      (let ((process-buffer (generate-new-buffer " *mark-untracked-tmp*")))
        (with-current-buffer process-buffer
          (setq default-directory (expand-file-name def-dir))
          (erase-buffer)
          (diff-hl-dired-status-files
           backend def-dir
           (when diff-hl-dired-extra-indicators
             (cl-loop for file in (directory-files def-dir)
                      unless (member file '("." ".." ".hg" ".git"))
                      collect file))
           (lambda (entries &optional more-to-come)
             (when (buffer-live-p current-buffer)
               (with-current-buffer current-buffer
                 (let ((inhibit-read-only t))
                   (dolist (entry entries)
                     (cl-destructuring-bind (file state &rest r) entry
                       (setq file (replace-regexp-in-string "\\` " "" file))
                       (when (memq state '(unregistered edited added removed)) ;; all-in-one
                         (save-excursion
                           (goto-char (point-min))
                           (when (dired-goto-file-1
                                  file (expand-file-name file def-dir) nil)
                             (dired-mark 1)))))))))
             (unless more-to-come
               (when (buffer-live-p process-buffer)
                 (kill-buffer process-buffer))))))))))
  )


;;;; my-org-hugo-rot13-company-name

(when my-company-name

  (defun my-org-hugo-rot13-company-name (text backend info)
    "Hugo로 내보낼 때 회사명을 ROT13으로 암호화합니다."
    (when (and my-company-name my-company-name-rot13
               (org-export-derived-backend-p backend 'hugo))
      ;; 미리 계산된 ROT13 값을 사용합니다.
      ;; 세 번째 인수인 t는 case-fold-search를 활성화하여
      ;; 대소문자 구분 없이 모두 찾아 바꿉니다.
      (replace-regexp-in-string my-company-name
                                my-company-name-rot13
                                text t t)))

  (add-to-list 'org-export-filter-plain-text-functions
               'my-org-hugo-rot13-company-name)
  (add-to-list 'org-export-filter-src-block-functions
               'my-org-hugo-rot13-company-name)
  (add-to-list 'org-export-filter-link-functions
               'my-org-hugo-rot13-company-name)
  (add-to-list 'org-export-filter-keyword-functions
               'my-org-hugo-rot13-company-name)
  )

;;;; my-post-export-replace-in-content-dir

(progn
  (defvar my-sensitive-strings-map nil
    "민감한 문자열과 대체 문자열의 매핑을 저장하는 해시테이블")

  (defun my-generate-consistent-replacement (original-string)
    "원본 문자열을 일관된 대체 문자열로 변환합니다.
   같은 입력에 대해 항상 같은 출력을 생성합니다."
    (let* ((hash (secure-hash 'sha256 original-string))
           ;; 해시의 앞 16자리를 사용하여 원본과 같은 길이의 문자열 생성
           (hex-chars (substring hash 0 (min 32 (* 2 (length original-string)))))
           (replacement ""))
      ;; 원본의 문자 패턴을 유지하면서 대체
      (dotimes (i (length original-string))
        (let ((orig-char (aref original-string i))
              (hex-index (* 2 (% i 16))))
          (cond
           ;; 숫자는 숫자로
           ((and (>= orig-char ?0) (<= orig-char ?9))
            (setq replacement
                  (concat replacement
                          (char-to-string
                           (+ ?0 (% (string-to-number
                                     (substring hex-chars hex-index (1+ hex-index)) 16) 10))))))
           ;; 소문자는 소문자로
           ((and (>= orig-char ?a) (<= orig-char ?z))
            (setq replacement
                  (concat replacement
                          (char-to-string
                           (+ ?a (% (string-to-number
                                     (substring hex-chars hex-index (1+ hex-index)) 16) 26))))))
           ;; 대문자는 대문자로
           ((and (>= orig-char ?A) (<= orig-char ?Z))
            (setq replacement
                  (concat replacement
                          (char-to-string
                           (+ ?A (% (string-to-number
                                     (substring hex-chars hex-index (1+ hex-index)) 16) 26))))))
           ;; 기타 문자는 그대로
           (t
            (setq replacement (concat replacement (char-to-string orig-char)))))))
      replacement))

  (defun my-load-sensitive-strings ()
    "민감한 문자열 파일을 로드합니다."
    (setq my-sensitive-strings-map (make-hash-table :test 'equal))
    (when (file-exists-p my-sensitive-strings-file)
      (with-temp-buffer
        (insert-file-contents my-sensitive-strings-file)
        (goto-char (point-min))
        (let ((data (read (current-buffer))))
          (dolist (item data)
            (puthash (car item) (cdr item) my-sensitive-strings-map))))))

  (defun my-save-sensitive-strings ()
    "민감한 문자열 매핑을 파일에 저장합니다."
    (let ((dir (file-name-directory my-sensitive-strings-file)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (with-temp-file my-sensitive-strings-file
      (let ((data '()))
        (maphash (lambda (k v) (push (cons k v) data)) my-sensitive-strings-map)
        (insert (format "%S" data)))))

  (defun my-add-sensitive-string (original)
    "새로운 민감한 문자열을 추가합니다."
    (interactive "s민감한 문자열 입력: ")
    (unless my-sensitive-strings-map
      (my-load-sensitive-strings))
    (unless (gethash original my-sensitive-strings-map)
      (let ((replacement (my-generate-consistent-replacement original)))
        (puthash original replacement my-sensitive-strings-map)
        (my-save-sensitive-strings)
        (message "추가됨: '%s' -> '%s'" original replacement))))

  (defun my-org-hugo-filter-sensitive-strings (text backend info)
    "Hugo 내보내기 시 민감한 문자열들을 대체합니다."
    (when (org-export-derived-backend-p backend 'hugo)
      (unless my-sensitive-strings-map
        (my-load-sensitive-strings))
      (when my-sensitive-strings-map
        (maphash (lambda (original replacement)
                   (setq text (replace-regexp-in-string
                               (regexp-quote original) replacement text t t)))
                 my-sensitive-strings-map))
      text))

  (defun my-post-export-replace-in-content-dir (content-dir)
    "content 디렉토리의 모든 마크다운 파일에서 민감한 문자열을 일괄 치환합니다."
    (interactive "Dcontent 디렉토리 경로: ")
    (unless my-sensitive-strings-map
      (my-load-sensitive-strings))
    (when my-sensitive-strings-map
      (let ((md-files (directory-files-recursively content-dir "\\.md$")))
        (dolist (file md-files)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((modified nil))
              (maphash (lambda (original replacement)
                         (goto-char (point-min))
                         (while (search-forward original nil t)
                           (replace-match replacement t t)
                           (setq modified t)))
                       my-sensitive-strings-map)
              (when modified
                (write-region (point-min) (point-max) file)
                (message "Updated: %s" file))))))))

  (defun my/export-replace-in-notes-content-dir ()
    (interactive)
    (my-post-export-replace-in-content-dir (concat org-hugo-base-dir "content"))
    )

  )

;; (defun my-setup-sensitive-string-filters ()
;;   "민감한 문자열 필터들을 설정합니다."
;;   (my-load-sensitive-strings)
;;   (add-to-list 'org-export-filter-plain-text-functions
;;                'my-org-hugo-filter-sensitive-strings)
;;   (add-to-list 'org-export-filter-src-block-functions
;;                'my-org-hugo-filter-sensitive-strings)
;;   (add-to-list 'org-export-filter-example-block-functions
;;                'my-org-hugo-filter-sensitive-strings)
;;   (add-to-list 'org-export-filter-fixed-width-functions
;;                'my-org-hugo-filter-sensitive-strings))

;; ;; 자동 설정
;; (my-setup-sensitive-string-filters)

;;; provide

(provide 'uniconfig)

;;; end-of configuration

;; Local Variables:
;; elisp-flymake-byte-compile: t
;; End:
