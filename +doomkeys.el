;;; +doomkeys.el -*- lexical-binding: t; -*-

(message "load +doomkeys.el")

;;; Global Leader Keybindings

;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/
;; NOTE: use `map!' macro for convienience
;;
;; Bind key onto Evil Normal state
;; (map! :after evil
;;       :map evil-normal-state-map
;;       "/" #'+default/search-buffer)

;; ------------------------------------------------
;; Over-ride or add to Doom Emacs default key bindings

;;;; Top-menu M-x

(map! :leader
      "TAB" nil
      "SPC" nil
      "." nil
      "," nil
      :desc "last-buffer" "TAB" #'evil-switch-to-windows-last-buffer
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Find file in project" "." #'projectile-find-file
      :desc "Find file in cwd" "," #'my/consult-fd
      :desc "consult-buffer" "`" #'consult-buffer
      :desc "Eval expression" "M-;" #'pp-eval-expression
      :desc "Search for symbol in cwd" "(" #'my/search-cwd-symbol-at-point

      :desc "window-1" "1" #'winum-select-window-1
      :desc "window-2" "2" #'winum-select-window-2
      :desc "window-3" "3" #'winum-select-window-3
      :desc "window-4" "4" #'winum-select-window-4)
;; :desc "1st workspace" "1" #'+workspace/switch-to-0

;;;; SPC

(progn
  ;; unset doom keybidings
  (map! (:when (modulep! :ui workspaces)
          :n "C-t"   #'nil
          :n "C-S-t" #'nil
          :g "M-0"   #'switch-to-minibuffer))

  (global-set-key (kbd "C-s-[") '+workspace/switch-left)
  (global-set-key (kbd "C-s-]") '+workspace/switch-right))

;;;; popup

(map! :leader
      ":" nil
      ";" nil
      ;; "`" nil
      ;; :desc "popup/toggle" "`" #'+popup/toggle
      :desc "popup/toggle" ";" #'+popup/toggle ; #'popper-toggle
      :desc "popup/close-all" ":" #'+popup/close-all)


;; doom /modules/config/default/+evil-bindings.el
(map! (:when (modulep! :ui popup)
        "C-`"   #'+popup/toggle
        "C-~"   #'+popup/raise
        "C-x p" #'+popup/other))

(global-set-key (kbd "C-`") #'+vterm/toggle) ;; vscode style

;;;; 'v' er/expand-region

(map! :leader
      :desc "er/expand-region" "v" #'er/expand-region
      :desc "expand-transient" "V" #'expand-transient)

;;;; Replace Doom `/' highlight with buffer-search - consult-line

(map! :after evil
      ;; :map evil-visual-state-map "v" #'expand-transient
      :map evil-normal-state-map
      "." #'+default/search-buffer) ;; / -> .

;;;; '=' Format

(map! :leader
      (:prefix ("=" . "format")
       :desc "buffer" "=" #'+format/buffer
       :desc "buffer" "b" #'+format/buffer
       :desc "region" "r" #'+format/region
       :desc "whitespace" "w" #'delete-trailing-whitespace))

;;;; 'b' buffer

(map! :leader
      (:prefix "b"
       "b" nil
       "s" nil
       "S" nil
       "k" nil
       "K" nil
       "h" nil
       :desc "Ash-Goto-Agenda" "A" #'ash-goto-org-agenda
       :desc "Dashboard" "h" #'+doom-dashboard/open
       :desc "switch-workspace-buffer" "b" #'+vertico/switch-workspace-buffer ; default
       :desc "consult-buffer" "SPC" #'consult-buffer
       :desc "consult-buffer" "." #'consult-buffer
       :desc "Kill all Dired buffers" "D" #'my/dired-kill-all-buffers
       :desc "Jump to Bookmark" "RET" #'consult-bookmark
       "1" #'bookmark-bmenu-list
       :desc "Kill buffer and window" "K" #'kill-buffer-and-window
       ;; :desc "Kill buffer and return previous" "K" #'kill-buffer-and-return-previous
       :desc "Kill all buffers" "M-k" #'doom/kill-all-buffers
       :desc "Stitch to Scratch" "s" #'bh/switch-to-scratch
       :desc "Switch to Agenda" "a" #'bh/switch-to-agenda
       :desc "Save all *Org* buffers" "S" #'org-save-all-org-buffers
       :desc "*evil* Write all buffers" "w" #'evil-write-all))

;; :desc "Toggle Last" "TAB" #'evil-switch-to-windows-last-buffer)

;; move/ swap buffer
;; (map! :leader
;;       (:prefix "b"
;;        :desc "Move buffer to window 1" "1" #'buffer-to-window-1
;;        :desc "Move buffer to window 2" "2" #'buffer-to-window-2
;;        :desc "Move buffer to window 3" "3" #'buffer-to-window-3
;;        :desc "Move buffer to window 4" "4" #'buffer-to-window-4
;;        :desc "Swap buffer to window 1" "M-1" #'swap-buffer-window-no-follow-1
;;        :desc "Swap buffer to window 2" "M-2" #'swap-buffer-window-no-follow-2
;;        :desc "Swap buffer to window 3" "M-3" #'swap-buffer-window-no-follow-3
;;        :desc "Swap buffer to window 4" "M-4" #'swap-buffer-window-no-follow-4
;;        ))

;;;; 'f' file

(map! :leader
      (:prefix "f"
      "y" #'+default/yank-buffer-absolute-path
      "M-f" #'+default/yank-buffer-path
       :desc "" "d" nil  ; remove existing binding
       (:prefix ("d" . "diff")
        :desc "3 files" "3" #'ediff3
        :desc "ediff" "d" #'diff
        :desc "ediff" "e" #'ediff
        :desc "version" "r" #'vc-root-diff
        :desc "version" "v" #'vc-ediff)))

;;;; 'p' project

;; Toggle treemacs project browser from project menu
;; (map! :leader
;;       (:prefix "p"
;;        "t" nil  ; disable project todos key binding
;;        :desc "Project browser" "t" #'+treemacs/toggle))

;;;; 'g' Git

;; Change SPC g s to call Magit Status, rather than stage hunk at point
;; (map! :leader
;;       (:prefix "g"
;;        :desc "" "s" nil  ; remove existing binding
;;        :desc "Magit Status" "s" #'magit-status))

(require 'my-git-link)

;; ~/sync/man/dotsamples/doom/agzam-dot-doom/config.el
(map! :leader
      (:prefix ("g" . "git")
       "h" nil
       :desc "magit-file" "F" #'magit-file-dispatch
       :desc "jump-list" "j" #'evil-show-jumps
       ;; :desc "git status" "s" #'magit-status
       :desc "git-log-grep" "K" #'consult-git-log-grep
       :desc "gh/search-repos" "SPC" #'consult-gh-search-repos
       :desc "gh/search-find-file" "1" #'consult-gh-find-file
       (:prefix ("h" . "consult-gh")
        :desc "gh/repo-clone" "c" #'consult-gh-repo-clone
        :desc "gh/default-repos" "d" #'consult-gh-default-repos
        :desc "gh/find-file" "f" #'consult-gh-find-file
        :desc "gh/search-repos" "s" #'consult-gh-search-repos
        :desc "gh-search-code" "S" #'consult-gh-search-code
        :desc "gh-search-prs" "p" #'consult-gh-search-prs
        :desc "gh-pr-list" "P" #'consult-gh-pr-list
        :desc "gh-search-issues" "i" #'consult-gh-search-issues
        :desc "gh-issue-list" "I" #'consult-gh-issue-list
        :desc "gh-fork-current-repo" "k" #'consult-gh-fork-current-repo)
       (:prefix (";" . "git blame/link/messenger")
        "p" #'git-messenger:popup-message
        "i" #'blamer-show-posframe-commit-info
        :desc "git-link-blame" "b" #'git-link-blame
        :desc "git-link-commit" "c" #'git-link-commit
        :desc "git-link" "l" #'git-link
        :desc "git-link-homepage" "h" #'git-link-homepage
        :desc "git-link-kill/copy" "k" #'git-link-kill
        :desc "git-link-main-branch" "m" #'git-link-main-branch)))

;;;; 'h' help

(map! :leader
      (:prefix "h"
       "SPC" #'consult-info
       "a" #'helpful-at-point
       "f" #'helpful-function
       "D" #'elisp-demos-find-demo
       "h" #'helpful-symbol
       "t" nil ; consult-theme
       :desc "themes-map" "t" ews-modus-themes-map
       "p" nil
       (:prefix ("p" . "packages")
                "l" #'list-packages
                "f" #'find-library-other-window
                "p" #'doom/help-packages
                "d" #'doom/describe-package)
       "1" #'find-function-other-window
       "v" #'helpful-variable
       "j" #'info-display-manual))

;;;; 'k' lisp

;; (map! :leader
;;       "k" nil
;;       "k" #'sexp-transient)

;; /john-dot-doom/+smartparens.el
(when (locate-library "smartparens")
  ;; A Spacemacs like Lisp state menu (without the transient state)
  (map! :leader
        "k" nil
        (:prefix ("k". "Smartparens")
         :desc "Delete Pair" "D" #'delete-pair
         :desc "Slurp forward" "s" #'sp-forward-slurp-sexp
         :desc "Slurp backward" "S" #'sp-backward-slurp-sexp
         :desc "End Sexp" "$"   #'sp-end-of-sexp
         (:prefix ("`" . "Hybrid")
          :desc "Kill" "k" #'sp-kill-hybrid-sexp
          :desc "Push" "p" #'sp-push-hybrid-sexp
          :desc "Slurp" "s" #'sp-slurp-hybrid-sexp
          :desc "Transpose" "t" #'sp-transpose-hybrid-sexp
          :desc "Absorb" "a" #'sp-absorb-sexp
          :desc "Barf forward" "b" #'sp-forward-barf-sexp
          :desc "Barf backward" "B" #'sp-backward-barf-sexp
          :desc "Convoluted" "c" #'sp-convolute-sexp)
         (:prefix ("d" . "Delete")
          :desc "Delete Pair" "d" #'delete-pair
          :desc "Symbol" "s" #'sp-kill-symbol
          :desc "Symbol Backward" "S" #'sp-backward-kill-symbol
          :desc "Word" "w" #'sp-kill-word
          :desc "Word Backward" "W" #'sp-backward-kill-word
          :desc "Kill" "x" #'sp-kill-sexp
          :desc "Kill Backward" "X" #'sp-backward-kill-sexp)
         :desc "Splice" "e" #'sp-splice-sexp-killing-forward
         :desc "Splice Backward" "E" #'sp-splice-sexp-killing-backward
         :desc "Symbol Backward" "h" #'sp-backward-symbol
         :desc "Sexp Backward" "H" #'sp-backward-sexp
         :desc "Join" "j" #'sp-join-sexp
         :desc "Sexp Forward" "l" #'sp-forward-sexp
         :desc "Sexp Forward" "L" #'sp-forward-sexp
         :desc "Raise" "r" #'sp-raise-sexp
         :desc "Slurp" "s" #'sp-forward-slurp-sexp
         :desc "Slurp Backward" "S" #'sp-backward-slurp-sexp
         :desc "Transpose" "t" #'sp-transpose-sexp
         :desc "Up Backward" "U" #'sp-backward-up-sexp
         (:prefix ("w" . "Wrap")
          :desc "()" "(" #'sp-wrap-round
          :desc "{}" "{" #'sp-wrap-curly
          :desc "[]" "[" #'sp-wrap-square
          :desc "Round" "w" #'sp-wrap-round
          :desc "Curly" "c" #'sp-wrap-curly
          :desc "Square" "s" #'sp-wrap-square
          :desc "Unwrap" "u" #'sp-unwrap-sexp)
         :desc "Copy sexp" "y" #'sp-copy-sexp)))


;;;; 'C' Capture

;; Add C for Capture
(map! :leader
      (:prefix ("C" . "Capture")
       :desc "orgabilize: url-to-org" "a" #'orgabilize-org-archive
       "n" #'org-capture
       :desc "orgabilize: insert-org-link" "l" #'orgabilize-insert-org-link
       :desc "orgabilize: find-archived-files" "f" #'orgabilize-org-find-archived-file
       :desc "org-web-tools: read-url-as-org" "t" #'org-web-tools-read-url-as-org
       :desc "org-web-tools: read-url-as-org" "T" #'org-web-tools-convert-links-to-page-entries
       :desc "jao-eww-to-org" "e" #'jao-eww-to-org
       :desc "wikinforg" "w" #'wikinforg
       ;; :desc "wiki-summary" "s" #'wiki-summary-insert
       :desc "remember" "r" #'remember
       :desc "remember-notes" "R" #'remember-notes
       "i" #'bh/insert-inactive-timestamp
       "o" #'org-pandoc-import-as-org
       :desc "org-remark-mark-line" "m" #'org-remark-mark-line
       :desc "hypothesis: to archive" "h" #'hypothesis-to-archive))


;;;; 'D' Diff/Compare

;; 둠에 어디 있는지 모르겠다만 참고해서 넣어놓고 사용하라
;; Diff of files
;; (map! :leader
;;       (:prefix ("D" . "Diff/Compare")
;;                (:prefix ("d" . "diff")
;;                 :desc "3 files" "3" #'ediff3
;;                 :desc "ediff" "d" #'diff
;;                 :desc "ediff" "e" #'ediff
;;                 :desc "version" "r" #'vc-root-diff
;;                 :desc "version" "v" #'vc-ediff)))


;; spacemacs : layers/+spacemacs/spacemacs-defaults/keybindings.el
;; ("D" "Diff/Compare"
;;    ("b"  "Buffers"
;;     ("3" ediff-buffers3 "Between 3 buffers...")
;;     ("b" ediff-buffers "Between 2 buffers...")
;;     ("B" ediff-backup "With backup file...")
;;     ("p" ediff-patch-buffer "With a patch..."))
;;    ("d" "Directories"
;;     ("3" ediff-directories3 "Between 3 directories...")
;;     ("d" ediff-directories "Between 2 directories...")
;;     ("r" ediff-directory-revisions "Using SCM revisions..."))
;;    ("f" "Files"
;;     ("." spacemacs/ediff-dotfile-and-template "With Spacemacs dotfile")
;;     ("3" ediff-files3 "Between 3 files...")
;;     ("f" ediff-files "Between 2 files...")
;;     ("p" ediff-patch-file "With a patch...")
;;     ("v" ediff-revision "Between file revisions..."))
;;    ("m" "Merge"
;;     ("b" "Buffers"
;;      ("3" ediff-merge-buffers-with-ancestor "3-way merge...")
;;      ("b" ediff-merge-buffers "2-way merge..."))
;;     ("d" "Directories"
;;      ("3" ediff-merge-directories-with-ancestor "3-way merge...")
;;      ("d" ediff-merge-directories "2-way merge..."))
;;     ("f" "Files"
;;      ("3" ediff-merge-files-with-ancestor "3-way merge...")
;;      ("f" ediff-merge-files "2-way merge..."))
;;     ("r" "Revisions"
;;      ("3" ediff-merge-revisions-with-ancestor "3-way merge...")
;;      ("r" ediff-merge-revisions "2-way merge...")))
;;    ("r" "Regions"
;;     ("l" ediff-regions-linewise "Between 2 regions (linewise)...")
;;     ("w" ediff-regions-wordwise "Between 2 regions (wordwise)..."))
;;    ("w" "Windows"
;;     ("l" ediff-windows-linewise "Linewise between visible text...")
;;     ("w" ediff-windows-wordwise "Wordwise between visible text..."))
;;    ("s" ediff-show-registry "Show registry")
;;    ("h" ediff-documentation "Documentation"))

;;;; 'w' window

(map! :leader
      :prefix "w"
      "1" nil "2" nil "3" nil "4" nil "5" nil "6" nil "7" nil "8" nil "9" nil "0" nil "-" nil "b" nil "d" nil "r" nil "R" nil "m" nil "<" nil ">" nil "_" nil "|" nil
      "C-=" nil "C-_" nil "C-b" nil "C-c" nil "C-f" nil "C-h" nil "C-j" nil "C-k" nil "C-l" nil "C-w" nil "C-n" nil "C-o" nil "C-p" nil "C-q" nil "C-r" nil "C-s" nil "C-t" nil "C-u" nil "C-v" nil "C-x" nil "C-S-h" nil "C-S-j" nil "C-S-k" nil "C-S-l" nil "C-S-r" nil "C-S-s" nil "C-S-w" nil "C-<down>" nil "C-<left>" nil "C-<right>" nil "C-<up>" nil
      "TAB" #'evil-window-prev
      "." #'window-transient
      "c" #'window-cleanup+
      "g" #'golden-ratio
      :desc "delete-window" "d" #'spacemacs/delete-window
      ;; "D" #'delete-window ; block delete workspace
      "M" #'ace-swap-window
      ;; "W" #'ace-window
      "m" #'toggle-maximize-buffer
      "=" #'balance-windows-area
      :desc "window-vsplit" "/" #'evil-window-vsplit
      ;; :desc "window-vsplit" "v" #'evil-window-vsplit
      ;; :desc "window-vsplit-follow" "V" #'+evil/window-vsplit-and-follow
      :desc "window-layout-toggle" "-" 'spacemacs/window-layout-toggle
      :desc "delete-other-window" "O" 'delete-other-windows)

;;;; 'i' insert/translate

(map! :leader
      (:prefix "i"
       :desc "time-stamp" "1" #'time-stamp
       :desc "hl-todo-insert" "t" #'hl-todo-insert
       :desc "add-global-abbrev" "a" #'add-global-abbrev
       :desc "list-unicode-display" "U" #'list-unicode-display
       :desc "txl-translate" "i" #'txl-translate-region-or-paragraph
       :desc "wiki-summary-insert" "w" #'wiki-summary-insert
       :desc "immersive-translate-paragraph" "m" #'immersive-translate-paragraph
       :desc "immersive-translate-auto-mode" "M" #'immersive-translate-auto-mode
       :desc "gt-do-translate" "d" #'gt-do-translate
       :desc "consult-register" "r" #'consult-register
       :desc "consult-yasnippet" "s" #'consult-yasnippet
       :desc "+default/yank-pop" "y" #'+default/yank-pop))


;;;; 'x' text

(map! :leader
      "x" nil
      (:prefix ("x" ."text")
               "x" #'jinx-correct-word
               ;; (:prefix ("l" . "language")
               ;;  :desc "define" "d" #'define-it-at-point
               ;;  :desc "grammarly check" "g" #'lsp-grammarly-check-grammar
               ;;  :desc "sdcv" "l" #'sdcv-search-pointer
               ;;  :desc "Merriam Webster" "m" #'mw-thesaurus-lookup-dwim
               ;;  :desc "wiktionary" "w" #'wiktionary-bro-dwim)
               (:prefix ("i" . "string-inflection")
                        "C" #'string-inflection-camelcase
                        "i" #'string-inflection-all-cycle
                        "-" #'string-inflection-kebab-case
                        "k" #'string-inflection-kebab-case
                        "_" #'string-inflection-underscore
                        "u" #'string-inflection-underscore
                        "U" #'string-inflection-upcase)
               (:prefix ("g" . "google-translate")
                :desc "en->ko" "k" #'google-translate-query-translate-reverse
                :desc "en->ko2" "K" #'+google-translate-en->ko
                :desc "ko->en" "e" #'google-translate-query-translate
                :desc "ko->en2" "E" #'+google-translate-ko->en
                :desc "translate-at-point" "g" #'google-translate-at-point)
               (:prefix ("c" . "chatgpt")
                        "c" #'gptel+
                        "m" #'gptel-menu
                        "e" #'+gptel-improve-text-transient
                        "p" #'gptel-save-as-org-with-denote-metadata
                        "s" #'gptel-send)))


;;;; 's' search/symbol

(map! :leader
      (:prefix ("s" . "search/symbol")
       ;; "/" nil
       ;; :desc "my/consult-fd" "s F" #'my/consult-fd
       ;; :desc "Search project" "/" #'+default/search-project
       ;; :desc "Search cwd" "/" #'+default/search-cwd
       :desc "+vertico/consult-fd-find" "f" #'+vertico/consult-fd-or-find ; Locate file
       :desc "Search for symbol in cwd" "SPC" #'my/search-cwd-symbol-at-point
       :desc "eww-search-words" "1" #'eww-search-words
       :desc "find-name-dired" "2" #'find-name-dired
       :desc "search-github-with-lang" "g" #'+search-github-with-lang
       ;; :desc "consult-omni-transient" "n" #'consult-omni-transient
       :desc "consult-locate" "M-l" #'consult-locate
       :desc "imenu" "j" #'imenu))

;;;; 'S' Search Plus : Custom Search

(require 'my-search)

(map! :leader
      (:prefix-map ("S" . "Search+")
       :desc "engine-mode >>" "s"  #'engine-mode-prefixed-map
       :desc "google"           "g"     #'my/search-google
       :desc "naver"            "N"     #'my/search-naver
       :desc "naver > terms"             "n"     #'my/search-terms-naver
       :desc "daum > dict"             "d"     #'my/search-dict-daum
       ;; :desc "dotnet"           "D"     #'my/search-dotnet
       ;; :desc "onelook"          "e"     #'my/search-onelook
       :desc "thesaurus"        "t"     #'my/search-thesaurus
       ;; :desc "elixir"           "x"     #'my/search-elixir
       ;; :desc "flutter"          "f"     #'my/search-flutter
       :desc "blogs"         "b"     #'my/search-blogs
       ;; :desc "sdcv > at-point" "/" 'sdcv-search-pointer
       ;; :desc "sdcv > input" "?" 'sdcv-search-input
       ;; :desc "sdcv > at-point posframe" "." 'sdcv-search-pointer+ ; posframe
       :desc "wordreference > ko->en" "\[" 'my/wr-koen
       :desc "wordreference > en->ko" "\]" 'my/wr-enko

       ;; :desc "lexic > search" "l" 'lexic-search
       ;; :desc "external-dict > search" "e" 'external-dict-dwim
       ;; :desc "mw-thesaurus > lookup" "X" 'mw-thesaurus-lookup-dwim
       ;; :desc "powerthesaurus > transient" "P" 'powerthesaurus-transient

       (:prefix ("w" . "wiktionary")
                "e" 'wiktionary-lookup-word-en
                "k" 'wiktionary-lookup-word-ko)
       (:prefix ("q" . "wikiquote")
                "e" 'wikiquote-lookup-quote-en
                "k" 'wikiquote-lookup-quote-ko)))


;;;; 't' toggle

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "visual-line-mode" "v" #'visual-line-mode
       :desc "visual-mode" "V" #'visual-mode
       :desc "consult-minor-mode" "m" #'consult-minor-mode-menu
       :desc "tab-line mode" "T" #'tab-line-mode
       ;; "y" #'consult-yank-from-kill-ring
       :desc "Copilot" "c" #'copilot-mode
       :desc "CamelCase" "C" #'subword-mode
       :desc "Column Indicator" "I" #'display-fill-column-indicator-mode
       ;; :desc "Window dedication" "d" #'spacemacs/toggle-current-window-dedication
       :desc "toggle-window-dedicated" "d" #'toggle-window-dedicated))

;;;; 'e' LLM - elysium/gptel

;; | `elysium-query`                         | send a query to the `gptel` backend                |
;; | `elysium-keep-all-suggested-changes`    | keep all of the AI-suggested changes               |
;; | `elysium-discard-all-suggested-changes` | discard all of the AI-suggested changes            |
;; | `elysium-clear-buffer`                  | clear the elysium buffer                           |
;; | `elysium-add-context`                   | add the contents of a region to the elysium buffer |
;; | `smerge-next`                           | go to the next conflicting hunk                    |
;; | `smerge-previous`                       | go to the next conflicting hunk                    |
;; | `smerge-keep-other`                     | keep this set of changes                           |
;; | `smerge-keep-mine`                      | discard this set of changes                        |
;; | `elysium-toggle-window`                 | toggle the chat window                             |


(progn
  (require 'smerge-mode)
  (map! :map smerge-mode-map
        :localleader
        (:prefix ("0" . "smerge")
                 "n" #'smerge-next
                 "p" #'smerge-prev
                 "r" #'smerge-resolve
                 "a" #'smerge-keep-all
                 "b" #'smerge-keep-base
                 "o" #'smerge-keep-lower
                 "l" #'smerge-keep-lower
                 "m" #'smerge-keep-upper
                 "u" #'smerge-keep-upper
                 "E" #'smerge-ediff
                 "C" #'smerge-combine-with-next
                 "R" #'smerge-refine
                 "C-m" #'smerge-keep-current
                 (:prefix ("=" . "diff")
                          "<" #'smerge-diff-base-upper
                          ">" #'smerge-diff-base-lower
                          "=" #'smerge-diff-upper-lower))))


;; (require 'gpt-babel)
(map! :leader
      "e" nil
      (:prefix ("e" . "gptel")
       ;; '(insert normal) 'gptel-mode-map "C-<return>" #'elysium-query

       (:prefix ("g" . "gpt-babel")
                "s" #'gpt-babel/send-block
                "p" #'gpt-babel/patch-block
                "f" #'gpt-babel/fix-block
                "i" #'gpt-babel/fix-with-instructions
                "w" #'gpt-babel/wish-complete

                (:prefix ("c" . "context-fix")
                         "a" #'gpt-babel/fix-block-file-above
                         "h" #'gpt-babel/fix-block-with-help
                         ))

       (:prefix ("e" . "elysium")
                "q" #'elysium-query
                "o" #'elysium-keep-all-suggested-changes
                "m" #'elysium-discard-all-suggested-changes
                "a" #'elysium-add-context
                "c" #'elysium-clear-buffer
                "t" #'elysium-toggle-window
                "e" #'elysium-toggle-window
                )

       "q" #'elysium-query-no-code
       :desc "gptel: gptel-mode" "SPC" #'gptel-mode
       :desc "gptel: gptel-mode" "RET" #'gptel-mode
       "t" #'gptel-org-toggle-branching-context
       ;; :desc "gptel: send default" :n "3" (cmd! (cashpw/gptel-send (alist-get 'default gptel-directives)))
       ;; :desc "gptel: send chain-of-thought" :n "4" (cmd! (cashpw/gptel-send (alist-get 'chain-of-thought gptel-directives)))
       ;; :desc "gptel: send follow-up" :n "5" (cmd! (cashpw/gptel-send (alist-get 'follow-up gptel-directives))))
       )
      )

;;;; '9' gptel

;; (map! :leader
;;       "9" nil
;;       (:prefix ("9" . "gptel")
;;        ;; "0" #'+gpt-dwim-current-buffer
;;        "9" #'gptel-mode
;;        "t" #'gptel-org-toggle-branching-context
;;        :desc "gptel-send: default" :n "l" (cmd! (cashpw/gptel-send (alist-get 'default gptel-directives)))
;;        :desc "gptel-send: chain-of-thought" :n "c" (cmd! (cashpw/gptel-send (alist-get 'chain-of-thought gptel-directives)))
;;        :desc "gptel-send: follow-up" :n "f" (cmd! (cashpw/gptel-send (alist-get 'follow-up gptel-directives))))
;;       )

;;;; '0' copilot / copilot-chat

(map! :leader
      (:prefix
       ("0" . "copilot")
       "c" #'copilot-mode
       "d" #'copilot-chat-display
       "e" #'copilot-chat-explain
       "r" #'copilot-chat-review
       "f" #'copilot-chat-fix
       "o" #'copilot-chat-optimize
       "t" #'copilot-chat-test
       "a" #'copilot-chat-add-current-buffer
       "D" #'copilot-chat-doc
       "R" #'copilot-chat-reset
       "x" #'copilot-chat-del-current-buffer))

(map! (:map copilot-mode-map
            "C-c M-f" #'copilot-complete
            "M-<tab>" #'copilot-complete)

      (:map copilot-completion-map
            "<tab>" nil ; jump-out-of-pair
            "M-<tab>" #'copilot-accept-completion))


;;;; 'o' open

(map! :leader
      (:prefix ("o" . "open")
       :desc "open workspaces" "o" #'my/open-workspaces
       "h" #'proced
       ))

;;;; 'j' junghanacs hotkey

(map! :leader
      (:prefix ("j" . "junghanacs")
       :desc "Mastodon" "m" #'mastodon
       :desc "Tmr" "t" #'tmr
       :desc "Tmr-view" "v" #'tmr-tabulated-view))
;; :desc "Anddo: Todos" "d" #'anddo

;;;; 'r' remote / register

(map! :leader
      :desc "jump to register" "rr" #'jump-to-register)

;; (set-register ?l '(file . "~/org/ledger/ledger-2024.dat"))
(set-register ?b '(file . "~/sync/org/blog/20240104T061355--blog.org"))
(set-register ?c '(file . "~/.doom.d/README.org"))

;;;; 'n' +notes denote

(map! :leader
      (:prefix ("n" . "notes")
       ;; :n "TAB" #'my/side-notes-toggle-daily-note
       ;; "a" nil
       "d" nil
       "n" nil
       "S" nil
       ;; default keybindings
       ;; :desc "ews-annotate-map"             "a" ews-annotate-map
       :desc "ash-goto-agenda"               "A" 'ash-goto-org-agenda
       ;; :desc "consult-org-agenda" "A" #'consult-org-agenda ; M-g a

       :desc "ews-bibliography-map"          "b" ews-bibliography-map
       :desc "org-capture"                   "c" #'org-capture
       :desc "+org/toggle-last-clock"        "C" #'+org/toggle-last-clock
       :desc "ews-denote-map"                "d" ews-denote-map

       :desc "+default/find-in-notes"        "f" #'+default/find-in-notes ; find-files
       :desc "+default/browse-notes"         "F" #'+default/browse-notes

       :desc "org-store-link"                "l" #'org-store-link
       :desc "org-store-link-id-optional"    "L" #'my/org-store-link-id-optional
       :desc "org-tags-view"                 "m" #'org-tags-view

       :desc "org-clock-cancel"              "M-c" #'org-clock-cancel
       ;; :desc "org-capture-goto-target"       "N" #'org-capture-goto-target

       :desc "org-clock-goto"                "o" #'org-clock-goto
       :desc "org-todo-list"                 "t" #'org-todo-list

       :desc "+default/org-notes-search"     "g" #'+default/org-notes-search ; grep
       ;; :desc "+default/org-notes-headlines"  "S" #'+default/org-notes-headlines

       :desc "org-search-veiw"               "v" #'org-search-view
       ;; "u" #'org-transclusion-mode

       :desc "+org/export-to-clipboard"      "y" #'+org/export-to-clipboard

       ;; :desc "org-journal-open-today" "SPC" #'org-journal-open-current-journal-file
       :desc "open current journal" :n "SPC" #'org-journal-open-current-journal-file
       ;; :desc "side-journal-toggle" "TAB" #'side-journal-toggle-notes

       :desc "org-drill"                     "D" #'org-drill

       :desc "consult-org-all"               "'" #'my/consult-org-all

       ;; :desc "ews-note-map"                  "n" ews-note-map
       :desc "ews-denote-map"                  "n" ews-denote-map))

;; :desc "my/denote-random-note"        "?" #'my/denote-random-note
;; :desc "org-roam-random-no-dates"        "?" #'ash/org-roam-node-random-no-dates


;;;; 'N' consult-notes

(after! consult-notes
  (map! :leader
        :desc "consult-notes" "N" 'consult-notes))

;;;; DONT SPC 1-4 window

;; (map! :leader
;;       :desc "select-window 1" "1" #'winum-select-window-1
;;       :desc "select-window 2" "2" #'winum-select-window-2
;;       :desc "select-window 3" "3" #'winum-select-window-3
;;       :desc "select-window 4" "4" #'winum-select-window-4)

;;; Custom EVIL Keys

;; agzam : /agzam-dot-doom/config.el
(map! :i "M-l" 'evil-forward-char ;; #'sp-forward-slurp-sexp ; downcase-word
      :i "M-h" 'evil-backward-char ;; #'sp-forward-barf-sexp  ; mark-paragraph
      ;; :v "s" #'evil-surround-region
      ;; "s-b" #'consult-buffer
      ;; "s-=" #'text-scale-increase
      ;; "s--" #'text-scale-decrease
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter)) ; nop
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter)) ; nop
      :n "zk" #'text-scale-increase ; fold
      :n "zj" #'text-scale-decrease
      ;; :n "DEL" #'previous-buffer
      :n "DEL" #'+vertico/switch-workspace-buffer ; default
      ;; :n "s-e" #'+scroll-line-down-other-window
      ;; :n "s-y" #'+scroll-line-up-other-window
      :i "M-/" #'hippie-expand
      ;; :n "g9" #'ibuffer-sidebar-jump ; 'gi' evil-insert-resume
      :n "g SPC" #'evil-jump-to-tag
      :i "C-v" #'evil-paste-after ; evil-quoted-insert : 'C-q'
      ;; :i "TAB" #'completion-at-point ; jump out of
      ;; (:when (featurep :system 'linux)
      ;;   :i "C-M-S-s-y" #'nerd-dictation-toggle)
      ;; SPC g [ / ]
      :n "[ g" #'+vc-gutter/previous-hunk ; remap diff-hl-previous-hunk
      :n "] g" #'+vc-gutter/next-hunk ; remap diff-hl-next-hunk

      :m "8" #'evil-ex-search-word-forward ; default *
      :m "3" #'evil-ex-search-word-backward ; default #
      :m "4" #'evil-end-of-line ; default $
      :m "0" #'evil-beginning-of-line

      ;; :m "C-i" #'evil-jump-forward ;; evil-want-C-i-jump - evil-maps.el
      :n "g ]" #'evil-jump-forward
      :n "g [" #'evil-jump-backward
      :n "g RET" #'tabgo

      (:prefix ("z'" . "string-inflection")
       :n "C" #'string-inflection-camelcase
       :n "i" #'string-inflection-all-cycle
       :n "-" #'string-inflection-kebab-case
       :n "k" #'string-inflection-kebab-case
       :n "_" #'string-inflection-underscore
       :n "u" #'string-inflection-underscore
       :n "U" #'string-inflection-upcase))

;; "[b" #'evil-prev-buffer
;; "]b" #'evil-next-buffer


;;; Major-Mode Leader Keybindings

;;;; for alice keyboard

(defun my/enable-alice-keyboard-toggle-input-method ()
  (interactive)
  (map! (:map vertico-map
              "`"   #'toggle-input-method)
        (:map vterm-mode-map
              "`"   #'toggle-input-method)
        (:map prog-mode-map
              "`"   #'toggle-input-method)
        (:map minibuffer-mode-map
              "`"   #'toggle-input-method)
        (:map minibuffer-local-map
              "`"   #'toggle-input-method)
        (:map org-mode-map
              "`"   #'toggle-input-method)))

;;;; TODO C-c M-a - M-a Bindings

(map! (:map prog-mode-map
            "C-c M-a" #'aider-transient-menu)
      (:map text-mode-map
            "C-c M-a" #'casual-avy-tmenu))

;;;; minibuffer-mode-map

(map! (:map minibuffer-mode-map
            "M-l" #'sp-forward-slurp-sexp
            "M-h" #'sp-forward-barf-sexp)
      (:map minibuffer-local-map
            "C-c C-s" #'embark-collect))

;;;; Doom's org-mode-map

;;;;; after! evil-org

(after! evil-org
  ;; (message "after evil-org - doomkeys")

  ;; doom에서 =C-RET= 키는 아래에 추가 =C-S-RET= 키는 위로 추가로 바인딩을
  ;; 변경한다. 새로 함수를 추가해서 해당 함수에 바인딩하는데, 해당 함수에
  ;; =org-blank-before-new-entry= 심볼 값이 반영이 안 되어 있어서 org mode의
  ;; 디폴트 함수로 바인딩을 했다.

  (map! :map evil-org-agenda-mode-map "P" 'org-procrastinate)
  (map! :map evil-org-mode-map
        :n "x" 'delete-forward-char
        :n "X" 'delete-backward-char
        :n "6" 'evil-first-non-blank
        :n "4" 'evil-end-of-line
        :n "8" 'evil-ex-search-word-forward
        :n "3" 'evil-ex-search-word-backward
        :n "M-8" #'tempel-insert
        :ni "M-t" #'txl-translate-region-or-paragraph

        ;; :niv "M-j" #'org-meta-down ; M-<up>
        ;; :niv "M-k" #'org-meta-up ; M-<down>
        :nv "M-S-p" #'outline-up-heading
        :nv "M-j" #'org-forward-heading-same-level
        :nv "M-k" #'org-backward-heading-same-level
        :nv "M-n" #'org-next-visible-heading
        :nv "M-p" #'org-previous-visible-heading

        ;; :ni [C-return]   #'org-insert-heading-respect-content
        ;; :ni [C-S-return] #'org-insert-todo-heading-respect-content
        :ni "C-c C-RET"      #'my/org-open-at-point-other-window
        :ni "C-c C-<return>" #'my/org-open-at-point-other-window))

;;;; markdown-mode-map

;; Changes
;; - move toggle prefix from `t' to `T'
;; - add table prefix `t'

(with-eval-after-load 'markdown-mode
  (map! :map markdown-mode-map
        :localleader
        "RET" #'toc-org-markdown-follow-thing-at-point
        "-" #'markdown-insert-list-item
        ";" #'my/clear-nbsp-and-ascii-punctuations
        ":" #'my/insert-nbsp-simple-all
        ;; ":" #'my/insert-nbsp-all-with-wordlist-and-tags
        ;; "M-;" #'my/add-to-glossary
        ;; "M-'" #'my/add-newlines-between-paragraphs
        "y" #'yank-as-org
        (:prefix ("b" . "Table")
         :desc "Header" "h" #'markdown-table-hline-at-point-p
         :desc "Sort" "s" #'markdown-table-sort-lines
         :desc "Region to table" "r" #'markdown-table-convert-region
         :desc "Table insert" "t" #'markdown-insert-table
         (:prefix ("d" . "Delete")
          :desc "column" "c" #'markdown-table-delete-column
          :desc "row" "r" #'markdown-table-delete-row)
         (:prefix ("i" . "Insert")
          :desc "Column" "c" #'markdown-table-insert-column
          :desc "Row" "r" #'markdown-table-insert-row))
        (:prefix ("t" . "toggle")
         :desc "Inline LaTeX"      "e" #'markdown-toggle-math
         :desc "Code highlights"   "f" #'markdown-toggle-fontify-code-blocks-natively
         :desc "Inline images"     "i" #'markdown-toggle-inline-images
         :desc "URL hiding"        "l" #'markdown-toggle-url-hiding
         :desc "Markup hiding"     "m" #'markdown-toggle-markup-hiding
         :desc "Wiki links"        "w" #'markdown-toggle-wiki-links
         :desc "GFM checkbox"      "x" #'markdown-toggle-gfm-checkbox)
        )
  )

;;;; eww-mode-map

(with-eval-after-load 'eww
  (with-eval-after-load "org" (require 'ol-eww nil t))

  (map! :map eww-mode-map
        "C-c C-o" #'eww-browse-with-external-browser
        :n "M-j" (cmd! () (pixel-scroll-precision-scroll-down 50))
        :n "M-k" (cmd! () (pixel-scroll-precision-scroll-up 50))
        :n "j" #'evil-next-visual-line
        :n "k" #'evil-previous-visual-line
        :n "q" #'kill-buffer-and-window

        (:localleader
         :desc "zoom" "z" #'eww-zoom-transient
         :desc "open-in-other-window" "e" #'+eww/open-in-other-window
         :desc "external browser" "E" #'eww-browse-with-external-browser
         "r" 'eww-readable
         "R" 'eww-reload
         "p" 'eww-previous-url
         "n" 'eww-next-url
         "h" 'eww-list-histories
         "d" 'eww-download
         "a" 'eww-add-bookmark
         "f" 'ace-link-eww
         ;; "c" 'eww-copy-page-url
         (:prefix ("v" . "view")
                  "x" 'eww-browse-with-external-browser
                  "f" 'eww-toggle-fonts
                  "r" 'eww-readable
                  "s" 'eww-view-source)
         (:prefix ("l" . "list")
                  "b" 'eww-list-buffers
                  "o" 'eww-list-bookmarks))))



;;;; corfu

(after! corfu
  (map! :map corfu-map
        "<escape>" #'+corfu-quit-and-escape
        "M-i"    #'corfu-insert-separator
        ;; "C-/" #'+corfu-move-to-minibuffer  ; undo-fu-only-undo
        "M-." #'+corfu-move-to-minibuffer) ;; default 'C-S-s'

  ;; corfu-indexed like in Company, M+number - inserts the thing
  ;; (map! :map corfu-map
  ;;       "M-0" (cmd! () (+corfu-insert-indexed 9))
  ;;       "M-1" (cmd! () (+corfu-insert-indexed 0))
  ;;       "M-2" (cmd! () (+corfu-insert-indexed 1))
  ;;       "M-3" (cmd! () (+corfu-insert-indexed 2))
  ;;       "M-4" (cmd! () (+corfu-insert-indexed 3))
  ;;       "M-5" (cmd! () (+corfu-insert-indexed 4))
  ;;       "M-6" (cmd! () (+corfu-insert-indexed 5))
  ;;       "M-7" (cmd! () (+corfu-insert-indexed 6))
  ;;       "M-8" (cmd! () (+corfu-insert-indexed 7))
  ;;       "M-9" (cmd! () (+corfu-insert-indexed 8)))
  )


;;;; TODO lsp-mode-map

;; (map! :map lsp-mode-map
;;       "C-M-." #'lsp-find-references
;;       "C-c r" #'lsp-rename)

;;;; DONT clojure-mode-map

;; Lookup functions in Clojure - The Essentail Reference book
;; https://github.com/p3r7/clojure-essential-ref

;; /evil-dot-doom/modules/custom/parenthesis/config.el
;;;###autoload
;; (defun bk/improve-last-parens ()
;;   (interactive)
;;   (evil-normal-state)
;;   (evil-append-line 1))

;; TODO: review evaluation key bindings from Spacemacs
;; (map! :after cider
;;       :map cider-mode-map
;;       :in "<f8>"   #'+treemacs/toggle
;;       :in "<C-f8>" #'treemacs-find-file
;;       :in "<M-f8>" #'treemacs-select-window
;;       :i "M-9" #'insert-parentheses
;;       :i "M-j" #'bk/improve-last-parens
;;       :i "M-l" #'sp-forward-sexp
;;       "M-RET" #'cider-eval-last-sexp
;;       "M-S-<return>" #'cider-eval-defun-to-comment
;;       "C-c M-RET" #'outline-insert-heading
;;       :map clojure-mode-map
;;       :localleader
;;       :desc "REPL session" "'" #'sesman-start
;;       :i "M-j" #'bk/improve-last-parens
;;       :i "M-l" #'sp-forward-sexp
;;       "M-RET" #'cider-eval-last-sexp
;;       "M-S-<return>" #'cider-eval-defun-to-comment
;;       "C-c M-RET" #'outline-insert-heading
;;       (:prefix ("h" . "help")
;;                "r" #'clojure-essential-ref))

;;       ;; Debug Clojure
;;       (:prefix ("d" . "debug/inspect")
;;        :desc "debug" "d" #'cider-debug-defun-at-point
;;        (:prefix ("i" . "inspect")
;;         :desc "last expression" "e" #'cider-inspect-last-sexp
;;         :desc "expression" "f" #'cider-inspect-defun-at-point
;;         :desc "inspector" "i" #'cider-inspect
;;         :desc "last result" "l" #'cider-inspect-last-result
;;         (:prefix ("p" . "portal")
;;          :desc "Clear" "c" #'portal.api/open
;;          :desc "Clear" "D" #'portal.api/close
;;          :desc "Open" "p" #'portal.api/open)
;;         :desc "value" "v" #'cider-inspect-expr))

;;       ;; Evaluation
;;       (:prefix "e"
;;        :desc "Expression to comment" ";" #'cider-eval-defun-to-comment
;;        ;; :desc "" "e$" #'spacemacs/cider-eval-sexp-end-of-line
;;        :desc "at point" "(" #'cider-eval-list-at-point
;;        :desc "buffer" "b" #'cider-eval-buffer
;;        "D" nil  ; Doom: send to repl
;;        :desc "prev expression" "e" #'cider-eval-last-sexp
;;        :desc "expresion" "f" #'cider-eval-defun-at-point
;;        :desc "interupt" "i" #'cider-interrupt
;;        ;; :desc "" "el" #'spacemacs/cider-eval-sexp-end-of-line
;;        :desc "macroexpand" "m" #'cider-macroexpand-1
;;        :desc "macroexpand all" "M" #'cider-macroexpand-all
;;        :desc "region" "r" #'cider-eval-region
;;        :desc "undefine" "u" #'cider-undef
;;        :desc "undefine" "U" #'cider-undef-all
;;        :desc "expresion at point" "v" #'cider-eval-sexp-at-point
;;        :desc "expresion upto point" "V" #'cider-eval-sexp-up-to-point
;;        :desc "replace with result" "w" #'cider-eval-last-sexp-and-replace)

;;       ;; Format Clojure
;;       (:prefix ("=" . "format")
;;        :desc "buffer" "=" #'cider-format-buffer
;;        :desc "region" "r" #'cider-format-region
;;        :desc "expression" "f" #'cider-format-defun
;;        (:prefix ("e" . "edn")
;;         :desc "expression" "b" #'cider-format-edn-buffer
;;         :desc "prev expression" "e" #'cider-format-edn-last-sexp
;;         :desc "expression" "r" #'cider-format-edn-region))

;;       ;; Goto / jump
;;       (:prefix ("g" . "goto/jump")
;;        :desc "pop back" "b" #'cider-pop-back
;;        :desc "classpath" "c" #'cider-classpath
;;        ;; :desc "Find var" "c" #'spacemacs/clj-find-var
;;        :desc "find ns" "n" #'cider-find-ns
;;        :desc "error" "e" #'cider-jump-to-compilation-error
;;        :desc "resource" "r" #'cider-find-resource
;;        :desc "spec" "s" #'cider-browse-spec
;;        :desc "spec All" "S" #'cider-browse-spec-all)

;;       ;; Help & Documentation
;;       (:prefix ("h" . "help")
;;        :desc "apropos" "a" #'cider-apropos
;;        :desc "cheetsheet" "c" #'cider-cheatsheet
;;        :desc "clojure docs" "d" #'cider-clojuredocs
;;        :desc "javadoc" "j" #'cider-javadoc
;;        :desc "browse ns" "n" #'cider-browse-ns
;;        :desc "browse all ns" "N" #'cider-browse-ns-all
;;        :desc "browse spec" "s" #'cider-browse-spec
;;        :desc "browse all spe" "S" #'cider-browse-spec-all)

;;       ;; Evaluation - Namespaces
;;       (:prefix ("n" . "namespace")
;;        :desc "reload all" "a" #'cider-ns-reload-all
;;        :desc "" "n" #'cider-eval-ns-form
;;        :desc "" "r" #'cider-ns-refresh
;;        :desc "" "l" #'cider-ns-reload
;;        :desc "" "L" #'cider-ns-reload-all)

;;       ;; Evaluation - Pretty print
;;       (:prefix ("n" . "Pretty print")
;;        :desc "Expression comment" ";" #'cider-pprint-eval-defun-to-comment
;;        :desc "Preceeding expresion comment" ":" #'cider-pprint-eval-last-sexp-to-comment
;;        :desc "Expression" "f" #'cider-pprint-eval-defun-at-point
;;        :desc "Preceeding Expression" "e" #'cider-pprint-eval-last-sexp)

;;       ;; Refactor - Doom clj-refactor hydra menu
;;       (:prefix-map ("R" . nil))

;;       ;; REPL Sesison management
;;       (:prefix ("s" . "REPL Session")
;;        ;; :desc "toggle buffer" "a" (if (eq m 'cider-repl-mode) 'cider-switch-to-last-clojure-buffer 'cider-switch-to-repl-buffer)
;;        :desc "Browse Session" "b" #'sesman-browser
;;        :desc "Goto Session" "g" #'sesman-goto
;;        :desc "Session Info" "i" #'sesman-info
;;        :desc "quit" "q" #'sesman-quit
;;        :desc "quit session" "Q" #'sesman-quit-session
;;        :desc "restart" "r" #'sesman-restart
;;        :desc "start Session" "s" #'sesman-start

;;        (:prefix ("l" . "Link Sessions")
;;         :desc "buffer" "b" #'sesman-link-with-buffer
;;         :desc "directory" "d" #'sesman-link-with-directory
;;         :desc "project" "p" #'sesman-link-with-project
;;         :desc "project" "s" #'cider-connect-sibling-clj
;;         :desc "project" "S" #'cider-connect-sibling-cljs
;;         :desc "unlink" "u" #'sesman-unlink))

;;       ;; Testing
;;       (:prefix ("t" . "Testing")
;;        :desc "loaded" "l" #'cider-test-run-loaded-tests
;;        :desc "namespace" "n" #'cider-test-run-ns-tests
;;        :desc "project" "p" #'cider-test-run-project-tests
;;        :desc "filters" "s" #'cider-test-run-ns-tests-with-filters
;;        :desc "show report" "S" #'cider-test-show-report
;;        :desc "filters" "r" #'cider-test-rerun-failed-tests
;;        :desc "filters" "R" #'cider-test-rerun-test
;;        :desc "test" "t" #'cider-test-run-test)

;;       (:prefix ("T" . "Toggle")
;;        :desc "auto-test" "a" #'cider-auto-test-mode
;;        :desc "enlightenment" "e" #'cider-enlighten-mode
;;        :desc "namespace" "n" #'cider-test-run-ns-tests
;;        :desc "project" "p" #'cider-test-run-project-tests
;;        :desc "filters" "s" #'cider-test-run-ns-tests-with-filters
;;        :desc "show report" "S" #'cider-test-show-report
;;        :desc "filters" "r" #'cider-test-rerun-failed-tests
;;        :desc "filters" "R" #'cider-test-rerun-test
;;        :desc "test" "t" #'cider-test-run-test))

;; Kaocha test runner from CIDER - Requires running REPL
;; next prefix expressions for key sequence, i.e. `SPC t k'
;; (map! :after kaocha-runner
;;       :map clojure-mode-map
;;       :localleader
;;       (:prefix "t"
;;                (:prefix ("k". "Kaocha")
;;                 :desc "Run current test" "t" #'kaocha-runner-run-test-at-point
;;                 :desc "Run test" "r" #'kaocha-runner-run-tests
;;                 :desc "Run all tests" "a" #'kaocha-runner-run-all-tests
;;                 :desc "Runner Warnings" "w" #'kaocha-runner-show-warnings
;;                 :desc "Kaocha Runner" "h" #'kaocha-runner-hide-windows)))

;;;; global map

(map!
 (:after
  consult
  ;; C-c bindings (mode-specific-map
  "C-c m" #'consult-mode-command
  "C-c b" #'consult-bookmark
  "C-c k" #'consult-kmacro
  ;; C-x bindings (ctl-x-map
  "C-x M-:" #'consult-complex-command ;; orig#'repeat-complex-command
  "C-x b" #'consult-buffer ;; orig#'switch-to-buffer
  "C-x 4 b" #'consult-buffer-other-window ;; orig#'switch-to-buffer-other-window
  "C-x 5 b" #'consult-buffer-other-frame ;; orig#'switch-to-buffer-other-frame
  ;; Custom M-# bindings for fast register access
  "M-#" #'consult-register-load
  "M-'" #'consult-register-store ;; orig#'abbrev-prefix-mark (unrelated
  ;; ("C-M-#" #'consult-register ; ugly
  "M-`" #'consult-register ; default tmm-menubar
  ;; Other custom bindings
  "M-y" #'consult-yank-pop ;; orig#'yank-pop
  ;; M-g bindings (goto-map
  "M-g E" #'consult-compile-error
  "M-g f" #'consult-flymake ;; Alternative: consult-flycheck
  "M-g g" #'consult-goto-line ;; orig#'goto-line
  ;; ("M-g M-g" #'consult-goto-line           ;; orig#'goto-line
  "M-g o" #'consult-outline ;; Alternative: consult-org-heading
  "M-g m" #'consult-mark
  "M-g k" #'consult-global-mark
  "M-g i" #'consult-imenu
  "M-g I" #'consult-imenu-multi
  ;; M-s bindings (search-map
  "M-s b" #'consult-buffer
  "M-s f" #'consult-find
  "M-s F" #'my/consult-fd
  "M-s L" #'consult-locate
  ;; "M-s g" #'consult-grep
  "M-s G" #'consult-git-grep
  "M-s K" #'consult-git-log-grep
  ;; "M-s r" #'consult-ripgrep
  "M-s i" #'consult-info
  "M-s l" #'consult-line
  "M-s m" #'consult-line-multi
  "M-s k" #'consult-keep-lines
  "M-s u" #'consult-focus-lines
  ;; Isearch integration
  "M-s e" #'consult-isearch-history
  ;; :map minibuffer-local-map ("M-r" #''consult-history ; doom's default C-s
  ;; :map read-expression-map ("M-r" #''consult-history
  (:map isearch-mode-map
        "M-e" #'consult-isearch-history ;; orig#'isearch-edit-string
        "M-s e" #'consult-isearch-history ;; orig#'isearch-edit-string
        "M-s l" #'consult-line)))


;;;; smartparens-mode-map

;; Doom's Default - /modules/config/default/+emacs-bindings.el
(map!
 (:after smartparens
  :map smartparens-mode-map
  "C-M-a"           #'sp-beginning-of-sexp
  "C-M-e"           #'sp-end-of-sexp
  "C-M-f"           #'sp-forward-sexp
  "C-M-b"           #'sp-backward-sexp
  "C-M-n"           #'sp-next-sexp
  "C-M-p"           #'sp-previous-sexp
  "C-M-u"           #'sp-up-sexp
  "C-M-d"           #'sp-down-sexp
  "C-M-k"           #'sp-kill-sexp
  "C-M-t"           #'sp-transpose-sexp
  "C-M-<backspace>" #'sp-splice-sexp

  "C-<right>" #'sp-forward-slurp-sexp
  "C-<left>" #'sp-forward-barf-sexp
  "M-<left>" #'sp-backward-slurp-sexp
  "M-<right>" #'sp-backward-barf-sexp

  "M-<up>"  #'sp-splice-sexp-killing-backward
  "M-<down>" #'sp-splice-sexp-killing-forward

  "C-c (" #'sp-wrap-round))
;; "C-c [" #'sp-wrap-square ; conflict org-mode-map
;; "C-c {" #'sp-wrap-curly


;;;; treemacs - f8

(when (modulep! :ui treemacs)
  (map!
   "<f8>"   #'+treemacs/toggle
   "<C-f8>" #'treemacs-find-file
   "<M-f8>" #'treemacs-select-window
   (:map treemacs-mode-map
         "." #'consult-line)
   (:map evil-treemacs-state-map
         "." #'consult-line)))

;;;; imenu-list - f9

(after! imenu-list
  (map!
   "<f9>"   #'imenu-list-smart-toggle
   "<M-f9>" #'spacemacs/imenu-list-smart-focus))

;;;; dired-mode-map

(map! :map dired-mode-map
      :n "r" #'revert-buffer
      :inv "M-\\" #'other-window

      :localleader
      "h" #'dired-omit-mode
      "SPC" #'dired-hide-details-mode
      "H" #'dired-hide-details-mode
      "p" #'dired-preview-mode
      :desc "sort-modified-date" "o" #'dired-sort-toggle-or-edit

      ;; "m" #'my/dired-attach-to-mastodon

      :desc "*denote-insert* marked-notes" "i" #'my/denote-link-dired-marked-notes
      "g" #'prot-dired-grep-marked-files
      "l" #'prot-dired-limit-regexp

      "y" #'+default/yank-buffer-absolute-path

      :desc "*denote-rename* files" "r" #'denote-dired-rename-files
      :desc "*denote-rename* using front-matter" "R" #'denote-dired-rename-marked-files-using-front-matter
      :desc "*denote-rename* with keywords" "w" #'denote-dired-rename-marked-files-with-keywords
      :desc "*denote-rename* add keywords" "k" #'denote-dired-rename-marked-files-add-keywords
      :desc "*denote-rename* remove keywords" "K" #'denote-dired-rename-marked-files-remove-keywords

      :desc "*casual-dired* menu" ";" #'casual-dired-tmenu
      "-" #'nerd-icons-dired-mode
      "P" #'my/dired-hugo-export-wim-to-md
      :desc "denote-map" "n" ews-denote-map
      "M" #'my/diff-mark-toggle-vc-modified
      "m" #'my/diff-hl-dired-mark-modified
      )
;; (:prefix ("y" . "copy")
;;          )

;;;; python-mode-map

;; jupyter-repl-associate-buffer

(after! python
  (map! :after python
        (:map python-ts-mode-map
        ;; :in "<C-f9>" #'treemacs-find-file
        ;; :in "<M-f9>" #'treemacs-select-window
        :i "M-9" #'insert-parentheses
        ;; :i "M-j" #'bk/improve-last-parens
        :i "M-l" #'sp-forward-sexp
        "M-RET" #'code-cells-eval)
        :localleader
        :map python-ts-mode-map
        "'" #'jupyter-repl-associate-buffer
        (:prefix ("j" . "jupyter")
                 "c" #'my/jupyter-connect-repl
                 "C" #'my/jupyter-cleanup-kernels
                 "r" #'my/jupyter-refresh-kernelspecs
                 "R" #'my/jupyter-refesh-langs
                 "l" #'my/list-jupyter-kernel-files)
        ;; #'my/select-jupyter-kernel
        ;; #'my/insert-jupyter-kernel
        ;; #'my/jupyter-qtconsole
        ;; #'my/jupyter-eval-region
        ;; #'my/org-babel-jupyter-strip-ansi-escapes-block
        ;; #'my/org-src-block-jupyter-eval-line-or-region

        (:prefix ("h" . "help")
                 "l" #'pylookup-lookup
                 "h" #'pylookup-lookup-at-point)
        (:prefix ("c" . "code-cells")
                 "a" #'code-cells-eval-above
                 "c" #'code-cells-eval
                 "b" #'code-cells-backward-cell
                 "f" #'code-cells-forward-cell))
  )

;; (after! python
;;   (map! :after python
;;         :localleader
;;         :map python-mode-map
;;         (:prefix ("t" . "test")
;;                  "a" #'python-pytest
;;                  "f" #'python-pytest-file-dwim
;;                  "F" #'python-pytest-file
;;                  "t" #'python-pytest-function-dwim
;;                  "T" #'python-pytest-function
;;                  "r" #'python-pytest-repeat
;;                  "p" #'python-pytest-dispatch)
;;         (:prefix ("e" . "pipenv")
;;          :desc "activate"    "a" #'pipenv-activate
;;          :desc "deactivate"  "d" #'pipenv-deactivate
;;          :desc "install"     "i" #'pipenv-install
;;          :desc "lock"        "l" #'pipenv-lock
;;          :desc "open module" "o" #'pipenv-open
;;          :desc "run"         "r" #'pipenv-run
;;          :desc "shell"       "s" #'pipenv-shell
;;          :desc "uninstall"   "u" #'pipenv-uninstall)
;;         (:prefix ("i" . "imports")
;;          :desc "Insert missing imports" "i" #'pyimport-insert-missing
;;          :desc "Remove unused imports"  "R" #'pyimport-remove-unused
;;          :desc "Optimize imports"       "o" #'+python/optimize-imports)
;;         ;; (:prefix ("g" . "conda")
;;         ;;          "a" #'conda-env-activate
;;         ;;          "d" #'conda-env-deactivate
;;         ;;          "l" #'conda-env-list
;;         ;;          "t" #'conda-env-autoactivate-mode)
;;         )
;;   )

;;;; csv-mode-map

(map! :after csv-mode
      :localleader
      :map csv-mode-map
      "a" #'csv-align-fields
      "d" #'csv-kill-fields
      "u" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "S" #'csv-sort-numeric-fields
      "k" #'csv-kill-fields
      "t" #'csv-transpose
      "h"  #'csv-header-line
      "i"  #'csv-toggle-invisibility
      "n"  #'csv-forward-field
      "p"  #'csv-backward-field
      "r"  #'csv-reverse-region
      "y" #'csv-yank-fields
      "Y" #'csv-yank-as-new-table)


;;;; Info-mode-map

(map! :map Info-mode-map
      :after info
      :n "M-," #'Info-history-back
      :n "M-." #'Info-history-forward
      :n "^" #'Info-up
      :n "C-n" #'Info-forward-node
      :n "C-p" #'Info-backward-node
      :n ">" #'Info-next
      :n "<" #'Info-prev
      :n "]" #'Info-next-reference
      :n "[" #'Info-prev-reference
      :n "H" #'Info-top-node
      :n "~" #'Info-directory [remap consult-imenu] #'Info-toc)

;;;; chatgpt-shell

;; (map!
;;  :map chatgpt-shell-mode-map
;;  :i "RET"
;;  #'+default/newline
;;  :i
;;  "M-<return>" #'shell-maker-submit
;;  :i "M-RET"
;;  #'shell-maker-submit
;;  :i
;;  "M-." #'dictionary-lookup-definition
;;  :i "C-c C-l"
;;  #'chatgpt-shell-clear-buffer
;;  (:localleader
;;   "p"
;;   #'chatgpt-shell-swap-system-prompt
;;   "m"
;;   #'chatgpt-shell-swap-model-version)
;;  :map comint-mode-map
;;  "C-c C-l" #'comint-clear-buffer)

;;;; osm-mode-map

(map! :map osm-mode-map
      :n
      ;; Navigation
      "h" #'osm-left
      "l" #'osm-right
      "j" #'osm-down
      "k" #'osm-up
      "H" #'osm-left-left
      "L" #'osm-right-right
      "J" #'osm-down-down
      "K" #'osm-up-up
      ;; Positioning
      "+" #'osm-zoom-in
      "-" #'osm-zoom-out
      "c" #'osm-center
      "g"  #'osm-home
      "r" #'revert-buffer
      ;; Url
      "u" #'osm-save-url
      "y" #'org-store-link
      ;; Other
      "s" #'osm-search
      "X" #'osm-gpx-hide
      "q" #'quit-window)

;;;; cdlatex

;; Using cdlatex's snippets despite having yasnippet
;; (map! :map cdlatex-mode-map
;;       :i "TAB" #'cdlatex-tab)

(after! org
  (map! :map org-cdlatex-mode-map
        "`" nil)) ; cdlatex-math-symbol


(map! :after latex
      :map cdlatex-mode-map
      :localleader
      :desc "Insert math symbol"
      "i" #'cdlatex-math-symbol
      :desc "Begin environment"
      "e" #'cdlatex-environment)

;;;; DONT anddo

;; (after! anddo
;;   (map! :map anddo-mode-map
;;         :n "n"   'anddo-new-item
;;         :n "e"   'anddo-edit-item
;;         :n "s"   'anddo-change-status
;;         :n "r"   'anddo-toggle-listing-mode
;;         :n "D"   'anddo-delete-item
;;         :n "l"   'anddo-show-body
;;         :n "<RET>" 'anddo-show-body
;;         :n "q" 'casual-anddo-tmenu
;;         :n "Q" 'kill-buffer-and-window
;;         :localleader
;;         "n"   'anddo-new-item
;;         "e"   'anddo-edit-item
;;         "s"   'anddo-change-status
;;         "r"   'anddo-toggle-listing-mode
;;         "D"   'anddo-delete-item
;;         "l"   'anddo-show-body
;;         "<RET>" 'anddo-show-body))

;;;; citar-denote embark-citation-map

(map!
 :after (embark citar-denote)
 (:map citar-embark-citation-map
       "1" #'citar-denote-find-citation ; really useful
       "2" #'citar-denote-open-note
       "3" #'citar-open-entry))

;;;; vertico-map

(map! :map vertico-map
      ;; "C-'" #'vertico-quick-insert
      ;; "C-h" #'vertico-directory-delete-word
      ;; "C-c C-g" #'vertico-grid-mode
      ;; "M-h" #'vertico-grid-left
      ;; "M-l" #'vertico-grid-right

      "M-j" #'vertico-next
      "M-k" #'vertico-previous
      "M-v" #'toggle-input-method
      "M-g" #'toggle-input-method
      "`"   #'toggle-input-method
      "M-8" #'tempel-insert
      "M-*" #'tempel-insert)
;; "M-S-j" #'vertico-scroll-up
;; "M-S-k" #'vertico-scroll-down

;; "C-e" #'vertico-scroll-up
;; "C-y" #'vertico-scroll-down
;; "]" #'vertico-next-group
;; "[" #'vertico-previous-group
;; "~" #'vertico-jump-to-home-dir-on~
;; "C-/" #'vertico-jump-root
;; "C-?" #'vertico-jump-sudo
;; "M-m" #'embark-select
;; "C-S-SPC" #'embark-preview+


;;;;  vterm-mode-map

(after! vterm
  (defun my/vterm-send-alt-return ()
    "Send <alt>-<return> to vterm."
    (interactive)
    (vterm-send-key "" nil t))

  (setq vterm-always-compile-module t)

  (undefine-key! vterm-mode-map
    "M-," "M-e" "M-." "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9" "M-0")

  ;; 한글 "ㅑ" 키를 evil의 i처럼 동작시키기 (vterm 버퍼에서 normal/visual → insert)
  (evil-define-key 'normal vterm-mode-map (kbd "ㅑ") #'evil-insert-state)
  (evil-define-key 'visual vterm-mode-map (kbd "ㅑ") #'evil-insert-state)

  (map! :map vterm-mode-map
        :i "M-RET" #'my/vterm-send-alt-return
        :inv "M-y" #'vterm-yank-pop
        :inv "M-\\" #'other-window
        :inv "M-z" #'evil-collection-vterm-toggle-send-escape
        :inv "M-u" #'evil-scroll-up
        :inv "M-v" #'evil-scroll-down)

  ;; vterm에서 evil-normal/visual 상태일 때 한글 자판을 영어 키보드 자판으로 해석하기
  (defvar my/vterm-hangul-to-english-alist
    '(("ㅂ" . "q")
      ("ㅈ" . "w")
      ("ㄷ" . "e")
      ("ㄱ" . "r")
      ("ㅅ" . "t")
      ("ㅛ" . "y")
      ("ㅕ" . "u")
      ("ㅑ" . "i")
      ("ㅐ" . "o")
      ("ㅔ" . "p")
      ("ㅁ" . "a")
      ("ㄴ" . "s")
      ("ㅇ" . "d")
      ("ㄹ" . "f")
      ("ㅎ" . "g")
      ("ㅗ" . "h")
      ("ㅓ" . "j")
      ("ㅏ" . "k")
      ("ㅣ" . "l")
      ("ㅋ" . "z")
      ("ㅌ" . "x")
      ("ㅊ" . "c")
      ("ㅍ" . "v")
      ("ㅠ" . "b")
      ("ㅜ" . "n")
      ("ㅡ" . "m"))
    "Hangul → English key translation table for vterm + evil normal/visual states.")

  (defvar-local my/vterm-original-key-translation-map nil)
  (defvar-local my/vterm-hangul-key-translation-map nil)

  (defun my/vterm-build-hangul-translation-map ()
    "Build a key-translation map that interprets Hangul keys as English keys."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (or my/vterm-original-key-translation-map
                                 (default-value 'key-translation-map)))
      (dolist (pair my/vterm-hangul-to-english-alist)
        (define-key map (kbd (car pair)) (kbd (cdr pair))))
      map))

  (defun my/vterm-init-hangul-translation ()
    "Initialize Hangul → English translation maps for this vterm buffer."
    (setq-local my/vterm-original-key-translation-map key-translation-map)
    (setq-local my/vterm-hangul-key-translation-map
                (my/vterm-build-hangul-translation-map)))

  (defun my/vterm-enable-hangul-translation ()
    "Enable Hangul → English key translation in this vterm buffer."
    (when (and (derived-mode-p 'vterm-mode)
               my/vterm-hangul-key-translation-map)
      (setq-local key-translation-map my/vterm-hangul-key-translation-map)))

  (defun my/vterm-disable-hangul-translation ()
    "Disable Hangul → English key translation in this vterm buffer."
    (when (derived-mode-p 'vterm-mode)
      (setq-local key-translation-map my/vterm-original-key-translation-map)))

  (add-hook 'vterm-mode-hook #'my/vterm-init-hangul-translation)

  (add-hook 'evil-normal-state-entry-hook #'my/vterm-enable-hangul-translation)
  (add-hook 'evil-visual-state-entry-hook #'my/vterm-enable-hangul-translation)
  (add-hook 'evil-motion-state-entry-hook #'my/vterm-enable-hangul-translation)

  (add-hook 'evil-insert-state-entry-hook #'my/vterm-disable-hangul-translation)
  (add-hook 'evil-emacs-state-entry-hook  #'my/vterm-disable-hangul-translation)
  )

;;;; prog-mode-map

(map! :map prog-mode-map
      :inv "M-\\" #'other-window
      )

;;;; outli-mode-map / markdown-mode-map

(after! outli
  (map! :map outli-mode-map
        :nv "M-j" #'outline-forward-same-level
        :nv "M-k" #'outline-backward-same-level
        :nv "M-n" #'outline-next-heading
        :nv "M-p" #'outline-previous-heading
        :nv "C-S-p" #'outline-up-heading
        :nv "z u"   #'outline-up-heading))
;; :i M-j default-indent-new-line


;;;; evil-markdown-mode-map

(after! evil-markdown
  (map! :map evil-markdown-mode-map
        :nv "M-j" #'markdown-outline-next-same-level
        :nv "M-k" #'markdown-outline-previous-same-level
        :nv "M-n" #'markdown-outline-next
        :nv "M-p" #'markdown-outline-previous
        :nv "C-S-p" #'outline-up-heading
        :nv "z u"   #'outline-up-heading))


;;;; imenu-list-mode-map

(after! imenu-list
  (map! :map imenu-list-major-mode-map
        :n "f"      #'hs-toggle-hiding
        :n "g"      #'imenu-list-refresh
        :n "r"      #'imenu-list-refresh
        :n "d"      #'imenu-list-display-dwim
        :n "RET"    #'imenu-list-ret-dwim
        :n "u"      #'imenu-list-up-level
        :n "z u"    #'imenu-list-up-level ; outline-up-heading
        :n "^"      #'imenu-list-up-level  ; dired style
        :n "C-S-p"  #'imenu-list-up-level  ; sync org-mode markdown-mode
        :n "M-j"    #'imenu-list-next-entry-same-level
        :n "M-k"    #'imenu-list-previous-entry-same-level
        :n "M-n"    #'evil-next-line
        :n "M-p"    #'evil-previous-line))

;;;; leetcode

(map!
 (:after leetcode
         (:map leetcode-solution-mode-map
          :g "C-4" #'leetcode-try
          :g "C-5" #'leetcode-restore-layout
          :g "C-7" #'leetcode-submit)))


;;;; Magit

;; Use `,,` to close a commit message and `,k' to cancel
;; Doom maps `ZZ` to commit, `ZQ' to quit
(map! :after magit
      :map magit-status-mode-map
      "M-RET" #'magit-diff-visit-file-other-window
      ;; :localleader
      ;; "o" #'magit-diff-visit-file-other-window
      ;; "f" #'my/magit-log-follow-current-file
      :map text-mode-map
      :localleader
      "," #'with-editor-finish
      "k" #'with-editor-cancel)

;;;; org

;;;;; after! org

(after! org
  (message "after org - doomkeys")

;;;;; org-mode-map

  (map! :map org-mode-map
        ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
        ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
        ;; that, and should probably be PRed to org.
        ;; [tab]        #'org-cycle
        "M-9" #'denote-link
        ;; "<f12>" #'org-transclusion-mode
        "C-M-y" #'org-rich-yank
        ;; "C-M-Y" #'cae-org-rich-yank
        "s-p" #'org-hugo-export-to-md ;; "M-p"
        "M-n" #'org-next-link
        "M-p" #'org-previous-link
        "C-c d"  #'cape-dict
        :i "<tab>" #'my/denote-try-to-complete-then-cycle ; denote-link
        ;; :i "<tab>"  #'completion-at-point ; 2025-02-03
        ;; :i "TAB"  #'completion-at-point
        ;; :n "ds" #'orgbox-schedule
        "M--" #'denote-find-backlink)

  ;; "C-c C-S-l"  #'+org/remove-link
  ;; "C-c C-i"    #'org-toggle-inline-images
  ;; ;; textmate-esque newline insertion
  ;; "S-RET"      #'+org/shift-return
  ;; "C-RET"      #'+org/insert-item-below
  ;; "C-S-RET"    #'+org/insert-item-above
  ;; "C-M-RET"    #'org-insert-subheading
  ;; [C-return]   #'+org/insert-item-below
  ;; [C-S-return] #'+org/insert-item-above
  ;; [C-M-return] #'org-insert-subheading
  ;; (:when (featurep :system 'macos)
  ;;   [s-return]   #'+org/insert-item-below
  ;;   [s-S-return] #'+org/insert-item-above
  ;;   [s-M-return] #'org-insert-subheading)
  ;; ;; Org-aware C-a/C-e
  ;; [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
  ;; [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line


;;;;; localleader 1

  (map! :map org-mode-map
        :localleader
        (:prefix ("RET" . "LLM")
                 "RET" #'gptel-mode
                 "SPC" #'gptel-menu)

        "#" #'org-update-statistics-cookies
        "'" #'org-edit-special
        "*" #'org-ctrl-c-star
        "+" #'org-ctrl-c-minus
        "," #'org-switchb
        "." #'org-goto
        "@" #'org-cite-insert
        "y" #'yank-as-markdown
        "C" #'my/toggle-comment-for-en-paragraph
        ;; #'my/find-headings-by-tag-rgrep

        (:when (modulep! :completion vertico)
          "." #'consult-org-heading
          "/" #'consult-org-agenda)
        "A" #'org-archive-subtree-default
        "e" #'org-export-dispatch
        "f" #'org-footnote-action
        "h" #'org-toggle-heading
        "i" #'org-toggle-item
        "I" #'org-id-get-create
        "k" #'org-babel-remove-result
        "K" #'+org/remove-result-blocks
        "n" #'org-store-link
        "o" #'org-set-property
        "q" #'org-set-tags-command
        "t" #'org-todo
        "T" #'org-todo-list
        "x" #'org-toggle-checkbox
        "V" #'org-marked-text-overview-mode
        (:prefix ("a" . "attachments")
                 "a" #'org-attach
                 "d" #'org-attach-delete-one
                 "D" #'org-attach-delete-all
                 "l" #'+org/attach-file-and-insert-link
                 "f" #'my/consult-org-screenshot
                 "F" #'+vertico/consult-fd-or-find
                 ;; "F" #'+org/find-file-in-attachments
                 "n" #'org-attach-new
                 "o" #'org-attach-open
                 "O" #'org-attach-open-in-emacs
                 "r" #'org-attach-reveal
                 "R" #'org-attach-reveal-in-emacs
                 "u" #'org-attach-url
                 "s" #'org-attach-set-directory
                 "S" #'org-attach-sync
                 (:when (modulep! +dragndrop)
                   "c" #'org-download-screenshot
                   "p" #'org-download-clipboard
                   "P" #'org-download-yank))
        (:prefix ("b" . "tables")
                 "-" #'org-table-insert-hline
                 "w" #'org-table-wrap-region
                 "j" #'+org/table-insert-row-below
                 "a" #'org-table-align
                 "b" #'org-table-blank-field
                 "c" #'org-table-create-or-convert-from-region
                 "e" #'org-table-edit-field
                 "f" #'org-table-edit-formulas
                 "h" #'org-table-field-info
                 "s" #'org-table-sort-lines
                 "r" #'org-table-recalculate
                 "R" #'org-table-recalculate-buffer-tables
                 (:prefix ("d" . "delete")
                          "c" #'org-table-delete-column
                          "r" #'org-table-kill-row)
                 (:prefix ("i" . "insert")
                          "c" #'org-table-insert-column
                          "h" #'org-table-insert-hline
                          "r" #'org-table-insert-row
                          "H" #'org-table-hline-and-move)
                 (:prefix ("t" . "toggle")
                          "f" #'org-table-toggle-formula-debugger
                          "o" #'org-table-toggle-coordinate-overlays)
                 (:when (modulep! +gnuplot)
                   "p" #'org-plot/gnuplot))
        (:prefix ("c" . "clock")
                 "c" #'org-clock-cancel
                 "d" #'org-clock-mark-default-task
                 "e" #'org-clock-modify-effort-estimate
                 "E" #'org-set-effort
                 "g" #'org-clock-goto
                 "G" (cmd! (org-clock-goto 'select))
                 "l" #'+org/toggle-last-clock
                 "i" #'org-clock-in
                 "I" #'org-clock-in-last
                 "o" #'org-clock-out
                 "r" #'org-resolve-clocks
                 "R" #'org-clock-report
                 "t" #'org-evaluate-time-range
                 "=" #'org-clock-timestamps-up
                 "-" #'org-clock-timestamps-down)
        (:prefix ("d" . "date/deadline")
                 "d" #'org-deadline
                 "s" #'org-schedule
                 "t" #'org-time-stamp
                 "T" #'org-time-stamp-inactive)
        (:prefix ("g" . "goto")
                 "g" #'org-goto
                 (:when (modulep! :completion ivy)
                   "g" #'counsel-org-goto
                   "G" #'counsel-org-goto-all)
                 (:when (modulep! :completion helm)
                   "g" #'helm-org-in-buffer-headings
                   "G" #'helm-org-agenda-files-headings)
                 (:when (modulep! :completion vertico)
                   "g" #'consult-org-heading
                   "G" #'consult-org-agenda)
                 "c" #'org-clock-goto
                 "C" (cmd! (org-clock-goto 'select))
                 "i" #'org-id-goto
                 "r" #'org-refile-goto-last-stored
                 "v" #'+org/goto-visible
                 "x" #'org-capture-goto-last-stored)
        (:prefix ("l" . "links")
                 "c" #'org-cliplink
                 "d" #'+org/remove-link
                 "i" #'org-id-store-link
                 "l" #'org-insert-link
                 "L" #'org-insert-all-links
                 "s" #'org-store-link
                 "S" #'org-insert-last-stored-link
                 "t" #'org-toggle-link-display
                 (:when (modulep! :os macos)
                   "g" #'org-mac-link-get-link))
        (:prefix ("P" . "publish")
                 "a" #'org-publish-all
                 "f" #'org-publish-current-file
                 "p" #'org-publish
                 "P" #'org-publish-current-project
                 "s" #'org-publish-sitemap)
        (:prefix ("r" . "refile")
                 "." #'+org/refile-to-current-file
                 "c" #'+org/refile-to-running-clock
                 "l" #'+org/refile-to-last-location
                 "f" #'+org/refile-to-file
                 "o" #'+org/refile-to-other-window
                 "O" #'+org/refile-to-other-buffer
                 "v" #'+org/refile-to-visible
                 "d" #'my/refile-heading-to-denote-file
                 "r" #'org-refile
                 "R" #'org-refile-reverse) ; to all `org-refile-targets'
        (:prefix ("s" . "tree/subtree")
                 "a" #'org-toggle-archive-tag
                 "b" #'org-tree-to-indirect-buffer
                 "c" #'org-clone-subtree-with-time-shift
                 "d" #'org-cut-subtree
                 "h" #'org-promote-subtree
                 "j" #'org-move-subtree-down
                 "k" #'org-move-subtree-up
                 "l" #'org-demote-subtree
                 "n" #'org-narrow-to-subtree
                 "r" #'org-refile
                 "s" #'org-sparse-tree
                 "A" #'org-archive-subtree-default
                 "N" #'widen
                 "S" #'org-sort)
        (:prefix ("p" . "priority")
                 "d" #'org-priority-down
                 "p" #'org-priority
                 "u" #'org-priority-up))

;;;;; localleader 2

  (map! :map org-mode-map
        :localleader
        "o" nil
        ;; :desc "@note-map" "n" ews-note-map
        :desc "@denote-map" "n" ews-denote-map
        ;; :desc "@org-noter-map" "o" ews-org-noter-map
        "o" #'my/denote-howmish-find-file

        ";" #'my/clear-nbsp-and-ascii-punctuations
        ":" #'my/insert-nbsp-simple-all
        ;; ":" #'my/insert-nbsp-all-with-wordlist-and-tags
        "M-;" #'my/add-to-glossary
        "M-'" #'my/add-newlines-between-paragraphs
        "M-l" #'my/link-to-headline

        :desc "org-set-effot" "E" #'org-set-effort
        :desc "time-stamp" "1" #'time-stamp
        :desc "insert-inactive-timestamp" "2" #'bh/insert-inactive-timestamp
        "m" #'org-babel-tangle
        :desc "focus-mode" "3" #'focus-mode
        :desc "org-appear-mode" "4" #'org-appear-mode

        :desc "insert checkbox\|bracket" "]" #'cae-org-insert-checkbox-or-bracket
        :desc "convert syntax to lower" "L" #'cae-org-syntax-convert-keyword-case-to-lower

        ;; l links
        :desc "cae-org-insert-file-link" "l f" #'cae-org-insert-file-link
        :desc "my/org-store-link-id-optional" "l I" #'my/org-store-link-id-optional
        "l h" #'my/link-to-headline

        :desc "org-paste-subtree" "s p" #'org-paste-subtree
        :desc "org-rich-yank" "l y" #'org-rich-yank
        ;; :desc "cae-org-rich-yank" "l Y" #'cae-org-rich-yank
        :desc "update statistics cookies" "#" #'org-update-statistics-cookies
        :desc "rename-file-and-buffer" "R" #'my/rename-file-and-buffer

        :desc "ox-reveal: export > html" "PR" #'org-reveal-export-to-html
        :desc "ox-re-reveal: export > html" "Pr" #'org-re-reveal-export-to-html
        :desc "ox-hugo: export > md" "Ph" #'org-hugo-export-to-md

        ;; math
        ;; :desc "math-preview-at-point" "/" #'math-preview-at-point
        ;; :desc "math-preview-all" "M-/" #'math-preview-all
        ;; :desc "math-preview-clear-all" "C-M-/" #'math-preview-clear-all

        :desc "math-symbol-list" "C-0" #'math-symbol-list
        (:prefix ("0" . "@custom")
                 "c" 'my/genfile-timestamp
                 "b" 'palimpsest-move-region-to-bottom
                 "B" 'palimpsest-move-region-to-top
                 "d" 'my/get-file-line
                 "e" 'my/get-file-link
                 "f" 'my/encode-buffer-to-utf8
                 "g" 'my/copy-word
                 "h" 'my/copy-line
                 "i" 'my/copy-paragraph
                 "j" 'my/copy-buffer
                 "k" 'my/backward-last-edit
                 "t" 'my/org-titlecase-level-1
                 "l" 'my/buffer-edit-hook
                 "R" 'my/rename-file-and-buffer
                 "n" 'my/grep-find
                 "o" 'my/open-external
                 "p" 'my/open-external-pdf
                 "q" 'my/unfill-paragraph-or-region
                 "0" 'cc-todo-item)

        (:prefix ("-" . "translate-mode")
                 "t" 'translate-mode
                 "p" 'translate/translate-current-reference-paragraph
                 "w" 'translate/translate-word-at-point
                 "f" 'translate-open-reference-file
                 "b" 'translate-select-reference-buffer
                 "h" 'translate-toggle-highlight)
        )

;;;;; after! org-journal

  (message "after org-journal - doomkeys")
  (require 'org-journal)

  (map! (:map org-journal-mode-map
         :n "]f"  #'org-journal-next-entry
         :n "[f"  #'org-journal-previous-entry
         :n "]b"  #'org-next-block
         :n "[b"  #'org-previous-block
         :n "C-n" #'org-next-visible-heading
         :n "C-p" #'org-previous-visible-heading)
        ;; :n "C-n" #'org-journal-next-entry
        ;; :n "C-p" #'org-journal-previous-entry
        (:map org-journal-search-mode-map
              "C-n" #'org-journal-search-next
              "C-p" #'org-journal-search-previous)
        :localleader
        (:map org-journal-mode-map
              (:prefix ("a" . "attachments"))
              (:prefix ("b" . "tables"))
              (:prefix ("c" . "clock"))
              (:prefix ("d" . "date/deadline"))
              (:prefix ("p" . "priority"))
              (:prefix ("g" . "goto"))
              (:prefix ("s" . "tree/subtree"))
              (:prefix ("r" . "refile"))
              (:prefix ("P" . "publish"))
              (:prefix ("l" . "links"))
              (:prefix ("0" . "@custom"))
              (:prefix ("-" . "translate-mode"))
              (:prefix ("u" . "xxxx"))
              (:prefix ("o" . "xxxx"))
              (:prefix ("n" . "xxxx"))
              (:prefix ("p" . "xxxx"))
              (:prefix ("j" . "journal")
                       "c" #'org-journal-new-entry
                       "d" #'org-journal-new-date-entry
                       "n" #'org-journal-next-entry
                       "p" #'org-journal-previous-entry)
              (:prefix ("S" . "journal-search")
                       "s" #'org-journal-search
                       "f" #'org-journal-search-forever
                       "F" #'org-journal-search-future
                       "w" #'org-journal-search-calendar-week
                       "m" #'org-journal-search-calendar-month
                       "y" #'org-journal-search-calendar-year))
        (:map org-journal-search-mode-map
              "n" #'org-journal-search-next
              "p" #'org-journal-search-prev)))

;;;; embark

;; ~/sync/man/dotsamples/doom/agzam-dot-doom/modules/custom/completion/config.el
(after! embark

  ;; (require 'org)
  ;; (setq embark-cycle-key "C-;"
  ;;       embark-confirm-act-all nil)
  ;; (setq embark-help-key "M-h") ;; doom's C-h

  ;; (setq embark-indicators '(embark-which-key-indicator
  ;;                           embark-highlight-indicator
  ;;                           embark-isearch-highlight-indicator))

  ;; (advice-add #'embark-completing-read-prompter
  ;;             :around #'embark-hide-which-key-indicator)
  (map!
   (:map embark-org-link-map
    :desc "open-at-point-other-window" "o" #'my/org-open-at-point-other-window)
   (:map embark-org-src-block-map
         "=" #'my/org-indent-src-block))

  (map!
   :after embark
   (:map embark-general-map
         ;; "C-<return>" #'embark-dwim
         "m" #'embark-select
         "/" #'+embark-project-search
         "[" #'gptel-quick
         "?" #'gptel-quick
         "C-c C-e" #'+vertico/embark-export-write)
   ;; (:prefix
   ;;  ("x" . "text")
   ;;  "p" #'awesome-switch-to-prev-app-and-type)

   (:map
    embark-file-map
    "O" #'consult-outline
    "x" #'embark-open-externally+
    "1" #'embark-open-externally+
    "5" #'embark-dired-merge-action
    "o" nil
    (:prefix ("o" . "open")
             "j" (embark-split-action find-file evil-window-split)
             "k" (embark-split-action find-file +evil/window-split-and-follow)
             "h" (embark-split-action find-file evil-window-vsplit)
             "l" (embark-split-action find-file +evil/window-vsplit-and-follow)
             "a" (embark-ace-action find-file)))

   (:map
    embark-buffer-map
    "o" nil
    (:prefix ("o" . "open")
             "j" (embark-split-action switch-to-buffer evil-window-split)
             "k" (embark-split-action switch-to-buffer +evil/window-split-and-follow)
             "l" (embark-split-action switch-to-buffer evil-window-vsplit)
             "h" (embark-split-action switch-to-buffer +evil/window-vsplit-and-follow)
             "a" (embark-ace-action switch-to-buffer)))

   (:map
    embark-org-heading-map
    (:prefix ("9" . "denote") ;; TODO add more denote function
     :desc "denote add links" "u" #'denote-add-links))
   ;; (:prefix ("9" . "roam")
   ;;  :desc "add ref" "u" #'roam-ref-add-for-active-tab)


   (:map
    embark-url-map
    "E" #'+default-browse-url
    "e" #'+eww/open-in-other-window2 ;; +eww-browse-url
    "v" #'forge-visit-topic-via-url)


   (:map embark-markdown-link-map
         "E" #'+default-browse-url
         "b" (cmd! () (browse-url (markdown-link-url)))
         "v" #'forge-visit-topic-via-url)


   (:map embark-org-link-map
         "E" #'+default-browse-url
         "e" #'+eww/open-in-other-window
         "b" #'org-open-at-point
         "V" #'+open-link-in-vlc
         "v" #'forge-visit-topic-via-url
         "x" #'embark-open-externally)


   (:map
    embark-collect-mode-map
    :n "[" #'embark-previous-symbol
    :n "]" #'embark-next-symbol
    :n "TAB" #'+embark-collect-outline-cycle
    :n "m" #'embark-select)

   (:map
    (embark-identifier-map
     embark-region-map
     embark-sentence-map
     embark-paragraph-map)
    (:desc "txl-translate" "M-t" #'txl-translate-region-or-paragraph)
    (:prefix
     ("x" . "text")
     :desc "txl-translate" "t" #'txl-translate-region-or-paragraph
     (:prefix ("g" . "google-translate")
      :desc "en->ko" "k" #'google-translate-query-translate-reverse
      :desc "en->ko2" "K" #'+google-translate-en->ko
      :desc "ko->en" "e" #'google-translate-query-translate
      :desc "ko->en2" "E" #'+google-translate-ko->en
      :desc "translate-at-point" "g" #'google-translate-at-point))))

  (add-hook! 'embark-collect-mode-hook
    (defun visual-line-mode-off-h ()
      (visual-line-mode -1)))

  ;; don't ask when killing buffers
  (setq embark-pre-action-hooks
        (cl-remove
         '(kill-buffer embark--confirm)
         embark-pre-action-hooks :test #'equal))

  (defadvice! embark-prev-next-recenter-a ()
    :after #'embark-previous-symbol
    :after #'embark-next-symbol
    (recenter)))


;;; TODO ctl-x maps

;; /home/junghan/sync/man/dotsamples/doom/yqdotfiles-dot-doom-clj/.doom.d/map.el
;; (map!
;;  (:map ctl-x-map
;;        "8" 'ctl-x-8-map
;;        ;; "9" 'ctl-x-9-map
;;        )
;;  )

;;; DONT eat

;; (after! eat
;;   (map! :leader
;;         :desc "Eat in project" "p SPC" #'eat-project
;;         (:prefix ("o" . "open")
;;          :desc "Eat terminal" "0" #'eat
;;          :desc "Eat in project" ")" #'eat-project)

;;         (:prefix ("o1" . "claude")
;;          :desc "Claude Code" "c" #'claude-code
;;          :desc "Claude menu" "m" #'claude-code-transient
;;          :desc "Continue session" "r" #'claude-code-continue
;;          :desc "Kill Claude" "k" #'claude-code-kill)
;;         )
;;   )

;;; end-of-func
