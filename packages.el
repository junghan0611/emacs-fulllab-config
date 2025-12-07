;; -*- no-byte-compile: t; -*-

;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;;; doom-unpin-packages

;;; DONT consult-omni consult-gh

(package! consult-omni :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))
(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh" :files ("*")))

;;; DONT NEVER use built-in on emacs 30

;; (when (eq emacs-major-version 30)
;;  (package! eldoc :built-in t) ; 2025-02-25 with flycheck
;;  )

;;; doom-disabled-packages

;; (unpin! demangle-mode) ;; 2025-09-30

(disable-packages!
 demangle-mode
 cuda-mode
 opencl-mode
 ;; yasnippet-capf ; too much information
 ;; lsp-mode
 ;; consult-lsp
 diredfl ; conflict denote
 dirvish
 code-review
 ;; nose ; python module
 ;; lsp-python-ms
 flyspell-lazy
 flymake-popon
 vundo
 undo-fu-session
 elfeed-goodies
 solaire-mode
 ace-window
 flycheck-popup-tip) ; conflict
;; (package! flycheck-plantuml :disable t)

(package! emojify :disable t) ; from mastodon

;; (package! corfu-popupinfo :disable t)

(package! evil-snipe :disable t)
;; (package! evil-mc :disable t)

;; Disable tty module
(package! evil-terminal-cursor-changer :disable t) ; conflict on kitty

;;; additional packages

;;;; :os tty

(package! term-keys :recipe (:host github :repo "junghan0611/term-keys"))

;;;; :ui - visual

;; (package! pulsar)
(package! transpose-frame)
(package! hydra)
(package! pretty-hydra)
(package! major-mode-hydra)

(package! modus-themes)
;; (package! ef-themes)
;; (package! doric-themes)

(package! show-font)
(package! hammy) ; cutte timmer

(package! info+)
(package! list-unicode-display)
(package! spacious-padding)

(unpin! doom-themes)
(package! doom-themes :recipe (:host github :repo "junghan0611/doom-themes" :branch "ko"))

(package! keycast)
;; (package! outli :recipe (:host github :repo "jdtsmith/outli" :files ("*.el")))
(package! outli)
(package! fontaine) ; break custom.el
(package! golden-ratio)
(package! mode-minder :recipe (:host github :repo "jdtsmith/mode-minder"))
(package! breadcrumb) ; with eglot
(package! celestial-mode-line)
(package! lin)
;; (package! nerd-icons-dired)
;; (package! nerd-icons-completion) ; 2025-03-26 disable conflict with what?!

(package! dired-preview)

(package! dired+)
;; (package! bookmark+)

;;;; :editor

(package! copy-as-format)
(package! expand-region)
(package! string-inflection)
(package! evil-matchit)
(package! evil-owl) ;; register
(package! tempel)
(package! tempel-collection)
(package! imenu-list :recipe (:host github :repo "junghan0611/imenu-list" :branch "master"))

;; (package! titlecase)
(package! deadgrep)
(package! rg) ; ripgrep
(package! affe)
(package! fzf)
(package! ace-link)
(package! unfill)
(package! translate-mode)
(package! separedit :recipe (:host github :repo "twlz0ne/separedit.el"))
(package! goto-last-change)

;;;; :lang org-mode

;; (package! org-headline-card :recipe (:host github :repo "yibie/org-headline-card")) ; plantuml
;; (package! orgbox)

(package! org-fragtog) ;; interactive toggling of inline latex formulas
(package! org-appear)
(package! orgabilize :recipe (:host github :repo "akirak/orgabilize.el"))

(package! org-cv :recipe (:host github :repo "ohyecloudy/org-cv"))

(package! org-glossary :recipe (:host github :repo "tecosaur/org-glossary" :files ("*.el" "*.org" "*.texi")))
(package! autocorrect :recipe (:host github :repo "tecosaur/autocorrect" :files ("*.el" "*.org")))
(package! org-pandoc-import :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "*.org" "filters" "preprocessors")))
(package! org-web-tools)
(package! org-index) ;; 색인 지원
(package! corg :recipe (:host github :repo "isamert/corg.el"))

(package! org-download)
(package! ox-epub)

(package! org-rainbow-tags)
(package! org-drill)

(package! org-rich-yank)
(package! ox-reveal)
(package! org-transclusion)
(package! org-remark)
(package! org-bookmarks :recipe (:host github :repo "emacsmirror/org-bookmarks"))

(package! ox-leanpub) ;; https://github.com/junghan0611/ox-leanpub

;; (package! ob-mermaid)
(package! mermaid-mode)

(package! org-ql)
(package! org-kanban)

;; (package! org-sliced-images :recipe (:host github :repo "ElleNajt/org-sliced-images"))
;; (package! image-slicing :recipe (:host github :repo "ginqi7/image-slicing"))

(package! parse-csv :recipe (:host github :repo "junghan0611/el-csv")) ; for om-dash
(package! om-dash :recipe (:host github :repo "gavv/om-dash" :files ("*.el" "*.org"))) ; org-based dashboards

;; (package! org-modern-indent :recipe (:host github :repo "jdtsmith/org-modern-indent"))

;; (package! org-bookmark-heading)
;; (package! d2-mode)
;; (package! ob-d2 :recipe (:host github :repo "dmacvicar/ob-d2"))

;; (package! org-linenote) ; require lsp-mode
;; (package! org-linenote :recipe (:local-repo "local/org-linenote"))
(package! org-linenote :recipe (:host github :repo "junghan0611/org-linenote" :branch "main")) ; eglot
;; (package! language-detection) ; html2org

;;;; :tools

;;;;; :tools infrastructure

;; (package! kubernetes)

;;;; :tools writing

(package! hypothesis :recipe (:host github :repo "EFLS/hypothesis"))
(package! side-notes)
(package! redacted)
(package! centered-cursor-mode)

(package! google-translate)
(package! jinx)
(package! logos)
(package! olivetti)
(package! palimpsest)
(package! immersive-translate)
;; (package! immersive-translate :recipe (:local-repo "~/git/clone/emacs-immersive-translate/"))
(package! focus)

(package! isbn :recipe (:host github :repo "cashpw/isbn.el"))

;; (package! quarto-mode :recipe (:host github :repo "quarto-dev/quarto-emacs" )) ; require polymode
;; (package! quarto-mode :pin "a7b974f7d22ef939eaed8b9919434bcf20b1438f")
(package! ox-quarto :recipe (:host github :repo "jrgant/ox-quarto"))

(package! math-preview)

(package! guess-language :recipe (:host github :repo "junghan0611/guess-language.el" :branch "master" :files ("*.el" "trigrams/*")))
;; (package! txl :recipe (:host github :repo "junghan0611/txl.el" :branch "ko"))
(package! txl :recipe (:local-repo "~/emacs/git/junghan0611/txl.el/"))
;; (package! html2org :recipe (:local-repo "~/emacs/git/default/html2org/"))
;; (package! flymake-vale :recipe (:host github :repo "tpeacock19/flymake-vale"))

;;;; :pkm

;;;;; :pkm denote

(package! denote)
(package! denote-org)

(package! denote-silo)
(package! denote-sequence)
(package! denote-markdown)
;; (package! denote-journal)

(package! denote-search)

(package! denote-regexp)

(package! gptel-denote :recipe (:host github :repo "pprevos/gptel-denote"))
(package! denote-explore :recipe (:host github :repo "pprevos/denote-explore"))

(package! consult-notes)

(package! consult-denote)

(package! citar-denote)
(package! citar-org-mode :recipe (:host github :repo "pprevos/citar-org-mode"))
(package! tmr) ; timer

;; (package! ekg)

(package! binder)
(package! astute :recipe (:host github :repo "rnkn/astute"))

(package! ten :recipe (:host sourcehut :repo "nobiot/ten")) ;; https://git.sr.ht/~nobiot/ten
;; (package! chu :recipe (:host sourcehut :repo "nobiot/chu")) ;; https://git.sr.ht/~nobiot/chu
;; (package! obsidian)

;; (package! org-fc
;;   :recipe (:host github
;;            ;; :repo "l3kn/org-fc"
;;            :repo "cashpw/org-fc"
;;            :branch "feat/classes"
;;            :files (:defaults "awk" "demo.org")))
;;;; modules/tools llm

(package! mcp)
(package! yaml) ; for gptel-prompt
(package! templatel) ; for gptel-prompt
(package! gptel-prompt :recipe (:host github :repo "jwiegley/gptel-prompts"))
(package! ob-prompt :recipe (:host github :repo "jwiegley/ob-gptel"))
(package! gptel-rag :recipe (:host github :repo "jwiegley/gptel-rag"))

(package! uuidgen)
(package! gptel-litellm :recipe (:host github :repo "jwiegley/gptel-litellm"))

(package! macher :recipe (:host github :repo "kmontag/macher"))

(package! ragmacs :recipe (:host github :repo "positron-solutions/ragmacs"))

;; (package! gpt-babel :recipe (:host github :repo "ElleNajt/gpt-babel" :branch "main" :files ("*.el")))
;; (package! gpt-babel :recipe (:local-repo "~/git/default/gpt-babel/"))
;; (package! org-auto-tangle :recipe (:local-repo "~/git/default/org-auto-tangle/"))

(package! claude-code-ide :recipe (:host github :repo "manzaltu/claude-code-ide.el" ))
(package! claude-code :recipe (:host github :repo "stevemolitor/claude-code.el" ))
(package! monet :recipe (:host github :repo "stevemolitor/monet" ))

(package! eshell-atuin)
(package! eat :recipe
  (:host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))

;;;; ACP (Agent Client Protocol)

(package! shell-maker)
(package! acp :recipe (:host github :repo "xenodium/acp.el"))
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell"))
(package! agent-shell-manager :recipe (:host github :repo "ElleNajt/agent-shell-manager"))
(package! agent-shell-sidebar :recipe (:host github :repo "cmacrae/agent-shell-sidebar"))

;;;;; extra packages

;; (package! khoj)

;; (package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))

(package! llm)
(package! semext :recipe (:host github :repo "ahyatt/semext"))
(package! pcsv)

(package! elysium)
;; (package! elysium :recipe (:local-repo "~/git/junghan0611/elysium/"))

;; (package! whisper :recipe (:host github :repo "natrys/whisper.el"))
;; (package! ellama)

;; git@github.com:ziova/wolfram.el.git
;; (package! wolfram :recipe (:host github :repo "ziova/wolfram.el"))

;;;; :lang

;;;;; TODO latex snippets

;; (package! aas)
(package! laas)
(package! math-symbol-lists)
(package! adoc-mode)

;;;; Coding

(package! elisp-autofmt)
(package! sideline-blame)
(package! git-messenger)

;; (package! eglot-booster :recipe (:type git :host github :repo "jdtsmith/eglot-booster"))

;; (package! auto-highlight-symbol)
;; (package! symbol-overlay)

;; (when (modulep! :lang clojure)
;;   (package! clojure-mode-extra-font-locking) ;; better looks
;;   (package! kaocha-runner) ; Koacha test runner in Emacs
;;   (package! vega-view)
;;   (package! clj-deps-new)
;;   (package! clojure-essential-ref-nov)
;;   (package! clay))

;; (unpin! jupyter)
;; (package! jupyter :recipe (:host github :repo "junghan0611/emacs-jupyter" :branch "ko"))

(package! docker-compose-mode)

(when (modulep! :lang python)
  (package! pydoc)
  (package! code-cells))

(package! evil-textobj-tree-sitter)

;; Use the latest available packages for Clojure
;; - cider, clojure-mode
;; (unpin! (:lang clojure))

;; (unpin! conda)
;; (package! conda)

(package! devdocs-browser)
(package! aggressive-indent)

;; (package! bats-mode) ; shell-scripts

(package! hy-mode :recipe (:host github :repo "jethack23/hy-mode"))
(package! ob-hy)

;; (package! geiser)
;; (package! geiser-mit :recipe (:host github :repo "emacsmirror/geiser-mit"))

;;;;; DONT python

;; (package! uv-mode :recipe (:host github :repo "z80dev/uv-mode"))
;; (package! uv-menu :recipe (:host github :repo "pizzatorque/uv-menu"))

;; (package! mise :recipe (:host github :repo "eki3z/mise.el"))

;;;; Git

(package! git-link :recipe (:host github :repo "sshaw/git-link"))
(package! git-cliff)
(package! gist)
(package! consult-git-log-grep)
(package! magit-todos)
(package! magit-blame-color-by-age :recipe (:host github :repo "jdtsmith/magit-blame-color-by-age"))


;;;; Reading

(package! tp)
(package! mastodon)
;; (package! adoc-mode)

(package! yeetube :recipe (:host github :repo "Boruch-Baum/emacs-yeetube.el"))
(package! youtube-sub-extractor)

(package! elfeed-tube-mpv)
;; (package! elfeed-webkit) ; not working on ubuntu

(package! browser-hist :recipe (:host github :repo "agzam/browser-hist.el"))

(package! subed :recipe (:host github :repo "sachac/subed" :files ("subed/*.el")))
;; (package! dwim-shell-command)
;; (package! bm) ; visible bookmark

(package! hnreader :recipe (:host github :repo "agzam/emacs-hnreader" :branch "major-mode"))
(package! consult-hn :recipe (:host github :repo "agzam/consult-hn"))
(package! reddigg :recipe (:host github :repo "agzam/emacs-reddigg" :branch "major-mode"))

;; filter marked text out
(package! org-marked-text-overview :recipe (:host github :repo "lijigang/org-marked-text-overview"))

(package! docsim)

;;(package! org-books :recipe (:host github :repo "junghan0611/org-books" :branch "ko"))
(package! org-books :recipe (:local-repo "~/emacs/git/junghan0611/org-books"))

;; (package! org-zettel-ref-mode :recipe (:host github :repo "junghan0611/org-zettel-ref-mode" :branch "ko"))
;; (package! org-zettel-ref-mode :recipe (:local-repo "~/emacs/git/junghan0611/org-zettel-ref-mode/"))

(package! org-supertag :recipe (:host github :repo "yibie/org-supertag")) ; require epc
;; (package! org-supertag :recipe (:local-repo "~/emacs/git/junghan0611/org-supertag/"))
;; (package! org-zotero-client :recipe (:local-repo "~/git/default/zotero-cli/elisp/"))

;;;; Workspace

(package! tabgo)

;;;; Misc

(package! command-log-mode)
(package! atomic-chrome)
(package! empv) ;; TODO mpv frontend
(package! djvu)
(package! calibredb :recipe (:files (:defaults (:exclude "*ivy*" "*helm*"))))
(package! nov)
(package! osm) ; OpenStreetMaps
(package! gif-screencast)
(package! lorem-ipsum)
;; (package! go-translate)
(package! jira)

;; (package! ready-player)
;; Very large files mode loads large files in chunks to open ridiculously large files.
;; (package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
;;   :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)

(package! zoxide)
;; (package! telega) ; telegram

;;;; Transient

;; (package! ccmenu :recipe (:host github :repo "junghan0611/ccmenu"))
(package! ccmenu :recipe (:local-repo "~/emacs/git/junghan0611/ccmenu"))

;; (package! casual-suite :recipe (:host github :repo "kickingvegas/casual-suite"))
(package! casual)
(package! recent-rgrep :recipe (:host github :repo "kickingvegas/recent-rgrep"))

(package! p-search :recipe (:host github :repo "zkry/p-search"))

(package! git-grep-transient)

(package! transient-posframe)

;;;; Forked PKGs

(unless IS-TERMUX
  ;; This is an emacs package which supports OWL Manchester Notation https://www.w3.org/TR/owl2-manchester-syntax/
  ;; (package! omn-mode)
  ;; (package! elot :recipe (:host github :repo "junghan0611/elot"  :branch "ko" :files("elot-package/*")))
  ;; "johanwk/elot"
  ;; (package! elot :recipe (:local-repo "local/elot" :branch "ko" :files("elot-package/*")))

  ;; (package! pylookup :recipe (:host github :repo "junghan0611/pylookup"))
  (package! pylookup :recipe (:local-repo "local/pylookup")))

;; (package! paw :recipe (:local-repo "local/paw" :branch "ko" :files ("*"))))

;; (package! paw :recipe (:host github :repo "junghan0611/paw" :branch "ko" :files ("*")))


;; (package! trekker
;;   :recipe (:host github :repo "junghan0611/trekker" :branch "ko" :files("*.md" "*.el" "*.py")))


;;;; password manager for nerds

(package! password-store-menu)

;;; Applications
;;;; Calculator

(package! literate-calc-mode)
(package! calctex :recipe (:host github
                                 :repo "johnbcoughlin/calctex"
                                 :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

;;;; TODO waiting

;; (package! default-text-scale :recipe (:host github :repo "purcell/default-text-scale"))
;; (package! moc :recipe (:host github :repo "positron-solutions/moc"))
;; (package! dslide :recipe (:host github :repo "positron-solutions/dslide"))
;; (package! anddo :recipe (:host github :repo "junghan0611/anddo.el"))

;; A method for blocking access to emacs commands based on time.
;; https://git.sr.ht/~swflint/time-block-command
;; randomly ask yourself a question to collect productivity data. The primary
;; entry point is the macro define-asker. It may be called as follows
;; https://git.sr.ht/~swflint/random-ask

;;;; choi

;; (package! google-this)
(package! webpaste)

;;;; MCP (Model Context Protocol)

(package! mcp-server-lib :recipe (:host github :repo "laurynas-biveinis/mcp-server-lib.el"))
(package! elisp-dev-mcp :recipe (:host github :repo "laurynas-biveinis/elisp-dev-mcp"))
(package! org-mcp :recipe (:host github :repo "laurynas-biveinis/org-mcp"))

;;;;; misc

(package! consult-jq :recipe (:host github :repo "elken/consult-jq"))
(package! fireplace)
(package! snow)
;; (package! oneko-macs :recipe (:host github :repo "ElleNajt/oneko-macs")) ; sudo apt-get install oneko
;; (package! selectric-mode)

;; (package! wiki-summary :recipe (:host github :repo "rnkn/wiki-summary.el"))
;; (package! wakatime-mode)

;;;; DONT Emacs Application Framework (EAF)

;; (progn
;;   (defun +eaf-install-deps-for-app(app-dir)
;;     "Install deps from dependencies.json."
;;     (let* ((deps-dict (with-temp-buffer
;;                         (insert-file-contents
;;                          (expand-file-name "dependencies.json" app-dir))
;;                         (json-parse-string (buffer-string))))
;;            (pip-deps (gethash (if IS-LINUX "linux" "darwin")
;;                               (or (gethash "pip" deps-dict)
;;                                   (make-hash-table))))
;;            (vue-install (gethash "vue_install" deps-dict))
;;            (npm-install (gethash "npm_install" deps-dict))
;;            (npm-rebuild (gethash "npm_rebuild" deps-dict)))
;;       (when pip-deps
;;         (dolist (pkg (append pip-deps nil))
;;           (message "%s" (shell-command-to-string (format "pip install %s" pkg)))))
;;       (when vue-install
;;         (let ((default-directory app-dir))
;;           (message "%s" (shell-command-to-string "npm install"))
;;           (message "%s" (shell-command-to-string "npm run build"))))
;;       (when npm-install
;;         (let ((default-directory app-dir))
;;           (message "%s" (shell-command-to-string "npm install"))))
;;       (when npm-rebuild
;;         (let ((default-directory app-dir))
;;           (message "%s" (shell-command-to-string "npm rebuild"))))))

;;   (package! eaf
;;     :recipe (:host github :repo "emacs-eaf/emacs-application-framework"
;;              :files ("*")
;;              :post-build
;;              (shell-command "/usr/bin/python install-eaf.py --install-core-deps"))) ;; use builtin python

;;   (package! eaf-browser
;;     :recipe (:host github :repo "emacs-eaf/eaf-browser"
;;              :files ("*")
;;              :post-build
;;              (+eaf-install-deps-for-app
;;               (concat straight-base-dir "/straight/" straight-build-dir "/eaf-browser"))))

;;   (package! eaf-pdf-viewer
;;     :recipe (:host github :repo "emacs-eaf/eaf-pdf-viewer"
;;              :files ("*")
;;              :post-build
;;              (+eaf-install-deps-for-app
;;               (concat straight-base-dir "/straight/" straight-build-dir "/eaf-pdf-viewer"))))

;;   (package! eaf-mind-elixir
;;     :recipe (:host github :repo "emacs-eaf/eaf-mind-elixir"
;;              :files ("*")
;;              :post-build
;;              (+eaf-install-deps-for-app
;;               (concat straight-base-dir "/straight/" straight-build-dir "/eaf-mind-elixir"))))
;;   )


;;; end-of file
