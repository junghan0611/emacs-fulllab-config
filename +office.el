;;; +office.el -*- lexical-binding: t; -*-

;; sample for work spaces configs

;; /home/junghan/sync/man/dotsamples/dotall/ericdallo-nix-doomemacs-clojure/.config/doom/+nubank.el
;; Specific configurations for Nubank (work) environment

;; (let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
;;   (when (file-directory-p nudev-emacs-path)
;;     (add-to-list 'load-path nudev-emacs-path)
;;     (require 'nu nil t)))

;; (add-to-list 'projectile-project-search-path "~/dev/nu/" "~/dev/nu/mini-meta-repo/packages")

;;;; Tramp - Remote Access

;; tramp-prefix "/ssh:host.example.com:".
(progn
  (require 'tramp)
  (tramp-set-completion-function "ssh"
                                 '(
                                   (tramp-parse-sconfig "~/.ssh/config")))

  ;; (defun my/set-git-user-for-tramp ()
  ;;   (when (and (fboundp 'tramp-tramp-file-p)
  ;;              (tramp-tramp-file-p default-directory))
  ;;     (let ((host (tramp-file-name-host (tramp-dissect-file-name default-directory))))
  ;;       (cond
  ;;        ((string-equal host "remote-server")
  ;;         (shell-command "git config user.name 'JungHan Kim'")
  ;;         (shell-command "git config user.email 'jhkim2@goqual.com'"))))))
  ;; (add-hook 'magit-status-mode-hook #'my/set-git-user-for-tramp)

  (defun my/set-git-user (name email)
    "Set Git user.name and user.email in the current directory.
NAME is the Git user.name, and EMAIL is the Git user.email."
    (interactive
     (list
      (read-string "Enter Git user.name: ")
      (read-string "Enter Git user.email: ")))
    (let ((default-directory (or (and (fboundp 'tramp-tramp-file-p)
                                      (tramp-tramp-file-p default-directory)
                                      default-directory)
                                 default-directory)))
      (shell-command (format "git config user.name \"%s\"" name))
      (shell-command (format "git config user.email \"%s\"" email))
      (message "Git user.name and user.email have been set in %s" default-directory)))
  )

;;;; mcp - model context process

(use-package! mcp
  :after gptel
  :config
  (require 'mcp-hub)
  (setq! mcp-log-level 'debug)
  (setq! mcp-log-level 'info)
  (setq mcp-hub-servers
        `(
          ("ddg-search" . (:command "uvx" :args ("duckduckgo-mcp-server")))
          ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
          ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
          ("filesystemgo". (:command "mcp-filesystem-server"
                            :args (
                                   "/home/goqual/sync/sandboxes/claude-desktop/"
                                   "/home/goqual/git/clone/"
                                   )))
          ("ripgrep" . (:command "npx" :args ("-y" "mcp-ripgrep@latest")))
          ("context7" . ( :command "npx"
                                   :args ("-y" "@upstash/context7-mcp@latest")))
          ("shell" . (:command "uvx"
                      :args ("mcp-shell-server")
                      :env (:ALLOW_COMMANDS
                            (concat "ls,cat,pwd,grep,wc,touch,find,cp,mv,echo,"
                                    "emacs,emacsclient,bun,npx,node" ))
                      ))
          ("git" . (:command "uvx" :args ("mcp-server-git")))
          ;; ("nixos" . (:command "uvx" :args ("mcp-nixos")))
          ;; ("docker-mcp" . (:command "uvx" :args ("docker-mcp")))
          ;; ("kagi" . (:command "uvx" :args ("kagimcp") :env (:KAGI_API_KEY ,(auth-info-password (car (auth-source-search :host "kagi.com" :user "apikey"))))))
          ("brave-search" . (:command "npx"
                             :args ("-y" "@modelcontextprotocol/server-brave-search")
                             :env (:BRAVE_API_KEY ,(auth-info-password (car (auth-source-search :host "api.search.brave.com" :user "apikey"))))))
          ("github" . (:command "docker"
                       :args ("run"
                              "--name" "github-mcp"
                              "--interactive"
                              "--rm"
                              "--env"
                              "GITHUB_PERSONAL_ACCESS_TOKEN"
                              "ghcr.io/github/github-mcp-server")
                       :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(auth-info-password (car (auth-source-search :host "github.com" :user "token"))))))
          ;; ("org-mcp" :command "uvx" :args ("--org-dir" ,org-directory))

          ;; ("mermaid" . (:command "npx"
          ;;               :args ("-y" "@peng-shawn/mermaid-mcp-server")))

          ;; ("mermaid" . (:command "/home/goqual/.config/nvm/versions/node/v22.16.0/bin/mermaid-mcp-server"))
          ;; ("qdrant" . (:url "http://localhost:8000/sse"))
          ))

  ;; (mcp-hub-start-all-server)
  )

;;;; other stuff

(when (string= (system-name) "jhkim2-goqual")
  ;;  (keycast-tab-bar-mode +1)

  (my/enable-alice-keyboard-toggle-input-method))

;;;; denote-silo

(after! denote
  (add-to-list 'denote-silo-directories (expand-file-name "~/claude-memory/")))
