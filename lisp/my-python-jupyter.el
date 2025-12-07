;;; lisp/my-python-jupyter.el -*- lexical-binding: t; -*-

;; https://github.com/emacs-jupyter/jupyter/pull/573/files

;;;; custom jupyter

;;;;; jupyter code completion with corfu

;; dotsamples/vanilla/karthink-dotfiles-popper/lisp/setup-python.el
(with-eval-after-load 'jupyter
  ;; :bind (:map jupyter-repl-interaction-mode-map
  ;;             ("M-i"   . nil)
  ;;             ("C-h ." . jupyter-inspect-at-point))

  ;; Make jupyter's completion work with corfu-show-documentation
  (unless (fboundp 'company-mode)
    (defun company-doc-buffer (&optional string)
      (with-current-buffer (get-buffer-create "*jupyter help*")
        (erase-buffer)
        (markdown-mode)
        (when string
          (save-excursion
            (insert string)
            (visual-line-mode)))
        (current-buffer))))

  (defun jupyter-completion--company-doc-buffer (arg)
    "Send an inspect request for ARG to the kernel.
Use the `company-doc-buffer' to insert the results."
    (let* ((buf (company-doc-buffer))
           (prefix (car (jupyter-code-context 'inspect)))
           (sym (if (string-match ".*\\." prefix)
                    (concat (match-string 0 prefix) arg)
                  arg)))
      (jupyter-inspect sym (length sym) buf)
      (with-current-buffer buf
        (when (> (point-max) (point-min))
          (let ((inhibit-read-only t))
            (remove-text-properties
             (point-min) (point-max) '(read-only))
            (font-lock-mode 1)
            (goto-char (point-min))
            (current-buffer))))))
  )

;;;;; jupyter - repl interaction

;; starterkit/snakemacs-python/main.el
;; sqrt-dotfiles-elfeed/Emacs.org
(progn
  (setq jupyter-repl-echo-eval-p t) ; default nil

  ;; When this minor mode is enabled you may evaluate code from the current
  ;; buffer using the associated REPL (see jupyter-repl-associate-buffer to
  ;; associate a REPL).

  (defun my/jupyter-refresh-kernelspecs ()
    "Refresh Jupyter kernelspecs"
    (interactive)
    (jupyter-available-kernelspecs t))

  (defun my/jupyter-refesh-langs ()
    "Refresh Jupyter languages"
    (interactive)
    (org-babel-jupyter-aliases-from-kernelspecs t))

  (setq my/jupyter-runtime-folder
        (expand-file-name "~/.local/share/jupyter/runtime"))

  (defun my/get-open-ports ()
    (mapcar
     #'string-to-number
     (split-string (shell-command-to-string
                    "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'")
                   "\n")))

  (defun my/list-jupyter-kernel-files ()
    (mapcar
     (lambda (file)
       (cons
        (car file)
        (cdr (assq 'shell_port (json-read-file (car file))))))
     (sort
      (directory-files-and-attributes my/jupyter-runtime-folder
                                      t ".*kernel.*json$")
      (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

  (defun my/select-jupyter-kernel ()
    (let ((ports (my/get-open-ports))
          (files (my/list-jupyter-kernel-files)))
      (completing-read
       "Jupyter kernels: "
       (seq-filter (lambda (file) (member (cdr file) ports)) files))))

  ;; #+PROPERTY: header-args:python :session <path-to-kernel>
  (defun my/insert-jupyter-kernel ()
    "Insert a path to an active Jupyter kernel into the buffer"
    (interactive)
    (insert (my/select-jupyter-kernel)))

  (defun my/jupyter-connect-repl ()
    "Open an emacs-jupyter REPL, connected to a Jupyter kernel"
    (interactive)
    (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

  (defun my/jupyter-qtconsole ()
    "Open Jupyter QtConsole, connected to a Jupyter kernel"
    (interactive)
    (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
                   (file-name-nondirectory (my/select-jupyter-kernel))))

  (defun my/jupyter-cleanup-kernels ()
    (interactive)
    (let* ((ports (my/get-open-ports))
           (files (my/list-jupyter-kernel-files))
           (to-delete
            (seq-filter
             (lambda (file) (not (member (cdr file) ports))) files)))
      (when (and (length> to-delete 0)
                 (y-or-n-p
                  (format "Delete %d files?" (length to-delete))))
        (dolist (file to-delete)
          (delete-file (car file))))))
  )

;;;;; code-cell with python with jupyter

;; https://github.com/emacs-jupyter/jupyter/pull/573#issuecomment-2677444974
;; My config uses the awesome doom emacs, which I highly recommend (note that emacs-jupyter is shipped as part of the org module in doom, so you can choose between enabling that and installing manually). Other than that, I like to use (setq jupyter-repl-echo-eval-p t), which conveniently echoes the code you have run in the REPL, and I also define the following function for sending code chunks from my python files to the REPL (which I borrowed from Hank Greenburg):

(when (locate-library "code-cells")
  (with-eval-after-load 'code-cells
    (when (locate-library "outli")
      (setq code-cells-boundary-regexp "^##\\(#+\\)"))

    ;; <option1>
    ;; (defun my/jupyter-eval-region (beg end)
    ;;   "Evaluate the region between BEG and END."
    ;;   (interactive "r")
    ;;   (let* ((string (buffer-substring beg end))
    ;;          (string (replace-regexp-in-string "\\`[\n]*" "" string)) ; Remove leading empty lines
    ;;          (indent-length (string-match "[^ \t]" string)) ; Find indent length of the first line
    ;;          (unindented-string (replace-regexp-in-string (format "^%s" (make-string indent-length ?\ ))
    ;;                                                       "" string t t))) ; Remove exactly that amount of indentation
    ;;     (jupyter-eval-string unindented-string)))
    ;; (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . my/jupyter-eval-region))

    ;; <option 2>
    ;; see https://github.com/astoff/code-cells.el/issues/22
    ;; (defun gm/jupyter-eval-region (beg end)
    ;;   (jupyter-eval-region nil beg end))
    ;; (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . gm/jupyter-eval-region))

    (let ((map code-cells-mode-map))
      (define-key map (kbd "C-c <up>") 'code-cells-backward-cell)
      (define-key map (kbd "C-c <down>") 'code-cells-forward-cell)
      ;; (define-key map (kbd "M-<up>") 'code-cells-move-cell-up)
      ;; (define-key map (kbd "M-<down>") 'code-cells-move-cell-down)
      ;; (define-key map (kbd "C-c C-c") 'code-cells-eval) ;; default jupyter-eval-line-or-region
      ;; Overriding other minor mode bindings requires some insistence...
      ;; (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval) ; never
      )

    ;; convert to org-mode it's very useful
    (setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
        				   ("pandoc" "--to" "org" "--from" "ipynb")
        				   org-mode))

    ;; (defun my/new-notebook (notebook-name &optional kernel)
    ;;   "Creates an empty notebook in the current directory with an associated kernel."
    ;;   (interactive "sEnter the notebook name: ")
    ;;   (when (file-name-extension notebook-name)
    ;;     (setq notebook-name (file-name-sans-extension notebook-name)))
    ;;   (unless kernel
    ;;     (setq kernel
    ;;           (jupyter-kernelspec-name
    ;;            (jupyter-completing-read-kernelspec))))
    ;;   (unless (executable-find "jupytext")
    ;;     (error "Can't find \"jupytext\""))
    ;;   (let ((notebook-py (concat notebook-name ".py")))
    ;;     (shell-command (concat "touch " notebook-py))
    ;;     (shell-command
    ;;      (concat "jupytext --set-kernel " kernel " " notebook-py))
    ;;     (shell-command (concat "jupytext --to notebook " notebook-py))
    ;;     (shell-command (concat "rm " notebook-py))
    ;;     (message
    ;;      (concat
    ;;       "Notebook successfully created at " notebook-name ".ipynb"))))
    )
  )

;;;; ob-jupyter - ansi block bugfix

(with-eval-after-load 'ob-jupyter
  ;; :bind (:map jupyter-org-interaction-mode-map
  ;;             ("M-i"   . nil)
  ;;             ("C-h ." . jupyter-inspect-at-point))
  ;; Clean up ob-jupyter source block output
  ;; From Henrik Lissner
  (defun my/org-babel-jupyter-strip-ansi-escapes-block ()
    (when (string-match-p "^jupyter-"
                          (nth 0 (org-babel-get-src-block-info)))
      (unless (or
               ;; ...but not while Emacs is exporting an org buffer (where
               ;; `org-display-inline-images' can be awfully slow).
               (bound-and-true-p org-export-current-backend)
               ;; ...and not while tangling org buffers (which happens in a temp
               ;; buffer where `buffer-file-name' is nil).
               (string-match-p "^ \\*temp" (buffer-name)))
        (save-excursion
          (when-let* ((beg (org-babel-where-is-src-block-result))
                      (end (progn (goto-char beg)
                                  (forward-line)
                                  (org-babel-result-end))))
            (ansi-color-apply-on-region (min beg end) (max beg end)))))))

  (add-hook 'org-babel-after-execute-hook
            #'my/org-babel-jupyter-strip-ansi-escapes-block)

  ;; (define-key jupyter-org-interaction-mode-map (kbd "C-c h") nil)
  (define-key jupyter-org-interaction-mode-map (kbd "M-h") 'jupyter-org-hydra/body)

  (setq org-babel-jupyter-resource-directory (concat user-emacs-directory "ob-jupyter"))

  (defun my/org-src-block-jupyter-eval-line-or-region ()
    (interactive)
    (jupyter-org-with-src-block-client
     (call-interactively #'jupyter-eval-line-or-region)))

  ;; (setq org-babel-default-header-args:jupyter-python
  ;;       '((:async . "yes") (:session . "py") (:kernel . "python3")
  ;;         ;; (:tangle . "jupyter-python/tangled.py")
  ;;         ;; (:exports . "both")
  ;;         ))
  )

;;;; provide

(provide 'my-python-jupyter)
