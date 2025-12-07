;;; ../dotemacs/+magit.el -*- lexical-binding: t; -*-

;;;; ENHANCED DIFF-MODE CONFIGURATION

(progn
  (after! diff-mode
    ;; diff-mode 개선 함수들
    (defun my/diff-apply-and-commit ()
      "현재 hunk를 적용하고 선택적으로 커밋합니다."
      (interactive)
      (diff-apply-hunk)
      (when (and (fboundp 'magit-anything-unstaged-p)
                 (magit-anything-unstaged-p)
                 (y-or-n-p "Stage and commit this change? "))
        (call-interactively 'magit-stage-file)
        (magit-commit-create)))

    (defun my/diff-apply-all-and-commit ()
      "모든 hunk를 적용하고 커밋합니다."
      (interactive)
      (when (y-or-n-p "Apply all hunks and commit? ")
        (diff-apply-buffer)
        (when (fboundp 'magit-stage-modified)
          (magit-stage-modified))
        (let ((commit-msg (format "Apply patch: %s"
                                  (file-name-nondirectory (buffer-name)))))
          (magit-commit-create (list "-m" commit-msg)))))

    (defun my/diff-reject-hunk ()
      "현재 hunk를 거부합니다 (reverse apply)."
      (interactive)
      (when (y-or-n-p "Reject this hunk? ")
        (diff-apply-hunk t)))

    (defun my/diff-navigate-and-preview ()
      "다음 hunk로 이동하고 미리보기를 보여줍니다."
      (interactive)
      (diff-hunk-next)
      (diff-goto-source)))

  ;; Macher 전역 키바인딩
  (map! :leader
        (:prefix-map ("-" . "AI/LLM")
         :desc "Macher implement"     "i" #'macher-implement
         :desc "Macher revise"        "r" #'macher-revise
         :desc "Macher discuss"       "d" #'macher-discuss
         :desc "Macher abort"         "a" #'macher-abort
         :desc "GPTel"                "g" #'gptel))

  ;; Diff-mode Evil 키바인딩
  (map! :after diff-mode
        :map diff-mode-map
        :n "RET"     #'diff-goto-source
        :n "o"       #'diff-goto-source
        :n "gd"      #'diff-goto-source
        :n "r"       #'diff-refine-hunk
        :n "R"       #'diff-reverse-direction
        :n "q"       #'quit-window
        :n "ZZ"      #'my/diff-apply-all-and-commit
        :n "ZQ"      #'quit-window

        ;; Hunk 네비게이션 (Vim 스타일)
        :n "j"       #'diff-hunk-next
        :n "k"       #'diff-hunk-prev
        :n "gj"      #'diff-file-next
        :n "gk"      #'diff-file-prev
        :n "J"       #'my/diff-navigate-and-preview
        :n "K"       #'diff-hunk-prev

        ;; Hunk 적용/거부
        :n "a"       #'diff-apply-hunk
        :n "A"       #'diff-apply-buffer
        :n "d"       #'my/diff-reject-hunk
        :n "u"       #'diff-undo

        ;; 편집 및 커밋
        :n "c"       #'my/diff-apply-and-commit
        :n "C"       #'my/diff-apply-all-and-commit
        :n "s"       #'diff-split-hunk
        :n "w"       #'diff-ignore-whitespace-hunk

        ;; Magit 통합
        :n "gs"      #'magit-status
        :n "gc"      #'magit-commit-create
        :n "gS"      #'magit-stage-file

        ;; 로컬 리더 키바인딩
        :localleader
        :desc "Apply hunk"           "a" #'diff-apply-hunk
        :desc "Apply buffer"         "A" #'diff-apply-buffer
        :desc "Reject hunk"          "d" #'my/diff-reject-hunk
        :desc "Commit hunk"          "c" #'my/diff-apply-and-commit
        :desc "Commit all"           "C" #'my/diff-apply-all-and-commit
        :desc "Refine hunk"          "r" #'diff-refine-hunk
        :desc "Reverse direction"    "R" #'diff-reverse-direction
        :desc "Split hunk"           "s" #'diff-split-hunk
        :desc "Goto source"          "g" #'diff-goto-source
        :desc "Ignore whitespace"    "w" #'diff-ignore-whitespace-hunk
        :desc "Ediff patch"          "e" #'diff-ediff-patch)

;;;; MACHER ACTION BUFFER EVIL BINDINGS (ORG-MODE)

  (defun my/setup-macher-action-buffer-bindings ()
    "Macher action buffer용 Evil 키바인딩을 설정합니다."
    (when (string-match-p "\\*macher:" (buffer-name))
      (evil-local-set-key 'normal (kbd "q") 'quit-window)
      (evil-local-set-key 'normal (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
      (evil-local-set-key 'normal (kbd "TAB") 'org-cycle)
      (evil-local-set-key 'normal (kbd "S-TAB") 'org-global-cycle)
      (evil-local-set-key 'normal (kbd "RET") 'org-open-at-point)))

  (add-hook 'macher-action-buffer-setup-hook #'my/setup-macher-action-buffer-bindings)

;;;; HELPER FUNCTIONS

  (defun my/macher-status ()
    "현재 macher 상태를 보여줍니다."
    (interactive)
    (if (get-buffer-window "*macher-patch:*")
        (message "Macher patch buffer is open")
      (message "No active macher session")))

  (defun my/macher-quick-implement ()
    "선택된 텍스트로 빠른 macher 구현 요청을 보냅니다."
    (interactive)
    (if (use-region-p)
        (macher-implement)
      (message "Select text first, then run this command")))

  ;; 추가 편의 키바인딩
  (map! :leader
        (:prefix-map ("-" . "AI/LLM")
         :desc "Quick implement"      "q" #'my/macher-quick-implement
         :desc "Macher status"        "s" #'my/macher-status))
  )

;;; Advanced macher + Magit Integration

;; (progn
;; (use-package! macher
;;   :after gptel magit
;;   :commands (macher-implement macher-revise macher-discuss macher-abort)
;;   :custom
;;   ;; 'org' UI는 코드 블록과 설명을 구조화하기에 매우 좋습니다.
;;   (macher-action-buffer-ui 'org)
;;   :config
;;   (macher-install))

;;   (require 'magit)
;;   (require 'magit-section)
;;   (require 'magit-git)

;;   ;;;--------------------------------------------------------------------------
;;   ;;; Macher-Magit Workflow Integration
;;   ;;;--------------------------------------------------------------------------

;;   (defun my/macher-apply-patch-to-magit (patch-content)
;;     "Apply PATCH-CONTENT to the Git staging area via stdin.
;; This avoids creating temporary files."
;;     (let ((default-directory (projectile-project-root))
;;           (process-connection-type nil)) ; Prevent creating a new buffer for output
;;       (with-temp-buffer
;;         (insert patch-content)
;;         ;; `call-process-region` is ideal for piping buffer content to a command.
;;         ;; It sends the region (here, the whole buffer) to git's stdin.
;;         (let ((exit-code (call-process-region (point-min) (point-max) "git" t nil nil "apply" "--cached" "-")))
;;           (unless (zerop exit-code)
;;             (error "Macher: Failed to apply patch. Git exit code: %s" exit-code))
;;           (message "📋 Macher patch applied to staging area. Review in Magit.")))))

;;   (defun my/macher-magit-workflow ()
;;     "Handle a ready macher patch by applying it to Magit's staging area.
;; This function is designed to be added to `macher-patch-ready-hook`."
;;     (interactive)
;;     (let ((patch-content (buffer-string)))
;;       (my/macher-apply-patch-to-magit patch-content)
;;       ;; After applying, bring up Magit for review.
;;       (magit-status)))

;;   (defun my/macher-smart-magit-workflow-for-hook ()
;;     "Detect project type and run the appropriate Magit workflow.
;; This function is intended for `macher-patch-ready-hook`."
;;     (my/macher-magit-workflow))

;;   ;; (defun my/macher-nixos-magit-workflow ()
;;   ;;   "Custom macher patch workflow for NixOS projects."
;;   ;;   (interactive)
;;   ;;   (let ((patch-content (buffer-string))
;;   ;;         (patch-id (or (and (string-match "# Patch ID: \\([a-zA-Z0-9]+\\)" patch-content)
;;   ;;                              (match-string 1 patch-content))
;;   ;;                         (format-time-string "%Y%m%d-%H%M%S"))))
;;   ;;     (my/macher-apply-patch-to-magit patch-content)
;;   ;;     (magit-status)
;;   ;;     ;; Optionally prepare a commit message.
;;   ;;     (when (y-or-n-p "Macher: Prepare commit message automatically? ")
;;   ;;       (let ((commit-msg (format "nixos: macher patch %s\n\nApplied via macher-implement + magit workflow.\n\n%s"
;;   ;;                                 patch-id
;;   ;;                                 (if (< (length patch-content) 800)
;;   ;;                                     (replace-regexp-in-string "^" "> " patch-content)
;;   ;;                                   "> Large patch - see macher history for details."))))
;;   ;;         ;; This is a robust way to set the commit message in Magit.
;;   ;;         (require 'magit-git)
;;   ;;         (magit-git-insert-message commit-msg)))))

;;   ;; (defun my/macher-smart-magit-workflow-for-hook ()
;;   ;;     "Detect project type and run the appropriate Magit workflow.
;;   ;; This function is intended for `macher-patch-ready-hook`."
;;   ;;     (cond
;;   ;;      NixOS Project Detection
;;   ;;      ((or (string-prefix-p "nixos" (projectile-project-name))
;;   ;;           (string-prefix-p "nix" (projectile-project-name))
;;   ;;           (file-exists-p (expand-file-name "flake.nix" (projectile-project-root))))
;;   ;;       (my/macher-nixos-magit-workflow))
;;   ;;      ;; Default Project
;;   ;;      (t
;;   ;;       (my/macher-magit-workflow))))

;;   ;; Use the standard hook for better integration.
;;   ;; (add-hook 'macher-patch-ready-hook #'my/macher-smart-magit-workflow-for-hook)
;;   (add-hook 'macher-after-action-functions #'my/macher-smart-magit-workflow-for-hook)

;;   ;;--------------------------------------------------------------------------
;;   ;; Magit Hunk/Region Commands
;;   ;;--------------------------------------------------------------------------


;;   (defun my/magit-macher-implement-region ()
;;     "Run `macher-implement` on the selected region or current hunk in Magit."
;;     (interactive)
;;     (if-let ((section (magit-section-at-point)))
;;         (cond
;;          ((magit-hunk-section-p section)
;;           (let ((content (magit-section-content section)))
;;             ;; Use a temporary buffer to pass the hunk content to macher
;;             (with-temp-buffer
;;               (insert content)
;;               (macher-implement (buffer-substring-no-properties (point-min) (point-max))))))
;;          ((magit-file-section-p section)
;;           (let ((file (magit-section-value section)))
;;             (with-current-buffer (find-file-noselect file)
;;               (call-interactively #'macher-implement))))
;;          (t (call-interactively #'macher-implement)))
;;       (call-interactively #'macher-implement)))

;;   (defun my/magit-macher-revise-hunk ()
;;     "Request a `macher-revise` on the current hunk."
;;     (interactive)
;;     (when-let* ((section (magit-section-at-point))
;;                 (_ (magit-hunk-section-p section)))
;;       (let* ((hunk-content (magit-section-content section))
;;              (file-name (magit-section-parent-value section))
;;              (prompt-content (format "File: %s\n\nCurrent hunk:\n%s\n\nRevision request: "
;;                                      file-name hunk-content)))
;;         (macher-revise prompt-content))))

;;   ;;--------------------------------------------------------------------------
;;   ;; Keybindings
;;   ;;--------------------------------------------------------------------------

;;   (map! :leader
;;         :prefix "g"
;;         (:prefix ("m" . "macher")
;;          :desc "Macher implement"         "i" #'macher-implement
;;          ;; :desc "Macher implement region"  "I" #'my/magit-macher-implement-region
;;          :desc "Macher revise"            "r" #'macher-revise
;;          ;; :desc "Macher revise hunk"       "R" #'my/magit-macher-revise-hunk
;;          :desc "Macher discuss"           "d" #'macher-discuss
;;          :desc "Macher abort"             "q" #'macher-abort))

;;   (map! :after magit
;;         :map magit-mode-map
;;         :localleader
;;         :prefix ("m" . "macher")
;;         :desc "Implement from hunk" "i" #'my/magit-macher-implement-region
;;         :desc "Revise hunk"         "r" #'my/magit-macher-revise-hunk
;;         :desc "Discuss changes"     "d" #'macher-discuss)

;;   ;; For evil users, these are very convenient in magit-status-mode
;;   (map! :after evil-magit
;;         :map magit-mode-map
;;         :n "M-i" #'my/magit-macher-implement-region
;;         :n "M-r" #'my/magit-macher-revise-hunk)

;;   ;; ===================================================================
;;   ;; Macher 액션 버퍼(org-mode)를 위한 LocalLeader 키 바인딩
;;   ;; ===================================================================
;;   (map! :map diff-mode-map
;;         :localleader
;;         ;; 가장 중요한 '수정/재요청' 기능
;;         (:desc "Revise implementation" "r" #'macher-revise)
;;         ;; 새로운 요청 시작
;;         (:desc "New implementation" "i" #'macher-implement)
;;         ;; 토론/질문
;;         (:desc "Discuss context" "d" #'macher-discuss)
;;         ;; 작업 중단
;;         (:desc "Abort macher" "q" #'macher-abort))

;;   (message "🤖 Advanced Macher + Magit integration loaded successfully!")
;;   )

;;; left blank on purpose
