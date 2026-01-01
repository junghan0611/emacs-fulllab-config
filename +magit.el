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

;;; left blank on purpose
