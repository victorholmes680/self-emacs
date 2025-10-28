;; init-lsp.el
;; Language Server Protocol configuration

;; LSP Mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :config
  (setq lsp-auto-guess-root t
        lsp-prefer-capf t
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-semantic-tokens-enable nil
        lsp-enable-indentation nil
        lsp-enable-links nil)

  ;; Performance improvements
  (setq read-process-output-max (* 1024 1024))  ; 1MB
  (setq gc-cons-threshold 100000000)  ; 100MB

  ;; Key bindings
  (define-key lsp-mode-map (kbd "C-c l r r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l g g") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c l g r") 'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-c l a a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l f f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l s s") 'lsp-workspace-symbol)
  )

;; LSP UI - Better UI for LSP
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  ;; Key bindings for LSP UI
  (define-key lsp-ui-mode-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "M-,") 'lsp-ui-peek-find-references)
  )

;; Company mode for completion
(use-package company
  :commands company-mode
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-show-numbers t
        company-transformers '(company-sort-by-statistics))

  ;; Key bindings for company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (global-company-mode))


;; Flycheck for syntax checking
(use-package flycheck
  :commands flycheck-mode
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

;; LSP Treemacs integration
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after (lsp-mode treemacs))

;; Projectile integration with LSP
(defun my-lsp-projectile-find-file ()
  "Find file in project with LSP support."
  (interactive)
  (call-interactively 'projectile-find-file))

;; Auto-enable LSP for programming modes
(dolist (hook '(java-mode-hook
                python-mode-hook
                javascript-mode-hook
                typescript-mode-hook
                js-mode-hook
                web-mode-hook
                css-mode-hook
                go-mode-hook
                rust-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook (lambda ()
                   (when (locate-dominating-file default-directory
                                                 (lambda (dir)
                                                   (or (file-exists-p (expand-file-name "pom.xml" dir))
                                                       (file-exists-p (expand-file-name "build.gradle" dir))
                                                       (file-exists-p (expand-file-name "package.json" dir))
                                                       (file-exists-p (expand-file-name "Cargo.toml" dir))
                                                       (file-exists-p (expand-file-name "go.mod" dir)))))
                     (lsp-deferred)))))

(provide 'init-lsp)