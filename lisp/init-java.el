;; init-java.el
;; Simplified Java development configuration

;; Java Mode basic setup
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; Maven integration
(setq maven-executable "/Users/wangzhixiong/Downloads/apache-maven-3.9.9/bin/mvn")

;; Maven functions
(defun my-java-mvn-compile ()
  "Run Maven compile."
  (interactive)
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile "mvn compile"))
      (message "No pom.xml found"))))

(defun my-java-mvn-test ()
  "Run Maven test."
  (interactive)
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile "mvn test"))
      (message "No pom.xml found"))))

;; Java LSP Server configuration
(use-package lsp-java
  :after lsp-mode
  :config
  ;; Java home
  (setq lsp-java-java-path "/opt/homebrew/Cellar/openjdk/23.0.2/libexec/openjdk.jdk/Contents/Home/bin/java")
  (setq lsp-java-java-home "/opt/homebrew/Cellar/openjdk/23.0.2/libexec/openjdk.jdk/Contents/Home")

  ;; JDT.LS location
  (setq lsp-java-server-install-dir "/opt/homebrew/Cellar/jdtls/1.47.0/libexec")

  ;; Basic settings
  (setq lsp-java-maven-download-sources t
        lsp-java-maven-download-javadoc t
        lsp-java-signature-help-enabled t
        lsp-java-completion-overwrite t
        lsp-java-workspace-dir (expand-file-name "~/.emacs.d/.lsp-java-workspace/")
        lsp-java-format-enabled t
        lsp-java-save-action-organize-imports t)

  ;; Lombok support
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-javaagent:/Users/wangzhixiong/.emacs.d/lombok.jar"
         "-Xbootclasspath/a:/Users/wangzhixiong/.emacs.d/lombok.jar"))

  ;; Java LSP key bindings
  (define-key java-mode-map (kbd "C-c j o") 'lsp-java-organize-imports)
  (define-key java-mode-map (kbd "C-c j f") 'lsp-java-format)
  (define-key java-mode-map (kbd "C-c j g") 'lsp-java-generate-getters-setters))

;; Auto-enable Java configuration
(add-hook 'java-mode-hook
          (lambda ()
            ;; Maven shortcuts
            (local-set-key (kbd "C-c C-c c") 'my-java-mvn-compile)
            (local-set-key (kbd "C-c C-c t") 'my-java-mvn-test)

            ;; LSP shortcuts
            (local-set-key (kbd "C-c .") 'lsp-find-definition)
            (local-set-key (kbd "C-c ,") 'lsp-find-references)
            (local-set-key (kbd "C-c /") 'lsp-rename)
            (local-set-key (kbd "C-c ;") 'lsp-execute-code-action)))

(provide 'init-java)