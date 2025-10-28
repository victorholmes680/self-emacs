;; init-java.el
;; Java development configuration with Maven support

;; Java Mode - built-in mode, no package needed
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; Maven integration using compilation mode instead of non-existent maven package
;; Alternative: You could use maven-mode if available, but it's not in MELPA
(setq maven-executable "/Users/wangzhixiong/Downloads/apache-maven-3.9.9/bin/mvn")

;; Custom Maven functions for better integration
(defun my-java-mvn-compile ()
  "Run Maven compile in project root."
  (interactive)
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile "mvn compile"))
      (message "No pom.xml found in project hierarchy"))))

(defun my-java-mvn-test ()
  "Run Maven test in project root."
  (interactive)
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile "mvn test"))
      (message "No pom.xml found in project hierarchy"))))

(defun my-java-mvn-package ()
  "Run Maven package in project root."
  (interactive)
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile "mvn package"))
      (message "No pom.xml found in project hierarchy"))))

(defun my-java-mvn-install ()
  "Run Maven install in project root."
  (interactive)
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile "mvn install"))
      (message "No pom.xml found in project hierarchy"))))

(defun my-java-mvn-clean ()
  "Run Maven clean in project root."
  (interactive)
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile "mvn clean"))
      (message "No pom.xml found in project hierarchy"))))

(defun my-java-mvn-custom-task (task)
  "Run custom Maven TASK in project root."
  (interactive (list (read-string "Maven task: " "mvn ")))
  (let ((root (locate-dominating-file default-directory "pom.xml")))
    (if root
        (progn
          (cd root)
          (compile task))
      (message "No pom.xml found in project hierarchy"))))

;; Java LSP Server configuration (Eclipse JDT.LS)
(use-package lsp-java
  :after lsp-mode
  :config
  ;; Set Java home based on your environment
  (setq lsp-java-java-path "/opt/homebrew/Cellar/openjdk/23.0.2/libexec/openjdk.jdk/Contents/Home/bin/java")
  (setq lsp-java-java-home "/opt/homebrew/Cellar/openjdk/23.0.2/libexec/openjdk.jdk/Contents/Home")

  ;; Use Homebrew-installed JDT.LS
  (setq lsp-java-server-install-dir "/opt/homebrew/Cellar/jdtls/1.47.0/libexec")

  ;; Set Maven configuration
  (setq lsp-java-maven-download-sources t
        lsp-java-maven-download-javadoc t
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-signature-help-enabled t
        lsp-java-completion-overwrite t
        lsp-java-max-concurrent-builds 1
        lsp-java-workspace-dir (expand-file-name "~/repo/.lsp-java-workspace/")
        lsp-java-format-enabled t
        lsp-java-format-comments-enabled t
        lsp-java-save-action-organize-imports t)

  ;; Custom Maven settings for JDT.LS
  (setq lsp-java-configuration-maven-user-settings
        (expand-file-name "~/.m2/settings.xml"))

  ;; Configure Maven behavior
  (setq lsp-java-maven-prefer-online-resources nil
        lsp-java-configuration-update-build-configuration t
        lsp-java-java-home "/opt/homebrew/Cellar/openjdk/23.0.2/libexec/openjdk.jdk/Contents/Home")

  ;; Key bindings for Java LSP
  (define-key java-mode-map (kbd "C-c j j") 'lsp-java-add-overridable-methods)
  (define-key java-mode-map (kbd "C-c j t") 'lsp-java-generate-to-string)
  (define-key java-mode-map (kbd "C-c j h") 'lsp-java-generate-hashcode-equals)
  (define-key java-mode-map (kbd "C-c j c") 'lsp-java-generate-constructors)
  (define-key java-mode-map (kbd "C-c j g") 'lsp-java-generate-getters-setters)
  (define-key java-mode-map (kbd "C-c j d") 'lsp-java-generate-delegate)
  (define-key java-mode-map (kbd "C-c j o") 'lsp-java-organize-imports)
  (define-key java-mode-map (kbd "C-c j f") 'lsp-java-format)
  (define-key java-mode-map (kbd "C-c j r") 'lsp-java-rename-symbol))

;; Java import organization
(use-package importmagic
  :commands importmagic-mode
  :config
  (setq importmagic-skip-compression t
        importmagic-auto-import-behavior 'ask)
  (add-hook 'java-mode-hook 'importmagic-mode))

;; Auto-enable Java configuration
(add-hook 'java-mode-hook
          (lambda ()
            ;; Enable LSP for Java
            (when (and (locate-dominating-file default-directory "pom.xml")
                       (fboundp 'lsp-deferred))
              (lsp-deferred))

            ;; Set up key bindings for Maven commands
            (local-set-key (kbd "C-c C-c c") 'my-java-mvn-compile)
            (local-set-key (kbd "C-c C-c t") 'my-java-mvn-test)
            (local-set-key (kbd "C-c C-c p") 'my-java-mvn-package)
            (local-set-key (kbd "C-c C-c i") 'my-java-mvn-install)
            (local-set-key (kbd "C-c C-c k") 'my-java-mvn-clean)
            (local-set-key (kbd "C-c C-c r") 'my-java-mvn-custom-task)

            ;; Java-specific settings
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; DAP mode for debugging (optional)
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :config
  ;; Java DAP configuration
  (require 'dap-java)
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)))

(provide 'init-java)