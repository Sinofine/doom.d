;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Speed up.
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(setq user-full-name "Sinofine Lotusie"
      user-mail-address "i@sinofine.me")

(defmacro add-fs-to-hook (hook &rest funcs)
  "Add functions to hook. A function is either an unquoted token, or a form.
If it's a token, then its treated as a function and enabled. Otherwise, the form is run."
  `(add-hook ,hook
    (fn ,@(mapcar (lambda (el)
                    (if (listp el)
                        el
                      (list el 1)))
                  funcs))))
;; Why?
(defmacro fn (&rest forms)
  (declare (indent 0))
  `(lambda () ,@forms))

(setq confirm-kill-emacs nil)

(map! :leader
      :desc "Open nix folder"
      :n "nix"
      (lambda ()
        (interactive)
        (doom-project-find-file "~/nixconfig")))

(setq doom-theme 'doom-opera)

(setq fancy-splash-image (concat doom-user-dir "logo/hfut.svg"))
(setq +doom-dashboard-banner-padding '(2 . 3))

(setq display-line-numbers-type 'relative)

(set-face-attribute 'line-number nil
                    :italic nil)

(setq default-frame-alist '((width . 90)
                            (height . 50)
                            (alpha-background . 80)))

(pixel-scroll-precision-mode)

(setq +doom-dashboard-menu-sections
      '(
        ("Recently opened files" :icon
         (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Open org-agenda" :icon
         (nerd-icons-octicon "nf-oct-calendar" :face 'doom-dashboard-menu-title)
         :when
         (fboundp 'org-agenda)
         :action org-agenda)
        ("Open telega" :icon
         (nerd-icons-faicon "nf-fae-telegram" :face 'doom-dashboard-menu-title)
         :action telega)
        ("Open project" :icon
         (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Open private configuration" :icon
         (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
         :when
         (file-directory-p doom-user-dir)
         :action doom/open-private-config)
        ))

(cnfonts-mode 1)
(setq cnfonts-personal-fontnames '(("Luxi Mono") ("HYXinRenWenSongW") () ())
      cnfonts-use-face-font-rescale t)
(defun init-cjk-fonts (_fsl)
  ;; (dolist (charset '(kana han cjk-misc bopomofo))
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;     charset (font-spec :family "HYXinRenWenSongW" :size 20)))
  (set-fontset-font (frame-parameter nil 'font) 'emoji (font-spec :family "Segoe UI Emoji"))
  )
(add-hook 'cnfonts-set-font-finish-hook 'init-cjk-fonts)

(define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
(define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

(setq org-export-use-babel nil)

(setq org-confirm-babel-evaluate nil)

(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(setq org-latex-preview-default-process 'dvisvgm
      org-latex-preview-options
      '(:foreground auto
        :background "Transparent"
        :scale 1
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
        :zoom 1.0)
      )

(setq telega-emoji-use-images nil)

(map! :leader :desc "Open telega" :n "ot" 'telega)

(setq telega-avatar-workaround-gaps-for '(return t))
(add-hook 'telega-load-hook
          (lambda ()
            (set-face-attribute 'telega-msg-heading nil
                                :background nil
                                :underline 't
                                :height 1.2
                                )
            (set-face-attribute 'telega-msg-inline-forward nil
                                ;; :background "light gray"
                                :underline nil
                                :height 0.84)
            (set-face-attribute 'telega-msg-inline-reply nil
                                ;; :background "light gray"
                                :underline nil
                                :height 0.84)
            ))

(require 'telega-bridge-bot)
(setq telega-bridge-bot-bridge-info-plist
      '(-1001773572820 ; id of the @emacs_china
        (420415423 ; id of the @matrix_t2bot
         ;; will fetch member info with this matrix room id
         (:type :matrix :chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org"))
        -1001882351848
        (5846938060
         (:type :matrix :chat-id "!uoEcEMNaQYWmDWUQYY:matrix.org")))
      telega-bridge-bot-matrix-user "@sinofine:envs.net")

(setq cdlatex-math-modify-alist '((102 "\\mathfrak" "\\textsf" t nil nil)))

(setq TeX-parse-self t)

(setq pdf-viewer-exec-alist '((sioyek . "Sioyek")
                              (zathura . "Zathura")
                              (evince . "evince")
                              (okular . "Okular")))
(setq my-pdf-viewer (->> pdf-viewer-exec-alist
                         (-first (-compose #'executable-find #'symbol-name #'car))
                         cdr))

(add-fs-to-hook 'LaTeX-mode-hook
                (setq TeX-view-program-selection
                      `((output-pdf ,my-pdf-viewer)
                        (output-dvi ,my-pdf-viewer)
                        (output-html "xdg-open")))
                auto-fill-mode)

(setq xenops-math-image-scale-factor .5)

(custom-set-variables
 '(sage-shell:use-prompt-toolkit nil)
 '(sage-shell:use-simple-prompt t)
 '(sage-shell:set-ipython-version-on-startup nil)
 '(sage-shell:check-ipython-version-on-startup nil))
(sage-shell:define-alias)
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

(setq org-babel-default-header-args:sage
      '((:session . t)
        (:results . "output")))
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c c")
              'ob-sagemath-execute-async))

(defun my-yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))
(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)

(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
