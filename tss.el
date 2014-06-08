;;; tss.el --- provide a interface for auto-complete.el/flymake.el on typescript-mode.

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: typescript, completion
;; URL: https://github.com/aki2o/emacs-tss
;; Version: 0.5.0
;; Package-Requires: ((auto-complete "1.4.0") (json-mode "1.1.0") (log4e "0.2.0") (yaxception "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension is a interface for typescript-tools.
;; This extension provides the following on typescript-mode.
;;  - Auto completion by using auto-complete.el
;;  - Check syntax by using flymake.el
;; 
;; For more infomation,
;; see <https://github.com/aki2o/emacs-tss/blob/master/README.md>

;;; Dependency:
;; 
;; - typescript.el ( see <http://www.typescriptlang.org/> )
;; - typescript-tools ( see <https://github.com/clausreinke/typescript-tools> )
;; - auto-complete.el ( see <https://github.com/auto-complete/auto-complete> )
;; - json-mode.el ( see <https://github.com/joshwnj/json-mode> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'tss)

;;; Configuration:
;; 
;; ;; Key Binding
;; (setq tss-popup-help-key "C-:")
;; (setq tss-jump-to-definition-key "C->")
;; 
;; ;; Make config suit for you. About the config item, see Customization or eval the following sexp.
;; ;; (customize-group "tss")
;; 
;; (tss-config-default)

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "tss-[^\-]" :docstring t)
;; `tss-popup-help-key'
;; Keystroke for popup help about anything at point.
;; `tss-jump-to-definition-key'
;; Keystroke for jump to method definition at point.
;; `tss-enable-modes'
;; Major modes TSS is enabled on.
;; `tss-ac-trigger-command-keys'
;; Keystrokes for doing `ac-start' with self insert.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "tss-[^\-]" :docstring t)
;; `tss-popup-help'
;; Popup help about anything at point.
;; `tss-jump-to-definition'
;; Jump to method definition at point.
;; `tss-run-flymake'
;; Run check by flymake for current buffer.
;; `tss-reload-current-project'
;; Reload project data for current buffer.
;; `tss-restart-current-buffer'
;; Restart TSS for current buffer.
;; `tss-setup-current-buffer'
;; Do setup for using TSS in current buffer.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
;; - typescript-tools ... Version For Typescript v0.9
;; - auto-complete.el ... Version 1.4.0
;; - json-mode.el ... Version 1.1.0
;; - yaxception.el ... Version 0.1
;; - log4e.el ... Version 0.2.0


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'auto-complete)
(require 'json-mode)
(require 'json)
(require 'ring)
(require 'etags)
(require 'flymake)
(require 'eldoc)
(require 'pos-tip nil t)
(require 'log4e)
(require 'yaxception)


(defgroup tss nil
  "Auto completion / Flymake for TypeScript."
  :group 'completion
  :prefix "tss-")

(defcustom tss-popup-help-key nil
  "Keystroke for popup help about anything at point."
  :type 'string
  :group 'tss)

(defcustom tss-jump-to-definition-key nil
  "Keystroke for jump to method definition at point."
  :type 'string
  :group 'tss)

(defcustom tss-enable-modes '(typescript-mode)
  "Major modes TSS is enabled on."
  :type '(repeat symbol)
  :group 'tss)

(defcustom tss-ac-trigger-command-keys '("SPC" "." ":")
  "Keystrokes for doing `ac-start' with self insert."
  :type '(repeat string)
  :group 'tss)


(log4e:deflogger "tss" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                 (error . "error")
                                                 (warn  . "warn")
                                                 (info  . "info")
                                                 (debug . "debug")
                                                 (trace . "trace")))
(tss--log-set-level 'trace)

(yaxception:deferror 'tss-command-not-found nil "Not found 'tss' command")


(defvar tss--builtin-keywords '("var" "function" "module" "class" "interface" "enum" "constructor" "get" "set"
                                "export" "import" "extends" "implements" "declare" "public" "private" "static"
                                "void" "any" "null" "true" "false" "undefined" "number" "boolean" "string"
                                "if" "else" "for" "while" "until" "do" "in" "switch" "case" "default" "with"
                                "return" "new" "break" "continue" "this" "super"))

(defvar tss--proc nil)
(make-variable-buffer-local 'tss--proc)
(defvar tss--server-response nil)
(make-variable-buffer-local 'tss--server-response)
(defvar tss--incomplete-server-response "")
(make-variable-buffer-local 'tss--incomplete-server-response)

(defvar tss--last-ac-start-point 1)
(make-variable-buffer-local 'tss--last-ac-start-point)
(defvar tss--last-ac-candidates nil)
(make-variable-buffer-local 'tss--last-ac-candidates)

(defvar tss--json-response-start-char "")
(make-variable-buffer-local 'tss--json-response-start-char)
(defvar tss--json-response-end-char "")
(make-variable-buffer-local 'tss--json-response-end-char)

(defvar tss--last-send-string-failed-p nil)
(make-variable-buffer-local 'tss--last-send-string-failed-p)
(defvar tss--current-active-p t)
(make-variable-buffer-local 'tss--current-active-p)

(defsubst tss--get-position-argument ()
  (save-excursion
    (widen)
    (format "%d %d"
            (line-number-at-pos)
 			(+ (- (point) (line-beginning-position)) 1))))

(defsubst tss--send-string (proc sendstr)
  (yaxception:$
    (yaxception:try
      (when proc
        (process-send-string proc (concat sendstr "\n"))
        t))
    (yaxception:catch 'error e
      (tss--error "failed send string : %s" (yaxception:get-text e))
      (tss--delete-process t)
      (if (not tss--last-send-string-failed-p)
          (setq tss--last-send-string-failed-p t)
        (setq tss--current-active-p nil)
        (tss--popup-tip (concat "Stopped TSS by errored on TypeScript Services Server.\n"
                                "Maybe it be caused by the incomplete source code in buffer.\n"
                                "At later, execute `tss-restart-current-buffer' for restart TSS."))
        (sleep-for 2))
      nil)))

(defun tss--popup-tip (text)
  (if (and (functionp 'ac-quick-help-use-pos-tip-p)
           (ac-quick-help-use-pos-tip-p))
      (pos-tip-show text 'popup-tip-face nil nil 300 popup-tip-max-width)
    (popup-tip text)))

;;;###autoload
(defun tss-popup-help ()
  "Popup help about anything at point."
  (interactive)
  (yaxception:$
    (yaxception:try
      (when (tss--sync-server)
        (let* ((posarg (tss--get-position-argument))
               (fpath (expand-file-name (buffer-file-name)))
               (cmdstr (format "type %s %s" posarg fpath))
               (ret (tss--get-server-response cmdstr))
               (name (when (listp ret) (cdr (assoc 'fullSymbolName ret))))
               (kind (when (listp ret) (cdr (assoc 'kind ret))))
               (type (when (listp ret) (cdr (assoc 'type ret))))
               (doc (when (listp ret) (cdr (assoc 'docComment ret)))))
          (tss--popup-tip (tss--get-any-document name kind type doc)))))
    (yaxception:catch 'error e
      (message "[TSS] %s" (yaxception:get-text e))
      (tss--error "failed popup help : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun tss-jump-to-definition ()
  "Jump to method definition at point."
  (interactive)
  (yaxception:$
    (yaxception:try
      (when (tss--sync-server)
        (let* ((posarg (tss--get-position-argument))
               (fpath (expand-file-name (buffer-file-name)))
               (cmdstr (format "definition %s %s" posarg fpath))
               (ret (tss--get-server-response cmdstr))
               (deffile (when (listp ret) (cdr (assoc 'file ret))))
               (startloc (when (listp ret) (cdr (assoc 'min ret))))
               (startrow (cond ((vectorp startloc) (elt startloc 0))
                               ((listp startloc)   (cdr (assoc 'line startloc)))))
               (startcol (cond ((vectorp startloc) (elt startloc 1))
                               ((listp startloc)   (cdr (assoc 'character startloc))))))
          (if (or (not deffile)
                  (not (file-exists-p deffile))
                  (not startrow)
                  (not startcol))
              (progn (message "[TSS] Not found definition location at point")
                     (tss--trace "Not found location file[%s] row[%s] col[%s]" deffile startrow startcol))
            (ring-insert find-tag-marker-ring (point-marker))
            (find-file deffile)
            (goto-char (point-min))
            (forward-line (- startrow 1))
            (forward-char (- startcol 1))))))
    (yaxception:catch 'error e
      (message "[TSS] %s" (yaxception:get-text e))
      (tss--error "failed jump to definition : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun tss-run-flymake ()
  "Run check by flymake for current buffer."
  (interactive)
  (yaxception:$
    (yaxception:try
      (when (and (tss--active-p)
                 (tss--exist-process))
        (tss--debug "Start run flymake")
        (tss--sync-server)
        (setq flymake-last-change-time nil)
        (setq flymake-check-start-time (float-time))
        (let* ((ret (tss--get-server-response "showErrors"
                                              :waitsec 2
                                              :response-start-char "["
                                              :response-end-char "]"))
               (errors (when (arrayp ret)
                         (mapcar (lambda (e)
                                   (let* ((file (cdr (assoc 'file e)))
                                          (start (cdr (assoc 'start e)))
                                          (line (cdr (assoc 'line start)))
                                          (col (cdr (or (assoc 'character start)
                                                        (assoc 'col start))))
                                          (text (cdr (assoc 'text e))))
                                     (tss--trace "Found error file[%s] line[%s] col[%s] ... %s" file line col text)
                                     (format "%s (%d,%d): %s" file (or line 0) (or col 0) text)))
                                 ret))))
          (when errors
            (setq flymake-new-err-info (flymake-parse-err-lines flymake-new-err-info errors)))
          (flymake-post-syntax-check 0 "tss")
          (setq flymake-is-running nil))))
    (yaxception:catch 'error e
      (message "[TSS] %s" (yaxception:get-text e))
      (tss--error "failed jump to definition : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun tss-reload-current-project ()
  "Reload project data for current buffer."
  (interactive)
  (yaxception:$
    (yaxception:try
      (when (and (tss--active-p)
                 (tss--exist-process))
        (tss--debug "Start reload current project of %s" (buffer-name))
        (tss--send-string (tss--get-process) "reload")
        (message "[TSS] Finished request reload.")))
    (yaxception:catch 'error e
      (message "[TSS] %s" (yaxception:get-text e))
      (tss--error "failed reload current project : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun tss-restart-current-buffer ()
  "Restart TSS for current buffer."
  (interactive)
  (setq tss--last-send-string-failed-p nil)
  (setq tss--current-active-p t)
  (tss--start-process))

;;;###autoload
(defun tss-setup-current-buffer ()
  "Do setup for using TSS in current buffer."
  (interactive)
  (yaxception:$
    (yaxception:try
      (when (tss--active-p)
        ;; Key binding
        (loop for stroke in tss-ac-trigger-command-keys
              if (not (string= stroke ""))
              do (local-set-key (read-kbd-macro stroke) 'tss--insert-with-ac-trigger-command))
        (when (and (stringp tss-popup-help-key)
                   (not (string= tss-popup-help-key "")))
          (local-set-key (read-kbd-macro tss-popup-help-key) 'tss-popup-help))
        (when (and (stringp tss-jump-to-definition-key)
                   (not (string= tss-jump-to-definition-key "")))
          (local-set-key (read-kbd-macro tss-jump-to-definition-key) 'tss-jump-to-definition))
        ;; For auto-complete
        (setq ac-sources '(ac-source-tss-member
                           ac-source-tss-type
                           ac-source-tss-new
                           ac-source-tss-anything
                           ac-source-tss-keyword))
        (auto-complete-mode t)
        ;; For flymake
        (setq flymake-err-line-patterns '(("\\`\\(.+?\\.ts\\) (\\([0-9]+\\),\\([0-9]+\\)): \\(.+\\)" 1 2 3 4)))
        (flymake-mode t)
        ;; Start TypeScript Services
        (tss--get-process t)
        (tss--info "finished setup for %s" (current-buffer))))
    (yaxception:catch 'error e
      (message "[TSS] Failed setup : %s" (yaxception:get-text e))
      (tss--error "failed setup : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun tss-config-default ()
  "Do setting recommemded configuration."
  ;; Activate auto-complete and setup TSS automatically when open tss-enable-modes buffer.
  (loop for mode in tss-enable-modes
        for hook = (intern-soft (concat (symbol-name mode) "-hook"))
        do (add-to-list 'ac-modes mode)
        if (and hook
                (symbolp hook))
        do (add-hook hook 'tss-setup-current-buffer t))
  ;; Run flymake when save buffer.
  (add-hook 'after-save-hook 'tss-run-flymake t)
  ;; Delete tss process of the buffer when kill buffer.
  (add-hook 'kill-buffer-hook 'tss--delete-process t))


(defvar ac-source-tss-member
  '((candidates . tss--get-ac-member-candidates)
    (prefix . "\\.\\([a-zA-Z0-9_]*\\)")
    (symbol . "m")
    (document . tss--get-ac-document)
    (requires . 0)
    (cache)))

(defvar ac-source-tss-type
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . ": *\\([a-zA-Z0-9_]*\\)")
    (symbol . "t")
    (document . tss--get-ac-document)
    (requires . 0)
    (cache)))

(defvar ac-source-tss-new
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . "new +\\([a-zA-Z0-9_]*\\)")
    (symbol . "c")
    (document . tss--get-ac-document)
    (requires . 0)
    (cache)))

(defvar ac-source-tss-anything
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . "\\(?:^\\|[^a-zA-Z0-9_.]\\) *\\([a-zA-Z0-9_]+\\)")
    (symbol . "a")
    (document . tss--get-ac-document)
    (requires . 1)
    (cache)))

(defvar ac-source-tss-keyword
  '((candidates . tss--get-ac-keyword-candidates)
    (prefix . "\\(?:^\\|[^a-zA-Z0-9_.]\\) *\\([a-zA-Z0-9_]+\\)")
    (symbol . "w")
    (document . tss--get-ac-document)
    (requires . 1)
    (cache)))

(defun tss--insert-with-ac-trigger-command (n)
  (interactive "p")
  (self-insert-command n)
  (auto-complete-1 :triggered 'trigger-key))


(defun tss--active-p ()
  (and tss--current-active-p
       (memq major-mode tss-enable-modes)
       t))

(defun* tss--get-server-response (cmdstr &key waitsec response-start-char response-end-char)
  (when (tss--active-p)
    (tss--debug "Start get server response. cmdstr[%s] waitsec[%s]" cmdstr waitsec)
    (let ((proc (tss--get-process))
          (waiti 0)
          (maxwaiti (* (or waitsec 1) 5)))
      (setq tss--server-response nil)
      (setq tss--incomplete-server-response "")
      (setq tss--json-response-start-char (or response-start-char "{"))
      (setq tss--json-response-end-char (or response-end-char "}"))
      (when (tss--send-string proc cmdstr)
        (tss--trace "Start wait response from server.")
        (while (and (< waiti maxwaiti)
                    (not tss--server-response))
          (accept-process-output proc 0.2 nil t)
          (incf waiti))
        (cond ((not (< waiti maxwaiti))
               (tss--warn "Timeout get response of %s" cmdstr)
               nil)
              (t
               (tss--trace "Got response from server.")
               tss--server-response))))))

(defun* tss--sync-server (&key endpt waitsec)
  (when (tss--active-p)
    (save-restriction
      (widen)
      (let ((proc (tss--get-process))
            (waiti 0)
            (maxwaiti (* (or waitsec 3) 5))
            (cmdstr (concat (format "update %d %s"
                                    (count-lines (point-min)
                                                 (or endpt (point-max)))
                                    (expand-file-name (buffer-file-name))))))
        (tss--debug "Start sync server : %s" cmdstr)
        (when (tss--send-string proc cmdstr)
          (setq tss--server-response nil)
          (setq tss--incomplete-server-response "")
          (setq tss--json-response-start-char "")
          (setq tss--json-response-end-char "")
          (tss--send-string proc (buffer-substring (point-min) (or endpt (point-max))))
          (tss--trace "Start wait sync server.")
          (while (and (< waiti maxwaiti)
                      (not tss--server-response))
            (accept-process-output proc 0.2 nil t)
            (incf waiti))
          (cond ((not (< waiti maxwaiti))
                 (tss--warn "Timeout sync server.")
                 nil)
                (t
                 (tss--trace "Finished sync server.")
                 (eq tss--server-response 'succeed))))))))

(defun tss--get-ac-member-candidates ()
  (tss--trace "start get ac member candidates.")
  (tss--get-ac-candidates t))

(defun tss--get-ac-non-member-candidates ()
  (tss--trace "start get ac non member candidates.")
  (tss--get-ac-candidates nil))

(defun tss--get-ac-candidates (memberp)
  (yaxception:$
    (yaxception:try
      (when (> tss--last-ac-start-point (point-max))
        (setq tss--last-ac-start-point (point-max)))
      (let* ((currpt (point))
             (code (buffer-substring-no-properties currpt tss--last-ac-start-point)))
        (if (and (> currpt tss--last-ac-start-point)
                 (string-match "\\`[a-zA-Z0-9_]+\\'" code))
            (progn (tss--trace "Use last ac candidates. code[%s]" code)
                   tss--last-ac-candidates)
          (tss--trace "Start get ac candidates. code[%s]" code)
          (setq tss--last-ac-start-point currpt)
          (setq tss--last-ac-candidates
                (when (tss--sync-server)
                  (let* ((posarg (tss--get-position-argument))
                         (memberarg (cond (memberp "true")
                                          (t       "false")))
                         (fpath (expand-file-name (buffer-file-name)))
                         (cmdnm (cond (memberp "completions")
                                      (t       "completions-brief")))
                         (cmdstr (format "%s %s %s %s" cmdnm memberarg posarg fpath))
                         (ret (tss--get-server-response cmdstr :waitsec 3))
                         (entries (when (listp ret)
                                    (cdr (assoc 'entries ret)))))
                    (mapcar (lambda (e)
                              (let ((name (cdr (assoc 'name e)))
                                    (kind (cdr (assoc 'kind e)))
                                    (type (cdr (assoc 'type e)))
                                    (doc  (cdr (assoc 'docComment e))))
                                (tss--debug "Got candidate name[%s] kind[%s] type[%s]" name kind type)
                                (propertize name
                                            'tss--ac-cand-kind kind
                                            'tss--ac-cand-type type
                                            'tss--ac-cand-doc doc)))
                            entries)))))))
    (yaxception:catch 'error e
      (message "[TSS] %s" (yaxception:get-text e))
      (tss--error "failed get ac candidates : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e))
      (setq tss--last-ac-candidates nil))))

(defun tss--get-ac-keyword-candidates ()
  (tss--trace "start get ac keyword candidates.")
  (mapcar (lambda (s)
            (tss--debug "Got candidate name[%s] kind[builtin-keyword] type[]" s)
            (propertize s 'tss--ac-cand-kind "builtin-keyword"))
          tss--builtin-keywords))

(defun tss--get-ac-document (selected)
  (yaxception:$
    (yaxception:try
      (if (not (stringp selected))
          (progn (tss--warn "Can't get ac document : Not string is '%s'" selected)
                 "")
        (tss--trace "Start get ac document : %s" selected)
        (let ((kind (get-text-property 0 'tss--ac-cand-kind selected))
              (type (get-text-property 0 'tss--ac-cand-type selected))
              (doc (get-text-property 0 'tss--ac-cand-doc selected)))
          (when (or kind type doc)
            (with-temp-buffer
              (let ((standard-output (current-buffer)))
                (princ (tss--get-any-document selected kind type doc))
                (buffer-string)))))))
    (yaxception:catch 'error e
      (message "[TSS] %s" (yaxception:get-text e))
      (tss--error "failed get ac document : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e))
      "")))

(defun tss--get-any-document (name kind type doc)
  (let* ((kind (or (and kind (format "%s" kind))
                   "unknown"))
         (type (or (and type (format "%s" type))
                   "unknown"))
         (doc (or doc ""))
         (typedesc (cond ((string= kind "builtin-keyword")
                          "")
                         ((string= kind "property")
                          (concat "Type: " (capitalize type) "\n\n"))
                         ((string= kind "method")
                          (concat "Signature: " type "\n\n"))
                         (t
                          (concat "Type: " (capitalize type) "\n\n")))))
    (concat name " is " (upcase kind) ".\n\n" typedesc doc "\n")))


(defun tss--get-process (&optional initializep)
  (or (and (processp tss--proc)
           (eq (process-status (process-name tss--proc)) 'run)
           tss--proc)
      (tss--start-process initializep)))

(defun tss--exist-process ()
  (and (processp tss--proc)
       (process-status (process-name tss--proc))
       t))

(defun tss--delete-process (&optional killp waitp)
  (when (tss--exist-process)
    (yaxception:$
      (yaxception:try
        (cond (killp
               (kill-process tss--proc)
               (when waitp (sleep-for 1)))
              (t
               (process-send-string tss--proc "quit\n")
               (delete-process tss--proc))))
      (yaxception:catch 'error e
        (tss--error "failed delete process : %s" (yaxception:get-text e))))))

(defun tss--start-process (&optional initializep)
  (when (not (executable-find "tss"))
    (yaxception:throw 'tss-command-not-found))
  (tss--trace "Start tss process for %s" (buffer-name))
  (tss--delete-process t t)
  (let* ((fpath (expand-file-name (buffer-file-name)))
         (procnm (format "typescript-service-%s" (buffer-name)))
         (cmdstr (format "tss %s" (shell-quote-argument fpath)))
         (process-connection-type nil)
         (proc (when (file-exists-p fpath)
                 (tss--trace "Do %s" cmdstr)
                 (cond (initializep (message "[TSS] Load '%s' ..." (buffer-name)))
                       (t           (message "[TSS] Reload '%s' ..." (buffer-name))))
                 (start-process-shell-command procnm nil cmdstr)))
         (waiti 0))
    (when proc
      (set-process-filter proc 'tss--receive-server-response)
      (process-query-on-exit-flag proc)
      (setq tss--server-response nil)
      (setq tss--incomplete-server-response "")
      (setq tss--json-response-start-char "")
      (setq tss--json-response-end-char "")
      (while (and (< waiti 50)
                  (not tss--server-response))
        (accept-process-output proc 0.2 nil t)
        (sleep-for 0.2)
        (incf waiti))
      (tss--info "Finished start tss process.")
      (setq tss--proc proc)
      (when (eq tss--server-response 'succeed)
        (cond (initializep (message "[TSS] Loaded '%s'." (buffer-name)))
              (t           (message "[TSS] Reloaded '%s'." (buffer-name))))
        t))))

(defun tss--balance-json-brace-p (str startbrace endbrace)
  (if (or (string= startbrace "")
          (string= endbrace ""))
      t
    (and (string= (substring str 0 1) startbrace)
         (string= (substring str -1) endbrace)
         ;; (let* ((str (replace-regexp-in-string "\".*?[^\\\\]\"" "" str)) ; delete part of some text
         ;;        (str (replace-regexp-in-string "\"\"" "" str))           ; delete part of empty text
         ;;        (str (replace-regexp-in-string "\"[^\"]*\\'" "" str))    ; delete part of imcomplete text
         ;;        (delstartstr (replace-regexp-in-string (regexp-quote startbrace) "" str))
         ;;        (delendstr (replace-regexp-in-string (regexp-quote endbrace) "" str))
         ;;        (len (length str)))
         ;;   (= (- len (length delstartstr))
         ;;      (- len (length delendstr)))))))
         ;; Use json-mode as substitute for self parsing
         (with-temp-buffer
           (insert str)
           (json-mode)
           (goto-char (point-max))
           (ignore-errors (backward-list))
           (= (point) (point-min))))))

(defun tss--receive-server-response (proc res)
  (tss--trace "Received server response.\n%s" res)
  (yaxception:$
    (yaxception:try
      (let* ((buffnm (replace-regexp-in-string "^typescript-service-" "" (process-name proc)))
             (buff (get-buffer buffnm)))
        (when (and (stringp res)
                   (buffer-live-p buff))
          (with-current-buffer buff
            (loop with fpath = (expand-file-name (buffer-file-name))
                  with endre = (rx-to-string `(and bol "\"" (or "loaded" "updated" "added")
                                                   (+ space) ,fpath))
                  for line in (split-string res "[\r\n]+")
                  if (string= line "null")
                  return (progn (tss--debug "Got null response")
                                (setq tss--server-response 'null))
                  if (and (not (string= line ""))
                          (or (not (string= tss--incomplete-server-response ""))
                              (string= (substring line 0 1) tss--json-response-start-char)))
                  return (progn (tss--debug "Got json response : %s" line)
                                (setq tss--incomplete-server-response (concat tss--incomplete-server-response line))
                                (when (tss--balance-json-brace-p tss--incomplete-server-response
                                                                 tss--json-response-start-char
                                                                 tss--json-response-end-char)
                                  (tss--trace "Finished getting json response")
                                  (setq tss--server-response (json-read-from-string tss--incomplete-server-response))
                                  (setq tss--incomplete-server-response "")))
                  if (string-match endre line)
                  return (progn (tss--debug "Got other response : %s" line)
                                (setq tss--server-response 'succeed))
                  if (string-match "\\`\"TSS +\\(.+\\)\"\\'" line)
                  do (tss--handle-err-response (match-string-no-properties 1 line)))))))
    (yaxception:catch 'json-error e
      (tss--warn "failed parse response : %s" (yaxception:get-text e))
      (setq tss--server-response 'failed))
    (yaxception:catch 'error e
      (tss--error "failed receive server response : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e))
      (setq tss--server-response 'failed)
      (message "[TSS] Failed receive TSS response : %s" (yaxception:get-text e)))))

(defun tss--handle-err-response (res)
  (cond ((string= res "closing")
         nil)
        ((string-match "\\`command syntax error:" res)
         nil)
        (t
         (tss--debug "Got error response : %s" res)
         (message "[TSS] %s" res))))


(provide 'tss)
;;; tss.el ends here
