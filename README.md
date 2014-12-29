[![Build Status](https://travis-ci.org/aki2o/emacs-tss.svg?branch=master)](https://travis-ci.org/aki2o/emacs-tss)

# TSS - **T**ype**S**cript **S**upport for Emacs

This is a extension of Emacs that provides completion/syntax-check by using typescript-tools in typescrript-mode.  

About typescript-tools, see <https://github.com/clausreinke/typescript-tools>

# Feature

### Auto completion by auto-complete.el

![demo1](image/demo1.png)

### Popup help by popup.el

![demo2](image/demo2.png)

### Echo method signature by eldoc.el

![eldoc](image/eldoc.png)

### Check syntax by flymake.el

![flymake](image/flymake.png)

### Jump to definition

### Implement inherit definition

![impl](image/impl.gif)

# Requirement

- [typescript-tools](https://github.com/clausreinke/typescript-tools)

# Install

### If use package.el

2013/09/10 It's available by using melpa.  

### If use el-get.el

2013/08/10 Not yet available.  
2013/09/08 It's available. But, master branch only.  

### If use auto-install.el

```lisp
(auto-install-from-url "https://raw.github.com/aki2o/emacs-tss/master/tss.el")
(auto-install-from-url "https://raw.github.com/aki2o/emacs-tss/master/typescript.el")
```
-   In this case, you need to install each of the following dependency.

### Manually

Download tss.el and put it on your load-path.  
-   In this case, you need to install each of the following dependency.

### Dependency

- [auto-complete.el](https://github.com/auto-complete/auto-complete)
- [json-mode.el](https://github.com/joshwnj/json-mode)
- [log4e.el](https://github.com/aki2o/log4e)
- [yaxception.el](https://github.com/aki2o/yaxception)

### About official TypeScript.el

![The official site](http://www.typescriptlang.org/) provides TypeScript.el for Emacs.  
But the official Typescript.el has some trouble ( wrong syntax, have no hook ).  
So, I have bundled typescript.el as modified version of the official TypeScript.el.

# Configuration

```lisp
;; If use bundled typescript.el,
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key binding
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
(tss-config-default)
```

# Consideration

### Activation

If you meet the following condition, do not need to do anything.  
-   use bundled typescript.el
-   use `tss-config-default` setting

Otherwise, need M-x `tss-setup-current-buffer` for activation in typescript-mode.  

-   Not activate on the buffer of the mode not included in `tss-enable-modes`

### Deactivation by trouble at typescript-tools

The function of this extension depends on typescript-tools.  
And, typescript-tools may become impossible depending on the contents of the buffer.  
If it happened, For avoiding Emacs performance degradation,  
this extention is deactivated automatically and the following popup notification is shown.  

![alert](image/alert.png)

The above case is caused by the contents of the buffer.  
So, typescript-tools maybe back to normal by activation after editing of the buffer.  
If you want to activate this extension again, do M-x `tss-restart-current-buffer`.  

Also, if you want to stop to use typescript-tools, do M-x `tss-stop-current-buffer`.

### Reflection of the update of referenced path

You can get the external definition using `/// <reference path ...`.  
But typescript-tools can not find the update of that.  
You have to do M-x `tss-reload-current-project`
if you updated the external definition of current buffer.  
