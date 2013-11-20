[Japanese](https://github.com/aki2o/emacs-tss/blob/master/README-ja.md)

What's this?
============

This is a extension of Emacs that provides completion/syntax-check by using typescript-tools in typescrript-mode.

About typescript-tools, see https://github.com/clausreinke/typescript-tools


Feature
=======

* **Auto completion by auto-complete.el**

![demo1](image/demo1.png)

* **Popup help by popup.el**

![demo2](image/demo2.png)

* **Jump to definition**

* **Check syntax by flymake.el**


Requirement
===========

* [typescript-tools](https://github.com/clausreinke/typescript-tools)


Install
=======

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

**Note:** Installing each the following dependency is required in this case.

### Manually

Download tss.el/typescript.el and put on your load-path.

**Note:** Installing each the following dependency is required in this case.

### Dependency

* [auto-complete.el](https://github.com/auto-complete/auto-complete)
* [json-mode.el](https://github.com/joshwnj/json-mode)
* [log4e.el](https://github.com/aki2o/log4e)
* [yaxception.el](https://github.com/aki2o/yaxception)

### About official TypeScript.el

[The official site](http://www.typescriptlang.org/) provides TypeScript.el for Emacs.  
But the official Typescript.el has some trouble ( wrong syntax, have no hook ).  
So, I have bundled typescript.el as modified version of the official TypeScript.el.  

#### Modification from the official version

    $ diff TypeScript.el typescript.el
    66c66
    < (declare-function ido-mode "ido" ())
    ---
    > (declare-function ido-mode "ido")
    486a487,491
    > (defcustom typescript-mode-hook nil
    >   "*Hook called by `typescript-mode'."
    >   :type 'hook
    >   :group 'typescript)
    > 
    3351c3356,3358
    <     (font-lock-fontify-buffer)))
    ---
    >     (font-lock-fontify-buffer))
    > 
    >   (run-mode-hooks 'typescript-mode-hook))

**Note:** It maybe happen that update of the official version is not merged into typescript.el.  
**Note:** The last date of checking merge is 2013/08/10.  


Configuration
=============

```lisp
;; If use bundled typescript.el,
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key binding
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")

;; If there is the mode, which you want to enable TSS,
(add-to-list 'tss-enable-modes 'hoge-mode)

;; If there is the key, which you want to start completion of auto-complete.el,
(add-to-list 'tss-ac-trigger-command-keys "=")

;; Do setting recommemded configuration
(tss-config-default)
```


Attention
=========

### Activation

If you meet the following condition, do not need to do anything.

* use bundled typescript.el
* use `tss-config-default` setting

Otherwise, need M-x `tss-setup-current-buffer` for activation in typescript-mode.  

**Note:** Not activate on the buffer of the mode not included in `tss-enable-modes`.  

### Deactivation by trouble at typescript-tools

The function of this extension depends on typescript-tools.  
And, typescript-tools may become impossible depending on the contents of the buffer.  
If it happened, For avoiding Emacs performance degradation,  
this extention is deactivated automatically and the following popup notification is shown.

![alert](image/alert.png)

The above case is caused by the contents of the buffer.  
So, typescript-tools maybe back to normal by activation after editing of the buffer.  
If you want to activate this extension again, do M-x `tss-restart-current-buffer`.

### Reflection of modified other file

In Typescript, can import definition of other file using `///<reference path='...'>` and so on.  
But, this extension can not notice update of them automatically.  
If you want to reflect update of them into current buffer, do M-x `tss-reload-current-project`.


Tested On
=========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
* typescript-tools ... master branch
* auto-complete.el ... 1.4.0
* json-mode.el ... 1.1.0
* log4e.el ... 0.1
* yaxception.el ... 0.1


**Enjoy!!!**

