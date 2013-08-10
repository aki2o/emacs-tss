これは何？
==========

Emacsでtypescript-mode時に、typescript-toolsを使用した入力補完/文法チェックを提供するEmacsの拡張です。

typescript-toolsについては、以下を参照して下さい。  
https://github.com/clausreinke/typescript-tools


特徴
====

* **auto-complete.elによる入力補完**

![demo1](image/demo1.png)

* **popuo.elによるヘルプ表示**

![demo2](image/demo2.png)

* **定義元ジャンプ**

* **flymake.elによる文法チェック**


Emacs以外に必要なもの
=====================

* [typescript-tools](https://github.com/clausreinke/typescript-tools)


インストール
============

インストールには、el-getを使うのを推奨します。  
手動やauto-install.elでも良いですが、下記の依存拡張もそれぞれインストールする必要があります。

### el-get.elを使う場合

2013/08/10 と言いつつ、まだ利用できません。

以下のように、el-get-sourcesに設定を追加すれば、el-getでインストールできるようになります。

    (setq el-get-sources
          '(
            (:name log4e
                   :website "https://github.com/aki2o/log4e"
                   :description "provide logging framework for elisp."
                   :type github
                   :pkgname "aki2o/log4e")
            (:name yaxception
                   :website "https://github.com/aki2o/yaxception"
                   :description "provide framework about exception like Java for elisp."
                   :type github
                   :pkgname "aki2o/yaxception")
            (:name tss
                   :website "https://github.com/aki2o/emacs-tss"
                   :description "provide a interface for auto-complete.el/flymake.el on typescript-mode."
                   :type github
                   :pkgname "aki2o/emacs-tss"
                   :depends (auto-complete log4e yaxception))
            ))

### auto-install.elを使う場合

    (auto-install-from-url "https://raw.github.com/aki2o/emacs-tss/master/tss.el")
    (auto-install-from-url "https://raw.github.com/aki2o/emacs-tss/master/typescript.el")

### 依存拡張

* [auto-complete.el](https://github.com/auto-complete/auto-complete)
* [log4e.el](https://github.com/aki2o/log4e)
* [yaxception.el](https://github.com/aki2o/yaxception)

### オリジナルのTypeScript.elについて

Typescript編集のための拡張が、[公式サイト](http://www.typescriptlang.org/)から提供されていますが、
エラーになる箇所や、フックが定義されていないといった問題点があるため、
修正したものをtypescript.elとして同梱しています。

#### オリジナル版からの修正情報

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

※ オリジナル版の更新状況が同梱版に反映されていないこともあるかも知れません。  
※ 反映の最終確認日は、2013/08/10です。  


設定
====

### 同梱されたtypescript.elを使う場合

```lisp
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(require 'tss)
(add-hook 'typescript-mode-hook 'tss-setup t)
;; ポイントしている要素についてのヘルプをポップアップ表示
(setq tss-popup-help-key "C-:")
;; ポイントしている要素の定義元へジャンプ
(setq tss-jump-to-definition-key "C->")
```

### オリジナルのTypeScript.elを使う場合

```lisp
(require 'tss)
;; ポイントしている要素についてのヘルプをポップアップ表示
(setq tss-popup-help-key "C-:")
;; ポイントしている要素の定義元へジャンプ
(setq tss-jump-to-definition-key "C->")
```


留意事項
========

### 有効化

同梱されたtypescript.elを使う場合は、自動で有効になるため気にする必要はありません。  
オリジナルのTypeScript.elを使う場合は、自動で有効にならないので、M-x tss-setup として下さい。

### 解析不能による無効化

本拡張の提供する入力補完/文法チェックなどの機能は、typescript-toolsに依存しており、  
バッファの内容によっては、typescript-toolsの解析機能が不能になる場合があるようです。  
もし、そうなった場合、タイムアウトにより本拡張の処理が非常に遅くなってしまうため、  
以下のようにポップアップを表示し、自動で本拡張を無効化します。

![alert](image/alert.png)

解析不能な状態になる原因はバッファの内容にあるため、  
バッファを変更すれば本拡張を再度有効にしても正常に動作するかも知れません。  
M-x tss-restart-current-buffer とすることで、本拡張を再度有効にすることができます。

### 入力補完候補数の制限

バッファのポイント箇所において入力できる候補を、typescript-toolsはjson形式で提供するため、  
本拡張で利用するためにデータ変換をしていますが、  
候補数が多いと変換処理でEmacsがフリーズしてしまう可能性があるため、候補数を制限する対処をしています。  

auto-complete.elでは、候補表示状態でC-sすることで候補を絞り込めますが、  
候補数が制限されている場合には出現すべき候補が出てこない場合もあります。  
その場合は、続けて文字を入力することで候補が更新され表示されると思います。  

※ typescript-toolsが解析に失敗して表示されない場合もあります。  

----

#### 2013/08/10 

現在、フリーズを回避する対処を行った修正版をtypescript-tools本家に取り込んでもらうように依頼しています。  
フリーズが多発するような場合には、以下を実施することで応急対処が可能です。

1. 修正版のtss.tsを取得する [修正版tss.ts](https://raw.github.com/aki2o/typescript-tools/maxresponses/tss.ts)
2. typescript-tools本家のREADMEを参照し、本家のソースをtss.tsだけ修正版に置き換えてコンパイルする。
3. コンパイル後、インストールする。

----

### 参照ファイルの更新内容の反映

Typescriptでは参照などにより、外部ファイルの機能を利用することが可能ですが、  
外部ファイルが更新されても、現在編集中のバッファには自動では反映されません。  
編集中のバッファから参照しているファイル内の定義が変更された場合、  
それを反映させるには、 M-x tss-reload-current-project として下さい。


動作確認
========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
* auto-complete.el ... 1.4.0
* log4e.el ... 0.2.0
* yaxception.el ... 0.1


**Enjoy!!!**

