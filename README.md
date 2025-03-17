# biquge.el

在Emacs里看小说！

这是 [biquge.nvim](https://github.com/v1nh1shungry/biquge.nvim) 的Emacs移植版本，允许你在Emacs中阅读小说。

本插件在光标所在位置以overlay的形式插入小说内容，将高亮设置为注释后，就可以完美cosplay注释了:) overlay也不会污染buffer的内容。

## 功能

* 支持小说搜索
* overlay显示小说内容，放心摸鱼
* 支持收藏小说，收藏的小说将记录阅读进度（精度仅限章节）
* 支持多种UI选择器（内置、ivy、helm、vertico）

## 依赖

* Emacs >= 27.1
* curl
* dom-select.el (用于CSS选择器解析HTML)

## 安装

### 手动安装

1. 克隆仓库
2. 将`elisp`目录添加到`load-path`
3. 在配置文件中添加：

```elisp
(require 'biquge)
(biquge-setup)
```

### 使用use-package

```elisp
(use-package biquge
  :load-path "/path/to/biquge.nvim/elisp"
  :config
  (biquge-setup))
```

### 使用straight.el

```elisp
(straight-use-package
 '(biquge :type git :host github :repo "nailuoGG/biquge.el"
          :files ("*.el")))
```

## 配置

```elisp
;; 默认配置
(setq biquge-width 30)                 ; 显示文本的宽度（字数）
(setq biquge-height 10)                ; 显示文本的行数
(setq biquge-face 'font-lock-comment-face) ; 高亮组
(setq biquge-bookshelf-file            ; 书架存储路径
      (expand-file-name "biquge_bookshelf.json" user-emacs-directory))
(setq biquge-picker 'builtin)          ; UI选择器
                                       ; 可选值：'builtin, 'ivy, 'helm, 'vertico
```

## 安装 dom-select.el

`dom-select.el` 是必需的依赖，提供CSS选择器语法来解析HTML。

安装方法：

```elisp
;; 使用quelpa安装
(quelpa '(dom-select :fetcher github :repo "twlz0ne/dom-select.el"))

;; 或者使用straight.el
(straight-use-package '(dom-select :type git :host github :repo "twlz0ne/dom-select.el"))

;; 或者使用use-package和straight.el
(use-package dom-select
  :straight (dom-select :type git :host github :repo "twlz0ne/dom-select.el"))
```

## 推荐键绑定

```elisp
(global-set-key (kbd "C-c b /") 'biquge-search)
(global-set-key (kbd "C-c b b") 'biquge-toggle)
(global-set-key (kbd "C-c b t") 'biquge-toc)
(global-set-key (kbd "C-c b n") 'biquge-next-chap)
(global-set-key (kbd "C-c b p") 'biquge-prev-chap)
(global-set-key (kbd "C-c b s") 'biquge-star)
(global-set-key (kbd "C-c b l") 'biquge-bookshelf)
(global-set-key (kbd "M-d") (lambda () (interactive) (biquge-scroll 1)))
(global-set-key (kbd "M-u") (lambda () (interactive) (biquge-scroll -1)))
```

## 使用

| 函数 | 描述 |
|------|------|
| `biquge-search` | 根据书名搜索小说 |
| `biquge-toc` | 打开目录 |
| `biquge-show` | 在当前光标所在位置显示 |
| `biquge-hide` | 隐藏 |
| `biquge-toggle` | 显示/隐藏 |
| `biquge-scroll` | 向下滚动（负数时向上滚动） |
| `biquge-star` | 收藏 |
| `biquge-next-chap` | 下一章节 |
| `biquge-prev-chap` | 上一章节 |
| `biquge-bookshelf` | 查看书架 |

## 书架操作

在书架界面：
- 选择一本书会在上次阅读的章节打开
- 使用 `C-c d` 可以取消收藏选中的书

## 宇宙安全声明

* 该软件仅供学习交流使用, 禁止个人用于非法商业用途, 请于安装后 24 小时内删除
* 所有资源来自网上, 该软件不参与任何制作, 上传, 储存等内容, 禁止传播违法资源
* 请支持正版
