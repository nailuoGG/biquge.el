# biquge.el

Read novels in Emacs!

This is an Emacs port of [biquge.nvim](https://github.com/v1nh1shungry/biquge.nvim), allowing you to read novels directly in Emacs.

The plugin displays novel content as an overlay at the cursor position, with highlighting set as comments, making it perfect for discreet reading :) The overlay doesn't pollute the buffer content.

[切换到中文版](README.md)

## Features

* Novel search support
* Display novel content as overlay, perfect for discreet reading
* Bookmark novels with reading progress tracking (chapter-level precision)
* Support for multiple UI selectors (built-in, ivy, helm, vertico)

## Dependencies

* Emacs >= 27.1
* curl
* dom-select.el (for CSS selector-based HTML parsing)

## Installation

### Manual Installation

1. Clone the repository
2. Add the `elisp` directory to your `load-path`
3. Add to your configuration file:

```elisp
(require 'biquge)
(biquge-setup)
```

### Using use-package

```elisp
(use-package biquge
  :load-path "/path/to/biquge.nvim/elisp"
  :config
  (biquge-setup))
```

### Using straight.el

```elisp
(straight-use-package
 '(biquge :type git :host github :repo "nailuoGG/biquge.el"
          :files ("*.el")))
```

## Configuration

```elisp
;; Default configuration
(setq biquge-width 30)                 ; Width of displayed text (characters)
(setq biquge-height 10)                ; Height of displayed text (lines)
(setq biquge-face 'font-lock-comment-face) ; Highlight face
(setq biquge-bookshelf-file            ; Bookshelf storage path
      (expand-file-name "biquge_bookshelf.json" user-emacs-directory))
(setq biquge-picker 'builtin)          ; UI selector
                                       ; Options: 'builtin, 'ivy, 'helm, 'vertico
(setq biquge-debug-mode nil)           ; Debug mode (set to t to enable debug messages)
```

## Installing dom-select.el

`dom-select.el` is a required dependency that provides CSS selector syntax for HTML parsing.

Installation methods:

```elisp
;; Using quelpa
(quelpa '(dom-select :fetcher github :repo "twlz0ne/dom-select.el"))

;; Or using straight.el
(straight-use-package '(dom-select :type git :host github :repo "twlz0ne/dom-select.el"))

;; Or using use-package with straight.el
(use-package dom-select
  :straight (dom-select :type git :host github :repo "twlz0ne/dom-select.el"))
```

## Recommended Key Bindings

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

## Usage

| Function | Description |
|------|------|
| `biquge-search` | Search for novels by title |
| `biquge-toc` | Open table of contents |
| `biquge-show` | Display at current cursor position |
| `biquge-hide` | Hide display |
| `biquge-toggle` | Toggle display on/off |
| `biquge-scroll` | Scroll down (negative number to scroll up) |
| `biquge-star` | Bookmark current novel |
| `biquge-next-chap` | Go to next chapter |
| `biquge-prev-chap` | Go to previous chapter |
| `biquge-bookshelf` | View bookshelf |
| `biquge-toggle-debug` | Toggle debug mode on/off |

## Bookshelf Operations

In the bookshelf interface:
- Selecting a book will open it at the last read chapter
- Use `C-c d` to remove a selected book from bookmarks

## Legal Disclaimer

* This software is for learning and communication purposes only. Commercial use is prohibited. Please delete within 24 hours after installation.
* All resources come from the internet. This software does not participate in any production, uploading, or storage of content. Distribution of illegal resources is prohibited.
* Please support official releases.
