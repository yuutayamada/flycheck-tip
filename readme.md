# flycheck-tip.el

This program can show you error by popup-tip.
(Note I'm changing popup-tip's face of below screenshot)

![screenshot](https://lh3.googleusercontent.com/-xQ9YEUo-ufc/UmSXPW51F5I/AAAAAAAACvw/VmendRlrXlA/s640/Screenshot%2520from%25202013-10-20%252022%253A51%253A32.png)

## Requirements

You need flycheck and popup packages.
But if you are already using auto-complete.el, then you don't need
install popup.el. Because auto-complete.el is including it.
<!-- WIP If you can install package from MELPA ...-->

## Configuration

```lisp
(require 'flycheck-tip)
(define-key your-prog-mode (kbd "C-c C-n") 'flycheck-tip-cycle)
```

## Usage

Do M-x flycheck-tip-cycle or push its key when error occurred on
flycheck-mode. Then you can move next error.
If next error wasn't exists then move to first error if it's exists.

# Note

This program avoid *flycheck-show-error-at-point* function to avoid
duplicated error message(i.e., minibuffer and popup-tip).
But if you want to regain this behavior, set following configuration
to your .emacs:

```lisp
(setq flycheck-tip-avoid-show-func nil)
```
