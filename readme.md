# flycheck-tip.el

This program can show you error by popup-tip.
(Note I'm changing popup-tip's face of below screenshot)

![screenshot](https://lh3.googleusercontent.com/-xQ9YEUo-ufc/UmSXPW51F5I/AAAAAAAACvw/VmendRlrXlA/s640/Screenshot%2520from%25202013-10-20%252022%253A51%253A32.png)

## Requirements

You need s, flycheck, and popup packages.
But if you are already using auto-complete.el, then you don't need
install popup.el. Because auto-complete.el is including it.
<!-- WIP If you can install package from MELPA ...-->

## Configuration

```lisp
(require 'flycheck-tip)
(define-key your-prog-mode (kbd "C-c C-n") 'flycheck-tip-cycle)
```

If you want to show current line errors by popup instead of flycheck's
echo area function, then configure like this:

```lisp
(flycheck-tip-use-timer 'verbose)
```

If you are still using flymake like me, you can use combined function that
show error by popup in flymake-mode or flycheck-mode.

```lisp
(define-key global-map (kbd "C-0") 'error-tip-cycle-dwim)
(define-key global-map (kbd "C-9") 'error-tip-cycle-dwim-reverse)
```

## Usage

Do M-x flycheck-tip-cycle or push its key when error occurred on
flycheck-mode. Then you can move next error.
If next error isn't exists then the cursor moves to first error if it's exists.

# Show eclim's error

This package also supports showing eclim's error feature.
If you want to use this feature, you can use `eclim-tip-cycle` and
`eclim-tip-cycle-reverse` functions.

# Note

This program avoid *flycheck-show-error-at-point* function to avoid
duplicated error message(i.e., minibuffer and popup-tip).
But if you want to regain this behavior, set following configuration
to your .emacs:

```lisp
(setq flycheck-tip-avoid-show-func nil)
```
