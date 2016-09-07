# flycheck-tip.el

A package to show flycheck's error(s) after jumping to the point.

![screenshot](https://lh3.googleusercontent.com/-xQ9YEUo-ufc/UmSXPW51F5I/AAAAAAAACvw/VmendRlrXlA/s640/Screenshot%2520from%25202013-10-20%252022%253A51%253A32.png)

# Note (for existing users)

*flycheck-tip-avoid-show-func* variable become obsolete. Please
set `flycheck-display-errors-function` variable to `ignore` if you
want to avoid echoing error message on minibuffer.

Also previously `flycheck-tip` supported popup error message at point, but
it was obsoleted because there is official flycheck's support:
flycheck-pos-tip: https://github.com/flycheck/flycheck-pos-tip

## Installation

You can install this package via MELPA

## Usage

```lisp
(require 'flycheck-tip)
(define-key your-prog-mode (kbd "C-c C-n") 'flycheck-tip-cycle)
;; To avoid echoing error message on minibuffer (optional)
(setq flycheck-display-errors-function 'ignore)
```

If you are still using flymake, you can use combined function that
show error by popup in flymake-mode or flycheck-mode.

```lisp
(define-key global-map (kbd "C-0") 'error-tip-cycle-dwim)
(define-key global-map (kbd "C-9") 'error-tip-cycle-dwim-reverse)
```

Do M-x flycheck-tip-cycle or push its key when error occurred on
flycheck-mode. Then you can move next error.
If next error isn't exists then the cursor moves to first error if it's exists.

# Keep errors on notification area
If you build Emacs with D-Bus option, you may configure following setting.
This keeps the errors on notification area. Please check
`error-tip-notify-timeout` to change limit of the timeout as well.

    (setq error-tip-notify-keep-messages t)

# Flycheck's official package
flycheck-pos-tip: https://github.com/flycheck/flycheck-pos-tip

