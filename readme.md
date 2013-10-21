# flycheck-tip.el

This program can show you popuped error.

![screenshot](https://lh3.googleusercontent.com/-xQ9YEUo-ufc/UmSXPW51F5I/AAAAAAAACvw/VmendRlrXlA/s640/Screenshot%2520from%25202013-10-20%252022%253A51%253A32.png)

## Requirement

You need flycheck and popup packages.

## Configuration
```lisp
(require 'flycheck-tip)
(define-key your-prog-mode (kbd "C-c C-n") 'flycheck-tip-cycle)
```

## Usage

Do M-x flycheck-tip-cycle
