# Overview

A passwords manager for emacs. It builds upon the pattern described in the pattern described by [emacs-fu](http://emacs-fu.blogspot.com/2011/02/keeping-your-secrets-secret.html) and lets you copy your stored passwords to the clipboard easily.

# Usage

## In your init.el
```elisp
(password-vault-register-secrets-file "passwords")
;; Or the module in which you keep your passwords
```

```elisp
M-x password-vault
```

# License
Copying is an act of Love, please copy.

# Author
PuercoPop <pirata@gmail.com>

