# Polymacs
Polymacs is a SuperMemo-inspired Emacs package intended for self-learners. [SuperMemo](https://www.super-memory.com/) is a learning method and software developed by Piotr Woźniak since 1985, which has introduced and implemented many techniques for lifelong and autodidactic learners, such as [incremental learning](https://super-memory.com/help/il.htm) and [spaced repetition](https://supermemo.guru/wiki/Spaced_repetition). We have adapted some of these techniques into the Emacs environment, taking advantage of its rich ecosystem.

Polymacs aims to be fast, scalable, future-proof, and modular, providing a robust open-source solution for self-taught learners, adaptable to their individual needs.

## Current state
The package is currently in an totally unstable and incomplete state, with core functionalities still under development.
For now, we rely on Python and other external tools to implement certain features more easily, with the goal of replacing them later with pure Emacs Lisp.

This project is also, for me, a way to discover Lisp and Emacs Lisp in a very hands-on way — making mistakes, approximations, and plenty of misconceptions.  
Reassuring, isn’t it?

## Install and load module
Simply clone this github repo and add to your init file (default to ~/.emacs.d/init.el or .config/emacs/init.el) :
```
(add-to-list 'load-path "path-to-polymacs-pkg/lisp/")
(require 'polymacs)
```

## Config
To set the path to the resources-directory (directory in which we save learning materials like web-pages converted to org-mode):
(setq polymacs-source-folder "~/polymacs-resources/")
Default is ~/polymacs-resources, you will be asked to create it when saving your first source.