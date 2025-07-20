# Polymacs
Polymacs is a SuperMemo-inspired Emacs package intended for self-learners. [SuperMemo](https://www.super-memory.com/) is a learning method and software developed by Piotr Woźniak since 1985, which has introduced and implemented many techniques for lifelong and autodidactic learners, such as [incremental learning](https://super-memory.com/help/il.htm) and [spaced repetition](https://supermemo.guru/wiki/Spaced_repetition). I plan to adapt these techniques into the Emacs environment, taking advantage of its rich ecosystem.

Polymacs aims to be fast, scalable, future-proof, and modular, providing a robust open-source solution for self-taught learners, adaptable to their individual needs.

## Installation
This package isn’t available on MELPA yet, so both it and its dependencies need to be installed manually.
### Required
- Clone this github repo and add to your init file (default to ~/.emacs.d/init.el or .config/emacs/init.el) :
```
(add-to-list 'load-path "path-to-polymacs-pkg/lisp/")
(require 'polymacs)
```

- You need to create a python venv named env in "path-to-polymacs-pkg" (`python -m venv env`) and install BeautifulSoup4 (`pip install beautifulsoup4`).

- Install pandoc on your system

### Optional
#### Display remote inline images
To allow remote images to be shown in the resource buffer, you need to install [Org-yt](https://github.com/TobiasZawada/org-yt?tab=readme-ov-file) and add in your init file :
```
(setq org-display-remote-inline-images 'cache)

(defun org-http-image-data-fn (protocol link _description)
  "Interpret LINK as an URL to an image file."
  (when (and (image-type-from-file-name link)
             (not (eq org-display-remote-inline-images 'skip)))
    (if-let (buf (url-retrieve-synchronously (concat protocol ":" link)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))
      (message "Download of image \"%s\" failed" link)
      nil)))

(org-link-set-parameters "http"  :image-data-fun #'org-http-image-data-fn)
(org-link-set-parameters "https" :image-data-fun #'org-http-image-data-fn)
```
Then you can use M-x "org-toggle-inline-images" to show them.<br>
It will quite noticeably slow down loading of an org-bfile containing remote images

I plan to include this feature by default in Polymacs.
## Configuration
To set the path to the resources-directory (directory in which we save learning materials like web-pages converted to org-mode): (setq polymacs-resource-folder "~/polymacs-resources/")<br>
Default is ~/polymacs-resources, you will be asked to create it when saving your first resource.

## About this project
### Current state
The package is currently in an totally unstable and incomplete state, with core functionalities still under development.
For now, we rely on Python and other external tools to implement certain features more easily, with the goal of replacing them later with pure Emacs Lisp.

This project is also, for me, a way to discover Lisp and Emacs Lisp in a very hands-on way — making mistakes, approximations, and plenty of misconceptions.  
Reassuring, isn’t it?

### Road-map and contributions
If you want to contribute to the project (which would be awesome!), feel free to check out the [Roadmap](./doc/ROADMAP.md) and pick a feature you'd like to see implemented.

You're also very welcome to work on other aspects of the project — improvements, ideas, bug fixes, documentation — anything that feels meaningful to you!

If you'd like to see your changes included in the project, please make sure they align with the project's philosophy, which is also described in the same document. Thanks a lot!

### Acknowledgments

