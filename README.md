# Polymacs
**/!\ This package is not in a usable state yet! /!\\**

[Installation](#Installation) | [Configuration](#Configuration) | [About this project](#About-this-project) | [Road-Map](#Road-Map) | [Philosophy](#Philosophy)

<hr>

Polymacs is a SuperMemo-inspired Emacs package intended for self-learners. [SuperMemo](https://www.super-memory.com/) is a learning method and software developed by Piotr Woźniak since 1985, which has introduced and implemented many techniques for lifelong and autodidactic learners, such as [incremental learning](https://super-memory.com/help/il.htm) and [spaced repetition](https://supermemo.guru/wiki/Spaced_repetition). I plan to adapt these techniques into the Emacs environment, taking advantage of its rich ecosystem.

Polymacs aims to be fast, scalable, future-proof, and modular, providing a robust open-source and adaptable solution for self-taught learners.

## Installation
- Python3 and Pandoc must be installed on your system.
- Add to your init file (default location: ~/emacs.d/init.el or ~/.config/emacs/init.el) : 
```
(use-package polymacs
  :vc (:url "https://github.com/paul-in/polymacs.git" :rev "HEAD")
:ensure t
  :defer t)
  
(require 'polymacs)
```
- Refresh your config (M-x "load-file" init.el) and run M-x "polymacs-install" (you can then leave the install output buffer with q or C-x k RET)

#### If it doesn't install automatically:
- Try running the install script manually: "~/.config/emacs/elpa/polymacs/scripts/install.sh" manually (or path/install.ps1 if on windows)

- Else, you need to create a python venv named env in "~/.config/emacs/elpa/polymacs" (`./env/bin/python3 -m venv env`) and install BeautifulSoup4 (`pip install beautifulsoup4`).
- Make sure Pandoc is installed on your system and added to your PATH so Emacs can find it.

## Configuration
To set the path to the resources-directory (directory in which we save learning materials like web-pages converted to org-mode): (setq polymacs-resource-folder "~/polymacs-resources/")<br>
Default is ~/polymacs-resources, you will be asked to create it when saving your first resource.

### Display remote inline images
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

## About this project
### Current state
The package is currently in an totally unstable and incomplete state, with core functionalities still under development.
For now, we rely on Python and other external tools to implement certain features more easily, with the goal of replacing them later with pure Emacs Lisp.

This project is also, for me, a way to discover Lisp and Emacs Lisp in a very hands-on way — making mistakes, approximations, and plenty of misconceptions. 
Reassuring, isn’t it?

### Contributions
If you want to contribute to the project (which would be awesome!), feel free to check out the Roadmap and pick a feature you'd like to see implemented.

You're also very welcome to work on other aspects of the project — improvements, ideas, bug fixes, documentation — anything that feels meaningful to you!

If you'd like to see your changes included in the project, please make sure they align with the project's philosophy, which is described in the project's philosophy section. Thanks a lot!

## Road-Map
(Check [Terminology](./doc/terminology.md) if any term seems obscure)
### For v0.1
- [x] Proof of concept: using Git to build contexts and extracts in IR ([POC file](./doc/git-context-poc.md))
- [x] Draft HTML-to-Org parser (using external tools for now)
- [ ] **In progress** Basic File handling

### For v0.2 (ultra basic review and IR)
- [ ] Basic review system (just cloze cards) with a temporary draft SRS algorithm
- [ ] "Extract" IR functionality for headings only
- [ ] Define/find the delimiters used by Polymacs

### For v0.3 
- [ ] Git building viewing (Implementation of POC from v0.1)
- [ ] Add extract functionality for arbitrary area
- [ ] Add view tree to navigate through topics tree

### For v0.X
- [ ] Basic adjustable priority queue
- [ ] Native inline images support

### For v1.0 (usable package)
- [ ] Native Elisp HTML-to-Org parser
- [ ] Database format aligned with an existing open-source standard (e.g., .anki2)
- [ ] Implement FSRS in Elisp
- [ ] Basic Incremental Reading functionalities (extract, split, dismiss, suspend, ignore, postpone)
- [ ] Minor mode to hide Polymacs snippets
- [ ] Add doc for existing functionnalities (.texi and online manual (polymacs.org?))
- [ ] Publish on MELPA

### For V1.X
- [ ] Ultra-beginner guide on how to use Polymacs on Windows, with no prior Emacs or Linux knowledge

## Philosophy 
### Future-Proof
Polymacs is designed to be as future-proof as possible, while maintaining a minimalist UI. It offloads data persistence to a SQLite3 database, natively supported since Emacs 29.1, and uses a Git-based reconstruction method in case the database is lost.<br>
This philosophy stems from a harsh truth: Polymacs — and even Emacs — can become obsolete, but the knowledge, especially structured learning data, must endure.<br>
Therefore, Polymacs relies as much as possible on two elements:
- Emacs-native features 
- Robust, well-documented, and actively maintained packages and projects<br>
This commitment ensures long-term durability and reduces technical debt.

### Accessible
Despite its minimal interface, Polymacs must be accessible to newcomers. Many users will arrive already facing Emacs's steep learning curve. How can advanced features be made usable in that context? 
Through:
- Clear and complete documentation
- Workflow demonstrations and learning examples
- Modularity and extensibility, both through package integration and diverse learning workflows
Emacs offers fertile ground for a rich, open-source, incremental learning platform. Polymacs aims to make that potential as approachable as possible.

### Efficient
Polymacs must be efficient on two levels:
1. Learning efficiency: using modern algorithms and advanced learning techniques to optimize retention.
2. Software efficiency: prioritizing fast, optimized workflows and minimal overhead.
Data stored in the database is intentionally lightweight and structured. Since incremental learning relies on text and structured content, which can accumulate quickly, effective data management is essential.

### Adaptable
Polymacs is designed to integrate cleanly into existing Emacs workflows and knowledge management ecosystems like `org-roam`, `denote`, or `org-zettelkasten`.
Where those tools focus on managing external knowledge and fostering creativity, Polymacs focuses on internalizing that knowledge through structured study. These goals are complementary and can work together in powerful ways.
Key priorities include:

- Compatibility with widely used Emacs tools
- Workflow interoperability
- A modular and extensible architecture

Users should be able to easily connect Polymacs with other tools or extend it to support their specific learning workflows.
