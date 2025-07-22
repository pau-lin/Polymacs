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

### Contributions
If you want to contribute to the project (which would be awesome!), feel free to check out the Roadmap and pick a feature you'd like to see implemented.

You're also very welcome to work on other aspects of the project — improvements, ideas, bug fixes, documentation — anything that feels meaningful to you!

If you'd like to see your changes included in the project, please make sure they align with the project's philosophy, which is described in the project's philosophy section. Thanks a lot!

# Polymacs Road-Map
## For v0.1
- [ ] Proof of concept: using Git to view context in IR
- [ ] Proof of concept: using Git as a source of truth to rebuild the database

## For v0.2
- [ ] Draft HTML-to-Org parser (using external tools for now)
- [ ] Basic SQLite database management
- [ ] Basic review system (just cloze cards) with a temporary draft SRS algorithm
- [ ] "Extract" IR functionality
- [ ] Define/find the delimiters used by Polymacs

# For v0.3 (if POC in v0.1 worked)
- [ ] Git databse rebuild system
- [ ] Git context viewing

## For v0.X
- [ ] Basic adjustable priority queue

## For v1.0 (usable package)
- [ ] Native Elisp HTML-to-Org parser
- [ ] Database format aligned with an existing open-source standard (e.g., .anki2)
- [ ] Implement FSRS in Elisp
- [ ] Basic Incremental Reading functionalities (extract, split, dismiss, suspend, ignore, postpone)
- [ ] Minor mode to hide Polymacs snippets

# Project's philosophy 
### Accessibility · Future-Proofing · Efficiency · Integration & Customization
## Future-Proof
Polymacs is designed to be as future-proof as possible, while maintaining a minimalist UI. It offloads data persistence to a SQLite3 database, natively supported since Emacs 29.1, and a Git-based reconstruction method in case the database is lost<br>
This philosophy stems from a harsh truth: Polymacs — and even Emacs — can become obsolete, but the knowledge, especially structured learning data, must endure.<br>
Therefore, Polymacs relies as much as possible on two elements:
- Emacs-native features 
- Robust, well-documented, and actively maintained packages and projects
This commitment ensures long-term durability and reduces technical debt.

## Accessibility
Despite its minimal interface, Polymacs must be accessible to newcomers. Many users will arrive already facing Emacs's steep learning curve. How can advanced features be made usable in that context? 
Through:
- Clear and complete documentation
- Workflow demonstrations and learning examples
- Modularity and extensibility, both through package integration and diverse learning workflows
Emacs offers fertile ground for a rich, open-source, incremental learning platform. Polymacs aims to make that potential as approachable as possible.

## Efficiency
Polymacs must be efficient on two levels:
1. Learning efficiency: using modern algorithms and advanced learning techniques to optimize retention.
2. Software efficiency: prioritizing fast, optimized workflows and minimal overhead.
Data stored in the database is intentionally lightweight and structured. Since incremental learning relies on text and structured content, which can accumulate quickly, effective data management is essential.

## Integration & Customization
Polymacs is designed to integrate cleanly into existing Emacs workflows and knowledge management ecosystems like `org-roam`, `denote`, or `org-zettelkasten`.
Where those tools focus on managing external knowledge and fostering creativity, Polymacs focuses on internalizing that knowledge through structured study. These goals are complementary and can work together in powerful ways.
Key priorities include:
- Compatibility with widely used Emacs tools
- Workflow interoperability
- A modular and extensible architecture
Users should be able to easily connect Polymacs with other tools or extend it to support their specific learning workflows.
