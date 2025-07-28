# Changelog
## v0.1 (2025-07-28)

This release lays the draft foundations of the package, introducing basic file handling (Polymacs directory, resource file creation and navigation) and parsing functionalities.
A good amount of thought has gone into design decisions, such as preferring plain text data storage over a database, and exploring the use of Git to dynamically build files and extracts.

- [x] Proof of concept: using Git to build contexts and extracts in IR ([POC file](./git-context-poc.md))
- [x] Draft HTML-to-Org parser (using external tools for now)
- [x] Basic File handling
