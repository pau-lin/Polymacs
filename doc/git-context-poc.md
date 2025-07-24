# POC: Using Git to Get Element Context

Preservation of context is an essential feature to implement in incremental reading. It helps to get a big picture of the subject, remember where the current element comes from, and see elements that could otherwise be deleted in descending nodes.

# Initial approach
One approach to this could be saving multiple instances of the same text area, with the necessary modifications related to the element's goal. 
Example ([source-text](https://en.wikipedia.org/wiki/Emacs)):

![example](./images/example_context1.png)

This could work, using Org files to save this data. However, duplicating the same text across multiple instances is quite heavy, not very elegant, and cumbersome.

# Selected approach
The approach that seems more viable would be to use Git as a version control system, allowing us to replay a snapshot of a file to rigorously track the modifications and thus the context, ensuring that data storage is optimized. `git show <commit>:<file>` will become an essential tool..

This approach introduces a few challenges:
1. Commits must be made systematically for every action performed on the text (modification, extraction, moving, etc.). Emacs handles this well with hooks if the file is modified. For other cases, there are two solutions: protect external modifications of Polymacs resource files by making them read-only or include a tool to clean changes based on file differences with existing commits. The first option looks much easier.
2. It requires a way to save commit hashes associated with specific extract creation/modification to quickly return to a past snapshot. Plain-text/org properties can handle this.
3. There is a limitation in cascading context modifications if children have been modified: if a user created and modified an extract and now wants to change something from the context element, this can’t easily propagate to children, as it will result in conflicts. (the first approach would have the same problem, though)

Example showing the general principle of this approach (The extracted section is here shown with #+begin_x| and #+end_x|) ([source-text](https://en.wikipedia.org/wiki/Carrot)):

![example](./images/example_context_modif1.png) 
![example](./images/example_context_modif2.png)

## Detailed example and viable solution

In fact, with this approach, it is still possible to modify the context from which an extract was made, without affecting the extract itself. In this example, we show that by using two branches, it is possible to build the buffer exactly as we want.

- The Polymacs-source branch is used as the source of truth: every modification, extraction, or deletion will be saved there.
- The Polymacs-build branch is used to construct what will be shown to the user, by selecting the appropriate commits gathered from the Polymacs-source branch.

It doesn't matter whether the file at the tip of Polymacs-source (i.e. HEAD) appears coherent or not. As long as the buffer history is properly maintained by the package, it will always be possible to rebuild a consistent version of the buffer from the Polymacs-build branch. ~~,saving the plain-text Org file in whatever state we choose. So even if we examine the file outside of Emacs (with cat, for example), it can still appear clean and coherent if we decided to build it that way before ending the learning session (which will typically be the case!).~~ (No, for this last point, the original file is actually where all the data will be stored. But it’s not a big deal if it can’t be accessed regularly outside of Emacs, as long as Polymacs can export a clean build of the file when needed. Or maybe this last point is possible if we can keep all of the data inside Git (to be explored...).)

Auto-commits will be triggered for every action that needs to be recorded in a Polymacs buffer. When the user saves the buffer, we will also check whether any modifications have been made and apply commits if needed.

We've kept this example on a simple structure (context - extract), but by following the same rules, this approach can scale to as many nested levels as desired.

Example ([source-text](https://en.wikipedia.org/wiki/Kindness)):

\~extract\~<br>
\<\<modifications\>\><br>
=infos=<br>
+ignored+<br>
/comment/<br>
... : Collapsing commit history for readability<br>

![example](./images/example_context_git3.png) 
![example](./images/example_context_git4.png) 
![example](./images/example_context_git5.png) 
![example](./images/example_context_git6.png) 
![example](./images/example_context_git7.png) 

To simplify this build process, commit hashes will be saved alongside the learning data in a dedicated tree (one tree per file), represented by an Org heading tree with each extract acting as a node in that tree. In this example, under the branch referring to extract 2, for instance, we would store "Commit 4" and "Commit 5" hashes. Whenever we want to visualize this extract, we simply rebuild the buffer by gathering those commits.

Git is fast and optimized for large collections of commits, especially when they are small, as will generally be the case here. Performing a large number of atomic commits will be invisible to the user, while still providing a robust way to visualize context, minimize data duplication, and maintain a clean, readable buffer.
