.emacs.d
============

Because I've gone through hell and back to make this the best editor on the planet. If I say I've "set up" something, it means I downloaded it from the brilliant people who spent their valuable time to make it and ingraciously shoved it into my own unified configuration. The most meaningful contribution I've done is find usable non-conflicting keybindings for all of these.

#### Major functionality already set up:

* [undo-tree](http://www.emacswiki.org/emacs/UndoTree) (persistent across reboots)
* [slime](http://common-lisp.net/project/slime/) and [paredit](http://www.emacswiki.org/emacs/ParEdit)
* [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) for [haskell-mode](http://www.haskell.org/haskellwiki/Emacs)
* [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
* [helm](https://github.com/emacs-helm/helm)
* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
* [evil](https://gitorious.org/evil/pages/Home)
* [smart-compile](http://www.emacswiki.org/emacs/SmartCompile)
* various language major modes, such as [go-mode](http://golang.org/misc/emacs/go-mode.el), [php-mode](http://sourceforge.net/projects/php-mode/), R and Julia through [ESS](http://ess.r-project.org/), and [qmake-mode](https://code.google.com/p/qmake-mode/source/browse/qmake.el)
* [linum](http://www.logic.at/prolog/linum/linum.html) line numbering (modified somewhat)
* [magit](https://github.com/magit/magit)
* [saveplace](http://www.emacswiki.org/emacs/SavePlace)
* a comprehensive [ibuffer](http://www.emacswiki.org/emacs/IbufferMode) filetype/name filter system based off [this brilliant post](http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html)

#### Setup

1. Initial
	* Start emacs with the ```-l``` option to load the .emacs file within this repository.
	* You can alias it: ```alias emacs='emacs -l ~/.emacs.d/.emacs'```
2. Lisp setup (not required if you don't use lisp)
	* If not using sbcl, change ```(setq inferior-lisp-program "sbcl")``` to the command calling your preferred compiler.
	* SBCL:
    	1. make the .sbclrc within your home directory to load everything contained within this one (i.e. make it have ```(load "~/.emacs.d/.sbclrc")```).
    * If you're not using sbcl, that's cool too: look at the documentation [here](http://www.quicklisp.org/beta/) to set up your preferred lisp compiler.
	* If things are not working (e.g. autocompletion, eldoc) in ```lisp-mode```, start slime using ```M-x slime```.
3. Haskell setup (not required if you don't use haskell)
	* Read the instructions for [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) installation.

#### Dependencies
These aren't actually required to run it, just to use certain modes. Email me about any others you find.

1. [sbcl](http://sbcl.org) (for slime usage)
2. [R](http://www.r-project.org) and [julia](http://julialang.org) languages (for ESS)
3. [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/)

#### Design decisions

* Why not use submodules to contain git repositories of all dependencies instead of rehosting them here?
	* I've rewritten enough of the code in these packages to suit my personal configuration that keeping it all sane would be a nightmare. In addition, submodules are extremely hard to use for most people who don't specialize in git and require extra very-easy-to-get-wrong setup procedures. I want this to be plug and play, for any boxes I reinstall this on and anyone else who doesn't want to spend years trying to get complex emacs plugins to work.
* Why not send a pull request to those projects?
	* Because the changes made are almost purely personalization, nothing that would actually improve the software for anyone else. emacs is very good at providing a common framework to add different functionality to, but once the functionality gets crowded they all start sliding around and hitting each other (thanks, dynamic scoping!!!). Working with changing dependencies on the fragile tower that is my bloated emacs configuration is, as you might imagine, hell.
