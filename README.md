.emacs.d
========

Because I've gone through hell and back to make this the best editor on the planet. If I say I've "set up" something, it means I downloaded it from the brilliant people who spent their valuable time to make it and ingraciously shoved it into my own unified configuration. The most meaningful contribution I've done is find usable non-conflicting keybindings for all of these, although I've received multiple reports that the keybindings are "weird" and "dumb." Ah well. Genius is rarely understood in its own time.

This is all licensed under GPLv3, which means do whatever you want with it except sell it.

#### Major functionality already set up:

* [undo-tree](http://www.emacswiki.org/emacs/UndoTree) (persistent across reboots)
* [slime](http://common-lisp.net/project/slime/) and [paredit](http://www.emacswiki.org/emacs/ParEdit)
* [multiple-cursors](https://github.com/magnars/multiple-cursors.el) which is super dope (and tbh a better implementation than sublime or atom's)
* [helm](https://github.com/emacs-helm/helm)
* [markdown-mode](http://jblevins.org/projects/markdown-mode/)
* [evil](https://gitorious.org/evil/pages/Home) although i don't use it too much
* [smart-compile](http://www.emacswiki.org/emacs/SmartCompile) and various extremely useful defaults for it, defined in [smart-compile.el](lisp/smart-compile.el)
* various language major modes that don't come packaged, such as [go-mode](http://golang.org/misc/emacs/go-mode.el), [php-mode](http://sourceforge.net/projects/php-mode/), R and Julia through [ESS](http://ess.r-project.org/), and [qmake-mode](https://code.google.com/p/qmake-mode/source/browse/qmake.el)
* [linum](http://www.logic.at/prolog/linum/linum.html) line numbering (modified somewhat)
* [magit](https://github.com/magit/magit)
* [saveplace](http://www.emacswiki.org/emacs/SavePlace)
* a comprehensive [ibuffer](http://www.emacswiki.org/emacs/IbufferMode) filetype/name filter system based off [this brilliant post](http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html)
* a simple not-very-robust script to remember files previously opened and restore them upon startup
* a really dope implementation of [web-beautify](https://github.com/yasuyk/web-beautify) and [clang-format](http://clang.llvm.org/docs/ClangFormat.html) for editing js/html/css and c/c++/obj-c
* [ein](https://github.com/tkf/emacs-ipython-notebook) which is actually pretty sweet

#### Setup

1. Initial
	* Start emacs with the ```-l``` option to load the .emacs file within this repository instead of one in ~/.emacs. You can also use a symlink, or you can alias it: ```alias emacs='emacs -l ~/.emacs.d/.emacs'```.
2. Lisp setup (not required if you don't use lisp)
	* Install slime and quicklisp according to [these instructions](http://www.mohiji.org/2011/01/31/modern-common-lisp-on-linux/)
	* If things are not working (e.g. autocompletion, eldoc) in ```lisp-mode```, start slime using ```M-x slime```.
3. Clojure setup (not required if you don't use clojure)
    * Install [leiningen](http://leiningen.org/) and put ```{:user {:plugins [[cider/cider-nrepl "0.8.2-SNAPSHOT"]]}}``` in `~/.lein/profiles.clj`.
