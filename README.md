.emacs.d
========

Because I've gone through hell and back to make this the best editor on the planet. If I say I've "set up" something, it means I downloaded it from the brilliant people who spent their valuable time to make it and ingraciously shoved it into my own unified configuration. The most meaningful contribution I've done is find usable non-conflicting keybindings for all of these, although I've received multiple reports that the keybindings are "weird" and "dumb." Ah well. Genius is rarely understood in its own time.

There's a lot of experimental stuff in here that I play around with that may not work completely or may do unexpected things, so test out any code you pull from this before using it. I apologize for the few 50+ line functions in advance. Maybe I'll refactor them someday.

This is all licensed under GPLv3, which means do whatever you want with it except sell it.

#### Major functionality beyond what's available in MELPA:

* [slime](http://common-lisp.net/project/slime/) and [paredit](http://www.emacswiki.org/emacs/ParEdit) setup with cool keybindings.
* [smart-compile](http://www.emacswiki.org/emacs/SmartCompile) and various extremely useful defaults for it, defined in [smart-compile.el](lisp/smart-compile.el).
* some language major modes that don't come packaged, such as R and Julia through [ESS](http://ess.r-project.org/), and [qmake-mode](https://code.google.com/p/qmake-mode/source/browse/qmake.el).
* [linum](http://www.logic.at/prolog/linum/linum.html) line numbering (modified to make it not incredibly dumb).
* [saveplace](http://www.emacswiki.org/emacs/SavePlace) (which works most of the time).
* a comprehensive [ibuffer](http://www.emacswiki.org/emacs/IbufferMode) filetype/name filter system based off [this brilliant post](http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html)
* a simple not-very-robust script to remember files previously opened and restore them upon startup
* a really dope implementation of [web-beautify](https://github.com/yasuyk/web-beautify) and [clang-format](http://clang.llvm.org/docs/ClangFormat.html) for editing js/html/css and c/c++/obj-c
* a "warning words" system to highlight certain specified text in `font-lock-warning-face`.
* a few scripts to setup ssh-agent on startup so you don't have to type in your ssh password constantly when using git/etc.

#### Setup

1. Initial:
	* Start emacs with the `-l` option to load the .emacs file within this repository instead of one in ~/.emacs. You can also use a symlink, or you can alias it: `alias emacs='emacs -l ~/.emacs.d/.emacs'`.
2. Lisp setup (not required if you don't use lisp):
	* There is a not-too-robust function in the setup files that automatically sets up quicklisp and emacs integration according to [these instructions](http://www.mohiji.org/2011/01/31/modern-common-lisp-on-linux/) if you have installed `sbcl` and are on a gnu/linux system. It should work automatically; it not, look at [how it's done](init-scripts/slime-setup.el) and add to it!
	* If things are not working (e.g. autocompletion, eldoc) in ```lisp-mode```, start slime using ```M-x slime```.
3. Clojure setup (not required if you don't use clojure):
    * Install [leiningen](http://leiningen.org/). The rest is done for you (hopefully).
4. Haskell setup (not required if you don't use haskell):
    * Install [ghc-mod](https://github.com/kazu-yamamoto/ghc-mod). You'll need to add `~/cabal/.bin` (or the corresponding directory on your operating system) to your PATH.
    * It appears from certain [issues](https://github.com/kazu-yamamoto/ghc-mod/issues/437) on the ghc-mod repo that running `cabal install ghc-mod` may not work. In that case, run `git clone git@github.com:kazu-yamamoto/ghc-mod.git && cd ghc-mod && cabal install` to install the newest release from git, which should work.
5. Scala setup (blah blah blah) from [this page](https://github.com/ensime/ensime-emacs/wiki/Quick-Start-Guide):
    * run `sbt` once to get the folder structure set up
    * add `addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.7")` to `~/.sbt/0.13/plugins/plugins.sbt`
    * run `sbt gen-ensime` to generate a per-project ensime folder
