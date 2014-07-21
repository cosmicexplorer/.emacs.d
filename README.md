.emacs.d
============

Because I've gone through hell and back to make this the best editor on the planet.

#### Major functionality

#### Dependencies
These aren't actually required to run it, just to use certain modes. Email me about any others you find.

1. [sbcl](http://sbcl.org) (for slime usage)
2. [R](http://www.r-project.org) and [julia](http://julialang.org) languages (for ESS)

#### Setup

1. Start emacs with the '-l' option to load the .emacs file within this repository.
	* You can alias it: ```alias emacs='emacs -l ~/.emacs.d/.emacs'```
2. .sbclrc loads things for slime; make the .sbclrc within your home directory (the one sbcl reads from when it starts) read ```(load "~/.emacs.d/.sbcl")``` to load everything contained within there.
    * If sbcl does not recognize packages, ensure that .sbclrc points to the correct location of the 'setup.lisp' file (for me it's in ~/.emacs.d/quicklisp/setup.lisp).
    * If you're not using sbcl, that's cool too: look at the documentation [here](http://www.quicklisp.org/beta/) to set up your preferred lisp compiler.
3. If things are not working (e.g. autocompletion, eldoc) in ```lisp-mode```, start slime using ```M-x slime```.

#### Design decisions

* Why not use submodules to contain git repositories of all dependencies instead of rehosting them here?
	* I've rewritten enough of the code in these packages to suit my personal configuration that keeping it all sane would be a nightmare. In addition, submodules are extremely hard to use for most people who don't specialize in git and require extra very-easy-to-get-wrong setup procedures. I want this to be plug and play, for any boxes I reinstall this on and anyone else who doesn't want to spend years trying to get complex emacs plugins to work.
* Why not send a pull request to those projects?
	* Because the changes made are almost purely personalization, nothing that would actually improve the software for anyone else. emacs is very good at providing a common framework to add different functionality to, but once the functionality gets crowded they all start sliding around and hitting each other. Working with changing dependencies on the fragile tower that is my bloated emacs configuration is, as you might imagine, hell.
