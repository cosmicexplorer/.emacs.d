.emacs.d
============

cause i spent a while getting my emacs perfect and it might help somebody else

#### for use
1. start emacs with the '-l' option to load the .emacs file within this repository. you can alias it: ```alias emacs='emacs -l ~/.emacs.d/.emacs'```
2. .sbclrc loads things for sbcl usage; make the .sbclrc within your home directory (the one sbcl reads from when it starts) read ```(load "~/.emacs.d/.sbcl")``` to load everything contained within there

#### no submodules?
Why not use submodules to contain git repositories of all dependencies?

1. Submodules are hard and annoying and nobody wants to spend the time learning how to use them, and to set them up. This repository is made to be installed and used immediately without any setup.
2. Package managers such as el-get and whatnot are incredibly non-portable and it's impossible to set them up on a new system without a hassle. In the current configuration you can just git clone and go.
3. Working with changing dependencies on the fragile tower that is my bloated emacs configuration is, as you might imagine, hell.

#### Appropriate Commands:
1. ```emacs -l ~/emacs.d/.emacs```

#### Explanation for Above Commands:
1. alias to load .emacs within emacs.d directory instead of home; change ~ to the parent directory containing this repository
