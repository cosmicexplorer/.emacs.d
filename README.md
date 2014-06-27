.emacs
============

cause i spent a while getting my emacs perfect and it might help somebody else

#### Appropriate Commands:
1. ```git submodule foreach git pull origin master```
2. ```emacs -l ~/emacs.d/.emacs```

#### Explanation for Above Commands:
1. runs git pull on all submodules to ensure packages such as el-get are updated
2. alias to load .emacs within emacs.d directory instead of home; change ~ to the parent directory containing this repository
