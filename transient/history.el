((magit-bisect nil)
 (magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-cherry-pick
  ("--ff")
  nil)
 (magit-commit nil
               ("--no-verify" "--reuse-message=ORIG_HEAD"))
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-fetch nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--color" "--decorate" "--stat")
  ("-n256" "--graph" "--decorate")
  ("-n256" "--graph" "--color" "--decorate")
  ("-n256" "-Sgpg" "--graph" "--decorate")
  ("-n256" "-Spin" "--graph" "--decorate")
  ("-n256" "-Spinentry" "--graph" "--decorate")
  ("-n256" "--graph" "--color" "--decorate" "--show-signature" "--stat")
  ("-n256" "--simplify-by-decoration" "--graph" "--color" "--decorate" "--show-signature" "--stat")
  ("-n256" "--color" "--decorate" "--show-signature" "--stat")
  ("-n256" "--color" "--decorate" "--show-signature" "--patch"))
 (magit-log:-S "gpg" "pin" "pinentry")
 (magit-margin-settings nil)
 (magit-merge nil)
 (magit-pull nil)
 (magit-push nil
             ("--force-with-lease"))
 (magit-rebase nil)
 (magit-remote
  ("-f"))
 (magit-revert
  ("--edit"))
 (magit-revision-history "ORIG_HEAD")
 (magit-stash nil))
