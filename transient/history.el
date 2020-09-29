((magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-cherry-pick nil
                    ("--ff"))
 (magit-commit nil
               ("--no-verify" "--reuse-message=ORIG_HEAD"))
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-fetch nil)
 (magit-log
  ("-n256" "--graph" "--decorate"))
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
