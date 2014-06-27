((cl-lib status "installed" recipe
				 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (dired-xattr status "installed" recipe
							(:name dired-xattr :type github :description "Handle MacOSX Finder color label in dired." :pkgname "renard/dired-xattr" :depends htmlize))
 (doc-view-fit-to-page status "installed" recipe
											 (:name doc-view-fit-to-page :type github :website "https://github.com/laysakura/doc-view-fit-to-page" :description "Fit-to-page extension for DocViewMode" :pkgname "laysakura/doc-view-fit-to-page"))
 (go-mode status "installed" recipe
					(:name go-mode :description "Major mode for the Go programming language" :type http :url "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip" :localname "go-mode.el"))
 (highlight-parentheses status "installed" recipe
												(:name highlight-parentheses :description "Highlight the matching parentheses surrounding point." :type github :pkgname "nschum/highlight-parentheses.el"))
 (htmlize status "installed" recipe
					(:name htmlize :website "http://www.emacswiki.org/emacs/Htmlize" :description "Convert buffer text and decorations to HTML." :type http :url "http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi" :localname "htmlize.el"))
 (linum-relative status "installed" recipe
								 (:name linum-relative :type emacswiki :description "Display relative line number in the left margin" :features linum-relative))
 (php-mode status "installed" recipe
					 (:name php-mode :description "A PHP mode for GNU Emacs " :type github :pkgname "ejmr/php-mode" :website "https://github.com/ejmr/php-mode"))
 (yasnippet status "installed" recipe
						(:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :compile "yasnippet.el" :submodule nil :build
									 (("git" "submodule" "update" "--init" "--" "snippets")))))
