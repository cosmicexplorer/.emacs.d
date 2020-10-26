;;; lisp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "clang-format" "clang-format.el" (0 0 0 0))
;;; Generated autoloads from clang-format.el

(autoload 'clang-format-line "clang-format" "\
Use clang-format to format the current line." t nil)

(autoload 'clang-format-dwim "clang-format" "\


\(fn &optional PFX)" t nil)

(autoload 'clang-format-newline "clang-format" nil t nil)

(register-definition-prefixes "clang-format" '("clang-format"))

;;;***

;;;### (autoloads nil "cuda-mode" "cuda-mode.el" (0 0 0 0))
;;; Generated autoloads from cuda-mode.el

(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

(autoload 'cuda-mode "cuda-mode" "\
Major mode for editing CUDA.
Cuda is a C like language extension for mixed native/GPU coding
created by NVIDIA

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `cuda-mode-hook'.

Key bindings:
\\{cuda-mode-map}

\(fn)" t nil)

(register-definition-prefixes "cuda-mode" '("cuda-"))

;;;***

;;;### (autoloads nil "fireplace" "fireplace.el" (0 0 0 0))
;;; Generated autoloads from fireplace.el

(register-definition-prefixes "fireplace" '("draw-f" "fireplace" "flame" "fp-" "gotoxy" "make-grid" "smoke"))

;;;***

;;;### (autoloads nil "gtags" "gtags.el" (0 0 0 0))
;;; Generated autoloads from gtags.el

(autoload 'gtags-mode "gtags" "\
Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.

Specify the root directory of project.
	\\[gtags-visit-rootdir]
Input tag name and move to the definition.
	\\[gtags-find-tag]
Input tag name and move to the definition in other window.
        \\[gtags-find-tag-other-window]
Input tag name and move to the referenced point.
	\\[gtags-find-rtag]
Input symbol and move to the locations.
	\\[gtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[gtags-find-with-grep]
Input pattern, search with idutils(1) and move to the locations.
	\\[gtags-find-with-idutils]
Input pattern and move to the top of the file.
	\\[gtags-find-file]
Input pattern and show the list of definitions of the file.
	\\[gtags-parse-file]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-from-here]
Display current screen on hypertext browser.
	\\[gtags-display-browser]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-by-event]
Move to previous point on the stack.
	\\[gtags-pop-stack]

Key definitions:
\\{gtags-mode-map}
Turning on Gtags mode calls the value of the variable `gtags-mode-hook'
with no args, if that value is non-nil.

\(fn &optional FORCES)" t nil)

(register-definition-prefixes "gtags" '("gtags-"))

;;;***

;;;### (autoloads nil "highlight-80+" "highlight-80+.el" (0 0 0 0))
;;; Generated autoloads from highlight-80+.el

(autoload 'highlight-80+-mode "highlight-80+" "\
Highlight the portions of lines longer than 80 characters.

If called interactively, enable Highlight-80+ mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "highlight-80+" '("highlight-80+-"))

;;;***

;;;### (autoloads nil "highlight-sexp" "highlight-sexp.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from highlight-sexp.el

(autoload 'highlight-sexp-mode "highlight-sexp" "\
Minor mode to highlight the current zone according to its
    context, i.e. sexp, comment, string.

If called interactively, enable Highlight-Sexp mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-highlight-sexp-mode 'globalized-minor-mode t)

(defvar global-highlight-sexp-mode nil "\
Non-nil if Global Highlight-Sexp mode is enabled.
See the `global-highlight-sexp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-sexp-mode'.")

(custom-autoload 'global-highlight-sexp-mode "highlight-sexp" nil)

(autoload 'global-highlight-sexp-mode "highlight-sexp" "\
Toggle Highlight-Sexp mode in all buffers.
With prefix ARG, enable Global Highlight-Sexp mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Sexp mode is enabled in all buffers where
`(lambda nil (highlight-sexp-mode t))' would do it.
See `highlight-sexp-mode' for more information on Highlight-Sexp mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "highlight-sexp" '("hl-sexp-"))

;;;***

;;;### (autoloads nil "llvm-mode" "llvm-mode.el" (0 0 0 0))
;;; Generated autoloads from llvm-mode.el

(autoload 'llvm-mode "llvm-mode" "\
Major mode for editing LLVM source files.
\\{llvm-mode-map}
  Runs `llvm-mode-hook' on startup.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.ll\\'") 'llvm-mode))

(register-definition-prefixes "llvm-mode" '("llvm-"))

;;;***

;;;### (autoloads nil "misc-cmds" "misc-cmds.el" (0 0 0 0))
;;; Generated autoloads from misc-cmds.el

(autoload 'forward-overlay "misc-cmds" "\
Move forward ARG overlays.
Move cursor to next position where an overlay starts or ends.
If there are no more overlay boundaries, move to (point-max).

\(fn &optional ARG)" t nil)

(autoload 'forward-char-same-line "misc-cmds" "\
Move forward a max of ARG chars on the same line, or backward if ARG < 0.
Returns the signed number of chars moved if /= ARG, else returns nil.

\(fn &optional ARG)" t nil)

(autoload 'end-of-line+ "misc-cmds" "\
Move cursor to end of current line or end of next line if repeated.
This is similar to `end-of-line', but:
  If called interactively with no prefix arg:
     If the previous command was also `end-of-line+', then move to the
     end of the next line.  Else, move to the end of the current line.
  Otherwise, move to the end of the Nth next line (Nth previous line
     if N<0).  Command `end-of-line', by contrast, moves to the end of
     the (N-1)th next line.

\(fn &optional N)" t nil)

(autoload 'beginning-of-line+ "misc-cmds" "\
Move cursor to beginning of current line or next line if repeated.
This is the similar to `beginning-of-line', but:
1. With arg N, the direction is the opposite: this command moves
   backward, not forward, N lines.
2. If called interactively with no prefix arg:
      If the previous command was also `beginning-of-line+', then move
      to the beginning of the previous line.  Else, move to the
      beginning of the current line.
   Otherwise, move to the beginning of the Nth previous line (Nth next
      line if N<0).  Command `beginning-of-line', by contrast, moves to
      the beginning of the (N-1)th next line.

\(fn &optional N)" t nil)
 (autoload 'end-of-visual-line+ "misc-cmds")
 (autoload 'beginning-of-visual-line+ "misc-cmds")

(autoload 'beginning-or-indentation "misc-cmds" "\
Move cursor to beginning of this line or to its indentation.
If at indentation position of this line, move to beginning of line.
If at beginning of line, move to beginning of previous line.
Else, move to indentation position of this line.

With arg N, move backward to the beginning of the Nth previous line.
Interactively, N is the prefix arg.

\(fn &optional N)" t nil)

(autoload 'indent-rigidly-tab-stops "misc-cmds" "\
Indent the region rigidly according to the NTH tab stop.
`tab-stop-list' defines the available tab stops.  NTH is the numeric
prefix arg.  One means indent rigidly the amount given by the first
tab stop.  If NTH is negative then indent negatively (outdent).

\(fn START END NTH)" t nil)

(autoload 'delete-extra-windows-for-buffer "misc-cmds" "\
Delete all other windows showing the selected window's buffer." t nil)

(autoload 'delete-window-maybe-kill-buffer "misc-cmds" "\
Delete selected window.
    If no other window shows its buffer, kill the buffer too." t nil)

(autoload 'recenter-top-bottom "misc-cmds" "\
Move current line to window center, top, and bottom, successively.
With a prefix argument, this is the same as `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center.

Otherwise move current line to window center on first call, and to
top, middle, or bottom on successive calls.

The starting position of the window determines the cycling order:
 If initially in the top or middle third: top -> middle -> bottom.
 If initially in the bottom third: bottom -> middle -> top.

Top and bottom destinations are actually `scroll-conservatively' lines
from true window top and bottom.

\(fn &optional ARG)" t nil)

(autoload 'recenter-top-bottom-1 "misc-cmds" "\
Move current line to window center, top, and bottom, successively.
With prefix ARG, move current line to window-line ARG.
Top and bottom destinations are actually `scroll-conservatively' lines
from true top and bottom.

\(fn &optional ARG)" t nil)

(autoload 'recenter-top-bottom-2 "misc-cmds" "\
Move current line to line ARG, window center, top, or bottom.
With a prefix argument, this is the same as `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center.

Otherwise, the window starting position determines the next position:
 If in the top third, move to bottom.
 If in middle third,  move to top.
 If in bottom third,  move tocenter.

Top and bottom destinations are actually `scroll-conservatively' lines
from true top and bottom.

\(fn &optional ARG)" t nil)

(autoload 'mark-buffer-after-point "misc-cmds" "\
Select the part of the buffer after point.
With a prefix argument, select the part before point.

\(fn REVERSEP)" t nil)

(autoload 'mark-buffer-before-point "misc-cmds" "\
Select the part of the buffer before point.
With a prefix argument, select the part after point.

\(fn REVERSEP)" t nil)

(defalias 'selection-length 'region-length)

(defalias 'count-chars-in-region 'region-length)

(autoload 'region-length "misc-cmds" "\
Display the number of characters in the region in a message." t nil)

(autoload 'goto-longest-line "misc-cmds" "\
Go to the first of the longest lines in the region or buffer.
If the region is active, it is checked.
If not, the buffer (or its restriction) is checked.

Returns a list of three elements:

 (LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE.
LINES-CHECKED is the number of lines measured.

Interactively, a message displays this information.

If there is only one line in the active region, then the region is
deactivated after this command, and the message mentions only LINE and
LINE-LENGTH.

If this command is repeated, it checks for the longest line after the
cursor.  That is *not* necessarily the longest line other than the
current line.  That longest line could be before or after the current
line.

To search only from the current line forward, not throughout the
buffer, you can use `C-SPC' to set the mark, then use this
\(repeatedly).

\(fn BEG END)" t nil)

(autoload 'goto-long-line "misc-cmds" "\
Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN.

\(fn LEN)" t nil)

(autoload 'delete-lines "misc-cmds" "\
Delete NUM-LINES lines, starting at point.
Lines are deleted, not killed.
With positive prefix arg, deletion is forward.
With negative prefix arg, deletion is backward.

\(fn NUM-LINES)" t nil)

(autoload 'comment-region-lines "misc-cmds" "\
Like `comment-region' (which see), but comment/uncomment whole lines.

\(fn BEG END &optional ARG)" t nil)

(autoload 'region-to-buffer "misc-cmds" "\
Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents.

\(fn START END BUFFER ARG)" t nil)

(autoload 'region-to-file "misc-cmds" "\
With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents.

\(fn START END FILENAME ARG)" t nil)

(autoload 'chmod "misc-cmds" "\
Execute Unix command `chmod'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chmod').

\(fn CMD)" t nil)

(autoload 'chgrp "misc-cmds" "\
Execute Unix command `chgrp'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chgrp').

\(fn CMD)" t nil)

(autoload 'chown "misc-cmds" "\
Execute Unix command `chown'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chown').

\(fn CMD)" t nil)

(autoload 'kill-buffer-and-its-windows "misc-cmds" "\
Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string).

\(fn BUFFER)" t nil)

(autoload 'indirect-buffer "misc-cmds" "\
Edit stuff in this buffer in an indirect-buffer window.
The indirect buffer can have a different major mode from current." t nil)

(defalias 'clear-search-ring 'clear-search-history)

(autoload 'clear-search-history "misc-cmds" "\
Clear the search history (empty it).
With prefix arg, clear also the regular-expression search history.

\(fn &optional REGEXP-TOO-P)" t nil)

(defalias 'clear-regexp-search-ring 'clear-regexp-search-history)

(autoload 'clear-regexp-search-history "misc-cmds" "\
Clear the regular-expression search history (empty it).
With prefix arg, clear also the simple search history.

\(fn &optional SIMPLE-TOO-P)" t nil)

(autoload 'clear-search-histories "misc-cmds" "\
Clear both search histories: simple search and regexp search." t nil)

(autoload 'switch-to-alternate-buffer "misc-cmds" "\
Like `switch-to-buffer', but also kill the current buffer.

\(fn BUFFER &optional NORECORD FORCE-SAME-WINDOW)" t nil)

(autoload 'switch-to-alternate-buffer-other-window "misc-cmds" "\
Like `switch-to-buffer-other-window', but also kill the current buffer.

\(fn BUFFER &optional NORECORD)" t nil)

(autoload 'revert-buffer-no-confirm "misc-cmds" "\
Revert buffer without confirmation." t nil)

(autoload 'view-X11-colors "misc-cmds" "\
View file `/usr/lib/X11/rgb.txt', which lists available X11 colors." t nil)

(register-definition-prefixes "misc-cmds" '("read-shell-file-command" "resolve-file-name"))

;;;***

;;;### (autoloads nil "pp-c-l" "pp-c-l.el" (0 0 0 0))
;;; Generated autoloads from pp-c-l.el

(let ((loads (get 'Pretty-Control-L 'custom-loads))) (if (member '"pp-c-l" loads) nil (put 'Pretty-Control-L 'custom-loads (cons '"pp-c-l" loads))))

(defface pp^L-highlight (if (> emacs-major-version 21) '((((type x w32 mac graphic) (class color)) (:box (:line-width 3 :style pressed-button))) (t (:inverse-video t))) '((((type x w32 mac graphic) (class color)) (:foreground "Blue" :background "DarkSeaGreen1")) (t (:inverse-video t)))) "\
*Face used to highlight `pp^L-^L-vector'." :group 'Pretty-Control-L :group 'faces)

(defvar pp^L-^L-string "          Section (Printable Page)          " "\
*Highlighted string displayed in place of each Control-l (^L) character.
If `pp^L-^L-string-function' is non-nil, then the string that function
returns is used instead of `pp^L-^L-string'.")

(custom-autoload 'pp^L-^L-string "pp-c-l" t)

(defalias 'pp^l 'pretty-control-l-mode)

(autoload 'refresh-pretty-control-l "pp-c-l" "\
Reinitialize `pretty-control-l-mode', if on, to update the display." t nil)

(register-definition-prefixes "pp-c-l" '("pp^L-^L-"))

;;;***

;;;### (autoloads nil "qmake" "qmake.el" (0 0 0 0))
;;; Generated autoloads from qmake.el

(register-definition-prefixes "qmake" '("qmake-"))

;;;***

;;;### (autoloads nil "revbufs" "revbufs.el" (0 0 0 0))
;;; Generated autoloads from revbufs.el

(register-definition-prefixes "revbufs" '("revbufs"))

;;;***

;;;### (autoloads nil "smart-compile" "smart-compile.el" (0 0 0 0))
;;; Generated autoloads from smart-compile.el

(autoload 'smart-compile "smart-compile" "\
An interface to `compile'.
It calls `compile' or other compile function,
which is defined in `smart-compile-alist'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "smart-compile" '("add-compile-script-name" "byte-compile-file-and-remove" "clear-" "count-chars-in-region" "global-" "go-fmt-file" "pants-build-file" "pp^" "sbt-build-file" "selection-length" "smart-compile-" "strip-minus-c"))

;;;***

;;;### (autoloads nil "undo-tree" "undo-tree.el" (0 0 0 0))
;;; Generated autoloads from undo-tree.el

(autoload 'undo-tree-mode "undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

If called interactively, enable Undo-Tree mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

\(fn &optional ARG)" t nil)

(put 'global-undo-tree-mode 'globalized-minor-mode t)

(defvar global-undo-tree-mode nil "\
Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree" "\
Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

(autoload 'undo-tree-undo "undo-tree" "\
Undo changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, only undo changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits undo to
changes within the current region.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "global-undo-tree-mode" "turn-on-undo-tree-mode" "undo-"))

;;;***

;;;### (autoloads nil nil ("llvm-stuff.el") (0 0 0 0))

;;;***

(provide 'lisp-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lisp-autoloads.el ends here
