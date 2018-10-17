;;; mtorus.el --- navigation with marks on a ring of rings (torus)
;; $Id: mtorus.el,v 1.6 2003/01/14 21:16:16 ska Exp $
;; Copyright (C) 2002-2003 by Stefan Kamphausen
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;; Keywords: bookmarks, navigation, tools

(defvar mtorus-version "1.6"
  "Version number of MTorus.")

;; This file is not part of XEmacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Credits:
;; - As it is with most of my Emacs stuff this was written together
;;   with Claus Brunzema <mail@cbrunzema.de>
;; - Christoph Conrad <christoph.conrad@gmx.de>
;;   for bug-fixes and GNU Emacs compatibility code hints


;;; Commentary:

;; MTorus on the Web:
;; Main page:
;; http://www.skamphausen.de/software/skamacs/mtorus.html
;; German intro:
;; http://www.skamphausen.de/xemacs/lisp/mtorus.html

;; Installation:
;; =============
;; Put into your `user-init-file' (rectangle selectable :-):
;;
;; ;; MTorus: navigation with marks on a ring of rings (torus)
;; (require 'mtorus)
;; (mtorus-init)
;; (mtorus-install-suggested-bindings)
;;
;; Maybe you don't want the whole torus but like the buffer cycling?
;; Don't init the torus and use some backend functions instead:
;; ;; MTorus: navigation with marks on a ring of rings (torus)
;; (require 'mtorus)
;; (global-set-key '[(shift right)] 'mtorus-cycle-blist-next)
;; (global-set-key '[(shift left)]  'mtorus-cycle-blist-prev)
;;
;; Suggested Keybindings:
;; Shift Left/Right     cycle markers on a ring
;; Shift Up/Down        cycle ring on the torus
;; F11 (with modifiers) create/delete/view a new ring
;; F12 (with modifiers) create/delete/     marker
;; modifiers:           None  /Shift /Control
;;
;; If you don't like those (maybe because you use the windows like
;; selection with shift hold down)  you should create your own
;; bindings to the functions
;;
;; mtorus-cycle-marker-next
;; mtorus-cycle-marker-previous
;; mtorus-cycle-ring-next
;; mtorus-cycle-ring-previous
;; mtorus-new-ring
;; mtorus-delete-current-ring
;; mtorus-describe-current-ring
;; mtorus-add-current-pos-to-current-ring
;; mtorus-delete-current-marker-from-current-ring
;; mtorus-update-current-marker
;;
;; For a slightly different installation which restores the torus next
;; time you open your (X)Emacs see below...
;;
;; Usage:
;; ======
;; MTorus lets you work with several groups of buffers, each group
;; being a separate ring. This is
;; all for easier navigation through buffers. I've been using some
;; buffer cycling functions on (shift left and shift right) for quite
;; a long time now and I find myself cycling through larger and larger
;; lists every day. Starting one instance of (X)Emacs for each editing
;; context isn't the way and I found no way of doing what I want with
;; frames. An `editing context' means a logical grouping of
;; buffers. This could be a group for quick edit of the emacs
;; configuration files while you're actually working on some
;; (Ruby/Perl/whatever-) program or it could be all the headers of
;; your C project while all the .c-files make up another
;; group. Whatever you can think of. You could even make different
;; parts of your buffers (point positions) show up in different groups
;; like when one set of functions spread throughout one (or more)
;; files is responsible for one specific task or like working on a
;; chapter in a LaTeX-document while defining some macros at the top
;; of the file.  There is always a default group which contains all
;; the open buffers and can not be altered.
;;
;; Like so:
;; +- Ring: C-code -------------------------------------------------+
;; |                                                                |
;; | +- marker ---+  +- marker ---+  +- marker ---+                 |
;; | | main.cc/224|  | main.cc/567|  | main.hh/312|                 |
;; | +------------+  +------------+  +------------+                 |
;; |                                                                |
;; | +- marker ---+  +- marker ----+                                |
;; | | *Occur*/84 |  | README/1388 |                                |
;; | +------------+  +-------------+                                |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; +- Ring: quick --------------------------------------------------+
;; |                                                                |
;; |                                                                |
;; | +- marker ---+  +- marker -----+                               |
;; | | .emacs/224 |  | *scratch*/33 |                               |
;; | +------------+  +--------------+                               |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; +- Ring: doc ----------------------------------------------------+
;; |                                                                |
;; |                                                                |
;; | +- marker ---+  +- marker ---+                                 |
;; | | *info*/999 |  | Man: ls/1  |                                 |
;; | +------------+  +------------+                                 |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; Choosing another entry in such a group is done by actually cycling
;; through the group (like with e.g. the kill-ring) and since the
;; entries are made of a buffer together with a position (and that is a
;; marker) each of the groups is comparable to the mark-ring. And
;; since we are cycling through a collection of those rings we are
;; using a Torus here. Hence the name: "mtorus": mark-torus.
;; (A "group" will usually be referred to as a "ring" from now.)

;; MTorus tries to be be somewhat intelligent in when to overwrite
;; stored positions (the current marker moves with the point) and hopes
;; to become as intuitive as a good emacs module has to be.  But this
;; will evolve with time in the daily life (and MTorus tries to keep
;; as many aspects configurable with customize variables).
;;
;;
;; Saving And Restoring the Torus
;; ------------------------------
;; The saving of rings in files is possible so that you can easily
;; resume work on the next day/month/year/after lunch.
;; This is VERY ALPHA!!
;; It might working out of the box when you set
;; mtorus-save-on-exit to t and put into your init file:
;; (mtorus-read-torus).
;; It's probably a good idea to put this at the very end of your
;; initializations, which might be the end of ~/.xemacs/init.el or of
;; ~/.emacs, right after the (desktop-read) function which you might
;; have in your init file. MTorus tries to load all the files
;; mentioned in that buffer and hopefully saves them in a reasonable
;; way.

;; TODO:
;; =====
;;  - I found swbuff.el by David Ponce at
;;    http://perso.wanadoo.fr/david.ponce/more-elisp.html and I _love_
;;    the status window of his buffer switching mechanism. Wannahave!
;;
;;  - MAJOR OVERHAUL
;;    Currently in the refactoring process...
;;    * renaming to mtorus should be done
;;    * renaming of some functions (like pos-add ..)
;;    * markers are introduced but not thoroughly tested
;;    * with the introduction of markers I don't need the buffer name
;;      and the cons cell anymore. This is probably a lot of work
;;      ... and hopefully done now!!
;;    * I don't think that desktop-globals-to-save can handle markers
;;      so we need a wrapper here:
;;        saving/reading in/from files: see code of
;;        term-read-input-ring in term.el! Convert marker to points
;;        when saving and recreate markers from those when
;;        reading. This assumes no other editor :-)?
;;        UPDATE: this is written but not thoroughly tested
;;
;; - create torus.el as an abstract interface to a ring of rings
;;   (using ring.el) and then create mtorus.el as an application of
;;   torus.el?
;;  - find good defaults for the intuitive-custom-settings
;;  - are all the common buffer lists (buffer-list, recent-files,
;;    bookmarks? of a structure we could interpret here? Then
;;    recent-files could be another 'special' list with a matching
;;    accessor function and we could browse through those groups, too?
;;    having a "special:"-string at the beginning of a ring-name or
;;    instead of a marker might be a good idea here?
;;  - see FIXMEs throughout the file
;;  - More Refactoring:
;;       * (if (equal ...) -> session-is-default-p ...
;;       * consistent use of buffer/entry and when to use -name ...
;;  - check whether all settings of intuition switches work correctly
;;    I really hope the The World does that for me.

;;; Code:

(eval-when-compile (require 'cl))

;; ==============================

;; Credit : https://github.com/emacs-ess/ESS/pull/400/files

(if (not (functionp 'remassoc))
    (defun remassoc (key a)
      "remove an association pair from an alist"
      (if a
          (let ((pair (car a)))
            (if (equal (car pair) key)
                (cdr a)
              (cons pair (remassoc key (cdr a))))))))

;; ==============================

;; user options (customize)
(defgroup mtorus nil
  "An interface to navigating groups of buffers.
This code should work fast so intuition is a very important
matter. Some of the customizable variables are switches to tune the
behavior of the functions to your habits. Hopefully it is possible to
find good settings for many people."
  :tag "MTorus"
  :link '(url-link :tag "Home Page"
                   "http://www.skamphausen.de/software/skamacs/")
  :link '(emacs-commentary-link
		  :tag "Commentary in mtorus.el" "mtorus.el")
  :prefix "mtorus-"
  :group 'environment
  :group 'extensions
  :group 'convenience)

(defcustom mtorus-save-on-exit nil
  "*If set to t `mtorus-init' will install saving of desktop on exit.
The function `mtorus-quit' is placed on `kill-emacs-hook'."
  :type 'boolean
  :group 'mtorus)


(defcustom mtorus-show-marker-list-when-cycling-p t
  "*Whether the buffer-names are displayed when you cycle rings.
When you tend to have long lists it may be disturbing to have so many
buffers displayed, but on the other hand it might help you navigating
on the torus. Using some nifty functions in
`mtorus-describe-marker-func' that find function names at that
position makes that display even longer."
  :type 'boolean
  :group 'mtorus)

(defcustom mtorus-switch-to-marker-when-cycling-p nil
  "*Whether to switch to the current marker when switching rings.
This is one of the intuition switches.
When set to t and you switch to another ring you instantly move to
the current marker in the new ring. This might help picking the
right ring and can save a few keystrokes per day \(*). On the other
hand when you keep switching to the default ring to pick a
buffer/position to add to another ring it can be annoying.

\(*) keystrokes per day (kpd): a measurement for the pain applied to
     fingers. Combined with lines (of code) per pay (lpd):
     e = lpd/kpd
     it gives a measure of effectiveness in coding.
     Well, somehow...  *grin* "
  :type 'boolean
  :group 'mtorus)

(defcustom mtorus-update-marker-when-cycling-markers-p nil
  "*Whether to store the local point into the local marker.
This is one of the intuition switches.
Setting this to t means that you always 'move' inside a marker while
nil means that you wander around freely and have your fix positions
stored. In the last case you'll probably like to explicitly use
`mtorus-update-current-marker' and bind it to a key."
  :type 'boolean
  :group 'mtorus)

(defcustom mtorus-update-marker-when-cycling-rings-p nil
  "*Whether to store the local point into the local marker.
This is one of the intuition switches.
It is close to `mtorus-update-marker-when-cycling-markers-p' but
determines the behavior when cycling through rings which feels
quite different than cycling markers."
  :type 'boolean
  :group 'mtorus)

(defcustom mtorus-switch-when-deleting-p nil
  "*Whether to switch to the next marker when current marker is deleted.
This is one of the intuition switches.
Setting to t means that when you delete a marker from the current
ring you instantly move to the new current marker while setting to
nil keeps you at the current position."
  :type 'boolean
  :group 'mtorus)

(defcustom mtorus-buffer-skip-p
  'mtorus-default-buffer-skip-p
  "Predicate to use to skip buffers when cycling the real buffer list.
This has nothing to do with the cycling inside a normal ring.
A good example would be to use the result of
  (string-match \"^[ \\*]+\" (buffer-name buffer))
which skips the buffers with a star or a space at the beginning of
their buffer names.
The default predicate `mtorus-default-buffer-skip-p'  skips
buffers whose names begin with a space."
  :type 'function
  :group 'mtorus)

;; FIXME: do I need the defvar _before_ defcustom to work?
(defvar mtorus-default-describe-marker-alist
  '((c-mode . fume-function-before-point)
	(c++-mode . fume-function-before-point)
	(cperl-mode . fume-function-before-point)
	(emacs-lisp-mode . fume-function-before-point)
	(latex-mode . fume-function-before-point)
	(LaTeX-mode . fume-function-before-point)
	(lisp-mode . fume-function-before-point)
	(lisp-interaction-mode . fume-what-looking-at)
	(Manual-mode . fume-function-before-point)
	(ruby-mode . fume-function-before-point)
	;; just to have another example:
	(text-mode . point))
  "Alist of mode specific functions to use when describing a marker.
This is just a collection of things the author has come across and
it is far from being complete. It is provided as a convenience
function and an example for your own `mtorus-describe-marker-func'.

If you write any rule here please let me know.")

(defcustom mtorus-describe-marker-func
  mtorus-default-describe-marker-alist
  "*Maybe mode specific functions to use when describing a marker.

If set to nil the position the marker points to is used.
If set to an alist it's elements should be of the form:
\(major-mode . function)
and if the major mode of the buffer of the marker matches one on the
list the appropriate function is called after the point is
\(temporarily) moved to the buffer and the position. In other words:
the function should return some reasonable string when called at the
position of the marker. If it returns nil the point is used.

If no function can be found `point' is used.

Should you tend to have large lists you might want to set this to nil
to avoid the temporal switching of buffers that takes place even if
the user doesn't see it."
  :type 'function
  :group 'mtorus)


(defface mtorus-highlight-face
  '((((class color) (background light))
	 (:background "green"))
	(((class color) (background dark))
	 (:background "sea green"))
	(((class grayscale monochrome)
	  (background light))
	 (:background "black"))
	(((class grayscale monochrome)
	  (background dark))
	 (:background "white")))
  "Face for the highlighting of the line jumped to."
  :group 'mtorus)

(defcustom mtorus-save-hook nil
  "Hook run before saving the torus."
  :type 'hook
  :group 'mtorus)

(defcustom mtorus-init-hook nil
  "Hook run after the torus is initialized."
  :type 'hook
  :group 'mtorus)

(defcustom mtorus-default-ring-name "all-buffers"
  "The name of the ring that always contains all open buffers.
Cycling within this ring is different from cycling the others since it
always uses the real buffer list. It skips all buffers that
`mtorus-buffer-skip-p' returns t for."
  :type 'string
  :group 'mtorus)

(defcustom mtorus-file-name ".mtorus.el"
  "The file-name to use when saving the current torus."
  :type 'string
  :group 'mtorus)

; This comes later:
;(defgroup mtorus-pop-up nil
;  "Controls the display of a pop up window which displays the cycled
;  list."
;  :tag "MTorusPopUp"
;  :prefix "mtorus-popup"
;  :group 'mtorus)

;(defcustom mtorus-popup-use-popup-p t
;  "*Whether to use a pop up window or not."
;  :type 'boolean
;  :group 'mtorus-pop-up)

;(defcustom mtorus-popup-clear-timeout 5
;  "*Time in seconds before the pop up window is removed."
;  :type 'number
;  :group 'mtorus-pop-up)

;(defcustom mtorus-popup-separator " - "
;  "*String appearing between two entries in pop up window."
;  :type 'string
;  :group 'mtorus-pop-up)


(defvar mtorus-dirname nil
  "The directory from which a torus was last read.
This is the directory to which to save the torus when exiting emacs
and when `mtorus-save-on-exit' is t.")

;; variables
;; main data structure of form
;; (("ring1" (marker1
;;            marker2
;;            marker3
;;            marker4))
;;  ("ring2" (marker5
;;            marker6
;;            marker7)))
(defvar mtorus-torus nil
  "Alist containing all marker rings with their buffers.
The main data structure of MTorus. It contains all buffers and
markers in a structure like:
 \(\(\"sess1\" \(\marker1
           marker2
           marker3
           marker4))
 \(\"sess2\" \(\marker5
           marker6
           marker7)))
Positions are stored as markers so that they keep in place when
altering the contents of the buffer.")

(defvar mtorus-current-ring-name nil
  "The name of the currently chosen ring."
  )

(defvar mtorus-highlight-extent nil
  "Extent to use for highlighting.")


;; convenience
(defun mtorus-install-suggested-bindings ()
  "This sets the key-bindings that I consider useful.
This does not fulfill the requirements of good key-defining but I like
it and I provide it only as a convenience function."
  (interactive)
  (global-set-key '[(shift right)] 'mtorus-cycle-marker-next)
  (global-set-key '[(shift left)]  'mtorus-cycle-marker-previous)
  (global-set-key '[(shift up)]    'mtorus-cycle-ring-next)
  (global-set-key '[(shift down)]  'mtorus-cycle-ring-previous)
  ;; ring handling: f11
  (global-set-key '[(f11)]
	'mtorus-new-ring)
  (global-set-key '[(shift f11)]
	'mtorus-delete-current-ring)
  (global-set-key '[(control f11)]
	'mtorus-describe-current-ring)
  ;; marker handling: f12
  (global-set-key '[(f12)]
	'mtorus-add-current-pos-to-current-ring)
  (global-set-key '[(shift f12)]
	'mtorus-delete-current-marker-from-current-ring)
  (global-set-key '[(control f12)]
	'mtorus-update-current-marker)
  ;; not yet available:
  ;;(global-set-key '[(??)] 'mtorus-list-rings)
  )

;; GNU Emacs and downward compatibility
;; (with help from Christoph Conrad and speedbar.el):
(if (fboundp 'make-extent)
    (progn ;; XEmacs
	  (defalias 'mtorus-make-extent         'make-extent)
	  (defalias 'mtorus-set-extent-property 'set-extent-property)
	  (defalias 'mtorus-delete-extent       'delete-extent)
	  (defun mtorus-default-directory ()
		(default-directory)))
  ;; GNU Emacs
  (defalias 'mtorus-make-extent 'make-overlay)
  (defalias 'mtorus-set-extent-property 'overlay-put)
  (defalias 'mtorus-delete-extent 'delete-overlay)
  (defun display-message (label message &optional frame stdout-p)
	(message message))
  (defun mtorus-default-directory ()
	default-directory))

(defun mtorus-set-extent-face (extent face)
  (mtorus-set-extent-property extent 'face face))
(when (not (fboundp 'point-at-bol))
	(progn
	  (defun point-at-bol ()
		(save-excursion
		  (beginning-of-line)
		  (point)))
	  (defun point-at-eol ()
		(save-excursion
		  (end-of-line)
		  (point)))))

;; User visible functions
(defun mtorus-init ()
  "This starts the torus and opens the default ring.
The name of the default ring is taken from
`mtorus-default-ring-name' and this is special since the
cycling of markers is different there and it is not editable.
You can use the mtorus-cycle-blist-* functions separately if
you don't call this function so that you always just cycle the real
buffer list. Then you can use the variable
`mtorus-buffer-skip-p' to tune the cycling to your desired
behavior."
  (interactive)
  (setq mtorus-torus nil)
  (mtorus-new-ring mtorus-default-ring-name)
  (if mtorus-save-on-exit
	  (add-hook 'kill-emacs-hook
				'mtorus-quit))
  (run-hooks 'mtorus-init-hook))

(defun mtorus-default-buffer-skip-p (buffer)
  "The default predicate used for `mtorus-buffer-skip-p'
is to skip only the special buffers whose name begins with a space."
  (string-match "^[ ]+" (buffer-name buffer)))

(defun mtorus-new-ring (name &optional empty-ring)
  "Create a new ring and asks the user for a name.
Makes the new ring the current ring and puts the current buffer
with the current point into it."
  ;; FIXME: adding current place should be customizable and empty
  ;; rings should be possible
  (interactive "sRing name: ")
  ;; test for existence
  (if (mtorus-get-ring name)
	  (display-message 'no-log
		(format "A Ring with name \"%s\" already exists."
				name))
	;; create ring
	(setq mtorus-torus
		  (append mtorus-torus
				  (list
				   (cons name
						 (list
						  (list
						   (cond ((equal mtorus-default-ring-name
										 name)
								  "special: all open buffers")
								 ;; FIXME: how to remove this nil from
								 ;; list?
								 (empty-ring nil)
								 (t
								  (point-marker)))))))))
	(mtorus-switch-to-ring name)))

(defun mtorus-describe-current-ring ()
  "Show the current markers in the echo area.
Uses `mtorus-describe-marker-func' to decide how to display the
markers."
  (interactive)
  (let ((ring (mtorus-current-ring)))
	(display-message 'no-log
	  (format "Ring \"%s\"%s" (first ring);; ring name
			  ;; all markers
			  ;; FIXME: this check here?
			  (if (and mtorus-show-marker-list-when-cycling-p
					   (not (equal mtorus-default-ring-name
								   mtorus-current-ring-name)))
				  (concat ": "
						  (mapconcat
						   #'(lambda (marker)
							   (format "%s"
									   (mtorus-marker-description
										marker)))
						   (second ring) " "))
				;; else empty string
				"")))))

;; FIXME: maybe we can find a clever way to have a look at the length
;; of a description and adjust it (cropping or using point or ...?)?
;; So not to (over)fill the echo area.
(defun mtorus-marker-description (marker)
  "Return a string with the description of marker MARKER.
Uses `mtorus-describe-marker-func'."
  (let* ((buf (marker-buffer marker))
		 (bufname (buffer-name buf))
		 (pos (marker-position marker)))
	(concat bufname "("
			(format
			 "%s"
			 (if (and mtorus-describe-marker-func
					  (listp mtorus-describe-marker-func)
					  (featurep 'func-menu))
				 (progn
				   (if (buffer-live-p buf)
					   (save-excursion
						 (save-restriction
						   (widen)
						   (set-buffer buf)
						   (goto-char pos)
						   ;; *yuck*
						   (if (functionp
								(cdr
								 (assoc
								  major-mode
								  mtorus-describe-marker-func)))
							   (or
								(funcall
								 (cdr
								  (assoc
								   major-mode
								   mtorus-describe-marker-func)))
								(point))
							 (point))))
					 "*del*"))
			   pos))
			")")))


(defun mtorus-cycle-ring-next ()
  "Make the next ring the current ring.
See `mtorus-switch-to-ring' for discussion."
  (interactive)
  (let* ((current-ring-num (mtorus-find-current-ring-pos))
		 (new-position (mod (1+ current-ring-num) (length mtorus-torus)))
		 (new-ring (car (elt mtorus-torus new-position))))
	(mtorus-switch-to-ring new-ring)
	(mtorus-describe-current-ring)))

(defun mtorus-cycle-ring-previous ()
  "Make the previous ring the current ring.
See `mtorus-switch-to-ring' for discussion."
  (interactive)
  (let* ((current-ring-num (mtorus-find-current-ring-pos))
		 (new-position (mod (1- current-ring-num)
							(length mtorus-torus)))
		 (new-ring (car (elt mtorus-torus
							 new-position))))
	(mtorus-switch-to-ring new-ring)
	(mtorus-describe-current-ring)))

(defun mtorus-cycle-marker-next ()
  "Switch to the next marker in the current ring.
This can be another buffer or another position in the same buffer.
The point stored in the just left marker is updated if
`mtorus-update-marker-when-cycling-markers-p' is set to t."
  (interactive)
  (if (not (equal mtorus-default-ring-name
				  mtorus-current-ring-name))
	  (progn
		(let ((ring (mtorus-current-ring)))
		  (if (not (equal (length (second ring)) 1))
			  (progn
				(if mtorus-update-marker-when-cycling-markers-p
					(mtorus-update-current-marker))
				(if (mtorus-current-buffer-is-current-marker-p)
					(setf (second ring)
						  (append (last (second ring))
								  (butlast (second ring))))))
			(display-message 'no-log
			  "only marker.")))
		(mtorus-switch-to-current-marker))
	;; else: default ring uses good ol' buffer list
	(mtorus-cycle-blist-next)))


(defun mtorus-cycle-marker-previous ()
  "Switch to the previous buffer in the current ring.
The point stored in the just left marker is updated if
`mtorus-update-marker-when-cycling-markers-p' is set to t."
  (interactive)
  (if (not (equal mtorus-default-ring-name
				  mtorus-current-ring-name))
	  (progn
		(let ((ring (mtorus-current-ring)))
		  (if (not (equal (length (second ring)) 1))
			  (progn
				(if mtorus-update-marker-when-cycling-markers-p
					(mtorus-update-current-marker))
				(if (mtorus-current-buffer-is-current-marker-p)
					(setf (second ring)
						  (append (cdr (second ring))
								  (list (first (second ring)))))))
			(display-message 'no-log
			  "only marker.")))
		(mtorus-switch-to-current-marker))
	;; else: default ring uses good ol' buffer list
	(mtorus-cycle-blist-prev)))


(defun mtorus-cycle-blist-prev ()
  "Cycle the real buffer list.
This can be used separately and is always used when navigating through
the default ring. It skips buffers for that
`mtorus-buffer-skip-p' returns t."
  (interactive)
  (let ((blist (mtorus-grep
				(buffer-list)
				(lambda (buf)
				  (not (funcall mtorus-buffer-skip-p buf))))))
    (when (> (length blist) 1)
      (bury-buffer)
      (while (funcall mtorus-buffer-skip-p (current-buffer))
		(bury-buffer)))))


(defun mtorus-cycle-blist-next ()
  "Cycle the real buffer list.
This can be used separately and is always used when navigating through
the default ring. It skips buffers for that
`mtorus-buffer-skip-p' returns t."
  (interactive)
  (let ((blist (mtorus-grep
				(buffer-list)
				(lambda (buf)
				  (not (funcall mtorus-buffer-skip-p buf))))))
    (if blist
		(switch-to-buffer (car (reverse blist))))))

(defun mtorus-add-current-pos-to-current-ring ()
  "Adds a marker at the current point to the current ring.
Makes the current buffer the current ring buffer, too."
  (interactive)
  (mtorus-add-pos-to-ring
   (current-buffer) mtorus-current-ring-name))

(defun mtorus-add-current-pos-to-ring ()
  "Ask the user which ring to add the current buffer to.
Otherwise just like
`mtorus-add-current-pos-to-current-ring'."
  (interactive)
  (mtorus-add-pos-to-ring
   (current-buffer) (mtorus-choose-ring)))

;; FIXME: rename those to mtorus-new-marker(-at?)
(defun mtorus-add-pos-to-ring (buffer ring-name)
  "Add buffer current point in BUFFER to ring RING.
See also: `mtorus-add-current-pos-to-current-ring'."
  (if (not (equal mtorus-default-ring-name
				  mtorus-current-ring-name))
	  (progn
		(let* ((ring (mtorus-get-ring ring-name))
			   ;; old: (marker (point-marker nil buffer)))
			   ;; GNU Emacs comp. version:
			   (marker (with-current-buffer buffer (point-marker))))
		  ;; FIXME: ask when adding identical element again
		  (setf (second ring)
				(append (list marker)
						(second ring)))
		  (display-message 'no-log
			(format
			 "Added %s to %s" (mtorus-marker-description marker)
			 ring-name))))
	;; else: default is not editable list
	(display-message 'no-log
	  (format "Default list is not editable!"))))

(defun mtorus-delete-current-marker-from-current-ring ()
  "Delete the current buffer from the current ring.
Actually just the current marker that consists of the buffer-name and
the point is deleted, so if the current buffer exists several times in
the current ring it is deleted only once.

If the value of `buffer-name' is not equal to the current ring
marker an appropriate message is given and nothing else is done."
  (interactive)
  (let* ((ring (mtorus-current-ring))
		 (current-buffer-name (mtorus-current-buffer-name ring)))
	(if (equal (buffer-name)
			   current-buffer-name)
		(mtorus-delete-marker-from-ring
		 current-buffer-name 0 mtorus-current-ring-name)
	  (display-message 'no-log
		(format "current marker is not in current buffer!"))))
  (if mtorus-switch-when-deleting-p
	  (mtorus-switch-to-current-marker)))

(defun mtorus-delete-marker-from-ring (bname nth ring-name)
  "Delete the NTH occurrence of BNAME in ring RINGNAME."
  (if (not (equal mtorus-default-ring-name
				  ring-name))
	  (progn
		(let* ((ring (mtorus-get-ring ring-name))
			   (occurrences (loop for marker in (second ring) append
							  (if (equal (buffer-name
										  (marker-buffer marker)) bname)
								  (list marker)
								nil))) pos)
		  (if (equal (length (second ring)) 1)
			  (progn
				(if (y-or-n-p (concat
							   "Deleting last marker will delete the "
							   "ring, too. Proceed? "))
					(mtorus-delete-current-ring)))
			;; else
			(setq pos (marker-position (nth nth occurrences)))
			(setf (second ring)
				  ;; FIXME: need to check for nil of nth here?
				  (remove (nth nth occurrences) (second ring)))
			;; message to user
			(display-message 'no-log
			  (format "Deleted %s(%d) from %s"
					  bname pos ring-name))
			)))
	;; else: default is not editable list
	(display-message 'no-log
	  (format "Default list is not editable!"))))

(defun mtorus-delete-current-ring ()
  "Delete the current ring from the torus."
  (interactive)
  (mtorus-delete-ring mtorus-current-ring-name))

(defun mtorus-delete-ring (&optional rname)
  "Delete ring with name RNAME from torus.
If no ring is given it is asked for."
  (interactive)
  (let ((ring-name (or rname (mtorus-choose-ring))))
	(if (equal mtorus-default-ring-name
			   ring-name)
		(display-message 'no-log
		  (format "Default list is not editable"))
	  ;; else
	  (if (y-or-n-p (format "delete ring \"%s\"? "
							ring-name))
		  (progn
			;; FIXME: need a intuitive-check here?
			(if (equal ring-name mtorus-current-ring-name)
				(mtorus-cycle-ring-previous))
			;; remassoc has prob when first elt matches -> setq...
			(setq mtorus-torus
				  (remassoc ring-name mtorus-torus)))))))

(defun mtorus-rename-ring ()
  "Asks for a ring to be renamed and for the new name."
  (interactive)
  (let* ((ring-name (mtorus-choose-ring))
		 (newname (read-string
				   (format "rename \"%s\" to: " ring-name))))
	(if (equal mtorus-default-ring-name
			   ring-name)
		(display-message 'no-log
		  (format "Default list is not editable"))
	  ;; else
	  (setcar (assoc ring-name mtorus-torus)
			  newname)
	  (if (equal ring-name mtorus-current-ring-name)
		  (setq mtorus-current-ring-name newname)))))


;; FIXME: there is a strange behavior when:
;; new ring "news"
;;     move point
;;     new marker
;; back to default
;; switch to ring "news"
;; then the point is moved sometimes (just one buffer open?)
(defun mtorus-switch-to-current-marker ()
  "Jumps to the current marker in the current ring."
  (interactive)
  (let* ((ring (mtorus-current-ring))
		 (marker) (buffer-name) (buffer) (pos))
	(if (second ring)
		(progn
		  (setq marker (mtorus-current-marker ring))
		  (setq buffer (mtorus-current-buffer ring))
		  (if buffer
			  (progn
				(setq buffer-name (mtorus-current-buffer-name ring))
				(setq pos (marker-position marker))
				(switch-to-buffer buffer)
				(goto-char pos)
				(mtorus-highlight-current-line)
				(display-message 'no-log
				  (format
				   "%s" (mtorus-marker-description marker))))
			(progn
			  ;; FIXME: ask for deletion or reopen or cancel
			  (display-message 'no-log
				(format "deleted: %s from ring \"%s\""
						;; we can't securely use marker-description here
						buffer-name
						mtorus-current-ring-name))
			  (setf (second ring)
					(cdr (second ring))))))
	  (display-message 'no-log
		(format "Session \"%s\" is empty"
				mtorus-current-ring-name)))
	))


(defun mtorus-switch-to-ring (&optional ring-name)
  "Switch to another ring of marks.
If RING-NAME is not given it is asked for.
This moves point to the position defined in the current marker
of that ring (and to that buffer) if
`mtorus-switch-to-marker-when-cycling-p' is set to true and the
selected ring is not the default ring.
If `mtorus-update-marker-when-cycling-rings-p' is set to t the point
in the just left marker is updated."
  (interactive)
  (if (and (not (equal mtorus-default-ring-name
					   mtorus-current-ring-name))
		   mtorus-update-marker-when-cycling-rings-p)
	  (mtorus-update-current-marker))
  (let* ((ring-name (or ring-name (mtorus-choose-ring))))
	(setq mtorus-current-ring-name ring-name)
	(if (and (not (equal mtorus-default-ring-name
						 ring-name))
			 mtorus-switch-to-marker-when-cycling-p)
		(mtorus-switch-to-current-marker))))

(defun mtorus-switch-to-marker (&optional mark-name)
  "Switch to another mark of ring.
If MARK-NAME is not given it is asked for.
This moves point to the position defined in the marker
if `mtorus-switch-to-marker-when-cycling-p' is set to true and the
selected ring is not the default ring.
If `mtorus-update-marker-when-cycling-rings-p' is set to t the point
in the just left marker is updated."
  (interactive)

  (if (not (equal mtorus-default-ring-name mtorus-current-ring-name)) (progn

    (setq entry (read (mtorus-choose-marker)))
    (setq ring (mtorus-current-ring))

    (if mtorus-update-marker-when-cycling-markers-p (mtorus-update-current-marker))

    (if (not (equal (length (second ring)) 1))

      (while (not (equal entry (read (mtorus-marker-to-cons-string (first (second ring))))))
          (setf (second ring) (append (last (second ring)) (butlast (second ring)))))

    (display-message 'no-log "only marker."))

    (mtorus-switch-to-current-marker))

  (call-interactively 'switch-to-buffer))
)

(defun mtorus-update-current-marker ()
  "Takes care the the current marker position is up to date."
  (interactive)
  (if (not (equal mtorus-default-ring-name
				  mtorus-current-ring-name))
	  (let ((ring (mtorus-current-ring)))
		(if (mtorus-current-buffer-is-current-marker-p)
			;; FIXME: empty lists?
			(set-marker (car (second ring)) (point))))))
		;;(setf (cdar (second ring)) (point)))))

(defun mtorus-quit ()
  "Maybe save torus."
  (mtorus-save-torus)
  ;; (let* ((dirname (or mtorus-dirname (mtorus-default-directory))))
  ;; 	(if mtorus-dirname
  ;; 	    (mtorus-save-torus dirname)))
  )

(defun mtorus-save-torus ()
  "Save the current torus to `mtorus-file-name'."

  ;; Asks for a directory where to save it (just like `desktop-save' does.
  ;; (interactive "DDirectory to save torus file in: ")

  ;;  (let ((filename (expand-file-name mtorus-file-name dirname))

  (interactive)

  (run-hooks 'mtorus-save-hook)
    (let ((filename (expand-file-name mtorus-file-name))
		  (tmpbuf (get-buffer-create " *mtorus-tmp*"))
		  )
	  (save-excursion
		(message "saving to %s" filename)
		(set-buffer tmpbuf)
		(erase-buffer)
		(insert
		 (concat
		  "(\n"
		  (mapconcat
		   #'(lambda (ring)
			   (if (not (equal mtorus-default-ring-name
							   (first ring)))
				   (format "(\"%s\" . (%s))\n"
						   (first ring)
						   (mapconcat
							#'(lambda (marker)
								(mtorus-marker-to-cons-string marker))
							(second ring) " "))
				 ""))
		   mtorus-torus " ")
		  ")\n"))
		(write-region (buffer-string) nil filename nil 'no-message)

		;; (setq mtorus-dirname dirname)

		(kill-buffer nil))))

(defun mtorus-marker-to-cons-string (marker)
  "Convert a marker into a string which looks like a cons cell."
  (if (and (not (string-match
				 "^\\*" (buffer-name
						 (marker-buffer marker))))
		   (marker-position marker))
	  (concat
	   "(\"" (buffer-file-name
			  (marker-buffer marker))
	   "\" . "
	   (format
		"%d" (marker-position
			  marker))
	   ")")))

(defun mtorus-read-torus ()
  "Read torus from the current directory.
If the torus is not initialized yet this function cancels
reading. You need to call `mtorus-init' first.
When XEmacs is running in batch mode nothing is done."
  (interactive)
  (if noninteractive
      nil
	(let* ((dirname (mtorus-default-directory))
		  (filename (expand-file-name mtorus-file-name dirname)))
	  (if (file-readable-p filename)
		  (let ((tmpbuf (get-buffer-create " *mtorus-tmp*"))
				(tmplist))
			(unwind-protect
				(save-excursion
				  ;; FIXME: this removes current torus
				  ;; do we want the adding-feature?
				  ;;(mtorus-init)
				  (set-buffer tmpbuf)
				  (erase-buffer)
				  (widen)
				  (insert-file-contents filename)
				  (goto-char (point-min))
				  (setq tmplist (read (current-buffer)))
				  (mapc #'(lambda (ring)
							;; create the ring
							(mtorus-new-ring (car ring))
							(mapc #'(lambda (entry)
								  (mtorus-read-create-marker entry))
								  (cdr ring)))
							tmplist)
				  (kill-buffer tmpbuf)
				  (setq mtorus-dirname dirname)
				  )))
		(message "No torus found in current directory.")))))

(defun mtorus-read-create-marker (entry)
  "Creates a marker at in buffer at position pointed to by ENTRY.
ENTRY is of the form \(\"buffer-name\" . position).
The file is opened if necessary."
  (save-excursion
	(let ((bufname (car entry)))
	  (if (not (string-match "^\\*" bufname))
		  (let ((bufpos (cdr entry))
				(buffer (find-file-noselect bufname t)))
			(set-buffer buffer)
			(goto-char bufpos)
			(mtorus-add-current-pos-to-current-ring))))))

;(defun mtorus-find-file-in-new-session ()
;  "Find file (as usual) and create a new session with it.
;The creation of the new session asks for a session name."
;  (interactive)
;  (call-interactively 'find-file)
;  (mtorus-new-session))


;; -------------------------------------------------------------------
;; Internals

;; Probably no interesting code for users from this point

(defun mtorus-highlight-current-line ()
  "Show the line you jumped to by highlighting it."
  (setq mtorus-highlight-extent
		(mtorus-make-extent
		 (point-at-bol)
		 (point-at-eol)))
  (mtorus-set-extent-face mtorus-highlight-extent
				   'mtorus-highlight-face)
  (add-hook 'pre-command-hook
			'mtorus-unhighlight-current-line))


(defun mtorus-unhighlight-current-line ()
  "Remove highlighting of the current line if any."
  (if mtorus-highlight-extent
	  (progn
		(mtorus-delete-extent mtorus-highlight-extent)
		(setq mtorus-highlight-extent nil)
		(remove-hook 'pre-command-hook
					 'mtorus-unhighlight-current-line))))

(defun mtorus-grep (l predicate)
  "Helper function for a grep over a list.
Apply predicate PREDICATE to all elements of list L and return a list
consisting of all elements PREDICATE returned t for."
  (defun helper (ret-list rest)
    (if (null rest)
		(reverse ret-list)
	  (progn
		(if (funcall predicate (car rest))
			(setq ret-list (cons (car rest) ret-list)))
		(helper ret-list (cdr rest)))))
  (helper '() l))


(defun mtorus-current-ring ()
  "Return the current ring structure."
  (mtorus-get-ring mtorus-current-ring-name))

(defun mtorus-get-ring (rname)
  "Return the ring structure with name RNAME."
  (assoc rname mtorus-torus))

(defun mtorus-current-buffer-name (ring)
  "Return the buffer name of the first marker in RING.
The first marker is always considered the current marker."
  (buffer-name (mtorus-current-buffer ring)))

(defun mtorus-current-buffer (ring)
  "Return the buffer object of the first marker in RING.
The first marker is always considered the current marker."
  (let ((marker (car (second ring))))
	(if (markerp marker)
		(marker-buffer marker)
	  nil)))

(defun mtorus-current-marker (ring)
  "Return the first marker in RING.
The first marker is always considered the current marker."
  (if (second ring)
	  (car (second ring))
	nil))

(defun mtorus-choose-ring ()
  "Ask the user to choose a ring (with completion)."
  (completing-read "choose ring: "
				   mtorus-torus
				   nil t))

(defun mtorus-choose-marker ()
  "Ask the user to choose a marker in the ring (with completion)."
  (completing-read "choose marker: "
				  (mapcar #'mtorus-marker-to-cons-string (second (mtorus-current-ring)))
				   nil t))

(defun mtorus-find-current-ring-pos ()
  "Find position of current ring in the torus."
  (mtorus-find-ring-pos mtorus-current-ring-name))

(defun mtorus-find-ring-pos (ring-name)
  "Find the position of RING-NAME in `mtorus-torus'.
This is somewhat ugly. I'd prefer to reorder
`mtorus-torus' every time so that the current marker is
at the top. But then ... it works the way it is *g*."
  (let ((counter 0)
		(hit 0))
	;; FIXME: could be more elegant
	(mapc #'(lambda (ring)
			  (setq counter (+ counter 1))
			  (if (equal (car ring) ring-name)
				  (setq hit counter)))
		  mtorus-torus)
	(- hit 1)))


(defun mtorus-current-buffer-is-current-marker-p ()
  "Compare the current marker in the current ring with current buffer."
  (let* ((ring (mtorus-current-ring))
		(current-marker-name (mtorus-current-buffer-name ring)))
	(equal (buffer-name)
		   current-marker-name)))


;; maintainance of rings in a buffer
;; (much like list-bookmarks and list-buffers)
;; this is a late TODO topic
;(defun mtorus-list-ring ()
;  "Open a buffer with a list of rings for editing."
;  (interactive)
;  )

;(defun mtorus-list-transpose-ring ()
;  "Interchange ring at point in list."
;  )

;(defun mtorus-list-delete-ring ()
;  "Delete ring at point."
;  )

;;(defun mtorus-find-ring-on-line ()
;; ""
;;)
;; test function for debugging and development
;; will be removed later
(defun ttt ()
  "testing..."
  (interactive)
  (setq-default debug-on-error t)
  (setq mtorus-torus nil)
  (mtorus-init)
  (mtorus-new-ring "neu1")
  (mtorus-new-ring "neu2")
  (mtorus-new-ring "neu3")
  (forward-char)
  (mtorus-add-current-pos-to-current-ring)
  (other-window 1)
  (mtorus-add-current-pos-to-current-ring)
  (other-window 1)
  (describe-variable 'mtorus-torus)
  )

(provide 'mtorus)



;;; mtorus.el ends here
