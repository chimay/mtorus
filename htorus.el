;;; HTorus : like MTorus but with hash table, seems easier to handle the data structure

;; TODO : nothing is done yet

;;; htorus.el --- navigation with marks on a ring of rings (torus)
;; $Id: htorus.el,v 1.6 2003/01/14 21:16:16 ska Exp $
;; Copyright (C) 2002-2003 by Stefan Kamphausen
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;; Keywords: bookmarks, navigation, tools

(defvar htorus-version "0.1"
  "Version number of htorus.")

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

;; htorus on the Web:
;; Main page:
;; http://www.skamphausen.de/software/skamacs/htorus.html
;; German intro:
;; http://www.skamphausen.de/xemacs/lisp/htorus.html

;; Installation:
;; =============
;; Put into your `user-init-file' (rectangle selectable :-):
;;
;; ;; htorus: navigation with marks on a ring of rings (torus)
;; (require 'htorus)
;; (htorus-init)
;; (htorus-install-suggested-bindings)
;;
;; Maybe you don't want the whole torus but like the buffer cycling?
;; Don't init the torus and use some backend functions instead:
;; ;; htorus: navigation with marks on a ring of rings (torus)
;; (require 'htorus)
;; (global-set-key '[(shift right)] 'htorus-cycle-blist-next)
;; (global-set-key '[(shift left)]  'htorus-cycle-blist-prev)
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
;; htorus-cycle-marker-next
;; htorus-cycle-marker-previous
;; htorus-cycle-ring-next
;; htorus-cycle-ring-previous
;; htorus-new-ring
;; htorus-delete-current-ring
;; htorus-describe-current-ring
;; htorus-add-current-pos-to-current-ring
;; htorus-delete-current-marker-from-current-ring
;; htorus-update-current-marker
;;
;; For a slightly different installation which restores the torus next
;; time you open your (X)Emacs see below...
;;
;; Usage:
;; ======
;; htorus lets you work with several groups of buffers, each group
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
;; using a Torus here. Hence the name: "htorus": mark-torus.
;; (A "group" will usually be referred to as a "ring" from now.)

;; htorus tries to be be somewhat intelligent in when to overwrite
;; stored positions (the current marker moves with the point) and hopes
;; to become as intuitive as a good emacs module has to be.  But this
;; will evolve with time in the daily life (and htorus tries to keep
;; as many aspects configurable with customize variables).
;;
;;
;; Saving And Restoring the Torus
;; ------------------------------
;; The saving of rings in files is possible so that you can easily
;; resume work on the next day/month/year/after lunch.
;; This is VERY ALPHA!!
;; It might working out of the box when you set
;; htorus-save-on-exit to t and put into your init file:
;; (htorus-read-torus).
;; It's probably a good idea to put this at the very end of your
;; initializations, which might be the end of ~/.xemacs/init.el or of
;; ~/.emacs, right after the (desktop-read) function which you might
;; have in your init file. htorus tries to load all the files
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
;;    * renaming to htorus should be done
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
;;   (using ring.el) and then create htorus.el as an application of
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

;; ==============================

;; user options (customize)
(defgroup htorus nil
  "An interface to navigating groups of buffers.
This code should work fast so intuition is a very important
matter. Some of the customizable variables are switches to tune the
behavior of the functions to your habits. Hopefully it is possible to
find good settings for many people."
  :tag "htorus"
  :link '(url-link :tag "Home Page"
                   "http://www.skamphausen.de/software/skamacs/")
  :link '(emacs-commentary-link
		  :tag "Commentary in htorus.el" "htorus.el")
  :prefix "htorus-"
  :group 'environment
  :group 'extensions
  :group 'convenience)

(defcustom htorus-save-on-exit nil
  "*If set to t `htorus-init' will install saving of desktop on exit.
The function `htorus-quit' is placed on `kill-emacs-hook'."
  :type 'boolean
  :group 'htorus)


(defcustom htorus-show-marker-list-when-cycling-p t
  "*Whether the buffer-names are displayed when you cycle rings.
When you tend to have long lists it may be disturbing to have so many
buffers displayed, but on the other hand it might help you navigating
on the torus. Using some nifty functions in
`htorus-describe-marker-func' that find function names at that
position makes that display even longer."
  :type 'boolean
  :group 'htorus)

(defcustom htorus-switch-to-marker-when-cycling-p nil
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
  :group 'htorus)

(defcustom htorus-update-marker-when-cycling-markers-p nil
  "*Whether to store the local point into the local marker.
This is one of the intuition switches.
Setting this to t means that you always 'move' inside a marker while
nil means that you wander around freely and have your fix positions
stored. In the last case you'll probably like to explicitly use
`htorus-update-current-marker' and bind it to a key."
  :type 'boolean
  :group 'htorus)

(defcustom htorus-update-marker-when-cycling-rings-p nil
  "*Whether to store the local point into the local marker.
This is one of the intuition switches.
It is close to `htorus-update-marker-when-cycling-markers-p' but
determines the behavior when cycling through rings which feels
quite different than cycling markers."
  :type 'boolean
  :group 'htorus)

(defcustom htorus-switch-when-deleting-p nil
  "*Whether to switch to the next marker when current marker is deleted.
This is one of the intuition switches.
Setting to t means that when you delete a marker from the current
ring you instantly move to the new current marker while setting to
nil keeps you at the current position."
  :type 'boolean
  :group 'htorus)

(defcustom htorus-buffer-skip-p
  'htorus-default-buffer-skip-p
  "Predicate to use to skip buffers when cycling the real buffer list.
This has nothing to do with the cycling inside a normal ring.
A good example would be to use the result of
  (string-match \"^[ \\*]+\" (buffer-name buffer))
which skips the buffers with a star or a space at the beginning of
their buffer names.
The default predicate `htorus-default-buffer-skip-p'  skips
buffers whose names begin with a space."
  :type 'function
  :group 'htorus)

;; FIXME: do I need the defvar _before_ defcustom to work?
(defvar htorus-default-describe-marker-alist
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
function and an example for your own `htorus-describe-marker-func'.

If you write any rule here please let me know.")

(defcustom htorus-describe-marker-func
  htorus-default-describe-marker-alist
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
  :group 'htorus)


(defface htorus-highlight-face
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
  :group 'htorus)

(defcustom htorus-save-hook nil
  "Hook run before saving the torus."
  :type 'hook
  :group 'htorus)

(defcustom htorus-init-hook nil
  "Hook run after the torus is initialized."
  :type 'hook
  :group 'htorus)

(defcustom htorus-default-ring-name "all-buffers"
  "The name of the ring that always contains all open buffers.
Cycling within this ring is different from cycling the others since it
always uses the real buffer list. It skips all buffers that
`htorus-buffer-skip-p' returns t for."
  :type 'string
  :group 'htorus)

(defcustom htorus-file-name ".htorus.el"
  "The file-name to use when saving the current torus."
  :type 'string
  :group 'htorus)

; This comes later:
;(defgroup htorus-pop-up nil
;  "Controls the display of a pop up window which displays the cycled
;  list."
;  :tag "htorusPopUp"
;  :prefix "htorus-popup"
;  :group 'htorus)

;(defcustom htorus-popup-use-popup-p t
;  "*Whether to use a pop up window or not."
;  :type 'boolean
;  :group 'htorus-pop-up)

;(defcustom htorus-popup-clear-timeout 5
;  "*Time in seconds before the pop up window is removed."
;  :type 'number
;  :group 'htorus-pop-up)

;(defcustom htorus-popup-separator " - "
;  "*String appearing between two entries in pop up window."
;  :type 'string
;  :group 'htorus-pop-up)


(defvar htorus-dirname nil
  "The directory from which a torus was last read.
This is the directory to which to save the torus when exiting emacs
and when `htorus-save-on-exit' is t.")

;; variables
;; main data structure of form
;; (("ring1" (marker1
;;            marker2
;;            marker3
;;            marker4))
;;  ("ring2" (marker5
;;            marker6
;;            marker7)))
(defvar htorus-torus nil
  "Alist containing all marker rings with their buffers.
The main data structure of htorus. It contains all buffers and
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

(defvar htorus-current-ring-name nil
  "The name of the currently chosen ring."
  )

(defvar htorus-highlight-extent nil
  "Extent to use for highlighting.")


;; convenience
(defun htorus-install-suggested-bindings ()
  "This sets the key-bindings that I consider useful.
This does not fulfill the requirements of good key-defining but I like
it and I provide it only as a convenience function."
  (interactive)
  (global-set-key '[(shift right)] 'htorus-cycle-marker-next)
  (global-set-key '[(shift left)]  'htorus-cycle-marker-previous)
  (global-set-key '[(shift up)]    'htorus-cycle-ring-next)
  (global-set-key '[(shift down)]  'htorus-cycle-ring-previous)
  ;; ring handling: f11
  (global-set-key '[(f11)]
	'htorus-new-ring)
  (global-set-key '[(shift f11)]
	'htorus-delete-current-ring)
  (global-set-key '[(control f11)]
	'htorus-describe-current-ring)
  ;; marker handling: f12
  (global-set-key '[(f12)]
	'htorus-add-current-pos-to-current-ring)
  (global-set-key '[(shift f12)]
	'htorus-delete-current-marker-from-current-ring)
  (global-set-key '[(control f12)]
	'htorus-update-current-marker)
  ;; not yet available:
  ;;(global-set-key '[(??)] 'htorus-list-rings)
  )

;; GNU Emacs and downward compatibility
;; (with help from Christoph Conrad and speedbar.el):
(if (fboundp 'make-extent)
    (progn ;; XEmacs
	  (defalias 'htorus-make-extent         'make-extent)
	  (defalias 'htorus-set-extent-property 'set-extent-property)
	  (defalias 'htorus-delete-extent       'delete-extent)
	  (defun htorus-default-directory ()
		(default-directory)))
  ;; GNU Emacs
  (defalias 'htorus-make-extent 'make-overlay)
  (defalias 'htorus-set-extent-property 'overlay-put)
  (defalias 'htorus-delete-extent 'delete-overlay)
  (defun display-message (label message &optional frame stdout-p)
	(message message))
  (defun htorus-default-directory ()
	default-directory))

(defun htorus-set-extent-face (extent face)
  (htorus-set-extent-property extent 'face face))
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
(defun htorus-init ()
  "This starts the torus and opens the default ring.
The name of the default ring is taken from
`htorus-default-ring-name' and this is special since the
cycling of markers is different there and it is not editable.
You can use the htorus-cycle-blist-* functions separately if
you don't call this function so that you always just cycle the real
buffer list. Then you can use the variable
`htorus-buffer-skip-p' to tune the cycling to your desired
behavior."
  (interactive)
  (setq htorus-torus nil)
  (setq htorus-current-ring-name htorus-default-ring-name)
  (htorus-new-ring htorus-default-ring-name)
  (if htorus-save-on-exit
	  (add-hook 'kill-emacs-hook
				'htorus-quit))
  (run-hooks 'htorus-init-hook))

(defun htorus-default-buffer-skip-p (buffer)
  "The default predicate used for `htorus-buffer-skip-p'
is to skip only the special buffers whose name begins with a space."
  (string-match "^[ ]+" (buffer-name buffer)))

(defun htorus-new-ring (name &optional empty-ring)
  "Create a new ring and asks the user for a name.
Makes the new ring the current ring and puts the current buffer
with the current point into it."
  ;; FIXME: adding current place should be customizable and empty
  ;; rings should be possible
  (interactive "sRing name: ")
  ;; test for existence
  (if (htorus-get-ring name)
	  (display-message 'no-log
		(format "A Ring with name \"%s\" already exists."
				name))
	;; create ring
	(setq htorus-torus
		  (append htorus-torus
				  (list
				   (cons name
						 (list
						  (list
						   (cond ((equal htorus-default-ring-name
										 name)
								  "special: all open buffers")
								 ;; FIXME: how to remove this nil from
								 ;; list?
								 (empty-ring nil)
								 (t
								  (point-marker)))))))))
	(htorus-switch-to-ring name)))

(defun htorus-describe-current-ring ()
  "Show the current markers in the echo area.
Uses `htorus-describe-marker-func' to decide how to display the
markers."
  (interactive)
  (let ((ring (htorus-current-ring)))
	(display-message 'no-log
	  (format "Ring \"%s\"%s" (first ring);; ring name
			  ;; all markers
			  ;; FIXME: this check here?
			  (if (and htorus-show-marker-list-when-cycling-p
					   (not (equal htorus-default-ring-name
								   htorus-current-ring-name)))
				  (concat ": "
						  (mapconcat
						   #'(lambda (marker)
							   (format "%s"
									   (htorus-marker-description
										marker)))
						   (second ring) " "))
				;; else empty string
				"")))))

;; FIXME: maybe we can find a clever way to have a look at the length
;; of a description and adjust it (cropping or using point or ...?)?
;; So not to (over)fill the echo area.
(defun htorus-marker-description (marker)
  "Return a string with the description of marker MARKER.
Uses `htorus-describe-marker-func'."
  (let* ((buf (marker-buffer marker))
		 (bufname (buffer-name buf))
		 (pos (marker-position marker)))
	(concat bufname "("
			(format
			 "%s"
			 (if (and htorus-describe-marker-func
					  (listp htorus-describe-marker-func)
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
								  htorus-describe-marker-func)))
							   (or
								(funcall
								 (cdr
								  (assoc
								   major-mode
								   htorus-describe-marker-func)))
								(point))
							 (point))))
					 "*del*"))
			   pos))
			")")))


(defun htorus-cycle-ring-next ()
  "Make the next ring the current ring.
See `htorus-switch-to-ring' for discussion."
  (interactive)
  (let* ((current-ring-num (htorus-find-current-ring-pos))
		 (new-position (mod (1+ current-ring-num) (length htorus-torus)))
		 (new-ring (car (elt htorus-torus new-position))))
	(htorus-switch-to-ring new-ring)
	(htorus-describe-current-ring)))

(defun htorus-cycle-ring-previous ()
  "Make the previous ring the current ring.
See `htorus-switch-to-ring' for discussion."
  (interactive)
  (let* ((current-ring-num (htorus-find-current-ring-pos))
		 (new-position (mod (1- current-ring-num)
							(length htorus-torus)))
		 (new-ring (car (elt htorus-torus
							 new-position))))
	(htorus-switch-to-ring new-ring)
	(htorus-describe-current-ring)))

(defun htorus-cycle-marker-next ()
  "Switch to the next marker in the current ring.
This can be another buffer or another position in the same buffer.
The point stored in the just left marker is updated if
`htorus-update-marker-when-cycling-markers-p' is set to t."
  (interactive)
  (if (not (equal htorus-default-ring-name
				  htorus-current-ring-name))
	  (progn
		(let ((ring (htorus-current-ring)))
		  (if (not (equal (length (second ring)) 1))
			  (progn
				(if htorus-update-marker-when-cycling-markers-p
					(htorus-update-current-marker))
				(if (htorus-current-buffer-is-current-marker-p)
					(setf (second ring)
						  (append (last (second ring))
								  (butlast (second ring))))))
			(display-message 'no-log
			  "only marker.")))
		(htorus-switch-to-current-marker))
	;; else: default ring uses good ol' buffer list
	(htorus-cycle-blist-next)))


(defun htorus-cycle-marker-previous ()
  "Switch to the previous buffer in the current ring.
The point stored in the just left marker is updated if
`htorus-update-marker-when-cycling-markers-p' is set to t."
  (interactive)
  (if (not (equal htorus-default-ring-name
				  htorus-current-ring-name))
	  (progn
		(let ((ring (htorus-current-ring)))
		  (if (not (equal (length (second ring)) 1))
			  (progn
				(if htorus-update-marker-when-cycling-markers-p
					(htorus-update-current-marker))
				(if (htorus-current-buffer-is-current-marker-p)
					(setf (second ring)
						  (append (cdr (second ring))
								  (list (first (second ring)))))))
			(display-message 'no-log
			  "only marker.")))
		(htorus-switch-to-current-marker))
	;; else: default ring uses good ol' buffer list
	(htorus-cycle-blist-prev)))


(defun htorus-cycle-blist-prev ()
  "Cycle the real buffer list.
This can be used separately and is always used when navigating through
the default ring. It skips buffers for that
`htorus-buffer-skip-p' returns t."
  (interactive)
  (let ((blist (htorus-grep
				(buffer-list)
				(lambda (buf)
				  (not (funcall htorus-buffer-skip-p buf))))))
    (when (> (length blist) 1)
      (bury-buffer)
      (while (funcall htorus-buffer-skip-p (current-buffer))
		(bury-buffer)))))


(defun htorus-cycle-blist-next ()
  "Cycle the real buffer list.
This can be used separately and is always used when navigating through
the default ring. It skips buffers for that
`htorus-buffer-skip-p' returns t."
  (interactive)
  (let ((blist (htorus-grep
				(buffer-list)
				(lambda (buf)
				  (not (funcall htorus-buffer-skip-p buf))))))
    (if blist
		(switch-to-buffer (car (reverse blist))))))

(defun htorus-add-current-pos-to-current-ring ()
  "Adds a marker at the current point to the current ring.
Makes the current buffer the current ring buffer, too."
  (interactive)
  (htorus-add-pos-to-ring
   (current-buffer) htorus-current-ring-name))

(defun htorus-add-current-pos-to-ring ()
  "Ask the user which ring to add the current buffer to.
Otherwise just like
`htorus-add-current-pos-to-current-ring'."
  (interactive)
  (htorus-add-pos-to-ring
   (current-buffer) (htorus-choose-ring)))

;; FIXME: rename those to htorus-new-marker(-at?)
(defun htorus-add-pos-to-ring (buffer ring-name)
  "Add buffer current point in BUFFER to ring RING.
See also: `htorus-add-current-pos-to-current-ring'."
  (if (not (equal htorus-default-ring-name
				  htorus-current-ring-name))
	  (progn
		(let* ((ring (htorus-get-ring ring-name))
			   ;; old: (marker (point-marker nil buffer)))
			   ;; GNU Emacs comp. version:
			   (marker (with-current-buffer buffer (point-marker))))
		  ;; FIXME: ask when adding identical element again
		  (setf (second ring)
				(append (list marker)
						(second ring)))
		  (display-message 'no-log
			(format
			 "Added %s to %s" (htorus-marker-description marker)
			 ring-name))))
	;; else: default is not editable list
	(display-message 'no-log
	  (format "Default list is not editable!"))))

(defun htorus-delete-current-marker-from-current-ring ()
  "Delete the current buffer from the current ring.
Actually just the current marker that consists of the buffer-name and
the point is deleted, so if the current buffer exists several times in
the current ring it is deleted only once.

If the value of `buffer-name' is not equal to the current ring
marker an appropriate message is given and nothing else is done."
  (interactive)
  (let* ((ring (htorus-current-ring))
		 (current-buffer-name (htorus-current-buffer-name ring)))
	(if (equal (buffer-name)
			   current-buffer-name)
		(htorus-delete-marker-from-ring
		 current-buffer-name 0 htorus-current-ring-name)
	  (display-message 'no-log
		(format "current marker is not in current buffer!"))))
  (if htorus-switch-when-deleting-p
	  (htorus-switch-to-current-marker)))

(defun htorus-delete-marker-from-ring (bname nth ring-name)
  "Delete the NTH occurrence of BNAME in ring RINGNAME."
  (if (not (equal htorus-default-ring-name
				  ring-name))
	  (progn
		(let* ((ring (htorus-get-ring ring-name))
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
					(htorus-delete-current-ring)))
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

(defun htorus-delete-current-ring ()
  "Delete the current ring from the torus."
  (interactive)
  (htorus-delete-ring htorus-current-ring-name))

(defun htorus-delete-ring (&optional rname)
  "Delete ring with name RNAME from torus.
If no ring is given it is asked for."
  (interactive)
  (let ((ring-name (or rname (htorus-choose-ring))))
	(if (equal htorus-default-ring-name
			   ring-name)
		(display-message 'no-log
		  (format "Default list is not editable"))
	  ;; else
	  (if (y-or-n-p (format "delete ring \"%s\"? "
							ring-name))
		  (progn
			;; FIXME: need a intuitive-check here?
			(if (equal ring-name htorus-current-ring-name)
				(htorus-cycle-ring-previous))
			;; remassoc has prob when first elt matches -> setq...
			(setq htorus-torus
				  (remassoc ring-name htorus-torus)))))))

(defun htorus-rename-ring ()
  "Asks for a ring to be renamed and for the new name."
  (interactive)
  (let* ((ring-name (htorus-choose-ring))
		 (newname (read-string
				   (format "rename \"%s\" to: " ring-name))))
	(if (equal htorus-default-ring-name
			   ring-name)
		(display-message 'no-log
		  (format "Default list is not editable"))
	  ;; else
	  (setcar (assoc ring-name htorus-torus)
			  newname)
	  (if (equal ring-name htorus-current-ring-name)
		  (setq htorus-current-ring-name newname)))))


;; FIXME: there is a strange behavior when:
;; new ring "news"
;;     move point
;;     new marker
;; back to default
;; switch to ring "news"
;; then the point is moved sometimes (just one buffer open?)
(defun htorus-switch-to-current-marker ()
  "Jumps to the current marker in the current ring."
  (interactive)
  (let* ((ring (htorus-current-ring))
		 (marker) (buffer-name) (buffer) (pos))
	(if (second ring)
		(progn
		  (setq marker (htorus-current-marker ring))
		  (setq buffer (htorus-current-buffer ring))
		  (if buffer
			  (progn
				(setq buffer-name (htorus-current-buffer-name ring))
				(setq pos (marker-position marker))
				(switch-to-buffer buffer)
				(goto-char pos)
				(htorus-highlight-current-line)
				(display-message 'no-log
				  (format
				   "%s" (htorus-marker-description marker))))
			(progn
			  ;; FIXME: ask for deletion or reopen or cancel
			  (display-message 'no-log
				(format "deleted: %s from ring \"%s\""
						;; we can't securely use marker-description here
						buffer-name
						htorus-current-ring-name))
			  (setf (second ring)
					(cdr (second ring))))))
	  (display-message 'no-log
		(format "Session \"%s\" is empty"
				htorus-current-ring-name)))
	))


(defun htorus-switch-to-ring (&optional ring-name)
  "Switch to another ring of marks.
If RING-NAME is not given it is asked for.
This moves point to the position defined in the current marker
of that ring (and to that buffer) if
`htorus-switch-to-marker-when-cycling-p' is set to true and the
selected ring is not the default ring.
If `htorus-update-marker-when-cycling-rings-p' is set to t the point
in the just left marker is updated."
  (interactive)
  (if (and (not (equal htorus-default-ring-name
					   htorus-current-ring-name))
		   htorus-update-marker-when-cycling-rings-p)
	  (htorus-update-current-marker))
  (let* ((ring-name (or ring-name (htorus-choose-ring))))
	(setq htorus-current-ring-name ring-name)
	(if (and (not (equal htorus-default-ring-name
						 ring-name))
			 htorus-switch-to-marker-when-cycling-p)
		(htorus-switch-to-current-marker))))

(defun htorus-switch-to-marker (&optional mark-name)
  "Switch to another mark of ring.
If MARK-NAME is not given it is asked for.
This moves point to the position defined in the marker
if `htorus-switch-to-marker-when-cycling-p' is set to true and the
selected ring is not the default ring.
If `htorus-update-marker-when-cycling-rings-p' is set to t the point
in the just left marker is updated."
  (interactive)

  (if (not (equal htorus-default-ring-name htorus-current-ring-name)) (progn

    (setq entry (read (htorus-choose-marker)))
    (setq ring (htorus-current-ring))

    (if htorus-update-marker-when-cycling-markers-p (htorus-update-current-marker))

    (if (not (equal (length (second ring)) 1))

      (while (not (equal entry (read (htorus-marker-to-cons-string (first (second ring))))))
          (setf (second ring) (append (last (second ring)) (butlast (second ring)))))

    (display-message 'no-log "only marker."))

    (htorus-switch-to-current-marker))

  (call-interactively 'switch-to-buffer))
)

(defun htorus-update-current-marker ()
  "Takes care the the current marker position is up to date."
  (interactive)
  (if (not (equal htorus-default-ring-name
				  htorus-current-ring-name))
	  (let ((ring (htorus-current-ring)))
		(if (htorus-current-buffer-is-current-marker-p)
			;; FIXME: empty lists?
			(set-marker (car (second ring)) (point))))))
		;;(setf (cdar (second ring)) (point)))))

(defun htorus-quit ()
  "Maybe save torus."
  (htorus-save-torus)
  ;; (let* ((dirname (or htorus-dirname (htorus-default-directory))))
  ;; 	(if htorus-dirname
  ;; 	    (htorus-save-torus dirname)))
  )

(defun htorus-remove-duplicates ()

  "Remove duplicates files in rings"

  ;; Ne marche pas

  ;; (htorus-map-into htorus-torus #'delete-dups htorus-torus)

  ;; (let (value)
  ;;   (dolist (elt htorus-torus value)
  ;;     (setcdr elt (delete-dups (cdr elt)))
  ;; 	)
  ;;   )
 )

(defun htorus-save-torus ()
  "Save the current torus to `htorus-file-name'."

  ;; Asks for a directory where to save it (just like `desktop-save' does.
  ;; (interactive "DDirectory to save torus file in: ")

  ;;  (let ((filename (expand-file-name htorus-file-name htorus-dirname)) ...
  ;;  (expand-file-name htorus-file-name htorus-dirname)

  (interactive)

  (htorus-remove-duplicates)

  (run-hooks 'htorus-save-hook)
    (let ((filename (read-file-name "Torus file : " htorus-dirname))
		  (tmpbuf (get-buffer-create " *htorus-tmp*"))
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
			   (if (not (equal htorus-default-ring-name
							   (first ring)))
				   (format "(\"%s\" . (%s))\n"
						   (first ring)
						   (mapconcat
							#'(lambda (marker)
								(htorus-marker-to-cons-string marker))
							(second ring) " "))
				 ""))
		   htorus-torus " ")
		  ")\n"))
		(write-region (buffer-string) nil filename nil 'no-message)

		;; (setq htorus-dirname dirname)

		(kill-buffer nil))))

(defun htorus-marker-to-cons-string (marker)
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

(defun htorus-read-torus ()
  "Read torus from the current directory.
If the torus is not initialized yet this function cancels
reading. You need to call `htorus-init' first.
When XEmacs is running in batch mode nothing is done."
  (interactive)

  ;; Let’s deletethe old torus
  (htorus-init)

  ;; (filename (expand-file-name htorus-file-name dirname))) ...
  ;; (expand-file-name htorus-file-name dirname)

  (if noninteractive
      nil
	(let* ((dirname (htorus-default-directory))
		  (filename (read-file-name "Torus file : " htorus-dirname)))
	  (if (file-readable-p filename)
		  (let ((tmpbuf (get-buffer-create " *htorus-tmp*"))
				(tmplist))
			(unwind-protect
				(save-excursion
				  ;; FIXME: this removes current torus
				  ;; do we want the adding-feature?
				  ;;(htorus-init)
				  (set-buffer tmpbuf)
				  (erase-buffer)
				  (widen)
				  (insert-file-contents filename)
				  (goto-char (point-min))
				  (setq tmplist (read (current-buffer)))
				  (mapc #'(lambda (ring)
							;; create the ring
							(htorus-new-ring (car ring))
							(mapc #'(lambda (entry)
								  (htorus-read-create-marker entry))
								  (cdr ring)))
							tmplist)
				  (kill-buffer tmpbuf)
				  ;; (setq htorus-dirname dirname)
				  )))
		(message "No torus found in current directory.")))))

(defun htorus-read-create-marker (entry)
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
			(htorus-add-current-pos-to-current-ring))))))

;(defun htorus-find-file-in-new-session ()
;  "Find file (as usual) and create a new session with it.
;The creation of the new session asks for a session name."
;  (interactive)
;  (call-interactively 'find-file)
;  (htorus-new-session))


;; -------------------------------------------------------------------
;; Internals

;; Probably no interesting code for users from this point

(defun htorus-highlight-current-line ()
  "Show the line you jumped to by highlighting it."
  (setq htorus-highlight-extent
		(htorus-make-extent
		 (point-at-bol)
		 (point-at-eol)))
  (htorus-set-extent-face htorus-highlight-extent
				   'htorus-highlight-face)
  (add-hook 'pre-command-hook
			'htorus-unhighlight-current-line))


(defun htorus-unhighlight-current-line ()
  "Remove highlighting of the current line if any."
  (if htorus-highlight-extent
	  (progn
		(htorus-delete-extent htorus-highlight-extent)
		(setq htorus-highlight-extent nil)
		(remove-hook 'pre-command-hook
					 'htorus-unhighlight-current-line))))

(defun htorus-grep (l predicate)
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


(defun htorus-current-ring ()
  "Return the current ring structure."
  (htorus-get-ring htorus-current-ring-name))

(defun htorus-get-ring (rname)
  "Return the ring structure with name RNAME."
  (assoc rname htorus-torus))

(defun htorus-current-buffer-name (ring)
  "Return the buffer name of the first marker in RING.
The first marker is always considered the current marker."
  (buffer-name (htorus-current-buffer ring)))

(defun htorus-current-buffer (ring)
  "Return the buffer object of the first marker in RING.
The first marker is always considered the current marker."
  (let ((marker (car (second ring))))
	(if (markerp marker)
		(marker-buffer marker)
	  nil)))

(defun htorus-current-marker (ring)
  "Return the first marker in RING.
The first marker is always considered the current marker."
  (if (second ring)
	  (car (second ring))
	nil))

(defun htorus-choose-ring ()
  "Ask the user to choose a ring (with completion)."
  (completing-read "choose ring: "
				   htorus-torus
				   nil t))

(defun htorus-choose-marker ()
  "Ask the user to choose a marker in the ring (with completion)."
  (completing-read "choose marker: "
				  (mapcar #'htorus-marker-to-cons-string (second (htorus-current-ring)))
				   nil t))

(defun htorus-find-current-ring-pos ()
  "Find position of current ring in the torus."
  (htorus-find-ring-pos htorus-current-ring-name))

(defun htorus-find-ring-pos (ring-name)
  "Find the position of RING-NAME in `htorus-torus'.
This is somewhat ugly. I'd prefer to reorder
`htorus-torus' every time so that the current marker is
at the top. But then ... it works the way it is *g*."
  (let ((counter 0)
		(hit 0))
	;; FIXME: could be more elegant
	(mapc #'(lambda (ring)
			  (setq counter (+ counter 1))
			  (if (equal (car ring) ring-name)
				  (setq hit counter)))
		  htorus-torus)
	(- hit 1)))


(defun htorus-current-buffer-is-current-marker-p ()
  "Compare the current marker in the current ring with current buffer."
  (let* ((ring (htorus-current-ring))
		(current-marker-name (htorus-current-buffer-name ring)))
	(equal (buffer-name)
		   current-marker-name)))


;; maintainance of rings in a buffer
;; (much like list-bookmarks and list-buffers)
;; this is a late TODO topic
;(defun htorus-list-ring ()
;  "Open a buffer with a list of rings for editing."
;  (interactive)
;  )

;(defun htorus-list-transpose-ring ()
;  "Interchange ring at point in list."
;  )

;(defun htorus-list-delete-ring ()
;  "Delete ring at point."
;  )

;;(defun htorus-find-ring-on-line ()
;; ""
;;)
;; test function for debugging and development
;; will be removed later
(defun ttt ()
  "testing..."
  (interactive)
  (setq-default debug-on-error t)
  (setq htorus-torus nil)
  (htorus-init)
  (htorus-new-ring "neu1")
  (htorus-new-ring "neu2")
  (htorus-new-ring "neu3")
  (forward-char)
  (htorus-add-current-pos-to-current-ring)
  (other-window 1)
  (htorus-add-current-pos-to-current-ring)
  (other-window 1)
  (describe-variable 'htorus-torus)
  )

(provide 'htorus)



;;; htorus.el ends here
