;;; Torus : Personal version of MTorus, from scratch

;; TODO : nothing is done yet

;; License
;; ------------------------------

;; This file is not part of Emacs.

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

;; Idea
;; ------------------------------
;;
;; A ring is a group of buffers
;; A torus is a group of ring, a kind of session if you will
;;
;; See README.org in the code repository for more details

;; Version
;; ------------------------------

(defvar torus/version "0.1"
  "Version number of torus.")

;; Custom group
;; ------------------------------

(defgroup torus nil

  "An interface to navigating groups of buffers.
This code should work fast so intuition is a very important
matter. Some of the customizable variables are switches to tune
the behavior of the functions to your habits. Hopefully it is
possible to find good settings for many people."

  :tag "Torus"
  :link '(url-link :tag "Home Page"
                   "https://github.com/chimay/mtorus")
  :link '(emacs-commentary-link
		  :tag "Commentary in torus.el" "torus.el")
  :prefix "torus-"
  :group 'environment
  :group 'extensions
  :group 'convenience)

(defvar torus/torus nil)

(defvar torus/dirname nil
  "The directory from which a torus was last read.
This is the directory to which to save the torus when exiting emacs
and when `torus-save-on-exit' is t.")

(defcustom torus/file-name "torus.el"
  "The file-name to use when saving the current torus."
  :type 'string
  :group 'torus)

(defcustom torus/save-on-exit nil

  "*If set to t `torus-init' will install saving of desktop on exit.
The function `torus-quit' is placed on `kill-emacs-hook'."

  :type 'boolean
  :group 'torus)

;; Keymap with prefix
;; ------------------------------

(define-prefix-command 'torus-map)

;; Functions
;; ------------------------------

(defun torus/install-default-bindings ()

  (global-set-key (kbd "<f6>") 'torus-map)

  (define-key torus-map (kbd "i") 'torus/init)

  (define-key torus-map (kbd "P") 'torus/print)

  (define-key torus-map (kbd "a") 'torus/add-ring)

  (define-key torus-map (kbd "r") 'torus/read)
  (define-key torus-map (kbd "w") 'torus/write)

  )


(defun torus/init ()

  (interactive)

  (setq torus/torus (make-hash-table :test 'equal))

  )

(defun torus/print ()

  (interactive)

  (print torus/torus)

  )

(defun torus/add-ring ()

  (interactive)

  (puthash
   (read-string "Name of the new ring : ")
   '((cons (buffer-file-name) (marker-position (point-marker))))
   torus/torus)

  )

(defun torus/write ()

  (interactive)

  (maphash (lambda (k v) (message "( %s . %s )" k v)) torus/torus)

  )

;; ------------------------------

(provide 'torus)

;;; torus.el ends here
