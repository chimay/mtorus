;;; HTorus : like MTorus but with hash table, seems easier to handle the data structure

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

(defvar htorus-version "0.1"
  "Version number of htorus.")


;; Custom group
;; ------------------------------

(defgroup htorus nil

  "An interface to navigating groups of buffers.
This code should work fast so intuition is a very important
matter. Some of the customizable variables are switches to tune
the behavior of the functions to your habits. Hopefully it is
possible to find good settings for many people."

  :tag "Htorus"
  :link '(url-link :tag "Home Page"
                   "https://github.com/chimay/mtorus")
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



;; Functions
;; ------------------------------



;; ------------------------------

(provide 'htorus)

;;; htorus.el ends here
