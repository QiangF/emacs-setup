;;; touchpad-toggle.el

;; Copyright (C) 2019 Jamie Beardslee

;; Author: Jamie Beardslee <beardslee@gmail.com>
;; Package-version: 20191101.2236

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Code:
(defvar touchpad-disabled-p nil
  "Whether the touchpad is disabled. If the value is non-nil, the
touchpad is disabled.")

(defun disable-touchpad ()
  "Disables the touchpad."
  (interactive)
  (shell-command
   "xinput disable 10; xinput disable 11")
  (setq touchpad-disabled-p t)
  (message "Mouse input disabled"))

(defun enable-touchpad ()
  "Enables the touchpad."
  (interactive)
  (shell-command
   "xinput enable 10; xinput enable 11")
  (setq touchpad-disabled-p nil)
  (message "Mouse input enabled"))

(defun toggle-touchpad ()
  "Toggles the touchpad. If the variable `touchpad-disabled-p' is
nil, enable it. Otherwise, enable it."
  (interactive)
  (if touchpad-disabled-p
      (enable-touchpad)
    (disable-touchpad)))

(provide 'toggle-touchpad)
