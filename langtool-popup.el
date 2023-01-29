;;; langtool-popup.el --- Popup message extension for langtool.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 Masahiro Hayashi

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: docs
;; URL: https://github.com/mhayashi1120/Emacs-langtool
;; Emacs: GNU Emacs 27 or later
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.3") (popup "20221231.1634"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ## Install:
;;
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'langtool-popup)

;; ## Usage:
;;
;; This idea originally come from:
;; ref: https://laclefyoshi.hatenablog.com/entry/20150912/langtool_popup

;;; Code:

(require 'popup)
(require 'langtool)

(defun langtool-popup-autoshow-detail (overlays)
  "Popup LanguageTool message (on OVERLAYS) with `popup`."

  ;; Do not interrupt current popup
  (unless (or popup-instances
              ;; suppress popup after type `C-g' .
              (memq last-command '(keyboard-quit)))
    (let ((msg (langtool-details-error-message overlays)))
      (popup-tip msg))))

(setq langtool-autoshow-message-function
      'langtool-popup-autoshow-detail)

;; To restore default while `unload-feature'
(defun langtool-popup-unload-function ()
  "Called when `unload-feature` ."
  ;; FIXME: or get defcustom form (Unable get `default-value`)
  (setq langtool-autoshow-message-function
        'langtool-autoshow-default-message))

(provide 'langtool-popup)

;;; langtool-popup.el ends here
