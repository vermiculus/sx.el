;;; sx-load.el --- load all files of the SX package  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(mapc #'require
      '(sx
        sx-time
        sx-auth
        sx-button
        sx-babel
        sx-cache
        sx-compose
        sx-encoding
        sx-favorites
        sx-filter
        sx-inbox
        sx-interaction
        sx-method
        sx-networks
        sx-notify
        sx-question
        sx-question-list
        sx-question-mode
        sx-question-print
        sx-request
        sx-search
        sx-site
        sx-switchto
        sx-tab
        sx-tag
        ))

(provide 'sx-load)
;;; sx-load.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
