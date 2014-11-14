;;; sx-encoding.el --- encoding for stack-mode

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>

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

;;

;;; Code:

(require 'cl-lib)

(defcustom sx-encoding-html-entities-plist
  '(Aacute "Á" aacute "á" Acirc "Â" acirc "â" acute "´" AElig "Æ" aelig "æ"
           Agrave "À" agrave "à" alefsym "ℵ" Alpha "Α" alpha "α" amp "&" and "∧"
           ang "∠" apos "'" aring "å" Aring "Å" asymp "≈" atilde "ã" Atilde "Ã"
           auml "ä" Auml "Ä" bdquo "„" Beta "Β" beta "β" brvbar "¦" bull "•"
           cap "∩" ccedil "ç" Ccedil "Ç" cedil "¸" cent "¢" Chi "Χ" chi "χ"
           circ "ˆ" clubs "♣" cong "≅" copy "©" crarr "↵" cup "∪" curren "¤"
           Dagger "‡" dagger "†" darr "↓" dArr "⇓" deg "°" Delta "Δ" delta "δ"
           diams "♦" divide "÷" eacute "é" Eacute "É" ecirc "ê" Ecirc "Ê" egrave "è"
           Egrave "È" empty "∅" emsp " " ensp " " Epsilon "Ε" epsilon "ε" equiv "≡"
           Eta "Η" eta "η" eth "ð" ETH "Ð" euml "ë" Euml "Ë" euro "€"
           exist "∃" fnof "ƒ" forall "∀" frac12 "½" frac14 "¼" frac34 "¾" frasl "⁄"
           Gamma "Γ" gamma "γ" ge "≥" gt ">" harr "↔" hArr "⇔" hearts "♥"
           hellip "…" iacute "í" Iacute "Í" icirc "î" Icirc "Î" iexcl "¡" igrave "ì"
           Igrave "Ì" image "ℑ" infin "∞" int "∫" Iota "Ι" iota "ι" iquest "¿"
           isin "∈" iuml "ï" Iuml "Ï" Kappa "Κ" kappa "κ" Lambda "Λ" lambda "λ"
           lang "〈" laquo "«" larr "←" lArr "⇐" lceil "⌈" ldquo "“" le "≤"
           lfloor "⌊" lowast "∗" loz "◊" lrm "" lsaquo "‹" lsquo "‘" lt "<"
           macr "¯" mdash "—" micro "µ" middot "·" minus "−" Mu "Μ" mu "μ"
           nabla "∇" nbsp "" ndash "–" ne "≠" ni "∋" not "¬" notin "∉"
           nsub "⊄" ntilde "ñ" Ntilde "Ñ" Nu "Ν" nu "ν" oacute "ó" Oacute "Ó"
           ocirc "ô" Ocirc "Ô" OElig "Œ" oelig "œ" ograve "ò" Ograve "Ò" oline "‾"
           omega "ω" Omega "Ω" Omicron "Ο" omicron "ο" oplus "⊕" or "∨" ordf "ª"
           ordm "º" oslash "ø" Oslash "Ø" otilde "õ" Otilde "Õ" otimes "⊗" ouml "ö"
           Ouml "Ö" para "¶" part "∂" permil "‰" perp "⊥" Phi "Φ" phi "φ"
           Pi "Π" pi "π" piv "ϖ" plusmn "±" pound "£" Prime "″" prime "′"
           prod "∏" prop "∝" Psi "Ψ" psi "ψ" quot "\"" radic "√" rang "〉"
           raquo "»" rarr "→" rArr "⇒" rceil "⌉" rdquo "”" real "ℜ" reg "®"
           rfloor "⌋" Rho "Ρ" rho "ρ" rlm "" rsaquo "›" rsquo "’" sbquo "‚"
           scaron "š" Scaron "Š" sdot "⋅" sect "§" shy "" Sigma "Σ" sigma "σ"
           sigmaf "ς" sim "∼" spades "♠" sub "⊂" sube "⊆" sum "∑" sup "⊃"
           sup1 "¹" sup2 "²" sup3 "³" supe "⊇" szlig "ß" Tau "Τ" tau "τ"
           there4 "∴" Theta "Θ" theta "θ" thetasym "ϑ" thinsp " " thorn "þ" THORN "Þ"
           tilde "˜" times "×" trade "™" uacute "ú" Uacute "Ú" uarr "↑" uArr "⇑"
           ucirc "û" Ucirc "Û" ugrave "ù" Ugrave "Ù" uml "¨" upsih "ϒ" Upsilon "Υ"
           upsilon "υ" uuml "ü" Uuml "Ü" weierp "℘" Xi "Ξ" xi "ξ" yacute "ý"
           Yacute "Ý" yen "¥" yuml "ÿ" Yuml "Ÿ" Zeta "Ζ" zeta "ζ" zwj "" zwnj "")
  "Plist of html entities to replace when displaying question titles and other text."
  :type '(repeat (choice symbol string))
  :group 'sx)

(defun sx-encoding-decode-entities (string)
  (let* ((plist sx-encoding-html-entities-plist)
         (get-function (lambda (s) (let ((ss (substring s 1 -1)))
                                     ;; Handle things like &quot;
                                     (or (plist-get plist (intern ss))
                                         ;; Handle things like &#39;
                                         (format "%c" (string-to-number
                                                       (substring ss 1))))))))
    (replace-regexp-in-string "&[^; ]*;" get-function string)))

(defun sx-encoding-normalize-line-endings (string)
  "Normalize the line endings for STRING"
  (delete ?\r string))

(defun sx-encoding-clean-content (string)
  "Cleans STRING for display.
Applies `sx-encoding-normalize-line-endings' and
`sx-encoding-decode-entities'."
  (sx-encoding-decode-entities
   (sx-encoding-normalize-line-endings
    string)))

;;; @TODO: This is a pretty ugly implementation.  It can likely be
;;; simplified and it should be improved.
(defun sx-encoding-clean-content-deep (data)
  "Clean DATA recursively where necessary.

See `sx-encoding-clean-content'."
  (cond
   ;; If we're looking at an atom, clean it if it's a string.
   ;; Otherwise, just return it.
   ((atom data)
    (if (stringp data) (sx-encoding-clean-content data) data))
   ;; Looking at a vector?  Recurse by mapping.
   ((vectorp data)
    (cl-map #'vector #'sx-encoding-clean-content-deep data))
   ;; Is our cdr a vector?  Map again, but reconstruct the cons cell.
   ((vectorp (cdr data))
    (cons (car data) (cl-map #'vector
                             #'sx-encoding-clean-content-deep
                             (cdr data))))
   ;; Is our cdr just a string?  Clean and return it, reconstructing.
   ((stringp (cdr data))
    (cons (car data) (sx-encoding-clean-content (cdr data))))
   ;; Is this a cons cell where the other part is an atom?  Return the
   ;; atom.  If we got here, it wasn't a string.
   ((and (consp data) (atom (cdr data))) data)
   ;; If it's a list, map.
   ((listp data) (mapcar #'sx-encoding-clean-content-deep data))
   ;; Default to identity.
   (t data)))

(defun sx-encoding-gzipped-p (data)
  "Checks for magic bytes in DATA.

Check if the first two bytes of a string in DATA match magic
numbers identifying the gzip file format. See [1] for the file
format description.

http://www.gzip.org/zlib/rfc-gzip.html

http://emacs.stackexchange.com/a/2978"
  (equal (substring (string-as-unibyte data) 0 2)
         (unibyte-string 31 139)))

(defun sx-encoding-gzipped-buffer-p (filename)
  "Check if the BUFFER is gzip-compressed.

See `gzip-check-magic' for details."
  (sx-encoding-gzip-check-magic (buffer-string)))

(defun sx-encoding-gzipped-file-p (file)
  "Check if the FILE is gzip-compressed.

See `gzip-check-magic' for details."
  (let ((first-two-bytes (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally file nil 0 2)
                           (buffer-string))))
    (sx-encoding-gzipped-p first-two-bytes)))

(provide 'sx-encoding)
;;; sx-encoding.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
