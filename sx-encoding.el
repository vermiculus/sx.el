;;; sx-encoding.el --- encoding                      -*- lexical-binding: t; -*-

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

;; This file handles decoding the responses we get from the API.  They
;; are received either as plain-text or as a `gzip' compressed archive.
;; For this, `sx-encoding-gzipped-p' is used to determine if content
;; has been compressed under `gzip'.

;;; Code:

(require 'cl-lib)


;;;; HTML Encoding

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
           nabla "∇" nbsp " " ndash "–" ne "≠" ni "∋" not "¬" notin "∉"
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
  "Plist of HTML entities and their respective glyphs.
See `sx-encoding-decode-entities'."
  :type '(repeat (choice symbol string))
  :group 'sx)

(defun sx-encoding-decode-entities (string)
  "Decode HTML entities (e.g. \"&quot;\") in STRING.

Done according to `sx-encoding-html-entities-plist'.  If this
list does not contain the entity, it is assumed to be a number
and converted to a string (with `char-to-string').

Return the decoded string."
  (let* ((plist sx-encoding-html-entities-plist)
         (get-function
          (lambda (s)
            (let ((ss (substring s 1 -1)))
              ;; Handle things like &quot;
              (or (plist-get plist (intern ss))
                  ;; Handle things like &#39;
                  (char-to-string
                   (string-to-number
                    ;; Skip the `#'
                    (substring ss 1))))))))
    (replace-regexp-in-string "&[^; ]*;" get-function string)))


;;;; Convenience Functions

(defun sx-encoding-normalize-line-endings (string)
  "Normalize the line endings for STRING.
The API returns strings that use Windows-style line endings.
These are largely useless in an Emacs environment.  Windows uses
\"\\r\\n\", Unix uses just \"\\n\".  Deleting \"\\r\" is sufficient for
conversion."
  (delete ?\r string))

(defun sx-encoding-clean-content (string)
  "Clean STRING for display.
Applies `sx-encoding-normalize-line-endings' and
`sx-encoding-decode-entities' (in that order) to prepare STRING
for sane display."
  (sx-encoding-decode-entities
   (sx-encoding-normalize-line-endings
    string)))

(defun sx-encoding-clean-content-deep (data)
  "Clean DATA recursively where necessary.

If DATA is a list or a vector, map this function over DATA and
return as the the same type of structure.

If DATA is a cons cell (but not a list), use
`sx-encoding-clean-content-deep' on the `cdr' of DATA.

If DATA is a string, return DATA after applying
`sx-encoding-clean-content'.

Otherwise, return DATA.

This function is highly specialized for the data structures
returned by `json-read' via `sx-request-make'.  It may fail in
some cases."
  (if (consp data)
      (if (listp (cdr data))
          (cl-map #'list #'sx-encoding-clean-content-deep data)
        (cons (car data) (sx-encoding-clean-content-deep (cdr data))))
    (cond
     ((stringp data)
      (sx-encoding-clean-content data))
     ((vectorp data)
      (cl-map #'vector #'sx-encoding-clean-content-deep data))
     (t data))))


;;;; GZIP

(defun sx-encoding-gzipped-p (data)
  "Check for magic bytes in DATA.
Check if the first two bytes of a string in DATA match the magic
numbers identifying the gzip file format.

See URL `http://www.gzip.org/zlib/rfc-gzip.html'."
  ;; Credit: http://emacs.stackexchange.com/a/2978
  (equal (substring (string-as-unibyte data) 0 2)
         (unibyte-string 31 139)))

(defun sx-encoding-gzipped-buffer-p (buffer)
  "Check if BUFFER is gzip-compressed.
See `sx-encoding-gzipped-p'."
  (with-current-buffer buffer
    (sx-encoding-gzipped-p
     (buffer-string))))

(defun sx-encoding-gzipped-file-p (file)
  "Check if the FILE is gzip-compressed.
See `sx-encoding-gzipped-p'."
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
