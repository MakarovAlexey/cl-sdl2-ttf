(in-package :sdl2-ttf)

;; Get the rendering style of the loaded font
(macrolet ((sdl-font-style (function-name constant)
             `(flet ((font-style-value (ttf-font-struct)
                       (logand
                        (ttf-get-font-style ttf-font-struct) ,constant)))
                (defun ,function-name (ttf-font-struct)
                  (> (font-style-value ttf-font-struct) 0))
                (defun (setf ,function-name) (value ttf-font-struct)
                  (let ((current-font-style
                         (font-style-value ttf-font-struct)))
                    (ttf-set-font-style ttf-font-struct
                                        (if (not (null value))
                                            (logior current-font-style
                                                    ,constant)
                                            (logand current-font-style
                                                    (lognot ,constant)))))))))
  (sdl-font-style font-style-bold-p sdl2-ffi:+ttf-style-bold+)
  (sdl-font-style font-style-italic-p sdl2-ffi:+ttf-style-italic+)
  (sdl-font-style font-style-underline-p sdl2-ffi:+ttf-style-underline+)
  (sdl-font-style font-style-style-strikethrough sdl2-ffi:+ttf-style-strikethrough+))

(defun font-outline (ttf-font-struct)
  "Get the current outline size of the loaded font"
  (ttf-get-font-outline ttf-font-struct))

(defun (setf font-outline) (value ttf-font-struct)
  "Set the outline pixel width of the loaded font"
  (when (not (= value (font-outline ttf-font-struct)))
    (ttf-set-font-outline ttf-font-struct value)))

(autowrap:define-enum-from-constants (sdl-font-hinting)
  sdl2-ffi:+ttf-hinting-normal+
  sdl2-ffi:+ttf-hinting-light+
  sdl2-ffi:+ttf-hinting-mono+
  sdl2-ffi:+ttf-hinting-none+)

(defun font-hinting (ttf-font-struct)
  "Get freetype hinter setting"
  (let ((font-hinting-value
         (ttf-get-font-hinting ttf-font-struct)))
    (autowrap:enum-key 'sdl-font-hinting font-hinting-value)))

(defun (setf font-hinting) (value ttf-font-struct)
  "Set the hinting of the loaded font. You should experiment with this
setting if you know which font you are using beforehand, especially
when using smaller sized fonts. If the user is selecting a font, you
may wish to let them select the hinting mode for that font as well"
  (let ((enum-value
         (autowrap:enum-value 'sdl-font-hinting value)))
    (ttf-set-font-hinting ttf-font-struct enum-value)))
  
(defun font-kerning-enabled-p (ttf-font-struct)
  "Get the current kerning setting of the loaded font"
  (> (ttf-get-font-kerning ttf-font-struct) 0))

(defun (setf font-kerning-allowed-p) (value ttf-font-struct)
  "Set whther to use kerning when rendering the loaded font. This has
no effect on individual glyphs, but rather when rendering whole
strings of characters, at least a word at a time. Perhaps the only
time to disable this is when kerning is not working for a specific
font, resulting in overlapping glyphs or abnormal spacing within
words"
  (if (not (null value))
      (ttf-set-font-kerning ttf-font-struct 1)
      (ttf-set-font-kerning ttf-font-struct 0)))

(defun font-height (ttf-font-struct)
  "Get font maximum total height"
  (ttf-font-height ttf-font-struct))

(defun font-ascent (ttf-font-struct)
  "Get font highest ascent (height above base)"
  (ttf-font-ascent ttf-font-struct))

(defun font-descent (ttf-font-struct)
  "Get font lowest descent (height below base)"
  (ttf-font-descent ttf-font-struct))

(defun font-line-skip (ttf-font-struct)
  "Get font recommended line spacing"
  (ttf-font-line-skip ttf-font-struct))

(defun font-faces (ttf-font-struct)
  "Get the number of faces in a font"
  (ttf-font-faces ttf-font-struct))

(defun font-face-fixed-with-p (ttf-font-struct)
  "Get whether font is monospaced or not"
  (> (ttf-font-face-is-fixed-width ttf-font-struct) 0))

(defun font-face-family-name (ttf-font-struct)
  "Get current font face family name string"
  (ttf-font-face-family-name ttf-font-struct))

(defun font-face-style-name (ttf-font-struct)
  "Get current font face style name string"
  (ttf-font-face-style-name ttf-font-struct))

(defun glyph-provided-p (ttf-font-struct char)
  "Get individual font glyph availability"
  (ttf-glyph-is-provided ttf-font-struct char))

(defun glyph-metrics (ttf-font-struct char)
  "Get individual font glyph metrics"
  (let (minx maxx miny maxy advance)
    (ttf-glyph-metrics ttf-font-struct char minx maxx miny maxy advance)
    (values minx maxx miny maxy advance)))

(defun size-text (ttf-font-struct text width height)
  "Get size of LATIN1 text string as would be rendered"
  (ttf-size-text ttf-font-struct text width height))

(defun size-utf8 (ttf-font-struct text width height)
  "Get size of UTF8 text string as would be rendered"
  (ttf-size-utf8 ttf-font-struct text width height))

(defun size-unicode (ttf-font-struct text width height)
  "Get size of UNICODE text string as would be rendered"
  (ttf-size-unicode ttf-font-struct text width height))
