(in-package :%3b-swf)
;; todo: define the rest of these (adding as needed for now)
(defconstant +swf-end-tag+ 0)
(defconstant +metadata-tag+ 77)

;; extra tag IDs from http://www.m2osw.com/en/swf_alexref.html#table_of_swf_tags

;; mapping from id -> name
(defparameter *tag-id-plist*
  '(0  end-tag
    1  show-frame-tag
    2  define-shape-tag
    ;;3 free-character ?
    4  place-object-tag
    5  remove-object-tag
    6  define-bits-tag
    7  define-button-tag
    8  jpeg-tables-tag
    9  set-background-color-tag
    10 define-font-tag
    11 define-text-tag
    12 do-action-tag
    13 define-font-info-tag
    14 define-sound-tag
    15 start-sound-tag
    ;;16 stop-sound ?
    17 define-button-sound-tag
    18 sound-stream-head-tag
    19 sound-stream-block-tag
    20 define-bits-lossless-tag
    21 define-bits-jpeg-2-tag
    22 define-shape-2-tag
    23 define-button-cxform-tag
    24 protect-tag
    ;;25 paths-are-postscript ?
    26 place-object-2-tag
    28 remove-object-2-tag
    ;;29 sync-frame ?
    ;;31 free-all ?
    32 define-shape-3-tag
    33 define-text-2-tag
    34 define-button-2-tag
    35 define-bits-jpeg-3-tag
    36 define-bits-lossless-2-tag
    37 define-edit-text-tag
    ;;38 define-video ?
    39 define-sprite-tag
    ;;40 name-character ?
    41 product-info-tag
    ;;42 define-text-format ?
    43 frame-label-tag
    45 sound-stream-head-2-tag
    46 define-morph-shape-tag
    ;;47 generate-frame ?
    48 define-font-2-tag
    ;;49 generator-command
    ;;50 define-command-object ?
    ;;51 character-set ?
    ;;52 external-font ?
    56 export-assets-tag
    57 import-assets-tag
    58 enable-debugger-tag
    59 do-init-action-tag
    60 define-video-stream-tag
    61 video-frame-tag
    62 define-font-info-2-tag
    63 debug-id-tag
    64 enable-debugger-2-tag
    65 script-limits-tag
    66 set-tab-index-tag
    69 file-attributes-tag
    70 place-object-3-tag
    71 import-assets-2-tag
    ;; 72 do-abc
    73 define-font-align-zones-tag
    74 csm-text-settings-tag
    75 define-font-3-tag
    76 symbol-class-tag
    77 metadata-tag
    78 define-scaling-grid-tag
    82 do-abc-tag
    83 define-shape-4-tag
    84 define-morph-shape-2-tag
    86 define-scene-and-frame-label-data-tag
    87 define-binary-data-tag
    88 define-font-name-tag
    89 start-sound-2-tag
    90 define-bits-jpeg-4-tag
    91 define-font-4-tag))

(defparameter *bogus-tag-ids*
  '((777 swf-tools-777-tag) ;; ~4 bytes, unknown?
    (255 unknown-encryptor-255-tag) ;; 0 bytes, unknown
    (253 unknown-encryptor-253-tag) ;; x bytes, code?
    ))
(defparameter *blob-tags* nil)

;; unknown-encryptor1:
;;   tag255 = 1 byte
;;   tag253 = x bytes, code with entry point as jump in last instr?
;;   do-action-tag(12) = extra bytes at end?
;;   bad utf8 in constant pools
;;   bad action records
;;   define-font-3 extra junk at end?