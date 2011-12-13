(defpackage #:%3b-swf
  (:use #:cl)
  (:nicknames #:%swf)
  (:export
   #:rect #:xmin #:xmax #:ymin #:ymax
   #:rgba #:rgb #:r #:g #:b #:a
   #:file-attributes-tag
   #:use-direct-blit
   #:use-gpu
   #:has-metadata
   #:actionscript-3
   #:use-network
   #:script-limits-tag
   #:max-recursion-depth
   #:script-timeout-seconds
   #:matrix-part-fixed
   #:value1
   #:value2
   #:matrix-part-translate
   #:matrix
   #:scale
   #:rotate-skew
   #:translate
   #:swf-tag
   #:swf-show-frame-tag
   #:read-swf-part
   #:%swf-part-size
   #:write-swf-part
   #:character-id
   #:define-shape-tag
   #:place-object-tag
   #:remove-object-tag
   #:define-bits-tag
   #:define-button
   #:jpeg-tables
   #:set-background-color-tag
   #:define-font-tag
   #:background-color
   #:define-text-tag
   #:do-action
   #:actions
   #:define-font-info-tag
   #:define-sound-tag
   #:start-sound-tag
   #:define-button-sound
   #:sound-stream-head
   #:sound-stream-block
   #:define-bits-lossless-tag
   #:define-bits-jpeg-2-tag
   #:image-data
   #:define-shape-2-tag
   #:shapes
   #:bounds
   #:shape-with-style
   #:define-button-cxform-tag
   #:protect-tag
   #:place-object-2-tag
   #:move-flag
   #:depth
   #:matrix
   #:color-transform
   #:po2-ratio
   #:name
   #:clip-depth
   #:clip-actions
   #:remove-object-2-tag
   #:define-shape-3-tag
   #:define-text-2-tag
   #:matrix
   #:define-button-2-tag
   #:define-bits-jpeg-3-tag
   #:define-bits-lossless-2-tag
   #:define-edit-text-tag
   #:define-sprite-tag
   #:frame-label-tag
   #:sound-stream-head-2-tag
   #:define-morph-shape
   #:start-bounds
   #:end-bounds
   #:offset
   #:morph-fill-styles
   #:morph-line-styles
   #:start-edges
   #:end-edges
   #:define-font-2-tag
   #:enable-debugger-tag
   #:do-init-action-tag
   #:define-video-stream-tag
   #:video-frame-tag
   #:define-font-info-2-tag
   #:enable-debugger-2
   #:place-object-3-tag
   #:has-image
   #:class-name
   #:matrix
   #:po3-ratio
   #:surface-filter-list
   #:blend-mode
   #:bitmap-cache
   #:import-assets-2-tag
   #:define-font-align-zones-tag
   #:csm-text-settings-tag
   #:define-font-3-tag
   #:symbol-class-tag
   #:num-symbols
   #:symbol-class-symbols
   #:metadata-tag
   #:metadata
   #:do-abc-tag
   #:flags
   #:data
   #:define-shape-4-tag
   #:edge-bounds
   #:uses-fill-winding-rule
   #:uses-non-scaling-strokes
   #:uses-scaling-strokes
   #:define-morph-shape-2-tag
   #:start-edge-bounds
   #:end-edge-bounds
   #:define-scene-and-frame-label-data-tag
   #:scene-count
   #:scenes
   #:frame-label-count
   #:frames
   #:define-font-name-tag
   #:swf-end-tag
   #:named-anchor-flag
   #:frame-count
   #:control-tags
   #:cxform-with-alpha
   #:cxform
   #:add
   #:mult
   #:blur-filter
   #:glow-filter
   #:blur-x
   #:blur-y
   #:passes
   #:inner-glow
   #:strength
   #:knockout
   #:inner-glow
   #:glow-color
   #:filter-list
   #:filters
   #:read-swf
   #:export-assets-tag
   #:assets
   #:has-character
   #:original-character-id
   #:new-character-id
   #:place-object-3-tag
   #:export-assets-tag
   #:assets
   #:do-abc-tag
   #:flags
   #:name
   #:minor-version
   #:major-version
   #:constant-pool
   #:method-count
   #:method-info
   #:metadata-info
   #:instance-info
   #:class-info
   #:script-info
   #:method-body-info
   #:abc-constant-pool
   #:integers
   #:unsigned-integers
   #:doubles
   #:strings
   #:namespaces
   #:ns-sets
   #:multinames
   #:abc-method-info
   #:return-type
   #:param-types
   #:name
   #:set-dxns
   #:reserved-flags
   #:resERVED-flags
   #:need-rest
   #:need-activation
   #:need-arguments
   #:options
   #:param-names
   #:abc-method-info-option-info
   #:abc-interned-value+kind-constant
   #:value
   #:kind
   #:abc-metadata-info
   #:items
   #:abc-metadata-item-info
   #:key
   #:value
   #:abc-instance-info
   #:super-name
   #:class-interface-p
   #:class-final-p
   #:class-sealed-p
   #:protected-ns
   #:interfaces
   #:instance-init
   #:traits
   #:abc-class-info
   #:abc-class-info
   #:abc-script-info
   #:script-init
   #:class-init
   #:abc-method-body-info
   #:abc-method-body-info
   #:method-name
   #:max-stack
   #:local-count
   #:init-scope-depth
   #:max-scope-depth
   #:code
   #:exceptions
   #:traits
   #:abc-method-body-info
   #:abc-exception-info
   #:from
   #:to
   #:target
   #:exception-type
   #:var-name
   #:abc-trait-info-slot
   #:abc-trait-info-constant
   #:slot-id
   #:type-name
   #:value
   #:abc-trait-info-method
   #:slot-id
   #:method-name
   #:abc-trait-info-getter
   #:abc-trait-info-setter
   #:abc-trait-info-class
   #:abc-trait-info-function
   #:abc-interned-value+optional-kind-constant
   #:abc-trait-info-class
   #:abc-trait-info-function
   #:function-name
   #:instance-init
   #:define-bits-lossless-2-tag
   #:bitmap-data
   #:bitmap-tag-data-rgba
   #:width
   #:height
   #:bitmap-tag-data-rgba-argb
   #:abc-namespace
   #:abc-namespace
   #:abc-ns-set
   #:ns
   #:abc-multiname
   #:abc-multiname
   #:ns-set
   #:font-id
   #:text-records
   #:place-character-id
   #:shapes
   #:fill-styles
   #:line-styles
   #:shape-records
   #:fill-style
   #:fill-repeating-bitmap-fill
   #:bitmap-id
   #:fill-clipped-bitmap-fill
   #:fill-non-smoothed-clipped-bitmap-fill
   #:fill-non-smoothed-repeating-bitmap-fill
   #:fill-clipped-bitmap-fill
   #:has-fill
   #:line-style-2
   #:line-style
   #:fill-type
   #:shape-record
   #:style-change-shape-record
   #:option-count
   #:options
   #:param-count
   #:method-id
   #:symbol-class-symbols
   #:abc-multiname-qname-a
   #:abc-multiname-rt-qname
   #:abc-multiname-rt-qname-a
   #:abc-multiname-rt-qname-l
   #:abc-multiname-rt-qname-la
   #:abc-multiname-multiname
   #:abc-multiname-multiname-a
   #:abc-multiname-multiname-l
   #:abc-multiname-multiname-la
   #:abc-multiname-generic
   #:abc-multiname-qname
   #:params
   #:exception-type
   #:sound-sample-count
   #:stereo
   #:16bit
   #:sound-rate
   #:sound-format
   #:sound-data
   #:mp3-sound-data
   #:seek-samples
   #:mp3-frames
   #:sound-info
   #:sync-stop
   #:sync-no-multiple
   #:has-envelope
   #:in-point
   #:out-point
   #:loop-count
   #:env-points
   #:envelope-records
   #:sound-envelope
   #:pos44
   #:left-level
   #:right-level
   #:straight-edge-shape-record
   #:move-to
   #:delta-x
   #:delta-y
   #:curved-edge-shape-record
   #:control-delta-x
   #:control-delta-y
   #:anchor-delta-x
   #:anchor-delta-y
   #:fill-style-solid
   #:color
   #:fill-style-0
   #:fill-style-1
   #:fill-linear-gradient
   #:fill-radial-gradient
   #:fill-focal-gradient
   #:gradient-records
   #:gradient
   #:gradient-matrix
   #:focal-point
   #:bitmap-matrix
   #:spread-mode
   #:interpolation-mode
   #:gradient-ratio
   #:override-p
   #:final-p
   ))
