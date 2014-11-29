;; map tile entities

;; map tile entities normally should have 'tile' set to true. this tells the
;; map generator to use the size and shape of all map tiles specified in the
;; scene's schema.

;; the only other option needed for map tile entities is either 'image' set to
;; the desired texture, or 'color' if not using the default of white.

;; map tile entities should not have 'lines' set. the geometry of a map tile
;; is programatically generated based on the defined 'tile-shape' in the
;; scene's schema.

(
 ;; a 6-sided map tile
 (:hex
   (:image "grass.png"
    :tile 0))
)
