;; map tile entities

;; map tile entities normally should have their 'size' set to :map-tile
;;   rather than their explicit size. with 'size' set to :map-tile, the
;;   size of the entity will be that of the map's 'tile-size' defined in the
;;   scene's schema.

;; map tile entities should have their 'shape' set to one of the following in
;;   order for the map grid to be rendered correctly:
;; supported shapes - :hexagon

;; map tile entities should not have 'lines' set. the geometry of a map tile
;;   is programatically generated based on its defined 'shape'.

(
 ;; a 6-sided map tile
 (:hex
   (:image "grass.png"
    :size :map-tile
    :shape :hexagon))
)
