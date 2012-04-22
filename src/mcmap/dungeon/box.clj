(ns mcmap.dungeon.box)

;;; Defines the 'Box' datatype, collections of which compose dungeons.

(defrecord Box [x0 y0 z0 xd yd zd zone])

;;; ... aaaand we're done.

