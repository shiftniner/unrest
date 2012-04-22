(ns mcmap.dungeon.box)

;;; Defines the 'Box' datatype, collections of which compose dungeons.

(defrecord Box [^int x0 ^int y0 ^int z0 ^int xd ^int yd ^int zd zone])

;;; ... aaaand we're done.

