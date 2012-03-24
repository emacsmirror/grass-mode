

(setq grass-command-arguments 
      ;; Contains all the arguments for each Grass command, for use with
      ;; completion functions. Not complete, working on automating the
      ;; extraction of this info from the help files.")
  '(("d.vect" . 
     (("map" "Name of input vector map")
      ("display" "Display")
      ("type" "Feature type")
      ("layer" "Layer number (if -1, all layers are displayed)")
      ("cats" "Category values")
      ("where" "WHERE conditions of SQL statement without 'where' keyword")
      ("color" "Line color")
      ("fcolor" "Area fill color")
      ("rgb_column" "Name of color definition column (for use with -a flag)")
      ("zcolor" "Type of color table (for use with -z flag)")
      ("width" "Line width")
      ("wcolumn" "Name of column for line widths (these values will be scaled by wscale)")
      ("wscale" "Scale factor for wcolumn")
      ("icon" "Point and centroid symbol")
      ("size" "Symbol size")
      ("size_column" "Name of numeric column containing symbol size")
      ("rot_column" "Name of numeric column containing symbol rotation angle")
      ("llayer" "Layer number")
      ("attrcol" "Name of column to be displayed")
      ("lcolor" "Label color")
      ("bgcolor" "Label background color")
      ("bcolor" "Label border color")
      ("lsize" "Label size (pixels)")
      ("font" "Font name")
      ("xref" "Label horizontal justification")
      ("yref" "Label vertical justification")
      ("minreg" "Minimum region size (average from height and width) when map is displayed")
      ("maxreg" "Maximum region size (average from height and width) when map is displayed")
      ("render" "Rendering method for filled polygons")))
    ("d.rast" . 
     (("map" "Name of raster map to be displayed")
      ("catlist" "List of categories to be displayed (INT maps)")
      ("vallist" "List of values to be displayed (FP maps)")
      ("bg" "Background color (for null)")))
    ("d.what.rast" .
     (("map" "Name of existing raster map(s)")
      ("fs" "Field separator (terse mode only)")))
    ("d.mon" .
     (("start" "Name of graphics monitor to start")
      ("stop" "Name of graphics monitor to stop")
      ("select" "Name of graphics monitor to select")
      ("unlock" "Name of graphics monitor to unlock")))
    ("g.mapset" .
     (("mapset" "Name of mapset where to switch")
      ("location" "Location name (not location path)")
      ("gisdbase" 
       "GIS data directory (full path to the directory where the new location is)"))))) 

("g.mapset"
 ("mapset" "Name of mapset where to switch")
 ("location" "Location name (not location path)")
 ("gisdbase" "GIS data directory (full path to the directory where the new location is)"))


(defvar grass-commands 
  '("d.ask" "d.barscale" "d.colorlist" "d.colors" "d.colortable"
  "d.correlate" "d.erase" "d.extend" "d.extract" "d.font" "d.frame"
  "d.geodesic" "d.graph" "d.grid" "d.his" "d.histogram" "d.info"
  "d.labels" "d.legend" "d.linegraph" "d.m" "d.mapgraph" "d.measure"
  "d.menu" "d.mon" "d.monsize" "d.mvmon" "d.nviz" "d.out.file"
  "d.out.gpsdrive" "d.out.png" "d.path" "d.polar" "d.profile"
  "d.rast.arrow" "d.rast.edit" "d.rast" "d.rast.leg" "d.rast.num"
  "d.redraw" "d.resize" "d.rgb" "d.rhumbline" "d.save" "d.shadedmap"
  "d.slide.show" "d.split.frame" "d.split" "d.text" "d.thematic.area"
  "d.title" "d.vect.chart" "d.vect" "d.vect.thematic" "d.what.rast"
  "d.what.vect" "d.where" "d.zoom"))



;; (setq font-lock-keywords-case-fold-search nil)



(defun grass-process-help-file (start end)
  "Takes the parameter list from a help file and converts it to an alist.
Not intended for users, this is a helper function for building the
completion lists "
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (beginning-of-buffer)
    (replace-regexp "^\\(\\(?:\\w\\|_\\)+\\)=[^\n]*\n    \\(.*\\)"
                    "(\"\\1\" \"\\2\")")
    (widen)))

(provide 'grass-commands)
