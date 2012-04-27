;; grass-doc-table contains all the help files in dotted pairs: (command-name . full-help-file) 
;; grass-commands contains all the grass commands and their parameters.

;; Re-parse this list from the help files with grass-init-command-list. This used to be
;; slow (8+ seconds), so I cached the results in this file. Switchting from find-file to
;; with-temp-buffer insert-file-contents resulted in loading taking less than a second, so
;; caching is no longer necessary.  

;; Note that I parse the help files to create grass-commands first, then add various
;; completion lists and functions to grass-commands using grass-p-comp below. This
;; separation means that additional completion options can be added piece-meal, and we
;; never need to hand-edit grass-commands.


(defun grass-p-comp (pairs completion)
  "set the completion string/function for the parameter of command"
  (dolist (p pairs)
    (setcdr
     (cdr (assoc (second p) (assoc (first p) grass-commands)))
     (cons completion nil))))

(grass-p-comp '(("d.vect" "icon")) 
             '("demo/smrk" "demo/muchomurka" "basic/x" "basic/box" "basic/star" "basic/pushpin"
               "basic/diamond" "basic/ triangle" "basic/point" "basic/arrow1" "basic/arrow2"
               "basic/circle" "basic/cross1" "basic/cross2" "basic/ marker" "basic/octagon"
               "extra/adcp" "extra/fish" "extra/ping" "extra/ring" "extra/compass" "extra/fiducial"
               "extra /4pt_star" "extra/dive_flag" "extra/half-box" "extra/bridge" "extra/offbox_ne"
               "extra/offbox_nw" "extra/ offbox_se" "extra/offbox_sw" "extra/fancy_compass"
               "extra/n_arrow1" "extra/n_arrow2" "extra/target" "extra/ airport" "extra/alpha_flag"
               "extra/pentagon" "extra/half-circle" "geology/strike_line" "geology/ strike_box"
               "geology/strike_circle" "geology/strike_triangle"))  


(grass-p-comp '(("d.vect" "type")) '("point" "line" "boundary" "centroid" "area" "face"))

(grass-p-comp '(("v.proj" "location") ("g.proj" "location")) 'grass-location-list)

(grass-p-comp '(("g.region" "region")) 'grass-regions)

(grass-p-comp '(("db.connect" "driver")) '("pg" "dbf" "ogr" "odbc" "mysql" "sqlite"))

;; Wouldn't it be nice if everytime an argument takes a vector map as its value, it was
;; called the same thing? Some of these are unavoidable (ainput, binput), but qgis?
;; Really? 

(grass-p-comp '(("d.vect" "map") ("d.extract" "input") ("d.path" "map")
                ("d.vect.chart" "map") ("d.vect.thematic" "map")
                ("d.what.vect" "map") ("d.zoom" "vector") 
                ("g.rename" "vect")
                ("g.remove" "vect")
                ("r.carve" "vect") ("r.drain" "vector_points") ("g.region" "vect")
                ("r.le.setup" "vect") ("r.region" "vector") ("r.volume" "centroids")
                ("v.buffer" "input") ("v.build" "map") ("v.build.polylines" "input")
                ("v.category" "input") ("v.centroids" "input") ("v.class" "map")
                ("v.clean" "input") ("v.colors" "map") ("v.convert" "input")
                ("v.db.addcol" "map") ("v.db.addtable" "map") ("v.db.connect" "map")
                ("v.db.dropcol" "map") ("v.db.droptable" "map") ("v.db.join" "map")
                ("v.db.renamecol" "map") ("v.db.select" "map") ("v.db.update" "map")
                ("v.delaunay" "input") ("v.digit" "map") ("v.dissolve" "input")
                ("v.distance" "to") ("v.distance" "from") ("v.drape" "input")
                ("v.edit" "bgmap") ("v.edit" "map") ("v.extract" "input")
                ("v.extrude" "input") ("v.generalize" "input") ("v.hull" "input")
                ("v.info" "map") ("v.kcv" "input") ("v.kernel" "net") ("v.kernel" "input")
                ("v.label" "map") ("v.label.sa" "map") ("v.lidar.correction" "input")
                ("v.lidar.edgedetection" "input") ("v.lidar.growing" "input")
                ("v.lrs.create" "points") ("v.lrs.create" "in_lines")
                ("v.lrs.label" "input") ("v.lrs.segment" "input")
                ("v.lrs.where" "points") ("v.lrs.where" "lines") ("v.neighbors" "input")
                ("v.net.alloc" "input") ("v.net" "points") ("v.net" "input")
                ("v.net.iso" "input") ("v.net.path" "input") ("v.net.salesman" "input")
                ("v.net.steiner" "input") ("v.net.visibility" "input") ("v.normal" "map") 
                ("v.out.ascii" "input") ("v.out.dxf" "input") ("v.out.gpsbabel" "input") 
                ("v.out.ogr" "input") ("v.out.pov" "input") ("v.out.svg" "input")
                ("v.out.vtk" "input") ("v.outlier" "qgis") ("v.outlier" "input")
                ("v.overlay" "binput") ("v.overlay" "ainput") ("v.parallel" "input")
                ("v.patch" "input") ("v.rast.stats" "vector")
                ("v.reclass" "input") ("v.report" "map") ("v.sample" "input")
                ("v.segment" "input") ("v.select" "binput") ("v.select" "ainput")
                ("v.split" "input") ("v.support" "map") ("v.surf.bspline" "sparse")
                ("v.surf.bspline" "input") ("v.surf.idw" "input") ("v.surf.rst" "input")
                ("v.to.3d" "input") ("v.to.db" "map") ("v.to.points" "input")
                ("v.to.rast" "input") ("v.to.rast3" "input") ("v.transform" "input")
                ("v.type" "input") ("v.univar" "map") ("v.vol.rst" "input")
                ("v.voronoi" "input") ("v.what" "map") ("v.what.rast" "vector")
                ("v.what.vect" "vector")) 'grass-vector-maps)

(grass-p-comp '(("db.columns" "table")) 'grass-all-maps)

(provide 'grass-commands)
