"""
Original GRASS icon set from old TCL/TK GUI
"""

import os

import wx

import globalvar

iconPath = os.path.join(globalvar.ETCDIR, "gui", "icons", "grass")
iconPathVDigit  = os.path.join(globalvar.ETCDIR, "gui", "icons", "grass", "edit")

iconSet = {
    # map display
    "show" : 'gui-display.gif',
    "layer-redraw"  : 'gui-redraw.gif',
    "erase"      : 'gui-erase.gif',
    "pointer"    : 'gui-pointer.gif',
    "zoom-in"    : 'gui-zoom_in.gif',
    "zoom-out"   : 'gui-zoom_out.gif',
    "pan"        : 'gui-pan.gif',
    "info"      : 'gui-query.gif',
    "zoom-last"  : 'gui-zoom_back.gif',
    "zoom-more"   : 'gui-mapzoom.gif',
    "zoom-extent" : wx.ART_ERROR, # FIXME
    "map-export"   : 'file-save.gif',
    "print"   : 'file-print.gif',
    "overlay-add"    : 'gui-overlay.gif',
    # digit
    ## add feature
    "point-create": 'new.point.gif',
    "line-create" : 'new.line.gif',
    "boundary-create": 'new.boundary.gif',
    "centroid-create": 'new.centroid.gif',
    "polygon-create": wx.ART_ERROR,
    ## vertex
    "vertex-create" : 'add.vertex.gif',
    "vertex-move" : 'move.vertex.gif',
    "vertex-delete" : 'rm.vertex.gif',
    "line-split" : 'split.line.gif',
    ## edit feature
    "line-edit" : 'edit.line.gif',
    "line-move" : 'move.line.gif',
    "line-delete" : 'delete.line.gif',
    ## cats
    "cats-copy" : 'copy.cats.gif',
    "cats-display" : 'display.cats.gif',
    ## attributes
    "attributes-display" : 'display.attributes.gif',
    ## general
    "undo" : wx.ART_ERROR, # FIXME
    "tools" : wx.ART_ERROR, # FIXME
    # layer manager
    "monitor-create" : 'gui-startmon.gif',
    "create"    : 'file-new.gif',
    "layer-open"   : 'file-new.gif', 
    "open"   : 'file-open.gif',
    "save"   : 'file-save.gif',
    "layer-import" : 'file-new.gif',
    "layer-raster-add"    : 'element-cell.gif',
    "layer-raster-more"   : 'module-d.shadedmap.gif',
    "layer-raster3d-add"  : 'element-grid3.gif',
    "layer-vector-add"    : 'element-vector.gif',
    "layer-vector-more"   : 'module-d.vect.thematic.gif',
    "layer-command-add"     : 'gui-cmd.gif',
    "layer-group-add"     : 'gui-group.gif',
    "layer-more"     : 'module-d.grid.gif',
    "layer-remove"     : 'edit-cut.gif',
    "table"  : 'db-values.gif',
    "edit"     :  wx.ART_ERROR,
    "layer-rgb-add"     : 'module-d.rgb.gif',
    "layer-his-add"     : 'channel-his.gif',
    "layer-shaded-relief-add"  : 'module-d.shadedmap.gif',
    "layer-aspect-arrow-add"  : 'module-d.rast.arrow.gif',
    "layer-cell-cats-add"    : 'module-d.rast.num.gif',
    "layer-vector-thematic-add": 'module-d.vect.thematic.gif',
    "layer-vector-chart-add"   : 'module-d.vect.chart.gif',
    "layer-grid-add"    : 'module-d.grid.gif',
    "options": 'module-d.geodesic.gif',
    "options"   : 'module-d.rhumbline.gif',
    "layer-label-add"  : 'module-d.labels.gif',
    "text-add"    : 'module-d.text.gif',
    "scalebar-add": 'module-d.barscale.gif',
    "legend-add"  : 'module-d.legend.gif',
    "quit"       : 'gui-exit.gif',
    "modeler-main"    : wx.ART_ERROR,
    # analyze raster
    "layer-raster-analyze"    : 'gui-rastanalyze.gif',
    "measure-length"    : 'gui-measure.gif',
    "font"       : 'gui-font.gif',
    "layer-raster-histogram"  : 'module-d.histogram.gif',
    "color"      : 'edit-color.gif',
    "options"  : 'gui-layeroptions.gif',
    # profile 
    "layer-raster-profile"    : 'gui-profile.gif',
    "layer-raster-profile"   : 'gui-profiledefine.gif',
    "show" : 'gui-display.gif',
    "profileopt" : 'gui-profileopt.gif',
    # georectify
    'gcp-remove'   : 'gui-gcperase.gif',
    'gcp-create'     : 'gui-gcpset.gif',
    'georectify'    : 'gui-georect.gif',
    'gcp-rms'     : 'gui-rms.gif',
    "gcp-save"    : 'file-save.gif', 
    "gcp-add"     : wx.ART_NEW, # FIXME
    "gcp-delete"  : wx.ART_DELETE, # FIXME
    "reload"  : 'gui-redraw.gif',
    # modeler
    "module-add" : wx.ART_ERROR,
    "data-add"   : wx.ART_ERROR,
    "relation-create"  : wx.ART_ERROR,
    "execute"       : wx.ART_ERROR,
    "check"  : wx.ART_ERROR,
    "image-export"      : wx.ART_ERROR,
    "python-export"     : wx.ART_ERROR,
    "options" : wx.ART_ERROR,
    "modeler-variables" : wx.ART_ERROR,
    # 3d view
    "3d-view"       : wx.ART_ERROR,
    "3d-raster"    : wx.ART_ERROR,
    "3d-vector"     : wx.ART_ERROR,
    "3d-volume"     : wx.ART_ERROR,
    "3d-light"      : wx.ART_ERROR,
    "3d-fringe"     : wx.ART_ERROR,
    # various
    "settings"       : 'edit-color.gif',
    "redraw"         : 'gui-display.gif',
    "help"           : wx.ART_ERROR,
    "ps-script"       : wx.ART_ERROR,
    "ps-export"       : wx.ART_ERROR,
    }
