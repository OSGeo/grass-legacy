"""
Silk icon set, v1.3
http://www.famfamfam.com/lab/icons/silk/

"""
__author__ = "Mark James"
__version__ = "1.3"

import os

import wx

import globalvar

iconPath = os.path.join(globalvar.ETCDIR, "gui", "icons", "silk")

iconSet = {
    "show" : 'map_go.png',
    "layer-redraw"  : 'arrow_refresh.png',
    "erase"      : 'cross.png',
    "pointer"    : 'cursor.png',
    "info"      : 'information.png',
    "map-export"   : 'picture_save.png',
    "print"   : 'printer.png',
    "pan"        : 'arrow_out.png', 
    "zoom-in"    : 'zoom_in.png',
    "zoom-out"   : 'zoom_out.png',
    "zoom-last"  : 'zoom_back.png',
    "zoom-more"   : 'zoom.png',
    "zoom-extent" : 'zoom_extent.png',
    "layer-raster-analyze"    : 'application_lightning.png',
    "measure-length"    : 'sum.png',
    "layer-raster-profile"    : 'wand.png',
    "layer-raster-histogram"  : 'chart_bar.png',
    "font"       : 'font.png',
    "overlay-add"    : 'overlays.png',
    "text-add"    : 'textfield_add.png',
    "scalebar-add": 'page_white_picture.png',
    "legend-add"  : 'page_green.png',
    "quit"       : 'door_in.png',
    "point-create": 'bullet_add.png',
    "line-create" : 'vector_add.png',
    "boundary-create": 'shape_handles.png',
    "centroid-create": 'shape_square_add.png',
    "vertex-create" : 'chart_line_add.png',
    "vertex-move" : 'chart_line.png',
    "vertex-delete" : 'chart_line_delete.png',
    "line-split" : 'chart_line_link.png',
    "line-edit" : 'chart_line_edit.png',
    "line-move" : 'bullet_go.png',
    "line-delete" : 'vector_delete.png',
    "cats-display" : 'chart_organisation.png',
    "cats-copy" : 'chart_organisation_add.png',
    "attributes-display" : 'table.png',
    "undo" : 'arrow_undo.png',
    "tools" : 'plugin.png',
    "monitor-create" : 'application_add.png',
    "create"    : 'page_white.png',
    "layer-open"   : 'page_white_get.png',
    "open"   : 'folder.png',
    "save"   : 'page_save.png',
    "layer-import" : 'page_white_get.png',
    "layer-raster-add"    : 'image_add.png',
    "layer-raster-more"   : 'picture_empty.png',
    "layer-raster3d-add"  : 'bricks.png',
    "layer-shaded-relief-add"  : 'picture_empty.png',
    "layer-aspect-arrow-add"  : 'arrow_inout.png',
    "layer-cell-cats-add"    : 'color_swatch.png',
    "layer-vector-add"    : 'map_add.png',
    "layer-vector-more"   : 'thematic.png',
    "layer-command-add"     : 'cog_add.png',
    "layer-group-add"     : 'folder_add.png',
    "layer-more"     : 'images.png',
    "layer-grid-add"    : 'application_view_icons.png',
    "layer-label-add"  : 'tag_blue_add.png',
    "layer-remove"     : 'bin_closed.png',
    "table"  : 'application_view_columns.png',
    "layer-rgb-add"     : 'rgb.png',
    "layer-his-add"     : 'his.png',
    "layer-vector-thematic-add": 'thematic.png',
    "layer-vector-chart-add"   : 'chart_bar.png',
    "options"  : 'map_edit.png',
    "layer-raster-profile"   : 'image_edit.png',
    "layer-raster-show"  : 'arrow_refresh.png',
    "layer-raster-profileopt"   : 'color_swatch.png',
    "gcp-create"     : 'bullet_add.png',
    'gcp-remove'   : 'cross.png',
    'georectify'    : 'application_lightning.png',
    'gcp-rms'     : 'error.png',
    "gcp-save"    : 'picture_save.png',
    "gcp-add"     : 'bullet_add.png', 
    "gcp-delete"  : 'bullet_delete.png',
    "reload"  : 'arrow_refresh.png',
    "settings"       : 'color_swatch.png',
    "redraw"         : 'arrow_refresh.png',
    }
