/*
 * File: RegionP.h
 *
 * Desc: Private interface for Region widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#ifndef _RegionR_h
#define _RegionR_h

#include "gis.h"
#include <InteractP.h>
#include <Region.h>

#ifndef XmREGION_BIT
#define XmREGION_BIT     (53)
#endif

/* New fields for the Region widget class record  */

typedef struct {
    caddr_t                         extension;  /* Pointer to extension
                                                 * record */
}                               RegionClassPart;


/* Full class record declaration */

typedef struct _RegionClassRec {
    CoreClassPart                   core_class;
    CompositeClassPart              composite_class;
    ConstraintClassPart             constraint_class;
    XmManagerClassPart              manager_class;
    XmBulletinBoardClassPart        bulletin_board_class;
    InteractorClassPart             interactor_class;
    RegionClassPart                 region_class;
}                               RegionClassRec;

externalref RegionClassRec      regionClassRec;


/* New fields for the Region widget record */

typedef struct {
    /* display area */
    Widget                          display_area;
    /* Region Editor stuff */
    Boolean		    	    edit_default_region;
    Widget                          buttons_separator;
    Widget                          save_frame;
    Widget                          save_container;
    Widget                          buttons_container;

    Widget                          data_table;

    Widget                          north_label;
    Widget                          south_label;
    Widget                          west_label;
    Widget                          east_label;
    Widget                          nsres_label;
    Widget                          ewres_label;

    Widget                          north_default;
    Widget                          south_default;
    Widget                          west_default;
    Widget                          east_default;
    Widget                          nsres_default;
    Widget                          ewres_default;

    Widget                          north_text;
    Widget                          south_text;
    Widget                          west_text;
    Widget                          east_text;
    Widget                          nsres_text;
    Widget                          ewres_text;

    Widget                          graphic_frame;
    Pixmap                          graphic_pixmap;
    Widget                          graphic_drawing_area;
    GC                              graphic_gc;
    GC                              fill_gc;
    GC                              rubber_gc;
    GC                              default_gc;
    int                             old_x1, old_x2, old_y1, old_y2;
    int                             rubber_x1, rubber_x2, rubber_y1, rubber_y2;
    Dimension                       graphic_height;
    Dimension                       graphic_width;
    Dimension                       total_height;
    Dimension                       total_width;
    Dimension                       graphic_x;
    Dimension                       graphic_y;



    struct Cell_head                default_region;
    struct Cell_head               *result_region;
    struct Cell_head               *input_region;
    XmString                        color_name;


    XColor                          grid_color;
    GC                              grid_gc;
    int                             grid_mode;

    Widget                          button_gridcolor;
    Widget                          button_saved;
    Widget                          button_save;
    Widget			    save_name;
    Widget                          button_default;
    Widget                          button_grid;
    Widget                          button_snap;

    Widget                          data_frame;

    struct Cell_head                grid_region;
    Boolean                         gridinfo_active;
    Widget                          gridinfo_frame;
    Widget                          gridinfo_container;
    Widget                          gridinfo_top_part;
    Widget                          gridinfo_buttons;
    Widget                          gridinfo_toggles;
    Widget                          gridinfo_fields;
    Widget                          gridinfo_fields_table;
    Widget                          gridinfo_vert_sep;
    Widget                          gridinfo_horiz_sep;
    Widget                          gridinfo_origin_label;
    Widget                          gridinfo_toggle_coord;
    Widget                          gridinfo_toggle_raster;
    Widget                          gridinfo_toggle_current;
    Widget                          gridinfo_toggle_default;
    Widget                          gridinfo_fields_label;
    Widget                          gridinfo_nsres_label;
    Widget                          gridinfo_nsres_text;
    Widget                          gridinfo_ewres_label;
    Widget                          gridinfo_ewres_text;
    double                          gridinfo_gap;
    Widget                          gridinfo_gap_text;
    Widget                          gridinfo_gap_label;
}                               RegionPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _RegionRec {
    CorePart                        core;
    CompositePart                   composite;
    ConstraintPart                  constraint;
    XmManagerPart                   manager;
    XmBulletinBoardPart             bulletin_board;
    InteractorPart                  interactor;
    RegionPart                      region;
}                               RegionRec;


#define XgREGION_DISPLAY_AREA   0

/* Access macros */

#define R_DisplayArea( w) (((RegionWidget) (w))->region.display_area)
#define R_Colormap( w) (((RegionWidget) (w))->core.colormap)

#define R_ButtonsSeparator( w) (((RegionWidget) (w))->region.buttons_separator)
#define R_SaveFrame( w) (((RegionWidget) (w))->region.save_frame)
#define R_SaveContainer( w) (((RegionWidget) (w))->region.save_container)
#define R_ButtonsContainer( w) (((RegionWidget) (w))->region.buttons_container)
#define R_DataFrame( w) (((RegionWidget) (w))->region.data_frame)
#define R_DataTable( w) (((RegionWidget) (w))->region.data_table)
#define R_GridInfoActive( w) (((RegionWidget) (w))->region.gridinfo_active)
#define R_GridInfoFrame( w) (((RegionWidget) (w))->region.gridinfo_frame)
#define R_GridInfoContainer( w) (((RegionWidget) (w))->region.gridinfo_container)
#define R_GridInfoTopPart( w) (((RegionWidget) (w))->region.gridinfo_top_part)
#define R_GridInfoButtons( w) (((RegionWidget) (w))->region.gridinfo_buttons)
#define R_GridInfoToggles( w) (((RegionWidget) (w))->region.gridinfo_toggles)
#define R_GridInfoFields( w) (((RegionWidget) (w))->region.gridinfo_fields)
#define R_GridInfoFieldsTable( w) (((RegionWidget) (w))->region.gridinfo_fields_table)
#define R_GridInfoVertSep( w) (((RegionWidget) (w))->region.gridinfo_vert_sep)
#define R_GridInfoHorizSep( w) (((RegionWidget) (w))->region.gridinfo_horiz_sep)
#define R_GridInfoOriginLabel( w) (((RegionWidget) (w))->region.gridinfo_origin_label)
#define R_GridInfoToggleCoord( w) (((RegionWidget) (w))->region.gridinfo_toggle_coord)
#define R_GridInfoToggleRaster( w) (((RegionWidget) (w))->region.gridinfo_toggle_raster)
#define R_GridInfoToggleCurrent( w) (((RegionWidget) (w))->region.gridinfo_toggle_current)
#define R_GridInfoToggleDefault( w) (((RegionWidget) (w))->region.gridinfo_toggle_default)
#define R_GridInfoFieldsLabel( w) (((RegionWidget) (w))->region.gridinfo_fields_label)
#define R_GridInfoNSResLabel( w) (((RegionWidget) (w))->region.gridinfo_nsres_label)
#define R_GridInfoNSResText( w) (((RegionWidget) (w))->region.gridinfo_nsres_text)
#define R_GridInfoEWResLabel( w) (((RegionWidget) (w))->region.gridinfo_ewres_label)
#define R_GridInfoEWResText( w) (((RegionWidget) (w))->region.gridinfo_ewres_text)
#define R_GridInfoGap( w) (((RegionWidget) (w))->region.gridinfo_gap)
#define R_GridInfoGapLabel( w) (((RegionWidget) (w))->region.gridinfo_gap_label)
#define R_GridInfoGapText( w) (((RegionWidget) (w))->region.gridinfo_gap_text)

#define R_GridColor( w) (((RegionWidget) (w))->region.grid_color)
#define R_GridMode( w) (((RegionWidget) (w))->region.grid_mode)

#define R_NorthLabel( w) (((RegionWidget) (w))->region.north_label)
#define R_SouthLabel( w) (((RegionWidget) (w))->region.south_label)
#define R_WestLabel( w) (((RegionWidget) (w))->region.west_label)
#define R_EastLabel( w) (((RegionWidget) (w))->region.east_label)
#define R_NSResLabel( w) (((RegionWidget) (w))->region.nsres_label)
#define R_EWResLabel( w) (((RegionWidget) (w))->region.ewres_label)

#define R_NorthDefault( w) (((RegionWidget) (w))->region.north_default)
#define R_SouthDefault( w) (((RegionWidget) (w))->region.south_default)
#define R_WestDefault( w) (((RegionWidget) (w))->region.west_default)
#define R_EastDefault( w) (((RegionWidget) (w))->region.east_default)
#define R_NSResDefault( w) (((RegionWidget) (w))->region.nsres_default)
#define R_EWResDefault( w) (((RegionWidget) (w))->region.ewres_default)

#define R_NorthText( w) (((RegionWidget) (w))->region.north_text)
#define R_SouthText( w) (((RegionWidget) (w))->region.south_text)
#define R_WestText( w) (((RegionWidget) (w))->region.west_text)
#define R_EastText( w) (((RegionWidget) (w))->region.east_text)
#define R_NSResText( w) (((RegionWidget) (w))->region.nsres_text)
#define R_EWResText( w) (((RegionWidget) (w))->region.ewres_text)

#define R_GraphicFrame( w) (((RegionWidget) (w))->region.graphic_frame)
#define R_GraphicPixmap( w) (((RegionWidget) (w))->region.graphic_pixmap)
#define R_GraphicDrawingArea( w) (((RegionWidget) (w))->region.graphic_drawing_area)
#define R_TotalHeight( w) (((RegionWidget) (w))->region.total_height)
#define R_TotalWidth( w) (((RegionWidget) (w))->region.total_width)
#define R_GraphicHeight( w) (((RegionWidget) (w))->region.graphic_height)
#define R_GraphicWidth( w) (((RegionWidget) (w))->region.graphic_width)
#define R_GraphicX( w) (((RegionWidget) (w))->region.graphic_x)
#define R_GraphicY( w) (((RegionWidget) (w))->region.graphic_y)
#define R_FillGC( w) (((RegionWidget) (w))->region.fill_gc)
#define R_RubberGC( w) (((RegionWidget) (w))->region.rubber_gc)
#define R_DefaultGC( w) (((RegionWidget) (w))->region.default_gc)
#define R_GridGC( w) (((RegionWidget) (w))->region.grid_gc)

#define R_OldX1( w) (((RegionWidget) (w))->region.old_x1)
#define R_OldX2( w) (((RegionWidget) (w))->region.old_x2)
#define R_OldY1( w) (((RegionWidget) (w))->region.old_y1)
#define R_OldY2( w) (((RegionWidget) (w))->region.old_y2)
#define R_RubberX1( w) (((RegionWidget) (w))->region.rubber_x1)
#define R_RubberX2( w) (((RegionWidget) (w))->region.rubber_x2)
#define R_RubberY1( w) (((RegionWidget) (w))->region.rubber_y1)
#define R_RubberY2( w) (((RegionWidget) (w))->region.rubber_y2)

#define R_DefaultRegion( w) (((RegionWidget) (w))->region.default_region)
#define R_GridRegion( w) (((RegionWidget) (w))->region.grid_region)
#define R_ResultRegion( w) (((RegionWidget) (w))->region.result_region)
#define R_InputRegion( w) (((RegionWidget) (w))->region.input_region)


#define R_GridColorButton( w) (((RegionWidget) (w))->region.button_gridcolor)
#define R_GridButton( w) (((RegionWidget) (w))->region.button_grid)
#define R_SnapButton( w) (((RegionWidget) (w))->region.button_snap)
#define R_SavedButton( w) (((RegionWidget) (w))->region.button_saved)
#define R_SaveButton( w) (((RegionWidget) (w))->region.button_save)
#define R_SaveName( w) (((RegionWidget) (w))->region.save_name)
#define R_DefaultButton( w) (((RegionWidget) (w))->region.button_default)

#define R_EditDefaultRegion( w) (((RegionWidget) (w))->region.edit_default_region)

#define R_ColorName( w) (((RegionWidget) (w))->region.color_name)


#endif                          /* _RegionR_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
