/*
 **********************************************************************
 *
 *   G_def_window(window)
 *
 *   Screen oriented user interactive session for defining a window.
 *   Uses the visual_ask V_ask routines.  As such, programs including
 *   this must load the GRASS library $(VASK), which includes -lcurses
 *   and -ltermlib.
 *
 *   parms:
 *      struct Cell_head *window   (window to be defined)
 *
 *   returns:
 *      -1 user canceled the edit
 *       0 user didn't accept the values after VASK. Call me again.
 *       1 user accept values, window is good.
 **********************************************************************/
#include "gis.h"

G_def_window(window)
    struct Cell_head *window ;
{
    return G_edit_cellhd (window, 0);
}
