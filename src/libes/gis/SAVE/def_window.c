/* %W% %G%
 **********************************************************************
 *
 *   G_def_window(window)
 *
 *   Screen oriented user interactive session for defining a window.
 *   Uses the visual_ask V_ask routines.  As such, programs including
 *   this must load the GRASS library lib_vask.a followed by -lcurses
 *   and -ltermlib.
 *
 *   parms:
 *      struct Cell_head *window   (window to be defined)
 *
 *   returns:
 *      the edited window
 **********************************************************************/
#include "gis.h"

G_def_window(window)
    struct Cell_head *window ;
{
    return G_edit_cellhd (window, 0);
}
