/****************************************************************/
/*								*/
/*	dlg_wr_area.c 	in	~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"					*/
/*								*/
/*	_dlg_write_area (area,fd)				*/
/*      FILE *fd ;						*/
/*      struct dlg area *area;					*/
/*								*/
/****************************************************************/

#include "gis.h"
#include "dlg.h"
#define        PUT(x,y)       fwrite(x,y,1,fd)

_dlg_write_area(area, fd)
      struct dlg_area *area ;
      FILE *fd ;
{
      PUT(&area->x,      sizeof(area->x));
      PUT(&area->y,      sizeof(area->y));
      PUT(&area->n_lines,sizeof(area->n_lines));
      PUT(&area->n_atts, sizeof(area->n_atts));
      PUT(&area->n_isles,sizeof(area->n_isles));

      if (area->n_lines)
      {
      fwrite((char *)area->lines,sizeof(int),area->n_lines,fd);
      }

      if (area->n_atts)
      {
      fwrite((char *)area->atts,sizeof(int),2*area->n_atts,fd); 
      }
}

/************ END OF FUNCTION "_DLG_WRITE_AREA" ******************/

