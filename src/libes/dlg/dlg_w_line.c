/****************************************************************/
/*								*/
/*	dlg_wr_line.c	in	~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"					*/
/*								*/
/*	_dlg_write_line (line,fd)				*/
/*      FILE *fd ;						*/
/*      struct dlg_line *line;					*/
/*								*/
/****************************************************************/

#include "gis.h"
#include "dlg.h"
#define  PUT(x,y)       fwrite(x,y,1,fd)


_dlg_write_line(line, fd)
        struct dlg_line *line ;
        FILE *fd ;
{
        PUT (&line->start_node,sizeof(line->start_node));
        PUT (&line->end_node,  sizeof(line->end_node));
        PUT (&line->left_area, sizeof(line->left_area));
        PUT (&line->right_area,sizeof(line->right_area));
        PUT (&line->n_coors,   sizeof(line->n_coors));
        PUT (&line->n_atts,    sizeof(line->n_atts));
        PUT (&line->N,         sizeof(line->N));
        PUT (&line->S,         sizeof(line->S));
        PUT (&line->E,         sizeof(line->E));
        PUT (&line->W,         sizeof(line->W));

        if (line->n_coors)
        {
 fwrite((char *)line->coors,sizeof(double),2*line->n_coors,fd);
        }

        if (line->n_atts)
        {
 fwrite((char *)line->atts,sizeof(int),2*line->n_atts,fd);
        }
}

/************* END OF FUNCTION "_DLG_WRITE_LINE" ****************/

