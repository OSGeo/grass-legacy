/****************************************************************/
/*	dlg_wr_node.c	in	~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"				  	*/		
/*								*/
/*	_dlg_write_node (node,fd)				*/
/*      FILE *fd ;						*/
/*      struct dlg node *node;					*/
/*								*/
/****************************************************************/

#include "gis.h"
#include "dlg.h"
#define	 PUT(x,y) 	fwrite(x,y,1,fd)


_dlg_write_node (node, fd)
        struct dlg_node *node ;
        FILE *fd ;
{
        PUT(&node->x,      sizeof(node->x));
        PUT(&node->y,      sizeof(node->y)) ;
        PUT(&node->n_lines,sizeof(node->n_lines)) ;
        PUT(&node->n_atts, sizeof(node->n_atts)) ;

        if (node->n_lines)
        {
	fwrite((char *)node->lines,sizeof(int),node->n_lines,fd);
        }

        if (node->n_atts)
        {
	fwrite((char *)node->atts,sizeof(int),2*node->n_atts,fd); 
        }

}

/************** END OF FUNCTION "_DLG_WRITE_NODE" ***************/

