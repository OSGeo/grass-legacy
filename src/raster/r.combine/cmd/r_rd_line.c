#include "lvw.h"
#include "tree.h"

/* =========================================================================
 * rcr_rd_line - recursively reads active data into leaf buffers
 */

rcr_rd_line(node, win_row) struct Node *node ; int win_row ;
{
    if (node == NULL)
	    return ;
    switch(node->oper)
    {
    case LEAF_OPR:
	    G_get_map_row(node->cellfd, node->cbuf, win_row) ;
	    break ;

    case NAM_OPR:
	    /* zero the map line buffer */
	    G_zero_cell_buf(node->cbuf) ;

	    /* name operator is UNARY!! */
	    rcr_rd_line(node->left, win_row) ;
	    break ;

    case COV_OPR:
	    G_get_map_row(node->cellfd, node->cbuf, win_row) ;
	    /* over operator is UNARY!! */
	    rcr_rd_line(node->left, win_row) ;
	    break ;

    case OV1_OPR:
    case OV2_OPR:
    case OV3_OPR:
    case OV4_OPR:
	    if ( *(node->name) )
		G_get_map_row(node->cellfd, node->cbuf, win_row) ;
	    else
		G_zero_cell_buf(node->cbuf) ;

	    /* over operator is UNARY!! */
	    rcr_rd_line(node->left, win_row) ;
	    break ;

    case GRP_OPR:
	    /* zero the map line buffer */
	    G_zero_cell_buf(node->cbuf) ;

	    /* group operator is UNARY!! */
	    rcr_rd_line(node->left, win_row) ;
	    break ;

    default:	/* binary operator */
	    /* zero the map line buffer */
	    G_zero_cell_buf(node->cbuf) ;

	    rcr_rd_line(node->left, win_row) ;
	    rcr_rd_line(node->rite, win_row) ;
	    break ;
    }
    return ;
}
