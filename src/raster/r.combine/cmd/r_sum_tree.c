#include "tree.h"

/* ========================================================================= */
/* rcr_sum_tree recursively totals the number of nodes below the             */
/* current node and assigns this total to the left and rite children         */
/* then returns the sum of the left and rite children left and rite          */

rcr_sum_tree(node) struct Node *node ;
{
    register int totl ;

    if (node == NULL)
	return (0) ;

    switch(node->oper)
    {
    case LEAF_OPR:
	    node->ltot = 0 ;
	    node->rtot = 0 ;
	    totl = 0 ;
	    break ;

    case NAM_OPR:
    case COV_OPR:
    case OV1_OPR:
    case OV2_OPR:
    case OV3_OPR:
    case OV4_OPR:
	    node->ltot = rcr_sum_tree(node->left) + 1 ;
	    node->rtot = 0 ;
	    totl       = node->ltot ;
	    break ;

    case GRP_OPR:
	    node->ltot = rcr_sum_tree(node->left) + 1 ;
	    node->rtot = 0 ;
	    totl       = node->ltot ;
	    break ;

    default:
	    node->ltot = rcr_sum_tree(node->left) + 1 ;
	    node->rtot = rcr_sum_tree(node->rite) + 1 ;
	    totl       = node->ltot + node->rtot ;
#ifdef DEBUG
	    fprintf(stderr,
		    "node->ltot = %3d,  node->rtot = %3d\n",
		     node->ltot,	node->rtot	 ) ;
#endif DEBUG
	    break ;
    }
    return(totl) ;
}
/* ========================================================================= */
