#include "tree.h"

#define CHIL_MULT 10
#define WINR_MULT 1

rcr_find_val(node, map_col) struct Node *node ; int map_col ;
{
    register int left_val, rite_val ;

    if (node == NULL)
		return 0 ;

    switch(node->oper)
    {
    case LEAF_OPR:
	    return( node->cbuf[map_col] ) ;
	    break ;

    case NAM_OPR:
	    left_val = rcr_find_val(node->left, map_col) ;
	    node->cbuf[map_col] = left_val ;
	    return( node->cbuf[map_col] ) ;
	    break ;

    case OV1_OPR:
	    left_val = rcr_find_val(node->left, map_col) ;
	    if (left_val) node->cbuf[map_col] |= 0001 ;
	    return( node->cbuf[map_col] ) ;
	    break ;

    case OV2_OPR:
	    left_val = rcr_find_val(node->left, map_col) ;
	    if (left_val) node->cbuf[map_col] |= 0002 ;
	    return( node->cbuf[map_col] ) ;
	    break ;

    case OV3_OPR:
	    left_val = rcr_find_val(node->left, map_col) ;
	    if (left_val) node->cbuf[map_col] |= 0004 ;
	    return( node->cbuf[map_col] ) ;
	    break ;

    case OV4_OPR:
	    left_val = rcr_find_val(node->left, map_col) ;
	    if (left_val) node->cbuf[map_col] |= 0010 ;
	    return( node->cbuf[map_col] ) ;
	    break ;

    case COV_OPR:
	    left_val = rcr_find_val(node->left, map_col) ;
	    if (left_val) 
		    node->cbuf[map_col] = node->new_cat ;
	    return (node->cbuf[map_col]) ;
	    break ;

    case GRP_OPR:
	    left_val = rcr_find_val(node->left, map_col) ;

	    if (node->group.min <= left_val
	    &&  node->group.max >= left_val
	    &&  node->group.table[left_val-node->group.min])
	    {
		node->cbuf[map_col] = 1 ;
		return(1) ;
	    }
	    else
	    {
		node->cbuf[map_col] = 0 ;
		return(0) ;
	    }
	    break ;

    case AND_OPR:
	    if(node->ltot <= node->rtot)
	    {
		left_val = rcr_find_val(node->left, map_col) ;
		if(left_val == 0) return(0) ;
		rite_val = rcr_find_val(node->rite, map_col) ;
		if(rite_val == 0) return(0) ;
	    }
	    else
	    {
		rite_val = rcr_find_val(node->rite, map_col) ;
		if(rite_val == 0) return(0) ;
		left_val = rcr_find_val(node->left, map_col) ;
		if(left_val == 0) return(0) ;
	    }
	    return(1) ;
	    break ;

    case  OR_OPR:
	    if(node->ltot <= node->rtot)
	    {
		left_val = rcr_find_val(node->left, map_col) ;
		if(left_val != 0) return(1) ;
		rite_val = rcr_find_val(node->rite, map_col) ;
		if(rite_val != 0) return(1) ;
	    }
	    else
	    {
		rite_val = rcr_find_val(node->rite, map_col) ;
		if(rite_val != 0) return(1) ;
		left_val = rcr_find_val(node->left, map_col) ;
		if(left_val != 0) return(1) ;
	    }
	    return(0) ;
	    break ;
    }
}
/* ========================================================================= */
