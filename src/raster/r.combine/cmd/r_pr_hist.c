#include "tree.h"
#define OPEN    i=depth; while(i--) printf("    "); printf("(") 
#define CLOSE   i=depth; while(i--) printf("    "); printf(")\n"); check_lines()

static int lines_out ;
start_line_count()
{
    lines_out = 0 ;
}

check_lines()
{
    char buff[64] ;
    if (lines_out++ < 19)
        return(0) ;
    printf("\nHit RETURN for more   ") ;
    gets(buff) ;
    printf("\n") ;
    lines_out = 1 ;
}


rcr_pr_hist(node, depth) struct Node *node ;
{
    int i ;
    int first ;
    int min, max;

    if (node == NULL)
        return ;
    OPEN ;
    switch(node->oper)
    {
    case LEAF_OPR:
        printf("%s)\n", node->name) ;
        check_lines() ;
        break ;

    case NAM_OPR:
        printf("NAME %s\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case COV_OPR:
        printf("COVER %s\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV1_OPR:
        printf("OVER %s red\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV2_OPR:
        printf("OVER %s yellow\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV3_OPR:
        printf("OVER %s blue\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case OV4_OPR:
        printf("OVER %s gray\n", node->name) ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case GRP_OPR:
        check_lines() ;
        printf("GROUP") ;
        min = node->group.min;
        max = node->group.max;
        first = min-1 ;
        for(i=min;i<=max;i++)
        {
        if(node->group.table[i-min])
        {
            if (first < min)
            first = i ;
        }
        else
        {
            if(first >= min)
            {
            if (first == i-1)
                printf(" %d", first) ;
            else
                printf(" %d-%d", first, i-1) ;
            }
            first = min-1 ;
        }
        }
        if(first >= min)
        {
        if (first == i-1)
            printf(" %d", first) ;
        else
            printf(" %d-%d", first, i-1) ;
        }
        printf("\n") ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        break ;

    case AND_OPR:
        printf("AND\n") ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        rcr_pr_hist(node->rite, depth+1) ;
        break ;

    case  OR_OPR:
        printf("OR\n") ;
        check_lines() ;
        rcr_pr_hist(node->left, depth+1) ;
        rcr_pr_hist(node->rite, depth+1) ;
        break ;
    
    default:
        break ;
    }
    if(node->oper != LEAF_OPR)
    {
    CLOSE ;
    }
    /* everything went okay if it got this far */
}

/* ========================================================================= */
