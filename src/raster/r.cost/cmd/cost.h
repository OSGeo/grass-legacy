/***************************************************************/
/*                                                             */
/*        cost.h    in   ~/src/Gcost                           */  
/*                                                             */
/*      This header file defines the data structure of a       */  
/*      point structure containing various attributes of       */
/*      a grid cell.                                           */
/*                                                             */
/***************************************************************/

struct cost{
    float  min_cost;
    int row;
    int col;
    struct cost *lower;
    struct cost *higher;
    struct cost *above;
};

/***************************************************************/
