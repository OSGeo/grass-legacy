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

/* btree.c */
struct cost *insert(float, int, int);
struct cost *find(float, int, int);
struct cost *get_lowest(void);
int delete(struct cost *);
int check(char *, struct cost *);
/***************************************************************/
