#define EXTERN extern
#include "global.h"

char wkget(row, col)

    int row, col;

{

    char wk;

    segment_get(&wkseg,&wk,row,col);
    return wk;

}
