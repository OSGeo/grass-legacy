#define EXTERN extern
#include "global.h"

CELL cumget(row, col)

    int row, col;

{

    CELL cum;

    segment_get(&cumseg,&cum,row,col);
    return cum;

}
