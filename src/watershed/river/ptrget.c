#define EXTERN extern
#include "global.h"

CELL ptrget(row, col)

    int row, col;

{

    CELL ptr;

    segment_get(&ptrseg,&ptr,row,col);
    return ptr;

}
