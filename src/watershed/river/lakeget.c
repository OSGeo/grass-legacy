#define EXTERN extern
#include "global.h"

CELL lakeget(row, col)

    int row, col;

{

    CELL ptr;

    segment_get(&lakeseg,&ptr,row,col);
    return ptr;

}
