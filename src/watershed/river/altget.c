#define EXTERN extern
#include "global.h"

CELL altget(row, col)

    int row, col;

{

    CELL alt;

    segment_get(&altseg,&alt,row,col);
    return alt;

}
