#define EXTERN extern
#include "global.h"

char slopeget(row, col)

    int row, col;

{

    char slope;

    segment_get(&slopeseg,&slope,row,col);
    return slope;

}
