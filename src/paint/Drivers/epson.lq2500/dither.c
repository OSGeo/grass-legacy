#include "P.h"

dither (col, cyan, yellow, magenta)
{
    int shift;
    int group;

    shift = 6 - (ras_row%4)*2;
    group = ras_row/4;

    switch (cyan)
    {
    case 4:  CYAN[1][group][col] |= 2 << shift; /*FALLTHROUGH*/
    case 3:  CYAN[1][group][col] |= 1 << shift;
    case 2:  CYAN[0][group][col] |= 2 << shift;
    case 1:  CYAN[0][group][col] |= 1 << shift;
    }

    switch (yellow)
    {
    case 4:  YELLOW[1][group][col] |= 2 << shift; /*FALLTHROUGH*/
    case 3:  YELLOW[1][group][col] |= 1 << shift;
    case 2:  YELLOW[0][group][col] |= 2 << shift;
    case 1:  YELLOW[0][group][col] |= 1 << shift;
    }

    switch (magenta)
    {
    case 4:  MAGENTA[1][group][col] |= 2 << shift; /*FALLTHROUGH*/
    case 3:  MAGENTA[1][group][col] |= 1 << shift;
    case 2:  MAGENTA[0][group][col] |= 2 << shift;
    case 1:  MAGENTA[0][group][col] |= 1 << shift;
    }
}
