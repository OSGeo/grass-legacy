#include "P.h"

dither (col, cyan, yellow, magenta)
{
    if (quality == 3)
	dither144 (col, cyan, yellow, magenta);
    else
	dither72 (col, cyan, yellow, magenta);
}

dither72 (col, cyan, yellow, magenta)
{
    int shift;

    shift = ras_row + ras_row;

    switch (cyan)
    {
    case 4:  CYAN[1][col] |= (2 << shift);	/*FALLTHROUGH*/
    case 3:  CYAN[1][col] |= (1 << shift);
    case 2:  CYAN[0][col] |= (2 << shift);
    case 1:  CYAN[0][col] |= (1 << shift);
    }
    switch (yellow)
    {
    case 4:  YELLOW[1][col] |= (2 << shift);	/*FALLTHROUGH*/
    case 3:  YELLOW[1][col] |= (1 << shift);
    case 2:  YELLOW[0][col] |= (2 << shift);
    case 1:  YELLOW[0][col] |= (1 << shift);
    }
    switch (magenta)
    {
    case 4:  MAGENTA[1][col] |= (2 << shift);	/*FALLTHROUGH*/
    case 3:  MAGENTA[1][col] |= (1 << shift);
    case 2:  MAGENTA[0][col] |= (2 << shift);
    case 1:  MAGENTA[0][col] |= (1 << shift);
    }
}

dither144 (col, cyan, yellow, magenta)
{
    unsigned char mask;

    mask = 1 << ras_row ;

    switch (cyan)
    {
    case 4:  CYAN[3][col] |= mask;	/*FALLTHROUGH*/
    case 3:  CYAN[2][col] |= mask;
    case 2:  CYAN[1][col] |= mask;
    case 1:  CYAN[0][col] |= mask;
    }

    switch (yellow)
    {
    case 4:  YELLOW[3][col] |= mask;	/*FALLTHROUGH*/
    case 3:  YELLOW[2][col] |= mask;
    case 2:  YELLOW[1][col] |= mask;
    case 1:  YELLOW[0][col] |= mask;
    }

    switch (magenta)
    {
    case 4:  MAGENTA[3][col] |= mask;	/*FALLTHROUGH*/
    case 3:  MAGENTA[2][col] |= mask;
    case 2:  MAGENTA[1][col] |= mask;
    case 1:  MAGENTA[0][col] |= mask;
    }
}
