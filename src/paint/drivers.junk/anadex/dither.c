#include "P.h"

dither (col, cyan, yellow, majenta)
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
    switch (majenta)
    {
    case 4:  MAJENTA[1][col] |= (2 << shift);	/*FALLTHROUGH*/
    case 3:  MAJENTA[1][col] |= (1 << shift);
    case 2:  MAJENTA[0][col] |= (2 << shift);
    case 1:  MAJENTA[0][col] |= (1 << shift);
    }
}
