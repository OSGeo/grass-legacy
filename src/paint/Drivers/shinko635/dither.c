#include "P.h"

dither (col, cyan, yellow, magenta)
{
    register int hi, lo;
    register int byte;
    register int shift;

    shift = /*6 - */ ((col&3)<<1);
    lo = 1 << shift;
    hi = 2 << shift;
    byte = col >> 2;

    switch (cyan)
    {
    case 4:  CYAN[1][byte] |= hi; /*FALLTHROUGH*/
    case 3:  CYAN[1][byte] |= lo;
    case 2:  CYAN[0][byte] |= hi;
    case 1:  CYAN[0][byte] |= lo;
    }

    switch (yellow)
    {
    case 4:  YELLOW[1][byte] |= hi; /*FALLTHROUGH*/
    case 3:  YELLOW[1][byte] |= lo;
    case 2:  YELLOW[0][byte] |= hi;
    case 1:  YELLOW[0][byte] |= lo;
    }

    switch (magenta)
    {
    case 4:  MAGENTA[1][byte] |= hi; /*FALLTHROUGH*/
    case 3:  MAGENTA[1][byte] |= lo;
    case 2:  MAGENTA[0][byte] |= hi;
    case 1:  MAGENTA[0][byte] |= lo;
    }
}
