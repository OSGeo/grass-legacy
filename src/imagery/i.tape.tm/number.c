#include "tape.h"
number (start,end)
{
    int n;

    n = 0;
    while (start <= end)
	n = n * 256 + tape.buf[start++];
    return (n);
}
