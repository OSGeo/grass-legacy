#include "tape.h"
int 
number (int start, int end)
{
    int n;

    n = 0;
    while (start <= end)
	n = n * 256 + tape.buf[start++];
    return (n);
}
