#include <stdio.h>

/*
 * error: print error message and return 0
 *     used only within this library.
 */

FILE *Bugs2;

int m_error (char *s)
{
    fprintf (stderr, "%s library error routine!", s);
    fclose (Bugs2);
    return 0;
}
