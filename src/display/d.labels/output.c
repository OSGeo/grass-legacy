#include <stdio.h>
#include <string.h>
#include "gis.h"

int output (FILE *out, char *label, char *value)
{
    char tvalue[100];
    char tlabel[100];
    strcpy (tvalue, value);
    G_strip (tvalue);
    sprintf(tlabel,"%s:", label);
    fprintf (out,"%-20s %s\n", tlabel, tvalue);

    return 0;
}
