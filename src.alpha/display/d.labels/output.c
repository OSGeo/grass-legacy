#include <stdio.h>

output (out, label, value)
    FILE *out;
    char *label, *value;
{
    char tvalue[100];
    char tlabel[100];
    strcpy (tvalue, value);
    G_strip (tvalue);
    sprintf(tlabel,"%s:", label);
    fprintf (out,"%-20s %s\n", tlabel, tvalue);
}
