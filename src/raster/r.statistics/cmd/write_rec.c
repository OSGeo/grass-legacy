#include <stdio.h>

write_reclass (fd, cat1, cat2, label, usecats)
FILE *fd;
long cat1, cat2;
char *label;
int usecats;
{
    if (cat1 == 0)
    {
	cat2 = 0;
	label = "no data";
    }

    fprintf (fd, "%ld = %ld ", cat1, cat1);
    if (*label && usecats)
	fprintf (fd, "%s\n", label);
    else
	fprintf (fd, "%ld\n", cat2);

}
