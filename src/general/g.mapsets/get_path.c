#include "externs.h"
#include "gis.h"
get_mapset_path ()
{
    static int first = 1;
    char buf[1024];
    char *b;
    char name[50];
    int n;

    printf("Hit RETURN to keep current list, or ");
    printf("enter a new list of mapsets\n");
    if (first)
    {
	printf("(enter the numbers before the mapset name, ");
	printf("or the names themselves)\n");
	first = 0;
    }

    nchoices = 0;

    printf ("\nnew list> ");
    if (!gets (b = buf)) goto same;
    if (sscanf (buf,"%s",name) != 1)
	    goto same;

    while (1)
    {
	if (sscanf (b, "%s", name) != 1)
	    return 1;

	for (n = 0; n < nmapsets; n++)
	    if (strcmp (name, mapset_name[n]) == 0)
	    {
		choice[nchoices++] = n;
		goto next;
	    }
	
	if (scan_int (name, &n))
	{
	    if (n > 0 && n <= nmapsets)
	    {
		choice[nchoices++] = n-1;
		goto next;
	    }
	}

	printf("\n<%s> not found\n\n", name);
	return -1;

next:
	while (*b == ' ' || *b == '\t')
		b++;
	while (*b && *b != ' ' && *b != '\t')
		b++;
    }
same:
    for (n = 0; b = G__mapset_name (n); n++)
    {
	int i;
	for (i = 0; i < nmapsets; i++)
	    if (strcmp (b, mapset_name[i]) == 0)
	    {
		choice[nchoices++] = i;
		break;
	    }
    }
    return 1;
}
