/* %W% %G% */

#include "list.h"

ask_remove(n,buf)
    char *buf;
{
    char *element;
    char *desc;
    char temp[3];

    element =list[n].element[0];
    desc = list[n].desc[0];
    while(1)
    {
	do{
	    printf ("\nEnter %s file(s) to be removed\n",desc);
	    printf ("Enter 'list' for a list of %s files\n", desc);
	    printf ("hit RETURN to cancel request\n");
	    printf ("> ");
	}
	while (!G_gets(buf));
	G_strip (buf);
	printf ("<%s>\n", buf);
	if (*buf == 0) return 0;
	if (strcmp (buf, "list") == 0)
	    G_list_element (element, desc, G_mapset(), (int(*)())0);
	else if (sscanf (buf, "list %1s", temp) == 1)
	    printf ("\n** illegal request **\n");
	else
	    return 1;
    }
}
