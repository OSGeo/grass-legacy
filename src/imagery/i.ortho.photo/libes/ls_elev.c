/*************************************************************
* I_list_elev (full)
*************************************************************/
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "imagery.h"

static char *tempfile = NULL;

int I_list_elev (int full)
{
    char *element;
    char buf[1024];
    FILE *ls, *temp;
    int any;

    if (tempfile == NULL)
	tempfile = G_tempfile();

    element = "cell";
    G__make_mapset_element (element);

    temp = fopen (tempfile, "w");
    if (temp == NULL)
	G_fatal_error ("can't open any temp files");
    fprintf (temp, "Available raster files:\n");
    fprintf (temp, "---------------------------------\n");

    any = 0;
    strcpy (buf, "cd ");
    G__file_name (buf+strlen(buf), element, " ", " " );
    strcat (buf, ";ls");
    strcat (buf, " -C");
    if(ls = popen (buf, "r"))
    {
	while (G_getl(buf, sizeof buf, ls))
	{
	    any=1;
	    fprintf (temp, "%s", buf);
	    fprintf (temp, "\n");
	}
	pclose (ls);
    }
    if (!any)
	fprintf (temp, "no raster files available\n");
    fprintf (temp, "---------------------------------\n");
    fclose (temp);
    sprintf (buf, "$PAGER %s", tempfile);
    system(buf);
    unlink (tempfile);
    fprintf (stderr, "hit RETURN to continue -->");
    G_gets(buf);

/******/
    G_list_element ("cell", "cell",  G_mapset(), NULL);


    return 0;
}
