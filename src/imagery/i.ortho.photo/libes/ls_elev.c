/*************************************************************
* I_list_elev (full)
*************************************************************/
#include "imagery.h"

static char *tempfile = NULL;

I_list_elev (full)
{
    char *element;
    int i;

    char buf[1024];
    char title[50];
    FILE *ls, *temp, *popen();
    struct Ref ref;
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
    sprintf (buf, "more -d %s", tempfile);
    system(buf);
    unlink (tempfile);
    printf ("hit RETURN to continue -->");
    G_gets(buf);

/******/
    G_list_element ("cell", "cell",  G_mapset(), NULL);

}








