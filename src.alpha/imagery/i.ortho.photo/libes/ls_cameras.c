/*************************************************************
* I_list_cameras (full)
*************************************************************/
#include "imagery.h"

static char *tempfile = NULL;

I_list_cameras (full)
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

    element = "camera";
    G__make_mapset_element (element);

    temp = fopen (tempfile, "w");
    if (temp == NULL)
	G_fatal_error ("can't open any temp files");
    fprintf (temp, "Available cameras\n");
    fprintf (temp, "---------------------------------\n");

    any = 0;
    strcpy (buf, "cd ");
    G__file_name (buf+strlen(buf), element, "", G_mapset());
    strcat (buf, ";ls");
    if (!full) strcat (buf, " -C");
    if(ls = popen (buf, "r"))
    {
	while (G_getl(buf, sizeof buf, ls))
	{
	    any=1;
	    fprintf (temp, "%s", buf);
	    if (full)
	    {
		I_get_cam_title (buf, title, sizeof title);
		if (*title)
		    fprintf (temp, " (%s)", title);
		fprintf (temp, "\n");
	    }
	    else 
		fprintf (temp, "\n");
	}
	pclose (ls);
    }
    if (!any)
	fprintf (temp, "no camera files available\n");
    fprintf (temp, "---------------------------------\n");
    fclose (temp);
    sprintf (buf, "more -d %s", tempfile);
    system(buf);
    unlink (tempfile);
    printf ("hit RETURN to continue -->");
    G_gets(buf);
}

