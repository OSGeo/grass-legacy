#include "gis.h"
#include "parms.h"

static char *help[] =
{
    "enter comments, line by line",
    ""
};
commentfile(name)
    char *name;
{
    char buf[1024];
    FILE *in, *out;
    int need_blank;

    in = NULL;
    if (*name)
    {
	in = fopen (name, "r");
	if (in == NULL)
	{
	    error ("comment file",name,"can't open");
	    return;
	}
    }
    if (parms.commentfile == NULL)
    {
	parms.commentfile = G_tempfile();
	need_blank = 0;
	if ((out = fopen (parms.commentfile,"w")) != NULL)
	    fclose (out);
    }
    else
	need_blank = 1;

    out = fopen (parms.commentfile, "a");
    if (out == NULL)
    {
	error ("can't create a comments file","","",1);
	if (in == NULL)
	    gobble_input();
	else
	    fclose (in);
	return ;
    }

    if (in == NULL)
	while (input(2,buf,help))
	{
	    if (need_blank)
	    {
		fprintf (out, "\n");
		need_blank = 0;
	    }
	    G_strip (buf);
	    fprintf (out, "%s\n", buf);
	}
    else
    {
	while (G_getl (buf, sizeof buf, in))
	{
	    if (need_blank)
	    {
		fprintf (out, "\n");
		need_blank = 0;
	    }
	    G_strip (buf);
	    fprintf (out, "%s\n", buf);
	}
	fclose (in);
    }
    fclose (out);
}
