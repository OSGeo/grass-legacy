/**********************************************************************
 *  G_list_element (element, desc, mapset, lister)
 *
 *      char *element    database element (eg, "cell", "cellhd", etc)
 *      char *desc       desc for element (if NULL, element is used)
 *      char *mapset     mapset to be listed.
 *                       "" to list all mapsets in mapset search list
 *                       "." will list current mapset
 *
 *      int (*lister)()  if given will call this routine to get a list
 *                       title. NULL if no titles desired.
 *
 *                       lister (name, mapset, buf)
 *                       char *name, *mapset, *buf
 *
 *                       given file 'name', and 'mapset',
 *                       lister() should copy a string into 'buf'
 *
 *                       when called with name == "", should set buf
 *                       to general title for mapset list.
 *
 *   General purpose list function. Will list files from all mapsets
 *   in the mapset list for a specified database element.
 *
 *   note:
 *      output is to stdout piped thru the more utility
 *********************************************************************/

#include "gis.h"

#include <signal.h>
static int broken_pipe;
static int hit_return = 0;

G_set_list_hit_return(flag)
{
    hit_return = flag;
}

G_list_element (element, desc, mapset, lister)
    char *element;
    char *desc;
    char *mapset;
    int (*lister)();
{
    int n;
    FILE *more;
    FILE *popen();
    int count;
    int sigpipe_catch();
    int (*sigpipe)();

/* must catch broken pipe in case "more" quits */
    broken_pipe = 0;
    sigpipe = (int (*)()) signal (SIGPIPE, sigpipe_catch);

    count = 0;
    if (desc == 0 || *desc == 0)
	desc = element;
/*
 * popen() the more command to page the output
 */
    if (isatty(1))
    {
	more = popen ("more","w");
	if (!more) more = stdout;
    }
    else
	more = stdout;
    fprintf (more,"----------------------------------------------\n");

/*
 * if no specific mapset is requested, list the mapsets
 * from the mapset search list
 * otherwise just list the specified mapset
 */
    if (mapset == 0 || *mapset == 0)
	for (n = 0; !broken_pipe && (mapset = G__mapset_name (n)); n++)
	    count += list_element (more, element, desc, mapset, lister);
    else
	count += list_element (more, element, desc, mapset, lister);

    if (!broken_pipe)
    {
	if (count == 0)
	    fprintf (more,"no %s files available\n", desc);

	fprintf (more,"----------------------------------------------\n");
    }
/*
 * close the more
 */
    if (more != stdout) pclose (more);
    signal (SIGPIPE, sigpipe);
    if (hit_return && isatty(1))
    {
	printf ("hit RETURN to continue -->");
	while (getchar() != '\n')
	    ;
    }
}

static
sigpipe_catch(n)
{
    broken_pipe = 1;
    signal (n,sigpipe_catch);
}

static
list_element (out, element, desc, mapset, lister)
    FILE *out;
    char *element;
    char *desc;
    char *mapset;
    int (*lister)();
{
    char path[1000];
    char buf[400];
    FILE *ls;
    FILE *popen();
    int count;

    count = 0;
/*
 * convert . to current mapset
 */
    if (strcmp (mapset,".") == 0)
	mapset = G_mapset();


/*
 * get the full name of the GIS directory within the mapset
 * and list its contents (if it exists)
 *
 * if lister() routine is given, the ls command must give 1 name
 */
    G__file_name (path, element, "", mapset);
    if(access(path, 0) == 0)
    {
/*
 * if a title so that we can call lister() with the names
 * otherwise the ls must be forced into columnar form.
 */
	if (lister)
	    sprintf(buf,"ls %s", path);
	else
	    sprintf(buf,"ls -C %s", path);

	if (ls = popen(buf,"r"))
	{
	    while (!broken_pipe && fgets(buf, sizeof buf, ls))
	    {
		if (count++ == 0)
		{
		    fprintf(out, "%s files available in mapset %s:\n", desc, mapset);
		    if (lister)
		    {
			char title[400];
			char name[30];

			*name = *title = 0;
			lister (name, mapset, title);
			if (*title)
			    fprintf(out,"\n%-18s %-.60s\n",name,title);
		    }
		}
		if (lister)
		{
		    char *b;
		    char title[400];

		/* remove the trailing newline */
		    for (b = buf; *b; b++)
			if (*b == '\n')
			    *b = 0;

		    lister (buf, mapset, title);
		    fprintf(out,"%-18s %-.60s\n",buf,title);
		}
		else
		    fprintf(out,"%s", buf);
	    }
	    pclose (ls);
	}
    }
    if (!broken_pipe && (count > 0))
	fprintf(out,"\n");
    return count;
}
