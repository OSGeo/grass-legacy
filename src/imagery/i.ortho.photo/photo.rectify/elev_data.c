#include "glob.h"
#include "function.h"
#include <signal.h>

elev_data(tl,buf)
char *tl;
char *buf1;
{
    char name[50];
    char expression[300];
    int outfd;
    int ok;
    int interactive;
    int fpe();
    int i;

/* come up with a very large integer */
    HUGE = 0;
    i = sizeof(int) * 8 - 1;
    while (i-- > 0)
	HUGE += (1 << i);

    G__setenv("LOCATION_NAME", tl);
    while (1)
    {
	expression_stack_depth = 0;
	execute_stack_depth = 0;

	if (strcmp (buf, "list") == 0)
	{
	    G_set_list_hit_return(1);
	    G_list_element ("cell", "raster", "", (int(*)())0);
        }
	else if (sscanf (buf, "list %s", name) == 1)
	{
	    G_set_list_hit_return(1);
	    G_list_element ("cell", "raster", name, (int(*)())0);
        }
	else if (assignment(buf, name, expression)
	      && polish (expression))
	{
	    if (interactive && G_find_file ("cell",name,G_mapset()))
	    {
	       sprintf (buf, "%s - already exists. ok to overwrite? [n] ",name);
	       if (!yes(buf,0)) continue;
	    }
	    outfd = G_open_cell_new (name);
	    if (outfd >= 0)
	    {
		printf ("EXECUTING %s = %s\n", name, expression);
		floating_point_exception_occurred = 0;
#ifdef SIGFPE
		signal (SIGFPE, fpe);
#endif
		ok = execute (outfd);
#ifdef SIGFPE
		signal (SIGFPE, SIG_DFL);
#endif
		if(ok)
		{
		    printf ("CREATING SUPPORT FILES FOR %s\n",name);
		    if (floating_point_exception_occurred)
			fprintf (stderr, "NOTE: floating point errors(s) occured in the calculation\n");
		    G_close_cell (outfd);
		    G_put_cell_title (name, expression);

		    printf ("minimum value %ld, maximum value %ld\n",
			(long) min_value, (long) max_value);
		}
		else
		    G_unopen_cell (outfd); /* do not create the raster file! */
		free_execute_stack();
	    }
	    else
		fprintf (stderr, "OOPS can't create raster file <%s>\n", name);
	}
	free_expression_stack();
	if (argc >= 2) break;
    }
    /* return; */
    exit(0); 
}

