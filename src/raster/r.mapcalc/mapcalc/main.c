#define MAIN
#include "glob.h"
#include <unistd.h>
#include <signal.h>
#include "function.h"
#include "mapcalc.h"

int 
main (int argc, char *argv[])
{
    char buf[400];
    char result[100];
    int ok;
    int allok = 0;
    int interactive;
    int i;
    char *tempfile;
    FILE *exp_fd;

    G_gisinit (argv[0]);

    G_get_window (&current_region);

    tempfile = G_tempfile();

    max_rows_in_memory = 3;
    if (argc > 1 && sscanf (argv[1], "-n%d", &max_rows_in_memory) == 1)
    {
	argv++;
	argc--;
    }

    if (argc >= 2)
    {
	exp_fd = fopen (tempfile, "w");
	if (exp_fd == NULL)
	    G_fatal_error ("can't open any temp files\n");
	fprintf (exp_fd, "%s", argv[1]);
	for (i = 2; i < argc; i++)
	    fprintf (exp_fd, " %s", argv[i]);
	fprintf (exp_fd, "\n");
	fclose (exp_fd);
	interactive = 0;
    }
    else
	interactive = isatty(0);

/* come up with a very large integer */
    HUGE = 0;
    i = sizeof(int) * 8 - 1;
    while (i-- > 0)
	HUGE += (1 << i);

/* initialize the allocated stacks as unallocated */
    expression_stack_nalloc = 0;
    expression_stack        = NULL;
    execute_stack_nalloc    = 0;
    execute_stack           = NULL;

    while (1)
    {
	if ((argc < 2) && (!input(tempfile))) break;
	expression_stack_depth = 0;
	execute_stack_depth = 0;
	execute_stack_depth_max = 0;

#ifdef DEBUG
sprintf (buf, "cat -n %s | fmt", tempfile);
system (buf);
#endif
	exp_fd = fopen (tempfile, "r");
	if (exp_fd == NULL)
	    G_fatal_error ("can't open temp files\n");
	if (fscanf (exp_fd, "%40s", buf) != 1)
	    break;
	if (strcmp (buf, "exit") == 0)
	    break;
	if (strcmp (buf, "list") == 0)
	{
	    if (fscanf (exp_fd, "%40s", buf) != 1)
		G_list_element ("cell", "cell", "", (int(*)())0);
	    else
		G_list_element ("cell", "cell", buf, (int(*)())0);
	    fclose(exp_fd);
	    continue;
	}
	fseek (exp_fd, 0L, 0);
	if (polish (exp_fd, result))
	{
	    fclose (exp_fd);
	    if (interactive && G_find_file ("cell",result,G_mapset()))
	    {
	       sprintf (buf, "%s - already exists. ok to overwrite? ",result);
	       if (!G_yes(buf,-1)) continue;
	    }

	    floating_point_exception_occurred = 0;
	    overflow_occurred = 0;
#ifdef SIGFPE
	    signal (SIGFPE, fpe);
#endif
	    fprintf (stderr, "\nEXECUTING %s = ... ", result); fflush(stdout);
	    ok = execute (result);
	    if ( !ok ) {
		allok = ok;
	    }
	    
#ifdef SIGFPE
	    signal (SIGFPE, SIG_DFL);
#endif
	    if(ok)
	    {
		fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n",result);
		if (floating_point_exception_occurred) {
		    fprintf (stderr, "NOTE: floating point error(s) occured in the calculation\n");
		    allok = 1;
		}
		if (overflow_occurred) {
		    fprintf (stderr, "NOTE: overflow occured in the calculation\n");
		    allok = 1;
		}
		create_support_files (result, tempfile);
		print_range (result);
	    }
	    free_execute_stack();
        } else {
	  allok = 1; /* added 11/00 Andreas Lange */
	}
	free_expression_stack();
	if (argc >= 2) break;
    }
    exit(allok); /* fixed 11/99 Luca Palmeri */
}
