#define MAIN
#include "glob.h"
#include "function.h"

int main(argc,argv) char *argv[];
{
    char buf[400];
    char result[100];
    int ok;
    int allok = 0;
    int interactive;
    int fpe();
    int i;
    char *tempfile;
    FILE *exp_fd;
	
    G_gisinit (argv[0]);

    G3d_getWindow (&current_region);
    G3d_readWindow(&current_region,NULL);
    
    tempfile = G_tempfile();

    if (argc >= 2)
    {
	exp_fd = fopen (tempfile, "w");
	if (exp_fd == NULL)
	    G3d_fatalError ("can't open any temp files\n");
	fprintf (exp_fd, "%s", argv[1]);
	for (i = 2; i < argc; i++)
	    fprintf (exp_fd, " %s", argv[i]);
	fprintf (exp_fd, "\n");
	fclose (exp_fd);
	interactive = 0;
    }
    else
        interactive = isatty(0);

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

	exp_fd = fopen (tempfile, "r");
	if (exp_fd == NULL)
	    G3d_fatalError ("can't open temp files\n");
	if (fscanf (exp_fd, "%40s", buf) != 1)
	    break;
	if (strcmp (buf, "exit") == 0)
	    break;
	if (strcmp (buf, "list") == 0)
	{
	    if (fscanf (exp_fd, "%40s", buf) != 1)
		G_list_element ("grid3", "3d-raster", "", (int(*)())0);
	    else
		G_list_element ("grid3", "3d-raster", buf, (int(*)())0);
	    fclose(exp_fd);
	    continue;
	}
	fseek (exp_fd, 0L, 0);
	if (polish (exp_fd, result))
        {
            fclose (exp_fd);
	    if (interactive && (G_find_grid3 (result,G_mapset()) != NULL) )
	    {
	       sprintf (buf, "%s - already exists. ok to overwrite? ",result);
	       if (!G_yes(buf,-1)) continue;
	    }

	    fprintf (stderr, "\nEXECUTING %s = ... ", result); fflush(stdout);
            ok = execute (result);
            
	    if ( ok ) {
                print_range(result);
                allok = ok;
            }

            free_execute_stack();
	}
        free_expression_stack();
	if (argc >= 2) break;
    }
    exit(!allok);
}
