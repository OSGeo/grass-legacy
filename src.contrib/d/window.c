#include "gis.h"
window (argc, argv) char *argv[];
{
    int nrows, ncols;
    int row, col;
    char command[1024];
    char name[100];
    extern char *tempfile;
    FILE *fd, *fopen();
    int new;

    if (argc == 0 && G_yes("Define a window? ",1))
    {
	unlink (tempfile);
	sprintf (command, "Dnew > %s", tempfile);
	G_system(command);
	if((fd = fopen (tempfile, "r")) && (fscanf (fd, "%s", name) == 1))
	{
	    sprintf (command, "Dchoose '%s'", name);
	    G_system (command);
	    basemap();
	}
	if (fd) fclose (fd);
	return 1;
    }
    if (argc == 0)
    {
	printf ("Choose a window.\n");
	G_system ("Dchoose");
	return 1;
    }
    if (argc == 5)
    {
	sprintf (command, "Dnew '%s' %s %s %s %s > /dev/null",
		argv[0], argv[1], argv[2], argv[3], argv[4]);
	if(G_system(command)) return 0;
	sprintf (command, "Dchoose '%s'", argv[0]);
	G_system (command);
	basemap();
	return 1;
    }
    if (argc == 1)
    {
	if (sscanf (argv[0], "%dx%d", &ncols, &nrows) == 2 
	&&  nrows > 0 && ncols > 0)
	{
	    G_system ("Dscreen");
	    if (row > 1 || col > 1)
	    {
		for (row = 0; row < nrows; row++)
		{
		    for (col = 0; col < ncols; col++)
		    {
			sprintf (command, "Dnew %d %d %d %d %d > /dev/null",
			    row*ncols+col+1,		/* window name */
			    100 - (row+1) * (100/nrows),	/* bottom */
			    100 - row * (100/nrows),	/* top */
			    col * (100/ncols),		/* left */
			    (col+1) * (100/ncols)		/* right */
			);
			G_system (command);
		    }
		}
		G_system ("Dchoose 1");
		printf ("Choose a window\n");
		G_system ("Dchoose");
	    }
	    basemap();
	    return 1;
	}
	else
	{
	    sprintf (command, "Define window %s? ", argv[0]);
	    if (new = G_yes(command,1))
	    {
		sprintf (command, "Dnew '%s'", argv[0], tempfile);
		if(G_system(command)) return 0;
	    }
	    sprintf (command, "Dchoose '%s' > /dev/null", argv[0]);
	    G_system (command);
	    if (new)
		basemap();
	    return 1;
	}
    }
    printf ("usage: ");
    printf ("w [name [bottom(0) top(100) left(0) right(100)]]\n");
    
    return 0;
}
