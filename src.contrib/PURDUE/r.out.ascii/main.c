#include "gis.h"

/* july, 1990  agricultural engineering, purdue university
   chris rewerts (rewerts@ecn.purdue.edu)

   export

   this program produces an ascii output rendition of a cell layer's
   contents.
*/

main(argc, argv)
char *argv[];
{
    char line[80];
    char *mapset;
    char *tempfile1, *tempfile2;
    char command1[256], command2[256], basename[128], name[128];
    FILE *fopen(), *infoname, *dataname;
    CELL *cell, c;
    int i;
    int ncols;
    int nrows;
    long x;
    int row, col;
    struct Categories cats;
    struct Cell_head window;
    int fd;
    char *G_get_cat();

    G_gisinit(argv[0]);

    G_get_window(&window);

    fprintf(stderr, "\n");
    fprintf(stderr, "   +-------------------------------------+\n");
    fprintf(stderr, "   |        AGEN Purdue Beta Test        |\n");
    fprintf(stderr, "   |      cell layer export program      |\n");
    fprintf(stderr, "   +-------------------------------------+\n");

/* ask for name of cell layer */

    mapset = G_ask_cell_old("", name);
    if (!mapset)
	exit(0);

    fprintf(stderr, "\n\n   This program will produce two files:\n\n");
    fprintf(stderr, "1. Cell layer information     (basename.txt)\n");
    fprintf(stderr, "2. ASCII numerical data layer (basename.dat)\n\n");

/* create two temporary files */

    tempfile1 = G_tempfile();
    infoname = fopen(tempfile1, "w");
    if (!infoname)
    {
	perror(tempfile1);
	exit(1);
    }

    tempfile2 = G_tempfile();
    dataname = fopen(tempfile2, "w");
    if (!dataname)
    {
	perror(tempfile2);
	exit(1);
    }

/* ask for name of output files */
    while (1)
    {
	fprintf(stderr, "Enter the basename for the output files --> ");
	if (!G_gets(line))
	    continue;
	if (sscanf(line, "%s", basename) != 1)
	    continue;
	G_strip(basename);
	if (*basename == 0)
	    continue;

/* create command to save files in user's home directory */

	if (basename[0] != '/')
	{
	    sprintf(command1, "cp %s %s/%s.txt", tempfile1, G_home(), basename);
	    sprintf(command2, "cp %s %s/%s.dat", tempfile2, G_home(), basename);
	    fprintf(stderr, "\n\n'%s.txt' and '%s.dat' being saved in your home directory\n", basename, basename);
	}

/* or else allow user to enter a full path to a file */

	else
	{
	    sprintf(command1, "cp %s %s.txt", tempfile1, basename);
	    sprintf(command2, "cp %s %s.dat", tempfile2, basename);
	    fprintf(stderr, "\n\n'%s.txt' and '%s.dat' being saved\n", basename, basename);
	}
	fprintf(stderr, "\n");
	break;
    }

/* get the category names and cell title */
    if (G_read_cats(name, mapset, &cats) < 0)
	exit(-1);

/* open the cell file */
    fd = G_open_cell_old(name, mapset);
    if (fd < 0)
	exit(-2);

    nrows = G_window_rows();
    ncols = G_window_cols();
    cell = G_allocate_cell_buf();

/* print cell layer information to temporary file */

    fprintf(infoname, "\n\n\nCell Layer Information\n\n");
    fprintf(infoname, "Original Layer Name: %s\n", name);
    fprintf(infoname, "Map title: %s\n\n", cats.title);
    fprintf(infoname, "Mapset: %s\n", mapset);
    fprintf(infoname, "Location: %s\n", G_location());
  

    fprintf(infoname, "Map projection is %s.\n", G__projection_name(G_projection()));
    fprintf(infoname, "Map zone: %d\n", G_zone());
    fprintf(infoname, "Rows: %d  Columns: %d\n", nrows, ncols);
    fprintf(infoname, "East-West   Resolution: %.2f\n", window.ew_res);
    fprintf(infoname, "North-South Resolution: %.2f\n", window.ns_res);
    fprintf(infoname, "\nCoordinates of Map Layer\n");
    fprintf(infoname, "North edge: %.2f\n", window.north);
    fprintf(infoname, "South edge: %.2f\n", window.south);
    fprintf(infoname, "East  edge: %.2f\n", window.east);
    fprintf(infoname, "West  edge: %.2f\n", window.west);
    fprintf(infoname, "\n\n");
    fprintf(infoname, "------------------------------------------------------------------\n");

    fprintf(infoname, "Map Layer Categories\n");
    fprintf(infoname, "------------------------------------------------------------------\n");
    for (i = 0; i <= cats.num; i++)
    {
	fprintf(infoname, "%d    %s\n", i, G_get_cat(i, &cats));
    }
    fprintf(infoname, "------------------------------------------------------------------\n");


    for (row = 0; row < nrows; row++)
    {
	G_get_map_row(fd, cell, row);

	for (col = 0; col < ncols; col++)
	{
	    c = cell[col];
	    fprintf(dataname, "%ld ", (long) c);
	}
	fprintf(dataname, "\n");
    }

/* make copies of tmp files to user's desired destination */

    fclose(dataname);
    fclose(infoname);
    system(command1);
    system(command2);

/* trash temporary files */

    unlink(tempfile1);
    unlink(tempfile2);
    fprintf(stderr, "Done.\n");
}
