#include "gis.h"
#include "parms.h"

static char *LABELS = "paint/labels";
main(argc, argv) char *argv[];
{
    struct parms parms;
    char cellname[50], *mapset;
    char labelname[50];
    struct Range range;
    struct Categories cats;
    struct Colors colr;
    struct Cell_head window;
    FILE *fd;

    G_gisinit(argv[0]);
    G_get_window (&window);


/* ask for cell file */
    mapset = G_ask_cell_old ("Enter cell file from which to extract labels",
	cellname);
    if (mapset == NULL)
	exit(0);

/* get the data range, category labels, and color table */
    if(G_read_range (cellname, mapset, &range) < 0)
	G_fatal_error ("Can't determine data range");
    if (G_read_cats (cellname, mapset, &cats) < 0)
	G_fatal_error ("Can't read category file");
    if (G_read_colors (cellname, mapset, &colr) < 0)
	G_fatal_error ("Can't read color table");


/* ask for labels file to hold resultant labels */
    if (!G_ask_any ("", labelname, LABELS, "labels", 1))
	exit(0);

/* initialize default parameters
 * that control size and placement of labels
 */
    parms.height = 25.0;
    parms.space  = 40.0;
    parms.xref = 1.0;
    parms.yref = 1.0;
    sprintf (parms.ref, "t");

/* ask user to set these parameters, check them */
    while(1)
    {
	ask_parms (&parms);
	if (parms.height <= 0.0 || parms.space < 0.0
	|| parms.xref < 0.0 || parms.yref < 0.0)
	    continue;
	if (parms.height > 100.0 || parms.xref > 100.0 || parms.yref > 100.0)
	    continue;
	if (*parms.ref == 'b') break;
	if (*parms.ref == 'c') break;
	if (*parms.ref == 't') break;
    }

/* generate the labels */
    fd = G_fopen_new (LABELS, labelname);
    gen_labels (fd, &parms, &cats, &colr, &range, &window, cellname, mapset);
    fclose (fd);
    printf ("Label file <%s> created for <%s>\n", labelname, cellname);
}
