/* %W% %G% */

#include "gis.h"
/*---------------------------------------------------------------------
 |      layer_info
 |      Map Layer Information Table
 |
 */

#define printline(x) fprintf (out," | %-74.74s |\n",x)
#define divider(x) \
    fprintf (out," %c",x);\
    for (i = 0; i < 76; i++)\
        fprintf(out,"-");\
    fprintf (out,"%c\n",x)

static stash_away ();
static int Got_name = 0;

struct Command_keys keys[] = {
    { "layer", 1 },
    { "cellfile", 1 },
    { "cell", 1 },
    { NULL, 0 }

};

static char *name;

main (argc, argv)   
    int argc;
    char *argv[];
{
    char *mapset;
    char *tempfile;
    char line[200];
    int i;
    CELL mincat, maxcat, cat;
    FILE *out, *fopen();
    struct Cell_head cellhd;
    struct Categories cats;
    struct History hist;
    int head_ok;
    int cats_ok;
    int hist_ok;
    int is_reclass;
    struct Reclass reclass;
    char *G_program_name();
    int ret;

    G_gisinit(argv[0]);

    if ((ret = G_parse_command (argc, argv, keys, stash_away)) < 0)
    {
	G_parse_command_usage (argv[0], keys, USAGE_LONG);
	exit (-1);
    }
    if (ret > 0)
	exit (1);

    if (!Got_name) {
	G_parse_command_usage (argv[0], keys, USAGE_LONG);
	exit (-1);
    }
    if ((mapset = G_find_cell2 (name, "")) == NULL)
    {
	sprintf (line, "Cannot find %s", name);
	G_fatal_error (line);
    }

    head_ok = G_get_cellhd (name, mapset, &cellhd) >= 0;
    cats_ok = G_read_cats (name, mapset, &cats) >= 0;
    hist_ok = G_read_history (name, mapset, &hist) >= 0;
    is_reclass = G_get_reclass (name, mapset, &reclass);

    out = stdout;

    divider ('+');

    sprintf (line, "Layer:    %-29.29s  Date: %s", name, hist_ok ? hist.mapid : "??");
    printline (line);

    sprintf (line, "Location: %-29.29s  Login of Creator: %s", G_location(), hist_ok ? hist.creator : "??");
    printline (line);

    sprintf (line, "Mapset:   %s", mapset);
    printline (line);

    sprintf (line, "Title:    %s", cats_ok ? cats.title : "??");
    printline (line);

    divider ('|');
    printline ("");

    sprintf (line, "  Type of Map:  %-20.20s", hist_ok ? hist.maptype : "??");
    strcat (line, "Number of Categories: ");
    if (cats_ok)
    {
        char temp[20];
        sprintf (temp, "%-9ld", (long)cats.num);
        strcat (line, temp);
    }
    else
        strcat (line, "??");
    printline (line);

    if (head_ok)
    {
	char temp1[100], temp2[100], temp3[100];

        sprintf (line, "  Rows:         %d", cellhd.rows);
	printline (line);

        sprintf (line, "  Columns:      %d", cellhd.cols);
	printline (line);

        sprintf (temp1, "%ld", (long)cellhd.rows * cellhd.cols);
	G_insert_commas(temp1);
        sprintf (line, "  Total Cells:  %s", temp1);
	printline (line);

	sprintf (line, "       Projection: %s (zone %d)",
	    G_projection_name(cellhd.proj), cellhd.zone);
        printline (line);

	G_format_northing (cellhd.north, temp1, cellhd.proj);
	G_format_northing (cellhd.south, temp2, cellhd.proj);
	G_format_resolution (cellhd.ns_res, temp3, cellhd.proj);
        sprintf (line, "           N: %-10s    S: %-10s   Res: %-5s",
	    temp1, temp2, temp3);
        printline (line);

	G_format_easting (cellhd.east, temp1, cellhd.proj);
	G_format_easting (cellhd.west, temp2, cellhd.proj);
	G_format_resolution (cellhd.ew_res, temp3, cellhd.proj);
        sprintf (line, "           E: %-10s    W: %-10s   Res: %-5s",
	    temp1, temp2, temp3);
        printline (line);
    }

    printline ("");

    if (hist_ok)
    {
        printline ("  Data Source:");
        sprintf (line, "   %s", hist.datsrc_1);
	printline(line);
        sprintf (line, "   %s", hist.datsrc_2);
	printline(line);
	printline("");

        printline ("  Data Description:");
        sprintf (line, "   %s", hist.keywrd);
	printline(line);
	printline("");
        if(hist.edlinecnt)
	{
	    printline ("  Comments:");

	    for (i = 0; i < hist.edlinecnt; i++)
	    {
		sprintf (line, "   %s", hist.edhist[i]);
		printline(line);
	    }
        }

        printline ("");
    }

    if (is_reclass > 0)
    {
        divider ('|');
        sprintf (line, "  Reclassification of [%s] in mapset [%s]",
            reclass.name, reclass.mapset);
        printline (line);
        printline ("");
        printline ("        Category        Original categories");
        printline ("");

	mincat = maxcat = reclass.table[0];
	for (i = 1; i < reclass.num; i++)
	    if (reclass.table[i] < mincat)
		mincat = reclass.table[i];
	    else if (reclass.table[i] > maxcat)
		maxcat = reclass.table[i];
        for (cat = mincat; cat <= maxcat; cat++)
        {
            char text[80];
            char num [10];
            int next;

	    if (cat == 0) continue;
            sprintf (num, "%5ld", (long)cat);
            next = 0;
            do
            {
                next = reclass_text (text, cat, &reclass, next);
                sprintf (line, "     %5s              %s", num, text);
                printline (line);
                *num = 0;
            }
            while (next >= 0);
        }
    }
    divider ('+');

    fprintf(out,"\n");
}

static
stash_away (position, alias)
    char *alias;
    int position;
{
    switch (position) {
	case 1:
	    Got_name = 1;
	    name = G_store(alias);
	    break;
	default:
	    return (-1);
	    break;
    }
    return (0);
}
