#include "gis.h"
main(argc, argv) char *argv[];
{
    char command[1024];
    char name[256], *mapset;
    char temp[256];

    G_gisinit(argv[0]);

    strcpy (command, "r.average");

/* base map */
    mapset = G_ask_cell_old ("Enter base map", name);
    if (mapset == NULL) exit(0);
    sprintf (temp, " base='%s'", G_fully_qualified_name(name, mapset));
    strcat (command, temp);

/* values map */
    mapset = G_ask_cell_old ("Enter values map", name);
    if (mapset == NULL) exit(0);
    sprintf (temp, " values='%s'", G_fully_qualified_name(name, mapset));
    strcat (command, temp);
    sprintf (temp, "Are the values to be looked up from the category file for %s? ", name);
    if (G_yes(temp, -1))
	strcat (command, " -c");

/* result map */
    if (!G_ask_cell_new("Enter name for the resulting map", name))
	exit(0);
    sprintf (temp, " result='%s'", name);
    strcat (command, temp);

/* run the command-line version */
    system (command);
}
