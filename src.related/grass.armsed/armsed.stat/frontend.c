/* %W% %G% */
#include "gis.h"

char *intro[] =
{
"STATISTICS COLLECTION",
"",
"This program produces statistics used as input to the MULTSED",
"hydrological modelling program.  It takes as input a division",
"map (giving the desired subdivision boundaries, which may denote",
"subwatersheds).  Optional input includes: a soils map, an elevation",
"map, and/or a ground cover map for the same geographical area.",
"",
"If this step is a continuation of a project already begun by",
"completing the steps to find the stream network and watershed",
"boundary, the project name should be supplied in place of the",
"division and elevation filenames.",
0
};

main()
{
    char project[30], *proj_mapset;
    char division[30], *div_mapset;
    char soils[30], *soils_mapset;
    char cover[30], *cover_mapset;
    char elev[30], *elev_mapset;
    char command[1024];
    int i;


    G_gisinit ("COLLECT");

    G_clear_screen();
    for (i = 0; intro[i]; i++)
    printf ("%s\n", intro[i]);
    
    G_set_ask_return_msg("if this is not a continuation of an existing project");
    proj_mapset = G_ask_old ("enter project name", project,"watershed/project","parameter storing");
    if (!proj_mapset)
    {
        *project = 0;
        div_mapset = G_ask_cell_old ("enter division file", division);
        if (!div_mapset)
        {
            fprintf(stderr,"Error -- you must provide either a project");
            fprintf(stderr," title or a division file name\n");
            exit(1);
        }
    }

    if (*project == 0)
    {
        G_set_ask_return_msg("if you don't have this map layer");
        elev_mapset = G_ask_cell_old("Enter elevation file",elev);
        if (!elev_mapset) *elev = 0;
    }

    G_set_ask_return_msg ("if you don't have this map layer");
    soils_mapset = G_ask_cell_old("Enter soils file",soils);
    if (!soils_mapset) *soils = 0;

    G_set_ask_return_msg ("if you don't have this map layer");
    cover_mapset = G_ask_cell_old("Enter cover file",cover);
    if (!cover_mapset) *cover = 0;

/* run the backend function 
 */

    if (*project != 0)
    {
        sprintf (command, "Gcollect 'project=%s in %s'", project, proj_mapset);
    }
    else
    {
        sprintf (command, "Gcollect 'division=%s in %s'", division, div_mapset);
    }

    if (*project == 0 && *elev)
    {
        strcat (command, " elev=");
        strcat (command, elev);
        strcat (command, " in ");
        strcat (command, elev_mapset);
    }
    if (*soils)
    {
        strcat (command, " soils=");
        strcat (command, soils);
        strcat (command, " in ");
        strcat (command, soils_mapset);
    }
    if (*cover)
    {
        strcat (command, " cover=");
        strcat (command, cover);
        strcat (command, " in ");
        strcat (command, cover_mapset);
    }

    system (command);
}
