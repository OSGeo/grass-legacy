#include "imagery.h"
main(argc,argv) char *argv[];
{
    char command[1024];
    char map[50];
    char group[50];
    char subgroup[50];
    char sigfile[50];

    G_gisinit(argv[0]);


    if (!I_ask_group_old ("", group)) exit(0);
    if(!I_ask_subgroup_old ("",group,subgroup)) exit(0);
    if(!I_ask_sigset_file_old ("", group, subgroup, sigfile)) exit(0);
    if(!G_ask_cell_new ("Enter resultant output raster map", map)) exit(0);

    sprintf (command,
	"i.smap group='%s' subgroup='%s' signaturefile='%s' output='%s'",
	    group, subgroup, sigfile, map);
    printf ("%s\n", command);
    
    exit(system(command));
}
