#define GLOBAL
#include "global.h"

main(argc, argv) char *argv[];
{
    char group[30];
    int n;

    setbuf (stdout, NULL);
    setbuf (stderr, NULL);
    G_gisinit (argv[0]);

    if (!I_ask_group_old ("Enter the group containing files to be rectified", group))
	exit(0);

/* determine the number of files in this group */
    I_get_group_ref (group, &ref);
    if (ref.nfiles <= 0)
    {
	fprintf (stderr, "No files in this group!\n");
	exit(0);
    }
    ref_list = (int *) G_malloc (ref.nfiles * sizeof(int));
    new_name = (char **) G_malloc (ref.nfiles * sizeof(char *));
    for (n = 0; n < ref.nfiles; n++)
	ref_list[n] = -1;

/* read the control points for the group */
    get_control_points (group);

/* get the target */
    get_target(group);

/* ask user for the files to be rectified */
    ask_files (group);
    get_target_window();

    exec_rectify ();
}
