#define GLOBAL
#include "global.h"
#include "crs.h"

main(argc, argv) char *argv[];
{
    char group[30];
    int order;      /* ADDED WITH CRS MODIFICATIONS */
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



  printf("\n\n");
  while(1)
    {
    char buf[100];

    printf("\nPlease enter the order of the transformation you want: ");
    if(!G_gets(buf))
      continue;
    order = atoi(buf);
    if(order >= 1 && order <= MAXORDER)
      break;

    printf("\nInvalid order please enter 1 to %d.\n",MAXORDER);
    }



/* read the control points for the group */
    get_control_points (group,order);

/* get the target */
    get_target(group);

/* ask user for the files to be rectified */
    ask_files (group);
    get_target_window(order);

    exec_rectify (order);
}
