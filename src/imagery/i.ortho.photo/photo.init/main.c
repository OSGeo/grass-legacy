/* main.c */

#define  MAIN  1
#include "globals.h"


static char *intro[]=
{
"This program creates or modifies the initail exposure station parameters \n and thier apriori standard variances ",0
};


main(argc, argv) char *argv[];
{
    int i;
    int have_old;
    int nfiles;
    char *location, *mapset, *initial;
    char *name;

    if (argc != 2)
    {
	fprintf (stderr, "usage: %s group\n", argv[0]);
	exit(1);
    }

    initial   = (char *) G_malloc (40*sizeof (char), 1);
    mapset    = (char *) G_malloc (40*sizeof (char), 1);
    location  = (char *) G_malloc (40*sizeof (char), 1);
    name      = (char *) G_malloc (40*sizeof (char), 1);

    G_gisinit (argv[0]);
    location = G_location();
    mapset = G_mapset();
    
    
    /* get group ref */
    name = argv[1];
    strcpy (group.name, name);
    if (!I_find_group (group.name))
    {
	fprintf (stderr, "Group  [%s] not found\n", name);
	exit(1);
    }
#   ifdef DEBUG
    printf("Found group %s\n", group.name);
#   endif

/*******************
    I_get_Ortho_Image_Group_Ref(group.name, &group.group_ref);
    nfiles = block.block_ref.nfiles;
#   ifdef DEBUG
    printf("Got group ref \n");
#   endif
*******************/

    
    /* get initial camera exposure infor */ 
    if (I_find_initial(group.name))
    {  have_old = 1;
        I_get_init_info (group.name, &group.camera_exp);
    } 

    /* modifiy infor */
    mod_init_info(have_old, &group.camera_exp);

    /* save info */
    I_put_init_info(group.name, &group.camera_exp);

    exit(0);
}


