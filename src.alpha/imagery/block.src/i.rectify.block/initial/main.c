#include "initial.h"


static char *intro[]=
{
"This program creates or modifies the initail exposure station parameters \n and thier apriori standard variances ",0
};

struct Camera_Exp_Init init_info;
Block block;

main(argc, argv) char *argv[];
{
    int i;
    int have_old;
    int nfiles;
    char *location, *mapset, *initial, *group, *file;
    char *name, *blockname, *subblock;

    if (argc != 3)
    {
	fprintf (stderr, "usage: %s blockname subblock\n", argv[0]);
	exit(1);
    }

    initial   = (char *) G_malloc (40*sizeof (char), 1);
    group     = (char *) G_malloc (40*sizeof (char), 1);
    mapset    = (char *) G_malloc (40*sizeof (char), 1);
    location  = (char *) G_malloc (40*sizeof (char), 1);
    blockname = (char *) G_malloc (40*sizeof (char), 1);
    subblock  = (char *) G_malloc (40*sizeof (char), 1);
    name      = (char *) G_malloc (40*sizeof (char), 1);

    G_gisinit (argv[0]);
    location = G_location();
    mapset = G_mapset();
    
/* get block ref */
    blockname = argv[1];
    strcpy (block.name, blockname);
    if (!I_find_block (blockname))
    {
	fprintf (stderr, "** Block [%s] not found\n", blockname);
	exit(1);
    }
#   ifdef DEBUG
    printf("Found block %s\n",blockname);
#   endif

    subblock = argv[2];
    I_get_subblock_Block_Image_Group_Ref(blockname, subblock, &block.block_ref);
    nfiles = block.block_ref.nfiles;
#   ifdef DEBUG
    printf("Back from I_get_subblock_Block_Image_Group_Ref\n");
#   endif

/** select one group **/
/** loop till done **/
    while (1)
    {
       if (!ask_one_file(block.name, subblock, group, block.block_ref))
          break;
 
       if (I_find_initial(group))
       {  have_old = 1;
          I_get_init_info (group, &init_info);
       } 
       mod_init_info(have_old, &init_info);

       I_put_init_info(group, &init_info);
    }
    exit(0);
}


