#include "global.h"

open_group()

{
  I_init_group_ref (&Ref);
    if (!I_find_group(group))
    {
	fprintf (stderr, "group=%s - not found\n", group);
	exit(1);
    }
    if (!I_find_subgroup(group, subgroup))
    {
	fprintf (stderr, "subgroup=%s (of group %s) - not found\n", subgroup, group);
	exit(1);
    }

    I_get_subgroup_ref (group, subgroup, &Ref);

    if (Ref.nfiles <= 1)
    {
	fprintf (stderr, "Subgroup [%s] of group [%s] ", subgroup, group);
	if (Ref.nfiles <= 0)
	    fprintf (stderr, "doesn't have any files\n");
	else
	    fprintf (stderr, "only has 1 file\n");
	fprintf (stderr, "The subgroup must have at least 2 files\n");
	exit(1);
    }


}


open_files()
{
    char *name, *mapset;
    FILE *fd;
    int n;

  
    cell = (CELL **) G_malloc (Ref.nfiles * sizeof (CELL *));
    cellfd = (int *) G_malloc (Ref.nfiles * sizeof (int));
    P = (double *) G_malloc (Ref.nfiles * sizeof (double));
    for (n=0; n < Ref.nfiles; n++)
    {
	cell[n] = G_allocate_cell_buf();
	name = Ref.file[n].name;
	mapset = Ref.file[n].mapset;
	if ((cellfd[n] = G_open_cell_old (name, mapset)) < 0){
            fprintf (stderr, "Unable to open layer [%s]in [%s]", name,mapset);
	    exit(1);
           }
    }


t_cell = G_allocate_cell_buf();
if ((t_fd = G_open_cell_old (t_map, t_mapset)) < 0){

            fprintf (stderr, "Unable toread training map");
	    exit(1);
   }

 
   if(class_name[0]=='\0')  {
      fprintf (stderr, "Unable to create class layer [%s]", class_name);
      exit(1) ;
     }

    class_fd = G_open_cell_new (class_name);
    if (class_fd < 0) {
      fprintf (stderr, "Unable to create class layer [%s]", class_name);
      exit(1) ;
     }
    class_cell = G_allocate_cell_buf();

    reject_cell = NULL;
    if (reject_name[0]!='\0')
    {
	reject_fd = G_open_cell_new (reject_name);
	if (reject_fd < 0)
	    fprintf (stderr, "will not create reject layer!");
	else
	    reject_cell = G_allocate_cell_buf();
    }
}


close_files()
{
    int n;


    for (n=0; n < Ref.nfiles; n++)
       {
         G_close_cell(cellfd[n]);
         free(cell[n]);
        }

    free(cell);
   
    G_close_cell(t_fd);
    free(t_cell);
    
    G_close_cell(class_fd);
    free(class_cell);
   
    if(reject_fd>0) {
           G_close_cell(reject_fd);
           free(reject_cell);
     }


}



exit1()

{
close_files();
free_stats();
exit(1);

}
