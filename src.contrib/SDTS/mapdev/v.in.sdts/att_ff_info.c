#include <stdlib.h>
#include <stdio.h>
#include     <string.h>
#include     "gis.h"
#include     "sdts_in.h"
#include     <stc123.h>
#include  "sdts_globals.h"

struct att_ff_file * alloc_att_ff_file ();


init_Sdts_att_ff_struct (Att_FF_info)
   struct att_ff_info *Att_FF_info;
{
     Att_FF_info->n_ap_files = 0;
     Att_FF_info->n_as_files = 0;
     Att_FF_info->n_ff_files = 0;
}

build_att_ff_file_struct (filename, att_ff_ptr, num)
  char *filename;
  struct att_ff_file **att_ff_ptr;
  int *num;
{
	struct att_ff_file *loc_ptr;

	loc_ptr = *att_ff_ptr;

	/*
	fprintf (stderr, "entering build_att file = '%s'\n", filename);
	*/
    loc_ptr = alloc_att_ff_file (loc_ptr, num);
	loc_ptr[*num - 1].fname = G_malloc (15);
	strcpy (loc_ptr[*num - 1].fname, filename);
	/*
	fprintf (stderr, "leaving: num = '%d' fname = '%s'\n", *num,
	  loc_ptr[*num -1].fname);
     */
    *att_ff_ptr = loc_ptr;
}

struct att_ff_file *
alloc_att_ff_file ( att_ff_ptr, num)
   struct att_ff_file *att_ff_ptr;
   int *num;
{
   struct att_ff_file *ptr;
   char Error_msg[100];


   ptr = att_ff_ptr;
   /*DEBUG*//* fprintf (stderr, "alloc_att: num = '%d'\n", *num);*/

   if (*num == 0) {
        if ((ptr = (struct att_ff_file *) G_malloc (sizeof (struct att_ff_file))) == 0) 
	       G_fatal_error ("Can't allocate memory for att_ff struct. Exiting.");
        else
           G_zero (ptr, sizeof (struct att_ff_file ));
   }
   else  {
		 /*DEBUG*//* fprintf (stderr, "before realloc '%s'\n", ptr[*num-1].fname);*/
         if ((ptr = (struct att_ff_file *) G_realloc ((char * ) ptr,
		 (*num + 1) * sizeof (struct att_ff_file)))==0) {
            sprintf (Error_msg, "Can't allocate memory for att_ff struct, %dth time. Exiting.\n", *num +1);
            G_fatal_error (Error_msg);
         }
         else
            G_zero (ptr + *num, sizeof (struct att_ff_file));
		
    }

	if (ptr)
	   (*num)++;       

    return (ptr);
}


#ifdef FOO
xfer (A_F_info)
   struct att_ff_info *A_F_info;
{
    struct att_ff_file *local_file;

	local_file = A_F_info->AP;

	printf ("local file '%s'\n", local_file[80].fname);
}
#endif

display_att_ff (Att_FF_info)
   struct att_ff_info *Att_FF_info;
{
	 int i;

	 fprintf (stderr, "displaying att_ff\n");
	 for (i = 0; i < Att_FF_info->n_ap_files; i++)
		fprintf (stderr, "AP file #%d = '%s'\n", 
		   i, Att_FF_info->AP[i].fname );
	 for (i = 0; i < Att_FF_info->n_as_files; i++)
		fprintf (stderr, "AS file #%d = '%s'\n", i, Att_FF_info->AS[i].fname);
	 for (i = 0; i < Att_FF_info->n_ff_files; i++)
		fprintf (stderr, "FF file #%d = '%s'\n", i, Att_FF_info->FF[i].fname);
}
