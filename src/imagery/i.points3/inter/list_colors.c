/*=======================================================================
				i.points
  list_colors.c --

     int  list_vector_colors (void)
          Build a list of some of the standard GRASS color names to the
	  color_list file.

=======================================================================*/


#include "globals.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
#else
#endif

/*---------------------------------------------------------------------*/
int find_vector_colors (void)
{ 
int  len1, len2;
FILE *color_list_file;

   color_list_file = fopen(color_list, "w");

   len1 = len2 = 20;
   fwrite (&len1, sizeof(len1), 1, color_list_file);
   fwrite (&len2, sizeof(len2), 1, color_list_file);

   fprintf (color_list_file, "white  %s\n", G_mapset());
   fprintf (color_list_file, "grey   %s\n", G_mapset());
   fprintf (color_list_file, "blue   %s\n", G_mapset());
   fprintf (color_list_file, "brown  %s\n", G_mapset());
   fprintf (color_list_file, "green  %s\n", G_mapset());
   fprintf (color_list_file, "orange %s\n", G_mapset());
   fprintf (color_list_file, "purple %s\n", G_mapset());
   fprintf (color_list_file, "red    %s\n", G_mapset());
   fprintf (color_list_file, "yellow %s\n", G_mapset());

   fclose (color_list_file);

   return 0;
}

