#define REMOVE

#include <stdio.h>
#include "globals.h"
#include "parse.h"
#include "gis.h"

remove_db()
{
   char buffer[INP_SIZ];
   int i;

        strcpy(buffer,"You can't remove a data base in another mapset.");
   if (strcmp(G_mapset(), RIM_db_mapset))
#ifdef DBVECT
                {
                G_warning(buffer);
                SLEEP3;
                return(-1);
                }
#else
      G_fatal_error(buffer);
#endif

   if (my_G_yes(Infile,Outfile,
                "\n\nDo you really wish to REMOVE the database?", 0)==1) {
      for (i=1; i<4; i++) {
         sprintf(buffer, "rm -f %s/%s.rimdb%d", RIM_db_path, File_name, i);
         if (G_system(buffer)!=0) {
            sprintf(buffer, "Warning: Unable to remove file '%s'.", buffer);
            G_fatal_error(buffer);
         }
      }
      fprintf(Outfile, "\nDatabase '%s' successfully removed.\n", File_name);
      parser(P_INST_DB_NA, "");
      *RIM_db_mapset = '\0';
      fprintf(Outfile, "No database now available.  You must '.make' one before continuing or exit.\n");
   }
   else
      fprintf(Outfile, "\nNo files were removed.\n");
}
