#define TABLES
#include <stdio.h>
#include "globals.h"


/* This routine gets the table and column information from the RIM database
   and sends it to Outfile.  */
tables()
{
   FILE *Tempfile;
   char tmpstr[INP_SIZ];

        get_tableinfo(File_name);
   if ((Tempfile=fopen(Tableinfof, "r"))==NULL) {
      fprintf(Outfile, "The table information file does not exist.\n");
      fprintf(Outfile, "Unable to print table information.\n");
      return(-1);
   }

   fprintf(Outfile, "The number of rows reported below may have changed.\n");
   while (fgets(tmpstr, INP_SIZ, Tempfile)!=NULL)
      fprintf(Outfile, "%s", tmpstr);

   close(Tempfile);
}

