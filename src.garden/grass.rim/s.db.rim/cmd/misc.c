#define MISC
#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"
#include "parse.h"


char *Base_Name(string)
char *string;
{
   char *temp;
   if ((temp = rindex(string, '/'))==NULL)
      return(string);
   else
      return(temp+1);
}


/* This routine searches through the current mapsets attempting to
   use RIM to open the database called "dbname". */
open_rim_db(dbname)
char *dbname;
{
   char cmd_buf[BUF_SIZ], *mapset;
   int count, status;

   if (*RIM_db_mapset == '\0') {
      for (count=0; mapset=G__mapset_name(count); count++) {
         sprintf(RIM_db_path, "%s/%s/rim/sites", G_location_path(), mapset);
         sprintf(cmd_buf, "open '%s/%s'", RIM_db_path, dbname);
         status = crim(1, cmd_buf);
                   if (status==0 || status == 13) break;
      }
      if (mapset) {
        strcpy(RIM_db_mapset,mapset);
        if (strcmp(mapset, G_mapset())!=0 && (status==0 || status==13)) {
          parser(P_INST_RONLY,"");
          status = 13;
        }
      }
   }
   else {
      sprintf(cmd_buf, "open '%s/%s'", RIM_db_path, dbname);
      status = crim(1, cmd_buf);
   }

   return(status);
}

syntax_err(inp_buf)
char *inp_buf;
{
   fprintf(Outfile, "Warning: Unrecognized command ==> %s\n", inp_buf);
}

nop()
{
   /*
   fprintf(Outfile, "\nIn function nop.\n");
   */
}

misplaced_end()
{
   fprintf(Outfile, "Warning: '.end' not appropriate in current context.\n");
}
