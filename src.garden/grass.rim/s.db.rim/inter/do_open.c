#define DO_OPEN
#include "gis.h"
#include "globals.h"
#include "parse.h"

do_open()
{
   char temp_name[8];


   *temp_name = 0;

   while (! *temp_name) {

      V_clear();
      V_line(1, "                     OPEN DATABASE SCREEN");
      V_line(7, "          Database name to open:");
      V_ques(temp_name, 's', 7, 33, 7);
      V_intrpt_ok();
      V_intrpt_msg("RETURN TO MAIN MENU");
      if (! V_call()) return;

      /* if a db is open close it before opening the next one. */
      if (*File_name) {
         close_db(File_name);
         *RIM_db_mapset = 0;
         *File_name = 0;
         parser(P_INST_RESET,"");
      }

      G_squeeze(temp_name);
      strcpy(File_name, temp_name);
      if (open_db(File_name, TRUE)==FALSE) {
         G_warning("\nUnable to open data base files.\n");
         SLEEP3;
         *File_name=0;
      }
      else {
         get_field_info();
         init_field_val();
      }
   }
}

