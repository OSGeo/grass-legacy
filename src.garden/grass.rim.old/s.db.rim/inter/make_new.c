#define MAKE_NEW

#include "gis.h"
#include "globals.h"
#include "make.h"
#include "parse.h"


make_new()
{
   char make_file[41], ed_name[41], *fname, buf[200];
   char db_name[8];
   int tempfile, temp_int;
   FILE *fp, *tempfp;

   *make_file = *ed_name = *db_name = 0;
   fname = NULL;

   Make_OK = FALSE;
   while (Make_OK==FALSE) {
      /* get a file name, and editor name or both */
      V_clear();
      V_line(0, "                          MAKE SCREEN LAYOUT");
      V_line(3, " Below enter the name of the database, an input file name and/or the name of");
      V_line(4, " an editor.  If just a file name is given the screen layout definition will");
      V_line(5, " come from that file.  If just an editor is given (example 'vi') a temporary");
      V_line(6, " file will be created and discarded when the database is made.  If both are");
      V_line(7, " given the editor will be invoked on the file named and when the editor");
      V_line(8 ," exits, the file will be used to define the screen layout.");
      V_line(13,"        Name of database to be made:");
      V_line(15,"        File name to input from:");
      V_line(17,"        Editor to create file with:");

      V_ques(db_name, 's',13, 36, 7);
      V_ques(make_file, 's',15, 32, 40);
      V_ques(ed_name, 's',17, 35, 40);

      V_intrpt_ok();
      V_intrpt_msg("RETURN TO MANAGEMENT MENU");

      if (! V_call()) return;

      G_squeeze(db_name);
      G_squeeze(make_file);
      G_squeeze(ed_name);

      sprintf(buf, "%s/%s/rim/sites/%s.rimdb1", G_location_path(), G_mapset(),
               db_name);
      if ((tempfp = fopen(buf, "r"))==NULL) {
         /* close any open database and set the new database name */
         if (*File_name) {
            close_db(File_name);
            *RIM_db_mapset = 0;
            parser(P_INST_RESET,"");
         }
         strcpy(File_name, db_name);

         /* resolve the file name */
         if (*make_file == 0) {
         if (fname==NULL) fname = G_tempfile();
            tempfile = TRUE;
         }
         else {
            fname = make_file;
            tempfile = FALSE;
         }

         /* do any editing */
         if (*ed_name != 0) {
            sprintf(buf, "%s %s", ed_name, fname);
            G_system(buf);
         }

         /* open the file */
         if ((fp=fopen(fname, "r"))==NULL) {
            sprintf(buf, "Unable to open the file %s to input for screen layout. ",
               fname);
            G_warning(buf);
            SLEEP3;
            *File_name = 0;
            return;
         }

         /* do the make */
         make_init();
         while (fgets(buf, INP_SIZ, fp)!=NULL) {
            /* if there is a new line char at end get rid of it */
            if (buf[(temp_int = strlen(buf)-1)]=='\n')
            buf[temp_int]='\0';
            make_add(buf);
         }
         make_done();
         SLEEP3;

         if (Make_OK==FALSE) {
         *File_name = 0;
         printf("\nHit return to continue.\n");
         G_gets(buf);
         }
      }
      else {
         fclose(tempfp);
         G_warning("Database already exists, choose another name.");
         SLEEP3;
      }
   }
   if (tempfile == TRUE)
      unlink(fname);
}
