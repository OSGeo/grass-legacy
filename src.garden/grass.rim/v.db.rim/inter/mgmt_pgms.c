#include "gis.h"
#include "globals.h"
#include "parse.h"
#include "rim.h"


do_remove()
{
   char db_name[8], buf[100], buf2[100];
   int remove_ok, i;
   FILE *tempfp;

   *db_name = 0;
   while (!*db_name) {
      V_clear();
      V_line(0, "                        REMOVE DATABASE SCREEN");
      V_line(6, "                Name of database to remove:");

      V_ques(db_name, 's', 6, 43, 7);
      V_intrpt_ok();
      V_intrpt_msg("RETURN TO MANAGEMENT MENU");

      if (! V_call()) return;

      G_squeeze(db_name);

      /* if we got a data base name */
      if (*db_name) {
         sprintf(buf,"%s/%s/rim/vect/%s.rimdb1", G_location_path(),
                  G_mapset(), db_name);
         if ((tempfp=fopen(buf, "r"))!=NULL) {
            fclose(tempfp);
            if (! strcmp(File_name, db_name)) {
               close_db(File_name);
               *RIM_db_mapset = 0;
               *File_name = 0;
               parser(P_INST_RESET,"");
            G_warning("Attempting to remove the currently opened database.");
            }
            sprintf(buf, "Do you really wish to REMOVE the database named '%s'?",db_name);
            if (G_yes(buf, 0)) {
               remove_ok = TRUE;
               for (i=1; i<4; i++) {
                  sprintf(buf,"%s/%s/rim/vect/%s.rimdb%d", G_location_path(),
                          G_mapset(), db_name, i);
                  if (unlink(buf)!=0) {
                     sprintf(buf2,"Warning: Unable to remove file '%s'.",buf);
                     G_warning(buf2);
                     SLEEP3;
                     remove_ok = FALSE;
                  }
               }
               if (remove_ok==TRUE)
                  printf("\nDatabase '%s' successfully removed.\n", db_name);
               parser(P_INST_DB_NA, "");
            }
            else
               G_warning("\nNo files were removed.\n");
               SLEEP3;
         }
      }
   }
}


do_rename()
{
   int i, reopen_flag;
   FILE *tempfp;
   char old_name[8], new_name[8];
   char buf[100], buf2[100];

   *old_name = *new_name = '\0';

   while (1) {
      do {
         V_clear();
         V_line(2,make_line_2());
         V_line(4,"   To rename a data base you must provide both of the following:");
         V_line(5,"   Note: you can only rename a data base in your current mapset.");
         V_line(7,"        Name of existing data base: ");
         V_line(9,"        New name for the data base: ");

         V_ques(old_name, 's', 7, 35, 7);
         V_ques(new_name, 's', 9, 35, 7);

         V_intrpt_ok();
         V_intrpt_msg("RETURN TO THE MANAGEMENT MENU");

         if (! V_call()) return;
      } while (*old_name=='\0' || *new_name=='\0');

      printf("\nRenaming data base...");
      G_squeeze(old_name);
      G_squeeze(new_name);
      reopen_flag = FALSE;

      /* make sure new_name doesn't already exist */
      sprintf(buf, "%s/%s/rim/vect/%s.rimdb1", G_location_path(), G_mapset(),
               new_name);
      if ((tempfp = fopen(buf, "r"))!=NULL) {
         fclose(tempfp);
         G_warning("\nThe new data base name that you provided already exists! No changes were made.");
         SLEEP3;
      }
      else {
         /* make sure old_name doesn't already exist */
         sprintf(buf, "%s/%s/rim/vect/%s.rimdb1", G_location_path(), G_mapset(),
                  old_name);
         if ((tempfp = fopen(buf, "r"))==NULL) {
            G_warning("\nThe old data base name that you provided doesn't exists!  No changes were made.");
            SLEEP3;
         }
         else {
            fclose(tempfp);
            /* if a db is open close it before renaming it. */
            if (strcmp(old_name, File_name)==0) {
                  close_db(File_name);
                  *RIM_db_mapset = 0;
                  *File_name = 0;
                  parser(P_INST_RESET,"");
                  reopen_flag = TRUE;
            }

            /* rename the files */
            for (i=1; i<=3; i++) {
               sprintf(buf,"%s/%s/rim/vect/%s.rimdb%d",G_location_path(),G_mapset(),
                       old_name, i);
               sprintf(buf2,"%s/%s/rim/vect/%s.rimdb%d",G_location_path(),G_mapset(),
                       new_name, i);
               rename(buf, buf2);
            }

            sprintf(buf, "cd $LOCATION/rim/vect/; %s", RIM_COMMAND_STR);
            if ((tempfp=popen(buf, "w"))==NULL)
               G_fatal_error("Unable to invoke RIM in rename().");
            fprintf(tempfp, "open %s\n", old_name);
            fprintf(tempfp, "name %s\n", new_name);
            fprintf(tempfp, "exit\n");
            pclose(tempfp);
            printf("done.\n");

            /* reopen the data base if it was previously open */
            if (reopen_flag==TRUE) {
               strcpy(File_name, new_name);
               if (open_db(File_name, TRUE)==FALSE) {
                  G_warning("\nUnable to open data base files.\n");
                  SLEEP3;
                  *File_name='\0';
               }
               else {
                  get_field_info();
                  init_field_val();
               }
            }
            *old_name = *new_name = '\0';
         } /*end of else*/
      }
   } /*end of while(1)*/
}


do_list()
{
   char dummy[80];

   list();
   printf("\nHit return to continue.\n");
   G_gets(dummy);
}


do_show()
{
   V_clear();
   v_show(TRUE);
   V_call();
}

do_read_vect()
{
char cmd[80], vect_map[40], *mapset;

mapset = G_ask_vector_old("Enter vector map to read new vect from.",vect_map);
sprintf(cmd,".read_vect %s",vect_map);
read_vect(cmd);
}


