#define BACKUP

/* To backup a GRASS-RIM data base to specified file */
#include "gis.h"
#include "globals.h"
#include "rim.h"

/* pack routine:  calls backup() with proper parameters */
pack()
{
return (backup("PACK") );
}

backup(buffer)
char *buffer;
{
        FILE *fp1;
        char *file;
        char cmd[200],pathname[200];
        char *script;
        char rim_file1[140],rim_file2[140],rim_file3[140];
        char tmp_file1[140],tmp_file2[140],tmp_file3[140];

        int pack_flag;

        if (strcmp(RIM_db_mapset,G_mapset())) {
                sprintf(cmd,"Data base <%s> is not in current mapset <%s>.\n",
                        File_name,G_mapset());
                strcat (cmd,".backup and .pack only possible in current mapset.\n");
                G_warning(cmd);
                SLEEP3;
                return (-1);
        }

   G_squeeze(buffer);

/* check for call from .pack */
        if (strcmp(buffer,"PACK")==0) {
                pack_flag = 1;
                strcpy( pathname,G_tempfile() );
        }
        else {
                pack_flag = 0;
/* get second item from buffer string (= backup path/file name) */
        /* find the second word on the input line */
                file = buffer + strcspn(buffer," ");
                if (! *file) {
                        G_warning("Backup NOT MADE: .backup requires path and/or file name.");
                        SLEEP3;
                        return(-1);
                        }
                if (*(++file) == '/')
                        strcpy (pathname, file);
                else {
                                getwd(pathname);
                                strcat(pathname,"/");
                                strcat(pathname,file);
                }
        }

/* close the data base */
        close_db(File_name);

/* create and execute the backup script (for both pack and backup) */
        script = G_tempfile();
        fp1 = fopen(script,"w");
        fprintf(fp1,"cd %s\n",RIM_db_path);
        fprintf(fp1,"%s <<EOF \n",RIM_COMMAND_STR);
        fprintf(fp1,"open %s\n",File_name);
        fprintf(fp1,"unload all to '%s'\n",pathname);
        fprintf(fp1,"exit\n");
        fprintf(fp1,"EOF");
        fclose(fp1);

        sprintf(cmd,"sh %s",script);
        G_system (cmd);
        unlink (script);


/* if packing, remove old files, create and execute the rebuild script */
if (pack_flag==1){
        /* construct file names */
        sprintf(rim_file1,"%s/%s.rimdb1",RIM_db_path,File_name);
        sprintf(rim_file2,"%s/%s.rimdb2",RIM_db_path,File_name);
        sprintf(rim_file3,"%s/%s.rimdb3",RIM_db_path,File_name);
        sprintf(tmp_file1,"%s/%s.bakdb1",RIM_db_path,File_name);
        sprintf(tmp_file2,"%s/%s.bakdb2",RIM_db_path,File_name);
        sprintf(tmp_file3,"%s/%s.bakdb3",RIM_db_path,File_name);
        /*move old files for safety*/
        if((rename(rim_file1,tmp_file1) +
           rename(rim_file2,tmp_file2) +
           rename(rim_file3,tmp_file3) ) < 0)
                G_fatal_error(
        "Error moving files to temp space--examine $LOCATION/rim directory.");

        /* unlink original files */
        unlink(rim_file1);
        unlink(rim_file2);
        unlink(rim_file3);

        /*rebuild from unload file */
        script = G_tempfile();
        fp1 = fopen(script,"w");
        fprintf(fp1,"cd %s\n",RIM_db_path);
        fprintf(fp1,"%s <<EOF \n",RIM_COMMAND_STR);
        fprintf(fp1,"input '%s'\n",pathname);
        fprintf(fp1,"exit\n");
        fprintf(fp1,"EOF");
        fclose(fp1);

        sprintf(cmd,"sh %s",script);
        G_system (cmd);
        unlink (script);
}

/* Give .backup information message */

if (! pack_flag) {
        fprintf(Outfile,"\nUnloaded GRASS-RIM data base %s from mapset %s\n",
                File_name, G_mapset() );
        fprintf(Outfile,"to %s\n",pathname);
}


/* open the data base again */
   /* Open the data base files through RIM */
        if (open_db(File_name,FALSE)==FALSE) {
                fprintf(Outfile,"\nThe database %s does not currently exist.", File_name);
                fprintf(Outfile,"\nPacking unsuccessful.  Your GRASS-RIM data base");
                fprintf(Outfile,"\n  has been renamed with .bakdb* extensions.\n");
                G_warning("Bailing out of pack/backup function.");
                SLEEP3;
                return(-1);
   }
   else {
      get_field_info();
      init_field_val();
      fprintf(Outfile,"\nThe data base %s is now open.\n", File_name);
          if (pack_flag) {
                unlink(tmp_file1);
                unlink(tmp_file2);
                unlink(tmp_file3);
          }
   }
return (1);
/* done */
}
