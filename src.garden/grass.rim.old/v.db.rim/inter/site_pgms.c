/* Display record locations, print records, and make vect/site map routines */

#include "gis.h"
#include "globals.h"
#include "make.h"

display_records()
{
char *device;

device = G__getenv("MONITOR");
if (device == NULL){
        G_warning("Graphics Monitor not selected.");
        SLEEP3;
        }
else
   disp_maps("               SELECTION MENU FOR ITEMS TO DISPLAY");
}

/* write out the query list to site_lists directory */
do_vect_site()
{
FILE *fp;
char buf[200];
char vect_name[20],att[20],cat[60];
char new_site_name[20],old_site_name[20];
char site_field[60];
int i, f;

*new_site_name= *old_site_name= *vect_name='\0';
*site_field= *att= *cat= '\0';

strcpy(site_field,Field_info[Sequence_field].column_name);
while (1) {
        V_clear();
        V_line(1, make_line_2() );
        sprintf(buf, "                 %d records selected by query/find.",
                 (Last_record - Record_list)+1);
        V_line(2, buf);
        V_line(4,
 "                    INFORMATION FOR VECTOR MAP TO BE MADE:");
        V_line(6,
 "                 Name of new vector file from selected records");
        V_line(7,
 "                     Field name (or value) to use for attribute/label:");
        V_line(8,
 "                     Field name (or fixed text in quotes) to use for");
        V_line(9,
 "                     category description:");

        V_line(12,
 "                    INFORMATION FOR SITE LIST(S) TO BE MADE:");
        V_line(14,
 "                 Name of new site list from selected records");
        V_line(15,
 "                 Name of existing site list to append selected records");
        V_line(17,
 "                     Field name (or fixed text in quotes) to use for");
        V_line(18,
 "                  category description:");
        V_ques(vect_name,'s',6,1,14);
        V_ques(att,'s',7,3,16);
        V_ques(cat,'s',10,3,59);
        V_ques(new_site_name,'s',14,1,16);
        V_ques(old_site_name,'s',15,1,16);
        V_ques(site_field,'s',19,3,59);
        V_intrpt_ok();
        if (! V_call() ) return;

        printf("\n  Working ...\n");
        /* handle vector case first */
        if (*vect_name) {
          if (!*att && *cat){
           sprintf(buf,"\nFor vector map: Need to specify attribute field, if");
           strcat(buf, "\ncategory description is specified for vector field.");
           G_warning(buf);
           SLEEP3;
           continue;
          }
          sprintf(buf,".vect %s %s %s",vect_name,att,cat);
          if (vect_map(buf) == -1) continue;
        }
        /* be sure of site files */
        if (*old_site_name) {
                if ((fp=G_fopen_sites_old(old_site_name,G_mapset()))!=NULL )
                        fclose(fp);
                else {
                        G_warning("Couldn't open old site_list requested.");
                        SLEEP3;
                        continue;
                }
        }
        if (*new_site_name) {
                if ((fp=G_fopen_sites_new(new_site_name))!=NULL )
                        fclose(fp);
                else {
                        G_warning("Couldn't open new site_list requested.");
                        SLEEP3;
                        continue;
                }
        }
        if (*new_site_name) {
                sprintf(buf,".site_list %s %s",new_site_name, site_field);
                if (site_list(buf) == -1)
                        continue;
        }
        if (*old_site_name) {
                sprintf(buf,".site_list %s %s",old_site_name, site_field);
                if (site_list(buf) == -1)
                        continue;
        }
  break;
  } /* end of while() */
}


FILE *print_file;  /* for the print() */
print_records()
{
char *file, real_file[70], flag[4];
char cmd[80];

*real_file = '\0';
*flag = '\0';

while (! *real_file) {
        V_clear();
        V_line(0,"             OUTPUT (PRINT) DESTINATION SELECTION");
        V_line(2, make_line_2() );
        sprintf(cmd, "   %d records selected by query/find",
                                (Last_record - Record_list)+1);
        V_line(4, cmd);
        V_line(6, "Enter file (or path/file) for output, lp for line printer");
        V_line(7, "    (current directory assumed if full path not given)");
        V_ques(real_file,'s',8,3,65);
        V_line(11,
                "Enter -l or -a for list format rather than screen layout format");
        V_line(12,"    (see .print in v.db.rim manual for information)");
        V_ques(flag,'s',13,2,2);
        V_intrpt_ok();

        if (! V_call() ) return;
        }
G_squeeze(real_file);
G_squeeze(flag);
if (strcmp(flag,"-l") && strcmp(flag,"-a") )
*flag = '\0';
file = G_tempfile();
print_file = fopen (file,"w");
sprintf(cmd,".print %s",flag);
print(cmd);
fclose(print_file);
if (strcmp(real_file,"lp") )
        sprintf(cmd,"mv %s %s",file,real_file);
else
        sprintf(cmd,"cat %s | lp", file);
G_system(cmd);
unlink(file);

}
