/* Display site locations, print sites, and make site_list routines */

#include "gis.h"
#include "globals.h"
#include "make.h"


display_sites()
{
char *device;

device = G__getenv("MONITOR");
if (device == NULL){
        G_warning("Graphics Monitor not selected.");
        SLEEP3;
        }
else {
        disp_maps("               SELECTION MENU FOR ITEMS TO DISPLAY");
     } /* end of else */
}

/* write out the site list to site_lists directory */
do_site_list()
{
char *mapset,site_name[50],cmd[100], old[3],new[3], extra_field[20];
int i, f;

*old = *new = '\0';
mapset = NULL; *site_name = '\0';

strcpy(extra_field,Field_info[Site_field].column_name);
while (mapset==NULL || !*site_name) {
        V_clear();
        V_line(1, make_line_2() );
        sprintf(cmd, "                 %d sites selected by query/find.",
                 (Last_site - Site_list)+1);
        V_line(3, cmd);
        V_line(6,
 "      Mark one of these choices with x");
        V_line(8,
 "          Write selected sites to new site list");
        V_line(9,
 "          Append selected sites to existing site list in your mapset");
        V_ques(new,'s',8,7,1);
        V_ques(old,'s',9,7,1);
        V_line(12,
 "      Enter the field name to use for #comment in the site list");
        V_ques(extra_field,'s',14,30,16);
        V_intrpt_ok();
        if (! V_call() ) return;

        /* be sure field requested exists */
        G_tolcase(extra_field);
        for (i=0, f=0; i<Field_num; i++)
                if (!strcmp(extra_field,Field_info[i].column_name))
                        { f=1; break;}
        if (!f) {
                G_warning("Invalid field name for #comment.  Current fields are: ");
                for (i=0, f=0; i<Field_num; i++) {
                        if (! (i%4)) fprintf(stderr,"\n");
                        fprintf(stderr,"%-19s",Field_info[i].column_name);
                        }
                fprintf(stderr,"\n\nHit return to continue");
                G_gets(cmd);
                continue;
                }

        if (*old == 'x')
                mapset = G_ask_sites_in_mapset("Enter name of site list to append to",
                                site_name);
        if (*new == 'x')
                mapset = G_ask_sites_new("", site_name);
        }
if (mapset != NULL) {
        sprintf(cmd,".site_list %s %s",site_name, extra_field);
        site_list(cmd);
        }
}


FILE *print_file;  /* for the print() */
print_sites()
{
char *file, real_file[70], flag[4];
char cmd[80];

*real_file = '\0';
*flag = '\0';

while (! *real_file) {
        V_clear();
        V_line(0,"  s.db.rim      OUTPUT (PRINT) DESTINATION SELECTION");
        V_line(2, make_line_2() );
        sprintf(cmd, "   %d site records selected by query/find",
                                (Last_site - Site_list)+1);
        V_line(4, cmd);
        V_line(6, "Enter file (or path/file) for output, lp for line printer");
        V_line(7, "    (current directory assumed if full path not given)");
        V_ques(real_file,'s',8,3,65);
        V_line(11,
                "Enter -l or -a for list format rather than screen layout format ");
        V_line(12,"    (see .print in s.db.rim manual for information)");
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
if (strcmp(real_file,"lp") ) {
        sprintf(cmd,"mv %s %s",file,real_file);
	G_system(cmd);
}
else {
        sprintf(cmd,"cat %s | lp", file);
	G_system(cmd);
	unlink(file);
}
}
