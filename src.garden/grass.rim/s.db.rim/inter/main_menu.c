/* Present main menu and return choice */
#include "gis.h"
#include "globals.h"
#include "parse.h"

#define OPEN 1
#define DONE 0

/* create line two of menus */

static char line_2[80];

char *
make_line_2()
{
int offset;
char work[100];

*line_2 = '\0';
if (*File_name) {
        sprintf(work,"Data base <%s>%s in mapset <%s> open.  %d records.",
                File_name,
                (parser(P_INST_RET_MODE,"")==TRUE) ? " (READONLY)" : "",
                RIM_db_mapset,
                Number_of_sites);
        }
else sprintf(work,"No Data Base Currently Open." );
if(strlen(work)<78) {
        offset = (78-strlen(work))/2;
        while (offset-- > 0) strcat(line_2," ");
        }
strcat(line_2,work);
return line_2;
}


main_menu()
{
int n,choice,nsites;
char text[100];

while (1)
{
nsites = 0;
if (! *File_name)
        choice = OPEN;
else {
        choice = DONE;
        if (Last_site) nsites = (Last_site - Site_list) + 1;
        }
                /* create screen */
V_clear();
n = 0;
V_line(n++,"        s.db.rim            MAIN  MENU          (V 1.4)");
V_line(n++, make_line_2() );
V_line(++n,
        "           ------------------ Select A Data Base ---------------------");
V_line(n++,"                 1  Open a data base");
V_line(n++,"                 2  List available data bases");
sprintf(text,
        "           ----------- Retrieve/Output Site Records (%d currently) ---" ,
        nsites);
V_line(n++,text);
V_line(n++,"                 3  Find sites in proximity to a Target point");
V_line(n++,"                 4  Query to select site records (SQL)");
V_line(n++," -------         5  Show selected site records on Terminal");
V_line(n,  "                 6  Display maps/selected sites on graphics terminal ");
V_ques(&choice,'i',n++,4,2);
V_line(n++," -------         7  Output selected site records to Printer or File");
V_line(n++,"                 8  Create a site_list from selected records");
V_line(n++,
        "           ------------------- Add/Edit Site Records -----------------");
V_line(n++,"                 9  View a single site record");
V_line(n++,"                10  Add a new site record");
V_line(n++,"                11  Change an existing site record");
V_line(n++,"                12  Delete one record or all selected records");
V_line(n++,
        "           ------- Other functions -- Shell Command -- Exit ----------");
V_line(n++,"                13  Make a new data base & Management Functions");
V_line(n++,
           "                14  Execute a shell command");
V_line(n++,"                 0  Done -- Exit from s.db.rim");
V_intrpt_ok();
V_intrpt_msg("EXIT THIS PROGRAM");

/* Show screen and get choice */

if (! V_call()) return;

switch (choice) {
case 0 : return;
case 1 : do_open(); break;
case 2 : do_list(); break;
                /* query/find Site_list cases */
case 3 : if (ck_open()) do_find(); break;
case 4 : if (ck_open()) do_query(); break;
case 5 : if (ck_open()&&ck_sites()) view_sites(); break;
case 6 : if (ck_open()) display_sites(); break;
case 7 : if (ck_open()&&ck_sites()) print_sites(); break;
case 8 : if (ck_open()&&ck_sites()) do_site_list(); break;
                /* single sites by number cases */
case 9 : if (ck_open()) view_one_site(); break;
case 10 : if (ck_open() && rw_ok()) add_a_site(); break;
case 11 : if (ck_open() && rw_ok()) change_a_site(); break;
case 12 : if (ck_open() && rw_ok()) delete_a_site(); break;
                /* other cases */
case 13 : mgmt_menu(); break;
case 14 : sh_cmd(); break;
} /* end of switch */

} /* end of while */

} /* end of main_menu() */

ck_open()
{
if (*File_name) return (1);
else
        {
        G_warning("Your choice not possible until a data base is open.");
        SLEEP3;
        return (0);
        }
}

ck_sites()
{
if (Last_site != NULL) return (1);
else
        {
        G_warning("Your choice not possible until a Query or Find has been run.");
        SLEEP3;
        return (0);
        }
}

rw_ok()
{
        if (parser(P_INST_RET_MODE,"")==TRUE) {
        G_warning("Data base modifications not allowed (READONLY)" );
        SLEEP3;
        return 0;
        }
        return 1;
}

sh_cmd()
{
 char cmd[500];
 while (1) {
      printf(
"\ns.db.rim: Enter a one-line Shell Command (<RETURN> to go to main menu)\n\n#") ;
   if (! G_gets(cmd)) return;
   if (*cmd) G_system(cmd);
      else return;
 }
}
