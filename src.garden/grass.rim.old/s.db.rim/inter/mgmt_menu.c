#include "gis.h"
#include "globals.h"

#define DONE 0

char *make_line_2();


mgmt_menu()
{
int choice, n;
char buf[100], buf2[100];

   while (1) {
        choice = DONE;

/* create screen */
        V_clear();
        V_line( 1,"    s.db.rim          DATA  BASE  MANAGEMENT  MENU");
        V_line( 2, make_line_2() );
        n = 4;
        V_line( n++,"      1   Make a New Data Base in Current Mapset");
        V_line( n++,"      2   List Available Data Bases");
        V_line( n++,"      3   Remove (PERMANENTLY) Data Base from Current Mapse t");
/*      V_line( n++,"      4   Rename a Data Base"); */
        V_line( n++,"      4   Recover a Data Base from a RIM ASCII File");
if (*File_name==NULL) {
        V_line( ++n,"  Choices 5 to 8 not available until a data base is opened.  ");
        n+=2;
        }
        V_line( n++,"      5   Show Screen Layout of Current Data Base");
        V_line( n++,"      6   Backup (UNLOAD) Data Base to RIM ASCII Format Fil e");
        V_line( n++,"      7   Pack the Current Data Base");
        V_line( n++,"      8   Read a site_list into the Current Data Base");

        sprintf(buf2,"      %d   Return to Main Menu",DONE);
        V_line(++n, buf2);

        V_line(n+2,"    Your selection");
        V_ques(&choice,'i',n+2,1,2);

/* Show screen and get choice */
        V_intrpt_ok();
        if (! V_call() ) return;

        switch (choice) {
                case 1  : make_new(); break;
                case 2  : do_list(); break;
                case 5  : if(ck_open()) do_show(); break;
                case 6  : if(ck_open()) do_backup(); break;
                case 4  : reload(); break;
                case 7  : if(ck_open() && rw_ok()) do_pack(); break;
/*              case 4  : do_rename(); break; */
                case 3  : do_remove(); break;
                case 8  : if(ck_open() && rw_ok()) do_read_sites(); break;
                case DONE : return;
                default  : G_warning("Not a valid choice.  Try again.");
                           SLEEP3;
        } /* end of switch */

   } /* end of while */

} /* end of mgmt_menu() */


do_backup()  /* s.db.rim driver to backup the current data base */
{
   char buffer[70];

   strncpy(buffer,".backup   ", 8);
   buffer[8]='\0';

   V_clear();
   V_line(2, make_line_2());
   V_line(4,"      Enter file name for backup text file");
   V_ques(&buffer[8],'s',6,6,60);
   V_intrpt_ok();

   if (V_call()) {

        if (backup(buffer)==-1 )
        G_warning("Problems with backing up.  Backup of data base NOT DONE");

        else
                printf("\n\n\nBackup accomplished to %s\n\n",buffer);

        SLEEP3; SLEEP3;
        }
} /* end of do_backup() */

do_pack()  /* s.db.rim driver to pack a data base */
{
   printf("packing...\n");
   if (backup("PACK") == -1 )
        G_warning("Problems in packing.  Pack of data base NOT DONE");

   else
        printf("\n\n\nPack accomplished.");

   SLEEP3; SLEEP3;
}  /* end of do_pack() */

/* reload a data base from backup text file */
/* Be sure no binary files exist, cd to $LOCATION/rim/sites, run RIM
   and input backup file specified by user. */

reload()
{
   V_clear();
   V_line( 1,"                   RELOAD");
   V_line( 4,"   See the s.db.rim manual for instructions on reloading a");
   V_line( 6,"   data base from a RIM backup text file.");
   V_line( 8,"   (This is done external to s.db.rim for reasons of data");
   V_line(10,"   integrity and security.)");
   V_intrpt_ok();

   V_call();
}


