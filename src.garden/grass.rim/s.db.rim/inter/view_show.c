#include "gis.h"
#include "globals.h"
#include "rim.h"

/* Show the sites on the query list one at a time */
static int counter;

view_sites()
{
struct query_site *s;
char buf[80];

 counter=1;
 s=Site_list;
 while (s>=Site_list && s<=Last_site){
        switch (show_one_site(s->site_number,1)) {
           case 0:
              sprintf(buf,"Unable to retrieve %s number %d.",
                      Field_info[Site_field].column_name, s->site_number);
              G_warning(buf);
              counter = 1;
              SLEEP3;
              break;
           case -1:
              return;
        }
        s = Site_list+(counter-1);
 }
}


/* show a site in V_ask form. Returns 0 for not found, -1 for
        CTRL-C exit by user */

show_one_site(number,status_line)
int number,status_line;
{
int error;
char cmd[100];

sprintf(cmd,"SELECT FROM DATA WHERE %s = %d",
        Field_info[Site_field].column_name, number);

error=crim(DATA_TABLE, cmd); /* do the select command */
if (error > 0) rim_error(error);
if (error == -1)
        return(0); /* no data rows */

        /* get one row */
if ( crimdm(DATA_TABLE,GET,Rim_buffer) == 0)
        {
        *cmd='\0';
        fill_values();
        V_clear();
        v_show(FALSE);  /* show screen layout constant text */
        v_const_fields(); /* show data inserted */
        if (status_line) {
        sprintf(cmd,
        "              # %d of %d currently selected.  View next #",
        counter, (int) (Last_site - Site_list) +1 );
        V_line(20,cmd);
        counter++;
        V_ques(&counter,'i',20,57,7);
        }
        V_intrpt_msg("RETURN TO MAIN MENU");
        V_intrpt_ok();
        if (! V_call() )
                return (-1);
        return(1);
        }
else
        return (0);
}

view_one_site()
{
int number;
char field[15], buf[80];

 *field = '\0';
 while(1) {
 sprintf(buf,"           Enter site number (%s) to view",
                        Field_info[Site_field].column_name);

 V_clear();
 V_line(1, make_line_2() );
 V_line(5,  buf);
 V_ques(field,'s',7,22,10);
 V_intrpt_ok();

 if (! V_call() ) return;

 if (sscanf(field,"%d",&number) != 1) {
        G_warning("Input not a number.  No action taken.");
        SLEEP3;
   }
 else
        if (show_one_site(number,0) == 0){
                G_warning("Requested site not found in data base.");
                SLEEP3;
        }
        else {
                number++;
                sprintf(field,"%d",number);
        }
 }
}
