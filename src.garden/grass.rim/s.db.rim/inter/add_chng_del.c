#define ADD_CHNG

#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"

extern short int field_changed[MAX_FIELDS];


v_add(number)
   int number;
{
   add_init();
   *((int *) Field_info[Site_field].value) = number;
   V_clear();
   v_show(FALSE);  /* show the screen definition */
   v_ques_fields1();  /* initialize the fields to be input */
   V_intrpt_ok();
   V_intrpt_msg("RETURN TO THE MAIN MENU");
   if (V_call()) add_done();
   else return(FALSE);
   return(TRUE);
}


int get_max_site()
{
   int i, status;

   i = 0;
   if ((status=crim(DATA_TABLE, "select from data"))==RIM_EOT)
       return(0);
   if (status!=0) rim_error(status);

   if (crimdm(DATA_TABLE,GET,Rim_buffer)!=0)
      return(0);
   i = Rim_buffer[0];
   while ((status=crimdm(DATA_TABLE,GET,Rim_buffer))==0) {
      if (i<Rim_buffer[0]) i = Rim_buffer[0];
   }
   if (status!=RIM_EOT) rim_error(status);
   return(i);
}

add_a_site()
{
   int field, largest_site;
   char buf[80], buf2[80], tempstr[200];

   largest_site = get_max_site();
   sprintf(buf,"           Enter site number (%s) to add",
                        Field_info[Site_field].column_name);
   while (1) {
      sprintf(buf2, "    %d is the largest %s in the current data base.",
              largest_site, Field_info[Site_field].column_name);
      field = largest_site + 1;
      V_clear();
      V_line(1, make_line_2() );
      V_line(3, buf2);
      V_line(5,  buf);
      V_ques(&field,'i',7,22,10);
      V_intrpt_ok();
      V_intrpt_msg("RETURN TO THE MAIN MENU");

      if (! V_call() ) return;

      /* cannot have more than one record with a given site number */
      sprintf(tempstr, "select from data where %s = %d",
              Field_info[Site_field].column_name, field);
      if (crim(DATA_TABLE, tempstr) != RIM_EOT) {
         sprintf(tempstr, "Site number (%d) duplicated one in the database, not added.\n", *((int *) Field_info[Site_field].value));
         G_warning(tempstr);
         SLEEP3;
      }
      else if (v_add(field)==FALSE) return;
      if (field>largest_site) largest_site = field;
   }
}



v_change()
{
   int i;

   change_init();
   for (i=0;i<Field_num;i++) field_changed[i]=TRUE;
   crimdm_w_err(DATA_TABLE, GET, Rim_buffer);
   fill_values();

   V_clear();
   v_show(FALSE);  /* show the screen definition */
   v_ques_fields1();  /* initialize the fields to be input */
   V_intrpt_ok();
   V_intrpt_msg("RETURN TO THE MAIN MENU");
   if (V_call()) change_done();
   else return(FALSE);
   return(TRUE);
}


change_a_site()
{
   int number, field;
   char buf[80], tempstr[200];

   field = 0;
   sprintf(buf,"           Enter site number (%s) to change",
                        Field_info[Site_field].column_name);
   while (1) {
      V_clear();
      V_line(1, make_line_2() );
      V_line(5,  buf);
      V_ques(&field,'i',7,22,10);
      V_intrpt_ok();
      V_intrpt_msg("RETURN TO THE MAIN MENU");

      if (! V_call() ) return;

      /* site should already be defined */
      sprintf(tempstr, "select from data where %s = %d",
              Field_info[Site_field].column_name, field);
      if (crim(DATA_TABLE, tempstr) == RIM_EOT) {
         sprintf(tempstr,"%s number (%d) not in the database, cannot be changed.\n",Field_info[Site_field].column_name,field);
         G_warning(tempstr);
         SLEEP3;
      }
      else {
         if (v_change()==FALSE) return;
      }
   }
}





delete_a_site()
{
   int dummy;
   char record[10], buf2[INP_SIZ];
   char buf[INP_SIZ];

   sprintf(buf,"           Enter site number (%s) to delete",
                        Field_info[Site_field].column_name);
   *record = '\0';
   while (1) {
      V_clear();
      V_line(1, make_line_2() );
      V_line(5,  buf);
      V_line(6, "           or 'list' to delete the query list.");
      V_ques(record,'s',8,22,10);
      V_intrpt_ok();
      V_intrpt_msg("RETURN TO THE MAIN MENU");

      if (! V_call() ) return;

      G_squeeze(record);
      if (sscanf(record, "%d", &dummy)==1)
         delete_site(record);
      else if (strcmp(record,"list")==0)
         delete_found_done();
      else {
         G_warning("Input to be deleted was not an integer or 'list'.");
      }
      SLEEP3;
   }
}
