#define LAYOUT
#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"


/* This file has the functions that setup a Vask screen for the data base
   screen layout definition. */

/* Display the background text for the screen layout in Vask screen.
   If fields is false the field definitions are stripped out of the text.*/

char screen[19][INP_SIZ];

v_show(fields)
   int fields;
{
   int status, line_num, i;

   crim_w_err(SCREEN_TABLE, "select from screenlayout");

   i = 0;
   while ((status = crimdm(SCREEN_TABLE, GET, Rim_buffer))!=RIM_EOT) {
      if (status!=0) rim_error(status);
      ret_s_table(Rim_buffer, &line_num, screen[i]);
      if (fields==FALSE) strip_fields(screen[i]);
      screen[i][INP_SIZ-1] = '\0';
      V_line(line_num, screen[i]);
      i++;
   }
   V_line(21, make_line_2());
}


#define VASK_F_STR 'd'
#define VASK_I_STR 'i'
#define VASK_T_STR 's'

/* This routine is passed a character representing a v.db.rim type and
   it returns the a character which represents the Vask data type
   that corresponds to the input character in the form definition code. */
char vask_field_type(type_c)
   char type_c;
{
  switch (type_c)
    {
    case M_FIELD_CHAR:
    case I_FIELD_CHAR:
    case S_FIELD_CHAR: return(VASK_I_STR);
    case F_FIELD_CHAR:
    case X_FIELD_CHAR:
    case Y_FIELD_CHAR: return(VASK_F_STR);
    case V_FIELD_CHAR:
    case T_FIELD_CHAR: return(VASK_T_STR);
    default: return('\0');
    }
}



/* Display a field as a constant in the Vask screen */
v_const_field(f_num, const_ptr)
int f_num;
char * const_ptr;
{
   V_const(const_ptr, vask_field_type(Field_info[f_num].column_type),
           Field_info[f_num].line_num, Field_info[f_num].column_num,
           Field_info[f_num].length);
}


v_ques_field(f_num)
int f_num;
{
    V_ques(Field_info[f_num].value,
           vask_field_type(Field_info[f_num].column_type),
           Field_info[f_num].line_num, Field_info[f_num].column_num,
           Field_info[f_num].length);
}



/* Setup all the fields as question fields in the Vask screen */
v_ques_fields()
{
   int count;

   for (count=0; count<Field_num; count++)
      v_ques_field(count);
}


/* Setup all the fields as question fields in the Vask screen, except
   the sequence field, make that a constant */
v_ques_fields1()
{
   int count;

   for (count=0; count<Field_num; count++) {
      if (count==Sequence_field)
         v_const_field(count, Field_info[count].value);
      else
         v_ques_field(count);
   }
}

/* Setup all the fields as constant fields in the Vask screen */
v_const_fields()
{
   int count;

   for (count=0; count<Field_num; count++)
      v_const_field(count, Field_info[count].value);
}


/* Setup all the fields as blank constant fields in the Vask screen */
v_blank_fields()
{
   int count, tempi;
   double tempd;

   tempi = 0;
   tempd = 0.0;

   for (count=0; count<Field_num; count++)
      switch (Field_info[count].column_type) {
      case M_FIELD_CHAR:
      case I_FIELD_CHAR:
      case S_FIELD_CHAR: v_const_field(count, "0");
                         break;
      case F_FIELD_CHAR:
      case X_FIELD_CHAR:
      case Y_FIELD_CHAR: v_const_field(count, "0.0");
                         break;
      case V_FIELD_CHAR:
      case T_FIELD_CHAR: v_const_field(count, "");
      }
}

