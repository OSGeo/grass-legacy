#define FILED_INFO
#include "gis.h"
#include "globals.h"
#include "make.h"
#include "rim.h"

/* This routine sends all of the Field_info data to the database */
put_field_info()
{
   int count;

   crim_w_err(FIELD_TABLE, "load fieldnames");
   for (count=0; count<Field_num; count++) {
      fil_f_table(Rim_buffer, count, Field_info[count].column_name,
         Field_info[count].rec_offset, Field_info[count].column_type,
         Field_info[count].line_num, Field_info[count].column_num,
         Field_info[count].length, Field_info[count].next_field[0],
         Field_info[count].next_field[1]);
      crimdm_w_err(FIELD_TABLE, LOAD, Rim_buffer);
   }
}

/* This routine gets all of the Field_info data from the database */
get_field_info()
{
   int status, tempint;
   char c;

   crim_w_err(FIELD_TABLE, "select from fieldnames");
   status = Field_num = 0;
   while ((status = crimdm(FIELD_TABLE, GET, Rim_buffer)) != RIM_EOT) {
      if (status != 0) rim_error(status);
      tempint = Field_num;
      ret_f_table(Rim_buffer, &tempint, Field_info[Field_num].column_name,
         &Field_info[Field_num].rec_offset, &Field_info[Field_num].column_type,
         &Field_info[Field_num].line_num, &Field_info[Field_num].column_num,
         &Field_info[Field_num].length, &Field_info[Field_num].next_field[0],
         &Field_info[Field_num].next_field[1]);
      G_squeeze(Field_info[tempint].column_name);
      G_tolcase(Field_info[tempint].column_name);

      c = Field_info[tempint].column_type;
      if (c==S_FIELD_CHAR) Site_field = tempint;
      if (c==X_FIELD_CHAR) East_field = tempint;
      if (c==Y_FIELD_CHAR) North_field = tempint;
      if ((c==X_FIELD_CHAR || c==Y_FIELD_CHAR || c==F_FIELD_CHAR)
         && Field_info[tempint].next_field[1] > 20)
            Field_info[tempint].next_field[1] = 2; /*for old data bases*/
          
      if (tempint!=Field_num)
         G_fatal_error("Corrupted field information in data base.");
      Field_num++;
      if (Field_num>=MAX_FIELDS)
         G_fatal_error("Too many fields in database during get_field_info().");

      /* zero out the rest of the array */
      for (tempint=Field_num; tempint<MAX_FIELDS; tempint++) {
         strcpy(Field_info[tempint].column_name, "");
         Field_info[tempint].rec_offset = 0;
         Field_info[tempint].column_type = '\0';
         Field_info[tempint].line_num = 0;
         Field_info[tempint].column_num = 0;
         Field_info[tempint].length = 0;
         Field_info[tempint].next_field[0] = 0;
         Field_info[tempint].next_field[1] = 0;
         Field_info[tempint].value = NULL;
      }
   }
}


