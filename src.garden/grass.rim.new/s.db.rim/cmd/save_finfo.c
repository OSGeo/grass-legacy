#define SAVE_FINFO

#include <stdio.h>
#include "globals.h"


save_finfo(number, col_name, rec_offset, f_type, line_num, col_num,
            f_length, next_field0, next_field1)
   int number, rec_offset, line_num, col_num, f_length, next_field0,next_field1;
   char *col_name, f_type;
{
   strcpy(Field_info[number].column_name, col_name);
   Field_info[number].rec_offset = rec_offset;
   Field_info[number].column_type = f_type;
   Field_info[number].line_num = line_num;
   Field_info[number].column_num = col_num;
   Field_info[number].length = f_length;
   Field_info[number].next_field[0] = next_field0;
   Field_info[number].next_field[1] = next_field1;
}



