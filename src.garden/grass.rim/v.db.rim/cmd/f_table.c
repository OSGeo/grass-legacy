#define FIL_F_TABLE

#include <stdio.h>
#include "globals.h"
#include "rim.h"

#define FNAME_OFFSET (FIELD_NAME_LENGTH + sizeof(int) -1)/sizeof(int)

/* fills the buffer with a row for the Fieldname table */
fil_f_table(buffer, field_num, field_name, rec_offset, field_type, line_num,
            column_num, length, split_field0, split_field1)
   int *buffer, field_num, line_num, column_num, length, split_field0,
         split_field1, rec_offset;
   char *field_name, field_type;
{
   int offset;

   offset = 0;
   fill_buf_i(buffer+offset, &field_num);
   offset += INT_OFFSET;
   fill_buf_t(buffer+offset, field_name, FIELD_NAME_LENGTH);
   offset += FNAME_OFFSET;
   fill_buf_i(buffer+offset, &rec_offset);
   offset += INT_OFFSET;
   fill_buf_t(buffer+offset, &field_type, 1);
   offset += INT_OFFSET;
   fill_buf_i(buffer+offset, &line_num);
   offset += INT_OFFSET;
   fill_buf_i(buffer+offset, &column_num);
   offset += INT_OFFSET;
   fill_buf_i(buffer+offset, &length);
   offset += INT_OFFSET;
   fill_buf_i(buffer+offset, &split_field0);
   offset += INT_OFFSET;
   fill_buf_i(buffer+offset, &split_field1);
}

#define RET_F_TABLE


/* fills the buffer with a row for the Fieldname table */
ret_f_table(buffer, field_num, field_name, rec_offset, field_type, line_num,
            column_num, length, split_field0, split_field1)
   int *buffer, *field_num, *line_num, *column_num, *length, *split_field0,
         *split_field1, *rec_offset;
   char *field_name, *field_type;
{
   retr_buf_i(buffer, field_num);
   retr_buf_t(&buffer[1], field_name, 16);
   retr_buf_i(&buffer[5], rec_offset);
   retr_buf_c(&buffer[6], field_type);
   retr_buf_i(&buffer[7], line_num);
   retr_buf_i(&buffer[8], column_num);
   retr_buf_i(&buffer[9], length);
   retr_buf_i(&buffer[10], split_field0);
   retr_buf_i(&buffer[11], split_field1);
}

