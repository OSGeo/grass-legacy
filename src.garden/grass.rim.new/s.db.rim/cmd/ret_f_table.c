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
   retr_buf_t(&buffer[6], field_type, 1);
   retr_buf_i(&buffer[7], line_num);
   retr_buf_i(&buffer[8], column_num);
   retr_buf_i(&buffer[9], length);
   retr_buf_i(&buffer[10], split_field0);
   retr_buf_i(&buffer[11], split_field1);
}

