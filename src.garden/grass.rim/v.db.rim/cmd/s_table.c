#define FIL_S_TABLE


/* This routine fills the buffer with a row for the Screenlayout table. */
fil_s_table(buffer, line_num, line_text)
   int *buffer, line_num;
   char *line_text;
{
   fill_buf_i(buffer, &line_num);
   fill_buf_t(buffer+1, line_text, 80);
}

#define RET_S_TABLE


/* This routine fills the buffer with a row for the Screenlayout table. */
ret_s_table(buffer, line_num, line_text)
   int *buffer, *line_num;
   char *line_text;
{
   retr_buf_i(buffer, line_num);
   retr_buf_t(&buffer[1], line_text, 80);
}
