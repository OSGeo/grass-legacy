
#define RETR_BUF


/* This file consists of several functions that retrieve a data value from
   the integer buffer sent from RIM as a table row.
   */


/* retrieve an integer from the row */
retr_buf_i(buffer_loc, int_data)
   int *buffer_loc, *int_data;
{
   *int_data = *buffer_loc;
}


/* retrieve a float from the row */
retr_buf_f(buffer_loc, float_data)
   int *buffer_loc;
   float *float_data;
{
   float *temp;

   temp = (float *) buffer_loc;
   *float_data = *temp;
}


/* retrieve a double from the row */
retr_buf_d(buffer_loc, double_data)
   int *buffer_loc;
   double *double_data;
{
   double *temp;

   temp = (double *) buffer_loc;
   *double_data = *temp;
}


/* retrieve a text string from the row */
retr_buf_t(buffer_loc, text_data, text_length)
   int *buffer_loc, text_length;
   char *text_data;
{
   strasc_(text_data, buffer_loc, &text_length, text_length);
   text_data[text_length] = 0;
}


/* added 1 jul 91  -dpg */
/* retrieve a text string from the row */
retr_buf_c(buffer_loc, text_data)
   int *buffer_loc;
   char *text_data;
{
   int text_length = 1;

   strasc_(text_data, buffer_loc, &text_length, text_length);
}
