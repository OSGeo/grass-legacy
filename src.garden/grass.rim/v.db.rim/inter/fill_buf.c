#define FILL_BUF

#include "gis.h"

/* This file consists of several functions that put a data value into
   the integer buffer to passed to RIM as a table row.
   */


/* put an integer into the row */
fill_buf_i(buffer_loc, int_data)
   int *buffer_loc, *int_data;
{
   *buffer_loc = *int_data;
}


/* put a float into the row */
fill_buf_f(buffer_loc, float_data)
   int *buffer_loc;
   float *float_data;
{
   int *temp;

   temp = (int *) float_data;
   *buffer_loc = *temp;
}


/* put a double into the row */
fill_buf_d(buffer_loc, double_data)
   int *buffer_loc;
   double *double_data;
{
   int *temp;

   temp = (int *) double_data;
   *buffer_loc = *temp;
   *(buffer_loc+1) = *(temp+1);
}


/* put a text string into the row */
fill_buf_t(buffer_loc, text_data, text_length)
   int *buffer_loc, text_length;
   char *text_data;
{
   char *text_buf;
   int i;

   text_buf = G_malloc(text_length);
   strcpy(text_buf, text_data);
   for (i=strlen(text_data); i<text_length; i++) text_buf[i]=' ';

   asctxt_(buffer_loc, &text_length, text_buf, text_length);

   free(text_buf);
}

