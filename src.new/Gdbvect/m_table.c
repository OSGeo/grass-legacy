#define FIL_M_TABLE

#include <stdio.h>
#include "globals.h"

#define NAME_OFFSET (MAP_NAME_LENGTH + sizeof(int) - 1)/sizeof(int)

/* This routine fills the buffer with a row for the Referencemaps table. */
fil_m_table(buffer, id_num, map_name, mapset)
   int *buffer, id_num;
   char *map_name, *mapset;
{
   fill_buf_i(buffer, &id_num);
   fill_buf_t(buffer+1, map_name, 20);
   fill_buf_t(buffer+NAME_OFFSET+1, mapset, 20);
}

#define RET_M_TABLE


/* This routine fills the buffer with a row for the Referencemaps table. */
ret_m_table(buffer, id_num, map_name, mapset)
   int *buffer, *id_num;
   char *map_name, *mapset;
{
   retr_buf_i(buffer, id_num);
   retr_buf_t(&buffer[1], map_name, 20);
   retr_buf_t(&buffer[NAME_OFFSET+1], mapset, 20);
}
