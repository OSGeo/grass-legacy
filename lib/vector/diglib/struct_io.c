#include <string.h>
#include "Vect.h"

/************************************************************/
/********************** struct_io.c *************************/
/************************************************************/

int 
dig_Rd_P_node (
		struct Map_info *map,
		struct P_node *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_node (map, ptr, fp);
}

int 
dig_Wr_P_node (
		struct Map_info *map,
		struct P_node *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_node (map, ptr, fp);
}

int 
dig_Rd_P_line (
		struct Map_info *map,
		struct P_line *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_line (map, ptr, fp);
}

int 
dig_Wr_P_line (
		struct Map_info *map,
		struct P_line *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_line (map, ptr, fp);
}

int 
dig_Rd_P_area (
		struct Map_info *map,
		struct P_area *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_area (map, ptr, fp);
}

int 
dig_Wr_P_area (
		struct Map_info *map,
		struct P_area *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_area (map, ptr, fp);
}

/* island stuff */
int 
dig_Rd_P_isle (
		struct Map_info *map,
		struct P_isle *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_isle (map, ptr, fp);
}

int 
dig_Wr_P_isle (
		struct Map_info *map,
		struct P_isle *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_isle (map, ptr, fp);
}

int 
dig_Rd_P_att (
	       struct Map_info *map,
	       struct P_att *ptr,
	       FILE * fp)
{
  return dig_x_Rd_P_att (map, ptr, fp);
}

int 
dig_Wr_P_att (
	       struct Map_info *map,
	       struct P_att *ptr,
	       FILE * fp)
{
  return dig_x_Wr_P_att (map, ptr, fp);
}

int 
dig_Rd_Plus_head (
		   struct Map_info *map,
		   struct Plus_head *ptr,
		   FILE * fp)
{
/*
   if (NULL == dig__get_head (map->digit))
   G_fatal_error ("Programmer did not call dig_P_init()");
 */
  return dig_x_Rd_Plus_head (map, ptr, fp);
}

int 
dig_Wr_Plus_head (
		   struct Map_info *map,
		   struct Plus_head *ptr,
		   FILE * fp)
{
  return dig_x_Wr_Plus_head (map, ptr, fp);
}


