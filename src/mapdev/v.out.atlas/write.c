/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       v.out.atlas 
 * AUTHOR(S):    R. L. Glenn, unknown GRASS author
 * PURPOSE:      write out vectors
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include "gis.h"
#include "Vect.h"

write_lines(name,mapset,map,lines_file,ctype)
     char *name, *mapset;
     struct Map_info map;
     FILE *lines_file;
     char *ctype;

{
  FILE *dig_fp;
  double ax, ay, E, N;
  double *xptr, *yptr;
  int area, line, att_num, zone; 
  int i, n, catflag=-1;
  int NumAreas, NumLines, type, typ;
  char att[1000];
  struct Categories cats;
  struct dig_head head;
  struct line_pnts *Points;
  
  
  if ((catflag=G_read_vector_cats(name,mapset,&cats)) == -1)
    G_warning("   error %d reading vector cats\n",catflag);
  
  if(Vect_open_old(&map, name, mapset) != 2)
    G_fatal_error("Vector file was not opened at level 2.");

  Points = Vect_new_line_struct();
  zone = map.head.plani_zone;
  
  /* LOOP through all lines in the dig file */
  if(strcmp(ctype,"A")==0 || strcmp(ctype,"a")==0) type = AREA;
  if(strcmp(ctype,"L")==0 || strcmp(ctype,"l")==0) type = LINE;
  switch(type)
    {
    case AREA: 
      NumAreas = V2_num_areas(&map);
      for (area = 1; area <= NumAreas; area++)
	{
	  att_num = V2_area_att( &map, area);
	  G_strcpy(att, G_get_cat( att_num, &cats));
	  typ = Vect_get_area_points(&map, area, Points);
	  if(typ==-1)
	    G_fatal_error("Out of Memory.\n");
	  if(typ==-2) return (0);
	  
	  fprintf(lines_file,"\"%s\",\"\",", att);
	  fprintf(lines_file,"%d\n", Points->n_points);
	  xptr = Points->x;
	  yptr = Points->y;
	  while (Points->n_points--){
	    E = *xptr;
	    N = *yptr;
	    fprintf(lines_file, "%.4lf,%.4lf\n", E, N);
	    xptr++;
	    yptr++;
	  }
	}
      break;
      
    case LINE:
      NumLines = V2_num_lines(&map);
      for (line= 1; line <= NumLines; line++)
	{
	  att_num = V2_line_att(&map, line);
	  G_strcpy(att,G_get_cat(att_num, &cats));
	  typ = V2_read_line(&map, Points, line);
	  if(typ==-1)
	    G_fatal_error("Out of Memory.\n");
	  if(typ==-2) return(0);
	  fprintf(lines_file,"\"%s\",\"\",", att);
	  fprintf(lines_file,"-%d\n", Points->n_points);
	  xptr = Points->x;
	  yptr = Points->y;
	  while (Points->n_points--){
	    E = *xptr;
	    N = *yptr;
	    fprintf(lines_file, "%.4lf,%.4lf\n", E, N);
	    xptr++;
	    yptr++;
	  }
	}
      break;
    }
  Vect_destroy_line_struct(Points);
  Vect_close(&map);
  return (0);
}
