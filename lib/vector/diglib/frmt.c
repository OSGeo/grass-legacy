#include <string.h>
#include <stdio.h>
#include "Vect.h"
#include "gis.h"

/* Read vector format.
*
*  Returns: format number
*           -1 on error
*/           
int 
dig_read_frmt_ascii ( FILE *dascii, struct Format_info *finfo)
{
  char buff[1024], buf1[1024];
  char *ptr;
  int  frmt = -1;

  /* read first line which must be FORMAT: */
  if ( NULL != fgets (buff, sizeof (buff), dascii) ) {
      G_chop (buff);
      
      if (!(ptr = G_index (buff, ':'))) {
          G_warning ("Vector format not recognized: %s", buff);
          return (-1);	 
      }

      strcpy ( buf1, buff ); buf1[ptr - buff] = '\0';
      
      ptr++;			/* Search for the start of text */
      while (*ptr == ' ') ptr++;

      if (strcmp (buf1, "FORMAT" ) == 0) {
          if (strcmp (ptr, "shape") == 0) {
	      frmt = GV_FORMAT_SHAPE; 
	  } else if (strcmp (ptr, "postgis") == 0) {
	      frmt = GV_FORMAT_POSTGIS; 
	  }	  
      }
  }    
  if ( frmt == -1) {
      G_warning ("Vector format not recognized: %s", buff);
      return (-1);	 
  }

  /* init format info values */
  switch ( frmt ) {
      case GV_FORMAT_SHAPE :
	  finfo->shp.file = NULL;
	  finfo->shp.cat_col = NULL;
          break;
	  
#ifdef HAVE_POSTGRES	  
      case GV_FORMAT_POSTGIS :
	  finfo->post.host       = NULL;
	  finfo->post.port       = NULL;
	  finfo->post.database   = NULL;
	  finfo->post.user       = NULL;
	  finfo->post.password   = NULL;
	  finfo->post.geom_table = NULL;
	  finfo->post.cat_table  = NULL;
	  finfo->post.geom_id    = G_store ("id");
	  finfo->post.geom_type  = G_store ("type");
	  finfo->post.geom_geom  = G_store ("geom");
	  finfo->post.cat_id     = G_store ("id");
	  finfo->post.cat_field  = G_store ("field");
	  finfo->post.cat_cat    = G_store ("cat");
          break;
#endif
  }
	  
  while ( (NULL != fgets (buff, sizeof (buff), dascii) ) )
  {
      G_chop (buff);
      
      if (!(ptr = G_index (buff, ':'))) {
	  G_warning ("Format definition is not correct: %s", buff);
	  continue;
      }

      strcpy ( buf1, buff ); buf1[ptr - buff] = '\0';

      ptr++;			/* Search for the start of text */
      while (*ptr == ' ') ptr++;
     
      switch ( frmt ) {
          case GV_FORMAT_SHAPE :
              if (strcmp (buf1, "SHAPE") == 0)
	          finfo->shp.file = G_store (ptr);
	      else if (strcmp (buf1, "CAT_COLUMN") == 0)
	          finfo->shp.cat_col = G_store (ptr);
              else
	          G_warning ("unknown keyword '%s' in vector format file\n", buff);

	      break;

#ifdef HAVE_POSTGRES	      
          case GV_FORMAT_POSTGIS :
              if (strcmp (buf1, "HOST") == 0)
	          finfo->post.host = G_store (ptr);
	      else if (strcmp (buf1, "PORT") == 0)
	          finfo->post.port = G_store (ptr);
	      else if (strcmp (buf1, "DATABASE") == 0)
	          finfo->post.database = G_store (ptr);
	      else if (strcmp (buf1, "USER") == 0)
	          finfo->post.user = G_store (ptr);
	      else if (strcmp (buf1, "PASSWORD") == 0)
	          finfo->post.password = G_store (ptr);
	      else if (strcmp (buf1, "GEOM_TABLE") == 0)
	          finfo->post.geom_table = G_store (ptr);
	      else if (strcmp (buf1, "CAT_TABLE") == 0)
	          finfo->post.cat_table = G_store (ptr);
	      else if (strcmp (buf1, "GEOM_ID") == 0)
	          finfo->post.geom_id = G_store (ptr);
	      else if (strcmp (buf1, "GEOM_TYPE") == 0)
	          finfo->post.geom_type = G_store (ptr);
	      else if (strcmp (buf1, "GEOM_GEOM") == 0)
	          finfo->post.geom_geom = G_store (ptr);
	      else if (strcmp (buf1, "CAT_ID") == 0)
	          finfo->post.cat_id = G_store (ptr);
	      else if (strcmp (buf1, "CAT_FIELD") == 0)
	          finfo->post.cat_field = G_store (ptr);
	      else if (strcmp (buf1, "CAT_CAT") == 0)
	          finfo->post.cat_cat = G_store (ptr);
              else
	          G_warning ("unknown keyword '%s' in vector format file\n", buff);

	      break;
#endif
      }
    }

    return frmt;
}

