#include <string.h>
#include <stdio.h>
#include "Vect.h"
#include "gis.h"

int 
write_head ( FILE * dascii, struct Map_info *Map)
{
  fprintf (dascii, "ORGANIZATION: %s\n", Vect_get_organization(Map) );
  fprintf (dascii, "DIGIT DATE:   %s\n", Vect_get_date(Map) );
  fprintf (dascii, "DIGIT NAME:   %s\n", Vect_get_person(Map) );
  fprintf (dascii, "MAP NAME:     %s\n", Vect_get_map_name(Map) );
  fprintf (dascii, "MAP DATE:     %s\n", Vect_get_map_date(Map) );
  fprintf (dascii, "MAP SCALE:    %d\n", Vect_get_scale(Map) );
  fprintf (dascii, "OTHER INFO:   %s\n", Vect_get_comment(Map) );
  fprintf (dascii, "ZONE:         %d\n", Vect_get_zone(Map) );
  fprintf (dascii, "MAP THRESH:   %f\n", Vect_get_thresh(Map) );
  return (0);
}

int 
read_head ( FILE * dascii, struct Map_info *Map )
{
  char buff[1024];
  char *ptr;

  for (;;)
    {
      if (NULL == fgets (buff, sizeof (buff), dascii))
	return (0);

      for (ptr = buff; *ptr != '\n'; ptr++);	/* Remove new-line char */
      *ptr = '\0';
      
      /* Last line of header */
      if (strncmp (buff, "VERTI:", 6) == 0)	  
	return (0);
	
      if (!(ptr = G_index (buff, ':')))
	return (-1);
      ptr++;			/* Search for the start of text */
      while (*ptr == ' ')
	ptr++;

      if (strncmp (buff, "ORGANIZATION:", 12) == 0)
	  Vect_set_organization ( Map, ptr );  
      else if (strncmp (buff, "DIGIT DATE:", 11) == 0)
	  Vect_set_date ( Map, ptr );  
      else if (strncmp (buff, "DIGIT NAME:", 11) == 0)
	  Vect_set_person ( Map, ptr );  
      else if (strncmp (buff, "MAP NAME:", 9) == 0)
	  Vect_set_map_name ( Map, ptr );  
      else if (strncmp (buff, "MAP DATE:", 9) == 0)
	  Vect_set_map_date ( Map, ptr );  
      else if (strncmp (buff, "MAP SCALE:", 10) == 0)
	  Vect_set_scale ( Map, atoi (ptr) );  
      else if (strncmp (buff, "OTHER INFO:", 11) == 0)
	  Vect_set_comment ( Map, ptr );  
      else if (strncmp (buff, "ZONE:", 5) == 0 || strncmp (buff, "UTM ZONE:", 9) == 0)
	  Vect_set_zone ( Map, atoi (ptr) );  
      else if (strncmp (buff, "WEST EDGE:", 10) == 0) {}
      else if (strncmp (buff, "EAST EDGE:", 10) == 0) {}
      else if (strncmp (buff, "SOUTH EDGE:", 11) == 0) {}
      else if (strncmp (buff, "NORTH EDGE:", 11) == 0) {}
      else if (strncmp (buff, "MAP THRESH:", 11) == 0)
	  Vect_set_thresh ( Map, atof (ptr) );  
      else
        {
	  G_warning("Unknown keyword %s in vector head\n", buff);
	}
    }
  /* NOTREACHED */
}
