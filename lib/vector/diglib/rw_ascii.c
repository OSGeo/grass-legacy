/*
 * $Id$
 *
 * 2.1  6/26/87  
 */

#include <string.h>
#include <stdio.h>
#include "Vect.h"
#include "gis.h"

/*
   *   2.0 digit files have "UTM ZONE",  3.0 digit files have "ZONE"
 */

int 
dig_write_head_ascii (
		       FILE * dascii,
		       struct dig_head *head)
{
  char buf[100];

  fprintf (dascii, "ORGANIZATION: %s\n", head->organization);
  fprintf (dascii, "DIGIT DATE:   %s\n", head->date);
  fprintf (dascii, "DIGIT NAME:   %s\n", head->your_name);
  fprintf (dascii, "MAP NAME:     %s\n", head->map_name);
  fprintf (dascii, "MAP DATE:     %s\n", head->source_date);
  fprintf (dascii, "MAP SCALE:    %ld\n", head->orig_scale);
  fprintf (dascii, "OTHER INFO:   %s\n", head->line_3);
  fprintf (dascii, "ZONE:         %d\n", head->plani_zone);
  /*
  G_format_northing (head->W, buf, -1);
  fprintf (dascii, "WEST EDGE:    %s\n", buf);
  G_format_northing (head->E, buf, -1);
  fprintf (dascii, "EAST EDGE:    %s\n", buf);
  G_format_northing (head->S, buf, -1);
  fprintf (dascii, "SOUTH EDGE:   %s\n", buf);
  G_format_northing (head->N, buf, -1);
  fprintf (dascii, "NORTH EDGE:   %s\n", buf);
  G_format_northing (head->map_thresh, buf, -1);
  */
  fprintf (dascii, "MAP THRESH:   %s\n", buf);
  /*
  fprintf (dascii, "VERTI:\n");
  */
  return (0);
}

int 
dig_read_head_ascii (
		      FILE * dascii,
		      struct dig_head *head)
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
	
      /* DIGIT DATE:   Thu Jun  1 23:19:39 2000
       *    => ptr points the wrong date string "39 2000".
       * Instead of G_rindex, use G_index.
       */
      if (!(ptr = G_index (buff, ':')))
	return (-1);
      ptr++;			/* Search for the start of text */
      while (*ptr == ' ')
	ptr++;

      /* fprintf (stdout,"%s\n", ptr) ; */
      if (strncmp (buff, "ORGANIZATION:", 12) == 0)
	{
	  if (strlen (ptr) >= sizeof (head->organization))
	    ptr[sizeof (head->organization) - 1] = 0;
	  strcpy (head->organization, ptr);
	}
      else if (strncmp (buff, "DIGIT DATE:", 11) == 0)
	{
	  if (strlen (ptr) >= sizeof (head->date))
	    ptr[sizeof (head->date) - 1] = 0;
	  strcpy (head->date, ptr);
	}
      else if (strncmp (buff, "DIGIT NAME:", 11) == 0)
	{
	  if (strlen (ptr) >= sizeof (head->your_name))
	    ptr[sizeof (head->your_name) - 1] = 0;
	  strcpy (head->your_name, ptr);
	}
      else if (strncmp (buff, "MAP NAME:", 9) == 0)
	{
	  if (strlen (ptr) >= sizeof (head->map_name))
	    ptr[sizeof (head->map_name) - 1] = 0;
	  strcpy (head->map_name, ptr);
	}
      else if (strncmp (buff, "MAP DATE:", 9) == 0)
	{
	  if (strlen (ptr) >= sizeof (head->source_date))
	    ptr[sizeof (head->source_date) - 1] = 0;
	  strcpy (head->source_date, ptr);
	}
      else if (strncmp (buff, "MAP SCALE:", 10) == 0)
	sscanf (ptr, "%ld", &(head->orig_scale));
      else if (strncmp (buff, "OTHER INFO:", 11) == 0)
	{
	  if (strlen (ptr) >= sizeof (head->line_3))
	    ptr[sizeof (head->line_3) - 1] = 0;
	  strcpy (head->line_3, ptr);
	}
      else if (strncmp (buff, "ZONE:", 5) == 0
	       || strncmp (buff, "UTM ZONE:", 9) == 0)
	sscanf (ptr, "%d", &(head->plani_zone));
      else if (strncmp (buff, "WEST EDGE:", 10) == 0)
        {     
/*	sscanf (ptr, "%lf", &(head->W)); */
	}
      else if (strncmp (buff, "EAST EDGE:", 10) == 0)
        {
/*	sscanf (ptr, "%lf", &(head->E)); */
	}
      else if (strncmp (buff, "SOUTH EDGE:", 11) == 0)
        {
/*	sscanf (ptr, "%lf", &(head->S)); */
	}
      else if (strncmp (buff, "NORTH EDGE:", 11) == 0)
        {
/*	sscanf (ptr, "%lf", &(head->N)); */
	}
      else if (strncmp (buff, "MAP THRESH:", 11) == 0)
	sscanf (ptr, "%lf", &(head->map_thresh));
      else
        {
	  fprintf (stderr, "WARNING: unknown keyword %s in vector head\n", buff);
	}
    }
  /* NOTREACHED */
}
