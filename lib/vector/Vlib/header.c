#include "gis.h"
#include "Vect.h"


int 
Vect_print_header (struct Map_info *Map)
{
  struct dig_head *dhead;

  dhead = &(Map->head);

  fprintf (stdout, "\nSelected information from dig header\n");
  fprintf (stdout, " Organization:  %s\n", dhead->organization);
  fprintf (stdout, " Map Name:      %s\n", dhead->map_name);
  fprintf (stdout, " Source Date:   %s\n", dhead->source_date);
  fprintf (stdout, " Orig. Scale:   %ld\n", dhead->orig_scale);

  return 0;
}

/* Vect__write_head () writes head information to text file.
 * returns: GRASS_OK - success
 *          GRASS_ERR - error
 */
int
Vect__write_head (struct Map_info *Map)
{
    char buf[200];	
    FILE *head_fp;

    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);

    head_fp = G_fopen_new (buf, GRASS_VECT_HEAD_ELEMENT);
    if ( head_fp == NULL)
      {
        G_warning ("Cannot Open Vector %s Head File\n", Map->name);
        return (GRASS_ERR);
      }
	
    dig_write_head_ascii (head_fp, &(Map->head));
    
    fclose (head_fp);
    return (GRASS_OK);
}

/* Vect__read_head () reads head information from text file.
 * returns: GRASS_OK - success
 *          GRASS_ERR - error
 */
int
Vect__read_head (struct Map_info *Map)
{
    char buf[200];	
    FILE *head_fp;

    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    head_fp = G_fopen_old (buf, GRASS_VECT_HEAD_ELEMENT, Map->mapset); 
    if ( head_fp == NULL)
      {
        G_warning ("Cannot Open Vector %s Head File\n", Map->name);
        return (GRASS_ERR);
      }
    dig_read_head_ascii (head_fp, &(Map->head));
    
    fclose (head_fp);
    return (GRASS_OK);
}


