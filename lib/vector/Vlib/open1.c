/*
   **  Written by:  Dave Gerdes   1-2/1991 
   **  US Army Construction Engineering Research Lab
   **
   **  Overhauling Library interface
 */

/*  
 *******************************************************************
 *  #include "head.h"
 *
 *  dig_init (fd)
 *      FILE *fd ;
 *
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include <unistd.h>
#include "Vect.h"
#include "gis.h"

#include <sys/types.h>
#include <sys/stat.h>

static char name_buf[1024];


int 
V1_open_old (
	      struct Map_info *Map,
	      char *name,
	      char *mapset)
{
  int ret;	
  char buf[200], buf2[200], xname[512], xmapset[512];
  FILE *fp;


  if (G__name_is_fully_qualified (name, xname, xmapset)) {
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, xname);
    sprintf (buf2, "%s@%s", GRASS_VECT_COOR_ELEMENT, xmapset); /* ==coor@mapset */     
    Map->name = G_store (xname);
    Map->mapset = G_store (xmapset);
  } else {
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);
    sprintf (buf2, "%s", GRASS_VECT_COOR_ELEMENT);
    Map->name = G_store (name);
    Map->mapset = G_store (mapset);
  }
  /*
  fprintf (stdout, "open: %s///%s///%s\n", mapset, buf, buf2);
  */
  Map->dig_fp = G_fopen_old (buf, buf2, mapset);
  if ( Map->dig_fp == NULL ) return -1;

  G__file_name (name_buf, buf, buf2, mapset);

  Map->digit_file = G_store (name_buf);		/*need? */
  
  Map->open = VECT_OPEN_CODE;
  Map->level = LEVEL_1;
  Map->mode = MODE_READ;
  Map->Constraint_region_flag = 0;
  Map->Constraint_type_flag = 0;

  if ( !(dig__read_head (Map)) ) return (-1);

  if ( (Vect__read_head (Map)) != GRASS_OK ) return (-1);
  
  /* set conversion matrices */
  dig__init_head_portable ( &(Map->head));
  
  return (0);
}

int 
V1_open_new (
	      struct Map_info *Map,
	      char *name,
	      int with_z)
{
  char buf[200];
  FILE *fp;
  struct stat info;


  sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);

/*DEBUG debugf ("Openning file %s level 1 WRITE\n", name); */
  Map->dig_fp = G_fopen_new (buf, GRASS_VECT_COOR_ELEMENT);
  if ( Map->dig_fp == NULL ) return (-1);

  /* check to see if dig_plus file exists and if so, remove it */
  G__file_name (name_buf, buf, GRASS_VECT_TOPO_ELEMENT, G_mapset ());
  if (stat (name_buf, &info) == 0)	/* file exists? */
       unlink (name_buf);

  G__file_name (name_buf, buf, GRASS_VECT_COOR_ELEMENT, G_mapset ());
  Map->digit_file = G_store (name_buf);		/*need? */

  Map->name = G_store (name);
  Map->mapset = G_store (G_mapset ());
  Map->open = VECT_OPEN_CODE;
  Map->level = LEVEL_1;
  Map->mode = MODE_WRITE;
  Map->Constraint_region_flag = 0;	/* these do not apply to to write, but */
  Map->Constraint_type_flag = 0;	/* init them anyway                   */

  Vect__init_head (&(Map->head));
  Map->head.with_z = with_z;
  Vect__write_head (Map);

  /* set byte order to native order of double */
  Map->head.byte_order = dig__byte_order_out ();
  
  /* set conversion matrices */
  dig__init_head_portable ( &(Map->head) );

  if ( !(dig__write_head (Map)) ) return (-1);
  
  return 0;
}

/*
   **  Not supported do not use
 */

int
V1__open_update_1 (struct Map_info *Map, char *name)
{
  FILE *fp;
  char buf[200];

  sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);

/*DEBUG debugf ("Openning file %s level 1 UPDATE\n", name); */
  Map->dig_fp = G_fopen_modify (buf, GRASS_VECT_COOR_ELEMENT);
  if ( Map->dig_fp == NULL ) return (-1);

  G__file_name (name_buf, buf, GRASS_VECT_COOR_ELEMENT, G_mapset ());
  Map->digit_file = G_store (name_buf);

  Map->open = VECT_OPEN_CODE;
  Map->level = LEVEL_1;
  Map->mode = MODE_RW;
  Map->name = G_store (name);
  Map->mapset = G_store (G_mapset ());
  Map->Constraint_region_flag = 0;
  Map->Constraint_type_flag = 0;

  if ( !(dig__read_head (Map)) ) return (-1);

  if ( (Vect__read_head (Map)) != GRASS_OK ) return (-1);
  
  /* set conversion matrices */
  dig__init_head_portable ( &(Map->head));
  
  return (0);
}
