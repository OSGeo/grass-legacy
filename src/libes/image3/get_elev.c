/*************************************************************************
* I_get_group_elev (group_name, elev)
    Read the group file ELEVATION and fill in the elev structure.

* I_put_group_elev (group_name, elev)
    Write the elev data structure into group file ELEVATION. 

* I_initialize_group_elev(elev)
    Clear the elev data structure.
*************************************************************************/

#include "ortho_image.h"
#define   ELEVATION_FILE "ELEVATION"

/* internal function prototypes */
#ifdef _NO_PROTO
static int   read_group_elev  ();
static int   write_group_elev ();
#else
static int   read_group_elev  (FILE *fd, Elevation *elev);
static int   write_group_elev (FILE *fd, Elevation  elev);
#endif

/*-----------------------------------------------------------------------
  Write the Elevation data structure into group file ELEVATION 

  Returns:
    -1  : Can't open ELEVATION file.
    -2  : Error writing to ELEVATION file.
     1  : o.k.
-----------------------------------------------------------------------*/
int 
I_put_group_elev (char *group_name, Elevation elev)
{
    FILE *fd;
    int   stat;

    /* open the ELEVATION file */
    fd = (FILE *) I_fopen_group_file_new (group_name, ELEVATION_FILE) ;

    /* cant open ELEVATION file */
    if (fd == NULL) {
      return (-1);
    }

    /* write the data */
    stat = write_group_elev(fd, elev);

    /* close the file */
    fclose (fd);

    if (stat < 0) {   /* error writting file */
      return (-2);
    } 
    else {             /* all o.k. */
      return (1);
    }
}


/*-----------------------------------------------------------------------
  Read the group file ELEVATION and fill in the elev structure.

  Returns:
     1  : everything o.k.
    -1  : Can't open ELEVATION file.
    -2  : Error read of ELEVATION files data.
-----------------------------------------------------------------------*/
int 
I_get_group_elev (char *group_name, Elevation *elev)
{
    char  msg[100];      /* warning message buffer */  
    FILE *fd;
    int   stat;
    
    /* initialize (i.e. clear) the elevation structure */
    I_initialize_group_elev(elev);

    /* open the group ELEV file */
    fd = (FILE *) I_fopen_group_file_old (group_name, ELEVATION_FILE) ; 

    /* error openeing ELEVATION file */
    if (!fd) {
      sprintf (msg, 
	       "unable to open elevation file for group [%s] in mapset [%s]", 
	       group_name, G_mapset());
      G_warning (msg);
      return (-1);
    }

    /* read the elev data */
    stat = read_group_elev (fd, elev);

    /* close file */
    fclose (fd);

    if (stat < 0) {   /* error reading file */
      return (-2);
    } 
    else {            /* all o.k. */
      return (1);
    }
}

/*-----------------------------------------------------------------------
  Clear the elev data structure.
  Returns:
     1:  o.k
-----------------------------------------------------------------------*/
int 
I_initialize_group_elev (Elevation *elev)
{
/** TODO - fix these properly **/
/**   *elev->elev_map = 0;
  *   *elev->elev_mapset = 0;
  *   *elev->tl = 0;
  *   *elev->math_exp = 0;
  *   *elev->units = 0;
  *   *elev->nd = 0;
**/

   elev->fd = 0;

   return (1);
}


/*-----------------------------------------------------------------------
  Actual write of ELEVATION data.

  TODO -- proper returns

  Returns:
     1:  write  o.k
-----------------------------------------------------------------------*/
static int 
write_group_elev (FILE *fd, Elevation elev)
{
    /* TODO -- check returns stats of fprintf and return -1 on fail **/
    fprintf (fd, "elevation layer :%s\n", elev.elev_map);
    fprintf (fd, "mapset elevation:%s\n", elev.elev_mapset);
    fprintf (fd, "location        :%s\n", elev.tl);
    fprintf (fd, "math expresion  :%s\n", elev.math_exp);
    fprintf (fd, "units           :%s\n", elev.units);
    fprintf (fd, "no data values  :%s\n", elev.nd);

    return (1);
}

/*-----------------------------------------------------------------------
  Actual read of ELEVATION data.

  TODO -- proper returns

  Returns:
     1:  write  o.k
-----------------------------------------------------------------------*/
static int 
read_group_elev (FILE *fd, Elevation *elev)
{
  char buf[100];

    /** TODO check for errors in read **/
    /** TODO use grass get and scan routines (see get_camera.c) **/
    fgets(buf, sizeof buf, fd);
	sscanf (buf, "elevation layer :%s\n", elev->elev_map);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "mapset elevation:%s\n", elev->elev_mapset);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "location        :%s\n", elev->tl);
     fgets(buf, sizeof buf, fd);   
        sscanf (buf, "math expresion  :%s\n", elev->math_exp);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "units           :%s\n", elev->units);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "no data values  :%s\n", elev->nd);

    return (1);
}
