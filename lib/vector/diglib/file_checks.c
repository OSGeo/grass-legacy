#include "Vect.h"
#include <sys/types.h>
#include <sys/stat.h>

#define SUPPORT_PROG "v.support"

static int tell_delta_time (char *);
static int tell_delta_size (char *);
/*
   **  write out file info to dig_plus file for checking that dig, dig_plus,
   **   and dig_att all agree w/ each other 
 */
int 
dig_write_file_checks (
			FILE * fp_plus,
			struct Plus_head *Plus)
{
  struct stat stat_buf;

  /* TODO */
  rewind (fp_plus);

  /* get info on dig file for consistancy checks */
  
  //if (0 != stat (map->digit_file, &stat_buf))
    //{
      //fprintf (stderr, "Could not stat file '%s'\n", map->digit_file);
      //Plus->Dig_code = -1L;	/* last modified time */
      //Plus->Dig_size = -1L;	/* file size */
    //}
  //else
    //{
      //Plus->Dig_code = (long) stat_buf.st_mtime;	/* last modified time */
      //Plus->Dig_size = (long) stat_buf.st_size;		/* file size */
    //}

  dig_Wr_Plus_head (fp_plus, Plus);
  fflush (fp_plus);
  
  return (0);
}

/*
   **  check file consistancy out file info to dig_plus file for checking that 
   **  dig, dig_plus, and dig_att all agree w/ each other 
 */
int 
dig_do_file_checks (
		     struct Map_info *map,
		     char *plus, char *dig)
{
  FILE *fp;
  struct Plus_head head;
  struct stat stat_buf;
  int ret;
  int flag = 0;


  /* TODO */
  ret = 0;
  if ((fp = fopen (plus, "r")) == NULL)
    {
      fprintf (stderr, "DO_FILE_CHECKS: failed opening %s\n", plus);
      return (-1);
    }

  dig_Rd_Plus_head (&head, fp);
  fclose (fp);

  /* get info on dig file for consistancy checks */
  if (0 != stat (dig, &stat_buf))
    {
      fprintf (stderr, "Could not stat file '%s'\n", dig);
      ret = 1;
    }
  else
    {
      if (head.Dig_size != (long) stat_buf.st_size)	/* file size */
	tell_delta_size (dig);
      if (head.Dig_code < (long) stat_buf.st_mtime)	/* modified since */
	tell_delta_time (dig);
      flag = 1;
    }

  return (ret);
}

static int 
tell_delta_time (char *file)
{
  /*   taken out so Gcopy won't cause an error 
     fprintf (stderr, "File modify times do not agree.  You should run '%s'\n", SUPPORT_PROG);
     if (!G_yes ("Do you wish to continue? ", 0))
     exit (-1);
   */

  return 0;
}

static int 
tell_delta_size (char *file)
{
  fprintf (stderr, "Data file has been modified.  You MUST first run '%s'\n", SUPPORT_PROG);
  exit (-1);
}
