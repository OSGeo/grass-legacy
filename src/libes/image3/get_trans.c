/***************************************************************************
* I_get_group_trans (the_group) 


* I_put_group_trans (the_group)
     Put the group.trans_type into the imagery group file "TRANS".

***************************************************************************/

#include "ortho_image.h"
#define  TRANS_FILE      "TRANS"


/*-----------------------------------------------------------------------
 Put the group.trans_type into the imagery group file "TRANS".

 Returns:
    -1  : Can't open TRANS file., also reset group.trans_type = ZERO
    -2  : Error writing to TRANS file.
     1  : o.k.
-----------------------------------------------------------------------*/
int 
I_put_group_trans (Rectify_Group *the_group)
{
  FILE *fd;
  int stat;

  /* open the group TRANS file */
  fd = (FILE *) I_fopen_group_file_new (the_group->name, TRANS_FILE) ; 

  /* can't open TRANS file */
  if (fd == NULL) {
    the_group->trans_type = ZERO_T;
    return (-1);
  }

  /*** TODO -- write out literal trans type not just enumerated value */

  /* write out the file */
  stat  = fprintf (fd, "TRANSFORMATION: %d\n", 
		   the_group->trans_type);

  if (stat == 0) {   /* error writting file */
    fclose (fd);
    return (-2);
  } 
  else {             /* all o.k. */
    fclose (fd);
    return (1);
  }
}

/*-----------------------------------------------------------------------
  Open the imagery group file "TRANS" and read the transformation type. 
  If file exist and have a valid transformation type, then assign       
  group.trans_type = to the value.  
  { i.e. ZERO_T, POLY1, POLY2, POLY3,ORTHO, or LAND_TM }.  

   RETURNS:                                                             
     0: everything o.k, sets group.trans_type == TRANS                
    -1: can't open TRANS file, set group.trans_type == ZERO_T           
    -2: can't read the TRANS param from the file, and                  
              sets group.trans_type == ZERO_T                            
-----------------------------------------------------------------------*/
int 
I_get_group_trans (Rectify_Group *the_group)
{
  FILE *fd;
  char msg[100];
  char buf[100];
  int type;


  /* open the group TRANS file */
  fd = (FILE *) I_fopen_group_file_old (the_group->name, TRANS_FILE) ; 

  /* if can't open, return (-1) */
  if (!fd) {
    sprintf (msg, 
	     "unable to open transformation for group [%s] in mapset [%s]", 
	     the_group->name, G_mapset());
    G_warning (msg);
    the_group->trans_type = ZERO_T;
    return (-1);
  }

  /* TODO -- use grass gets and sscanf routines **/

  /* get the first line of the TRANS file.  -- TODO read # comments */  
  if (fgets(buf, sizeof buf, fd) == NULL) {
    sprintf (msg, 
	     "unable to read transformation for group [%s] in mapset [%s]", 
	     the_group->name, G_mapset());
    G_warning (msg);
    the_group->trans_type = ZERO_T;
    return (-2);
  }

  /* scan for the TRANSFORMATION type */
  if (sscanf(buf, "TRANSFORMATION: %d\n", &type) != 1) {
    sprintf (msg, 
	     "unable to read trans file for group [%s] in mapset [%s]", 
	     the_group->name, G_mapset());
    G_warning (msg);
    the_group->trans_type = ZERO_T;
    return (-2);
  }


  /* if we get here everything is o.k. */
  the_group->trans_type = (TransType) type;
  fclose (fd);
  return (0);

}






