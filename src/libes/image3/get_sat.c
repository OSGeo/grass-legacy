/***********************************************************************
 I_get_group_sat (char *group_name, Satellite *sat)
    Read the group file SATELLITE and fill in the Satellite structure.

 I_put_group_sat (char *group_name, Satellite *sat)
    Write the satellite data structure into group file SATELLITE. 
***********************************************************************/

#include "ortho_image.h"
#define   SAT_FILE    "SATELLITE"

/* internal function prototypes */
#ifdef _NO_PROTO
static int   read_group_sat  ();
static int   write_group_sat ();
#else
static int   read_group_sat  (FILE *, Satellite *);
static int   write_group_sat (FILE *, Satellite *);
#endif

/*-----------------------------------------------------------------------
  Read the group file SATELLITE and fill in the satellite structure.

  Returns:
     1  : everything o.k.
    -1  : Can't open file.
    -2  : Error read of SATELLITE data.
-----------------------------------------------------------------------*/
int 
I_get_group_sat (char *group_name, Satellite *sat)
{
  FILE *fd;
  char  msg[100];
  int   stat;


  /* open the group SATELLITE file */
  fd = (FILE *) I_fopen_group_file_old (group_name, SAT_FILE) ; 
  
  /* error openeing SAT file */
  if (!fd) {
    sprintf (msg, 
	     "unable to open SATELLITE file for group [%s] in mapset [%s]", 
	     group_name, G_mapset());
    G_warning (msg);
    return (-1);
  }

  /* read the sat info */
  stat = read_group_sat (fd, sat);

  /* close the file */
  fclose (fd);

  /* check read status and return */
  if (stat < 0) {
    sprintf (msg, 
	     "Bad format in sat file for group <%s> in <%s>",
	     group_name, G_mapset());
    G_warning (msg);
    return (-2);
  }
  else {       /* everything o.k. */
    return (1);
  }
}


/*-----------------------------------------------------------------------
  Write the Satellite data structure into group file SATELLITE. 

  Returns:
    -1  : Can't open file.
    -2  : Error writing to file.
     1  : o.k.
-----------------------------------------------------------------------*/
int 
I_put_group_sat (char *group_name, Satellite *sat)
{
  FILE *fd;
  char msg[100];
  int  stat;

  /* open the group SAT file */
  fd = (FILE *) I_fopen_group_file_new (group_name, SAT_FILE) ; 
  
  /* error openeing SAT file */
  if (!fd) {
    sprintf (msg, 
	     "unable to open CAMERA file for group [%s] in mapset [%s]", 
	     group_name, G_mapset());
    G_warning (msg);
    return (-1);
  }

  /* write the sat data */
  stat = write_group_sat (fd, sat);

  /* close the file */
  fclose (fd);

  /* check write  status and return */
  if (stat < 0) {
    sprintf (msg, 
	     "Problem writing satellite file for group <%s> in <%s>",
	     group_name, G_mapset());
    G_warning (msg);
    return (-2);
  }
  else {       /* everything o.k. */
    return (1);
  }
}

/*-----------------------------------------------------------------------
  Actual read of Satellite file.

  TODO -- proper returns

  Returns:
     1:  read o.k
-----------------------------------------------------------------------*/
static int read_group_sat (FILE *fd, Satellite *sat_info)
{   
  int n;
  char buf[100];
  double Xp,Yp;
  double Xf,Yf;
  double Ef, Nf;

  
  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"SAT XP     %lf \n",&Xp) == 1)
    sat_info->Xpix = Xp;
  
  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"SAT YP     %lf \n",&Yp) == 1)
    sat_info->Ypix = Yp;
  

  /* always have four corners for the satellite */
  
/**  G_getl (buf, sizeof buf, fd); 
  *  G_strip(buf);
  *  if (sscanf(buf,"NUM FID       %d \n",&num_fid) == 1)
  *    cam_info->num_fid = num_fid;
**/
  
  for (n=0; n<4; n++) 
    {
      G_getl (buf, sizeof buf, fd);
      G_strip(buf);
      if (sscanf (buf, "%lf %lf %lf %lf", &Xf, &Yf, &Ef, &Nf) == 4)
	{  sat_info->corners[n].Xf = Xf;
           sat_info->corners[n].Yf = Yf; 
           sat_info->corners[n].Ef = Ef;
           sat_info->corners[n].Nf = Nf; 
	}     
    }
  
  return 1;
}

/*-----------------------------------------------------------------------
  Actual write of SATELLITE data.

  TODO -- proper returns

  Returns:
     1:  write  o.k
-----------------------------------------------------------------------*/
static int 
write_group_sat (FILE *fd, Satellite *sat_info)
{
  int i;

/**  fprintf (fd,"CAMERA NAME   %s \n",cam_info.cam_name);
 **  fprintf (fd,"CAMERA ID     %s \n",cam_info.cam_id);
**/

  fprintf (fd,"SAT XP     %f \n",sat_info->Xpix);
  fprintf (fd,"SAT YP     %f \n",sat_info->Ypix);

/**  fprintf (fd,"CAMERA CFL    %lf \n",cam_info.CFL);
 **  fprintf (fd,"NUM FID       %d \n",cam_info.num_fid);
**/

  /* 4 corners */
  for (i = 0; i < 4; i++)
    fprintf (fd, "  %15f %15f %15f %15f \n",
	     sat_info->corners[i].Xf,
	     sat_info->corners[i].Yf,
	     sat_info->corners[i].Ef,
	     sat_info->corners[i].Nf);
  return 1;
}
