/***********************************************************************
 I_get_group_camera (char *group_name, Camera *camera)
    Read the group file CAMERA and fill in the camera structure.

 I_put_group_camera (char *group_name, Camera *camera)
    Write the camera data structure into group file CAMERA. 
***********************************************************************/

#include "ortho_image.h"
#include <string.h>
#define   CAMERA_FILE    "CAMERA"

/* internal function prototypes */
#ifdef _NO_PROTO
static int   read_group_camera  ();
static int   write_group_camera ();
#else
static int   read_group_camera  (FILE *, Camera *);
static int   write_group_camera (FILE *, Camera *);
#endif

/*-----------------------------------------------------------------------
  Read the group file CAMERA and fill in the camera structure.

  Returns:
     1  : everything o.k.
    -1  : Can't open file.
    -2  : Error read of CAMERA data.
-----------------------------------------------------------------------*/
int 
I_get_group_camera (char *group_name, Camera *camera)
{
  FILE *fd;
  char  msg[100];
  int   stat;


  /* open the group CAMERA file */
  fd = (FILE *) I_fopen_group_file_old (group_name, CAMERA_FILE) ; 
  
  /* error openeing CAMERA file */
  if (!fd) {
    sprintf (msg, 
	     "unable to open CAMERA file for group [%s] in mapset [%s]", 
	     group_name, G_mapset());
    G_warning (msg);
    return (-1);
  }

  /* read the camera info */
  stat = read_group_camera (fd, camera);

  /* close the file */
  fclose (fd);

  /* check read status and return */
  if (stat < 0) {
    sprintf (msg, 
	     "Bad format in camera file for group <%s> in <%s>",
	     group_name, G_mapset());
    G_warning (msg);
    return (-2);
  }
  else {       /* everything o.k. */
    return (1);
  }
}


/*-----------------------------------------------------------------------
  Write the camera data structure into group file CAMERA. 

  Returns:
    -1  : Can't open file.
    -2  : Error writing to file.
     1  : o.k.
-----------------------------------------------------------------------*/
int 
I_put_group_camera (char *group_name, Camera *camera)
{
  FILE *fd;
  char msg[100];
  int  stat;

  /* open the group CAMERA file */
  fd = (FILE *) I_fopen_group_file_new (group_name, CAMERA_FILE) ; 
  
  /* error openeing CAMERA file */
  if (!fd) {
    sprintf (msg, 
	     "unable to open CAMERA file for group [%s] in mapset [%s]", 
	     group_name, G_mapset());
    G_warning (msg);
    return (-1);
  }

  /* write the camera data */
  stat = write_group_camera (fd, camera);

  /* close the file */
  fclose (fd);

  /* check write  status and return */
  if (stat < 0) {
    sprintf (msg, 
	     "Problem writing camera file for group <%s> in <%s>",
	     group_name, G_mapset());
    G_warning (msg);
    return (-2);
  }
  else {       /* everything o.k. */
    return (1);
  }
}

/*-----------------------------------------------------------------------
  Actual read of CAMERA file.

  TODO -- proper returns

  Returns:
     1:  read o.k
-----------------------------------------------------------------------*/
static int 
read_group_camera (FILE *fd, Camera *cam_info)
{   
  int n;
  char buf[100];
  char cam_name[30];
  char cam_id[30];
  double Xp,Yp,CFL;
  int num_fid;
  char fid_id[30];
  double Xf,Yf;

  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"CAMERA NAME   %s \n",cam_name) == 1)
    strcpy(cam_info->cam_name,cam_name);
  
  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"CAMERA ID     %s \n",cam_id) == 1)
    strcpy(cam_info->cam_id,cam_id);
  
  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"CAMERA XP     %lf \n",&Xp) == 1)
    cam_info->Xp = Xp;
  
  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"CAMERA YP     %lf \n",&Yp) == 1)
    cam_info->Yp = Yp;
  
  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"CAMERA CFL    %lf \n",&CFL) == 1)
    cam_info->CFL = CFL;
  
  G_getl (buf, sizeof buf, fd); 
  G_strip(buf);
  if (sscanf(buf,"NUM FID       %d \n",&num_fid) == 1)
    cam_info->num_fid = num_fid;
  
  for (n=0; n<cam_info->num_fid; n++) 
    {
      G_getl (buf, sizeof buf, fd);
      G_strip(buf);
      if (sscanf (buf, "%s %lf %lf", fid_id, &Xf, &Yf) == 3)
	{  strcpy(cam_info->fiducials[n].fid_id, fid_id);
           cam_info->fiducials[n].Xf = Xf;
           cam_info->fiducials[n].Yf = Yf; 
	 }     
    }
  
  return 1;
}

/*-----------------------------------------------------------------------
  Actual write of CAMERA data.

  TODO -- proper returns

  Returns:
     1:  write  o.k
-----------------------------------------------------------------------*/
static int 
write_group_camera (FILE *fd, Camera *cam_info)
{
  int i;

  fprintf (fd,"CAMERA NAME   %s \n",cam_info->cam_name);
  fprintf (fd,"CAMERA ID     %s \n",cam_info->cam_id);
  fprintf (fd,"CAMERA XP     %f \n",cam_info->Xp);
  fprintf (fd,"CAMERA YP     %f \n",cam_info->Yp);
  fprintf (fd,"CAMERA CFL    %f \n",cam_info->CFL);
  fprintf (fd,"NUM FID       %d \n",cam_info->num_fid);
  for (i = 0; i < cam_info->num_fid; i++)
    fprintf (fd, "  %5s %15f %15f \n",
	     cam_info->fiducials[i].fid_id,
	     cam_info->fiducials[i].Xf,
	     cam_info->fiducials[i].Yf);
  return 1;
}


