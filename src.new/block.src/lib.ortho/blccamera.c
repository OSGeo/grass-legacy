/**********************************************************
* I_get_block_camera (block, &Cam_Ref);
* I_put_block_camera (block, &Cam_Ref);
**********************************************************/
#include "dba_imagery.h"

/* Put the "camera" name into the block file "CAMERA" */
I_put_block_camera (block, camera)
    char *block;
    char *camera;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    FILE *fd;
    FILE *I_fopen_block_camera_new();
    
    G_suppress_warnings(1);
    fd = I_fopen_block_camera_new(block) ;
    G_suppress_warnings(0);
    if (!fd) return 0;

    fprintf (fd, "%s", camera);
}

/* Return the camera name from the block file CAMERA */
I_get_block_camera (block, camera)
    char *block;
    char *camera;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    FILE *fd;
    FILE *I_fopen_block_camera_old();
    
/*      Suppress warnings need to be from the calling program */
/*    G_suppress_warnings(1); */
    fd = I_fopen_block_camera_old(block) ;
/*    G_suppress_warnings(0); */
    if (!fd) 
      {
	sprintf (buf, "unable to open camera file for block [%s] in mapset [%s]", block, G_mapset());
	G_warning (buf);
	return 0;
      }
    fgets(buf, sizeof buf, fd);
	sscanf (buf, "%s", camera);
    return (1);
}






