/**********************************************************
* I_get_group_camera (group, &Cam_Ref);
* I_put_group_camera (group, &Cam_Ref);
**********************************************************/
#include "orthophoto.h"

/* Put the "camera" name into the group file "CAMERA" */
I_put_group_camera (group, camera)
    char *group;
    char *camera;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    FILE *fd;
    FILE *I_fopen_group_camera_new();
    
    G_suppress_warnings(1);
    fd = I_fopen_group_camera_new(group) ;
    G_suppress_warnings(0);
    if (!fd) return 0;

    fprintf (fd, "%s", camera);
}

/* Return the camera name from the group file CAMERA */
I_get_group_camera (group, camera)
    char *group;
    char *camera;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    FILE *fd;
    FILE *I_fopen_group_camera_old();
    
    G_suppress_warnings(1);
    fd = I_fopen_group_camera_old(group) ;
    G_suppress_warnings(0);
    if (!fd) 
      {
	sprintf (buf, "unable to open camera file for group [%s] in mapset [%s]", group, G_mapset());
	G_warning (buf);
	return 0;
      }
    fgets(buf, sizeof buf, fd);
	sscanf (buf, "%s", camera);
    return (1);
}






