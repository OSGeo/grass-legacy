/**********************************************************
* I_get_camera (camera);
* I_put_camera (camera);
*
* I_get_group_camera (group, &Cam_Ref);
* I_put_group_camera (group, &Cam_Ref);
* I_init_group_camera (&Cam_Ref);
* I_free_group_camera (&Cam_Ref);
**********************************************************/
#include "imagery2.h"

/* get current camera name from file CAMERA in current mapset */
I_get_camera (camera)
    char *camera;
{
    FILE *fd;
    int stat;

    *camera = 0;
    G_suppress_warnings(1);
    fd = G_fopen_old ("", "CAMERA", G_mapset());
    G_suppress_warnings(0);
    if (fd == NULL)
	return 0;
    stat = (fscanf (fd, "%s", camera) == 1);
    fclose (fd);
    return stat;
}

/* write camera name to file CAMERA in current mapset */
I_put_camera (camera)
    char *camera;
{
    FILE *fd;

    fd = G_fopen_new ("", "CAMERA");
    if (fd == NULL)
	return 0;
    fprintf (fd, "%s\n", camera);
    fclose (fd);
    return 1;
}

I_put_group_camera (group, camera)
    char *group;
    char *camera;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    char color[20];
    FILE *fd;
    FILE *I_fopen_group_camera_new();
    
    G_suppress_warnings(1);
    fd = I_fopen_group_camera_new(group) ;
    G_suppress_warnings(0);
    if (!fd) return 0;

    fprintf (fd, "%s", camera);
}

I_get_group_camera (group, camera)
    char *group;
    char *camera;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    char color[20];
    FILE *fd;
    FILE *I_fopen_group_camera_old();
    
    G_suppress_warnings(1);
    fd = I_fopen_group_camera_old(group) ;
    G_suppress_warnings(0);
    if (!fd) return 0;

    fgets(buf, sizeof buf, fd);
	sscanf (buf, "%s", camera);
}





