/* %W%  %G% */
#include "gis.h"
/******************************************************
* I_fopen_cam_file_new()
* I_fopen_cam_file_append()
* I_fopen_cam_file_old()
*
* fopen new camera files in the current mapset
* fopen old camera files anywhere
*******************************************************/

FILE *
I_fopen_cam_file_new (camera, file)
    char *camera;
    char *file;
{
    FILE *fd;
    char element[100];

/* get group element name */
    sprintf (element, "camera");
    
    fd = G_fopen_new (element, camera);
    if (!fd)
	error (camera, file, "can't create ", "");
    return fd;
}

FILE *
I_fopen_cam_file_append (camera, file)
    char *camera;
    char *file;
{
    FILE *fd;
    char element[100];

/* get group element name */
    sprintf (element, "camera/%s", camera);

    fd = G_fopen_append (element, file);
    if (!fd)
	error (camera, file, "unable to open ", "");
    return fd;
}

FILE *
I_fopen_cam_file_old (camera, file)
    char *camera;
    char *file;
{
    FILE *fd;
    char element[100];

/* get group element name */
    sprintf (element, "camera");

    fd = G_fopen_old (element,camera, G_mapset());
    if (!fd)
	error (camera, file, "can't open ", "");
    return fd;
}

static error (camera,file,msga,msgb)
    char *camera;
    char *file;
    char *msga;
    char *msgb;
{
    char buf[100];
    sprintf (buf, "%s camera file [%s] in [%s %s] %s" ,
	msga, camera, G_location(), G_mapset(), msgb);
    G_warning (buf);
}

