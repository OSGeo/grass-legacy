/***********************************************************
* I_get_con_points (group)
* I_put_con_points (group)
*
* Read or Write the Control Points (3D) for an
* imagery group.  Camera info is stored in an ascii file
* "CONTROL_POINTS" in the imagery group directory.
*
* The control points (3D) file has the format:
*
*      image           target               status
*  row      col       x   y  z              (1=ok)

   
   NOTES:  - a value of (z=-99999.9) denotes that no elevation
             for that control point was given.

   
**********************************************************/
#include "ortho_image.h"


#define POINT_FILE "CONTROL_POINTS"
#define Z_NOT_USED -99999.9

int I_get_con_points (char *group_name, Control_Points_3D *cp)
{
    FILE *fd;
    char msg[100];
    int stat;

    fd = I_fopen_group_file_old (group_name, POINT_FILE);
    if (fd == NULL)
    {
	sprintf (msg, "unable to open control point (Z) file for group [%s in %s]",
		group_name, G_mapset());
	G_warning (msg);

	cp->e1 = NULL;
	cp->e2 = NULL;
	cp->n1 = NULL;
	cp->n2 = NULL;
	cp->z1 = NULL;
	cp->z2 = NULL;
	cp->status = NULL;

	return 0;
    }

    stat = I_read_con_points (fd, cp);
    fclose (fd);
    if (stat < 0)
    {
	sprintf (msg, "bad format in control point file for group [%s in %s]",
		group_name, G_mapset());
	G_warning (msg);
	return 0;
    }
    return 1;
}

int 
I_put_con_points (char *group_name, Control_Points_3D *cp)
{
    FILE *fd;
    char msg[100];

    fd = I_fopen_group_file_new (group_name, POINT_FILE);
    if (fd == NULL)
    {
	sprintf (msg, "unable to create control point file for group [%s in %s]",
		group_name, G_mapset());
	G_warning (msg);
	return 0;
    }

    I_write_con_points (fd, cp);
    fclose (fd);
    return 1;
}


/* read the control points from group file "CON_POINTS" into */
/* the struct Con_Points */
int 
I_read_con_points (FILE *fd, Control_Points_3D *cp)
{
    char buf[300];
    double e1,e2,n1,n2,z1,z2;
    int status;

    cp->count = 0;

/* read the control point lines. format is (on one line):
   photo_x        photo_y         -CFL 
   control_east control_north  control_elev  status(1=ok)
*/
    cp->e1 = NULL;
    cp->e2 = NULL;
    cp->n1 = NULL;
    cp->n2 = NULL;
    cp->z1 = NULL;
    cp->z2 = NULL;

    cp->status = NULL;

    while (G_getl (buf, sizeof buf, fd))
    {
	G_strip(buf);
	if (*buf == '#' || *buf == 0) continue;
	if (sscanf (buf, "%lf%lf%lf%lf%lf%lf%d", 
            &e1, &n1, &z1, &e2, &n2, &z2, &status) ==  7)
	    I_new_con_point (cp, e1, n1, z1, e2, n2, z2, status);
	else
	    return -4;
    }

    return 1;
}

int 
I_new_con_point (Control_Points_3D *cp, double e1, double n1, double z1, double e2, double n2, double z2, int status)
{
    int i;
    unsigned int size;

    if (status < 0) return 0;
    i = (cp->count)++ ;
    size =  cp->count * sizeof(double) ;
    cp->e1 = (double *) G_realloc (cp->e1, size);
    cp->e2 = (double *) G_realloc (cp->e2, size);
    cp->n1 = (double *) G_realloc (cp->n1, size);
    cp->n2 = (double *) G_realloc (cp->n2, size);
    cp->z1 = (double *) G_realloc (cp->z1, size);
    cp->z2 = (double *) G_realloc (cp->z2, size);

    size =  cp->count * sizeof(int) ;
    cp->status = (int *) G_realloc (cp->status, size);

    cp->e1[i] = e1;
    cp->e2[i] = e2;
    cp->n1[i] = n1;
    cp->n2[i] = n2;
    cp->z1[i] = z1;
    cp->z2[i] = z2;

    cp->status[i] = status;

    return 0;
}

int 
I_write_con_points (FILE *fd, Control_Points_3D *cp)
{
    int i;

    fprintf (fd,"#--------------------- photo --------------------   ------------------ control ------------------    status\n");
    fprintf (fd,"#               x               y             cfl            east           north           elev.    (1=ok)\n");
    fprintf (fd,"#\n");
#if 0
    fprintf (fd,"# %7s %15s %30s %15s %9s status\n","","photo","","control","");
    fprintf (fd,"# %15s %15s %15s %15s %15s %15s   (1=ok)\n","x","y","-cfl","east","north","elev.");
    fprintf (fd,"#\n");
#endif

    for (i = 0; i < cp->count; i++)
	if (cp->status[i] >= 0)
	    fprintf (fd, "  %15f %15f %15f %15f %15f %15f %4d\n",
		cp->e1[i], cp->n1[i], cp->z1[i], cp->e2[i], cp->n2[i],  cp->z2[i], cp->status[i]);

    return 0;
}



