/*----------------------------------------------------------------
 * I_get_con_points_ll (group_name, cp)
 * I_put_con_points_ll (group_name, cp)
 * I_new_con_point_ll
 * 
 *
 * Read or Write the Control Points (Lat/Lon format) for an
 * imagery group.  Control Points are stored in an ascii file
 * "CONTROL_POINTS" in the imagery group directory.
 *
 * The control points (Lat/Lon format) file has the format:
 *
 *     source_image                target               status
 *  row (y)     col (x)       lat (y)   lon (x)         (1=ok)
 *
 *  
 *  NOTES: -  Source image coordinates need NOT correspond 1-to-1
 *            with row and column numbers.
 *         -  Target Lat and Lon will need to be projected into 
 *            the target location coordinates system.
 *   
 * 
 *  I_get_con_points_ll
 *       RETURNS:    1  ok.
 *                   0  problem.  
 ----------------------------------------------------------------*/

#include "ortho_image.h"

#define POINT_FILE "CONTROL_POINTS_LL"


/*----------------------------------------------------------------*/
int 
I_get_con_points_ll (char *group_name, Control_Points_LL *cp)
{
    FILE *fd;
    char msg[100];
    int stat;

    fd = I_fopen_group_file_old (group_name, POINT_FILE);
    if (fd == NULL)
    {
	sprintf (msg, "Unable to open control point (Lat/Lon) file for group [%s in %s]",  group_name, G_mapset());
	G_warning (msg);

	cp->e1     = NULL;
	cp->n1     = NULL;
	cp->lat2   = NULL;
	cp->lon2   = NULL;
	cp->status = NULL;

	return 0;
    }

    stat = I_read_con_points_ll (fd, cp);
    fclose (fd);
    if (stat < 0)
    {
	sprintf (msg, "Bad format in control point file for imagery group %s in mapset  %s.",
		group_name, G_mapset());
	G_warning (msg);
	return 0;
    }
    return 1;
}


/*----------------------------------------------------------------*/
int 
I_put_con_points_ll (char *group_name, Control_Points_LL *cp)
{
    FILE *fd;
    char msg[100];

    fd = I_fopen_group_file_new (group_name, POINT_FILE);
    if (fd == NULL)
    {
	sprintf (msg, "Unable to create control point file for imagery group %s in mapset %s.",
		group_name, G_mapset());
	G_warning (msg);
	return 0;
    }

    I_write_con_points_ll (fd, cp);
    fclose (fd);
    return 1;
}


/*----------------------------------------------------------------*/
int 
I_read_con_points_ll (FILE *fd, Control_Points_LL *cp)
{
    char buf[300];
    double e1, n1, lat2, lon2;
    int status;

    cp->count = 0;
 
/*    read the control point lines. format is (on one line):
 *    source_x  source_y     target_lon target_lat      status(1=ok,0=ignore)
*/
    cp->e1   = NULL;
    cp->n1   = NULL;
    cp->lat2 = NULL;
    cp->lon2 = NULL;

    cp->status = NULL;

    while (G_getl (buf, sizeof buf, fd))
    {
	G_strip(buf); 

	/* ignore comment lines beginning with '#' */
	if (*buf == '#' || *buf == 0) continue;

	/* scan for 5 doubles per control point line */
	if (sscanf (buf, "%lf %lf %lf %lf %d", 
            &e1, &n1, &lon2, &lat2, &status) ==  5)
	    I_new_con_point_ll (cp, e1, n1, lon2, lat2, status);
	else
	    return -4;
    }

    return 1;
}


/*----------------------------------------------------------------*/
int 
I_new_con_point_ll (Control_Points_LL *cp, double e1, double n1, double lon2, double lat2, int status)
{
    int i;
    unsigned int size;

    if (status < 0) return 0;
    i = (cp->count)++ ;
    size     =  cp->count * sizeof(double) ;
    cp->e1   = (double *) G_realloc (cp->e1, size);
    cp->n1   = (double *) G_realloc (cp->n1, size);
    cp->lat2 = (double *) G_realloc (cp->lat2, size);
    cp->lon2 = (double *) G_realloc (cp->lon2, size);

    size =  cp->count * sizeof(int) ;
    cp->status = (int *) G_realloc (cp->status, size);

    cp->e1[i]     = e1;
    cp->n1[i]     = n1;
    cp->lat2[i]   = lat2;
    cp->lon2[i]   = lon2;
    cp->status[i] = status;

fprintf(stderr, "BOBBY %d %d %f %f %f %f\n", cp->count, i, cp->e1[i], cp->n1[i], cp->lat2[i], cp->lon2[i] );

    return 0;
}


/*----------------------------------------------------------------*/
int 
I_write_con_points_ll (FILE *fd, Control_Points_LL *cp)
{
    int i;

    fprintf (fd,"#------------ source ---------      ---------- target -----------    status\n");
    fprintf (fd,"#     x               y                  lon            lat          (1=ok)\n");
    fprintf (fd,"#\n");


    for (i = 0; i < cp->count; i++)
	if (cp->status[i] >= 0)
	    fprintf (fd, "  %15f %15f %15f %15f %4d\n",
		cp->e1[i], cp->n1[i], cp->lon2[i], cp->lat2[i], cp->status[i]);

    return 0;
}
