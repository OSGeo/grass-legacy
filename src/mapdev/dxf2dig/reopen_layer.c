/* written by J Moorman  
** 7/23/90
*/

#include "dxf2vect.h"
#include "gis.h"

dxf_reopen_layer (type, cl_count, o_count)
    int type;
    int cl_count;
    int o_count;
{
    char	filename[300];
    int		afd;


    sprintf (filename, "%s.%s", basename, closed_layers[cl_count].name);

    if (type == DXF_ASCII)
    {
	afd = G_open_update ("dig_ascii", filename);
    }
    else
	afd = G_open_update ("dig_att",filename);
    if (afd == -1)
    {
	fprintf(stderr,"error: unable to open dig file\n");
	exit (-1);
    }
    /* COPY INFO FROM CLOSED_LAYERS STRUCT TO LAYERS STRUCT */    
    layers[o_count].fd = fdopen (afd, "r+");
    layers[o_count].name = closed_layers[cl_count].name;
    layers[o_count].type = closed_layers[cl_count].type;
    layers[o_count].status = 0; /* INDICATES OPENED MOST RECENTLY */

    /* CHANGE THE STATUS OF CLOSED FILE POSITION TO INACTIVE */
	closed_layers[cl_count].status = -2;



}

