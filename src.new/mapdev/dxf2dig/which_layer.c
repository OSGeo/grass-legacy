/* rewritten by J Moorman
** 7/23/90     
*/

#include "gis.h"
#include "dxf2vect.h"

FILE *
dxf_which_layer (layer_name, type)
    char	*layer_name;
    int type;
{
    
    int	open_count, closed_count;
    FILE *layer_fd; 
    FILE *dxf_open_layer();
    int find_highest_status();
    int found_flag = 0;

    /* convert DXF name to user alias before doing any file or 'layers' stuff */

    if (from_table)
	layer_name = remap (layer_name, type);

    if (layer_name == NULL)    /* layer is not to be written */
	return (NULL);

    if (type == DXF_LABEL_LINE)
	type = DXF_ASCII;



    /* CHECK TO SEE IF THE FILE IS ALREADY OPEN */

    for (open_count = 0; open_count < num_open_layers; open_count++)
    {

	if (type == layers[open_count].type && 
	    strcmp (layer_name, layers[open_count].name) == 0)
	{
	    /* give most recently used file the lowest status */
	    set_status(open_count);
	    return (layers[open_count].fd);
	}
    }

    /* CHECK TO SEE IF THE FILE HAS BEEN OPENED PREVIOUSLY */

    if (num_closed_layers)
	for (closed_count = 0;closed_count < num_closed_layers; closed_count++)
	{

	    if (type == closed_layers[closed_count].type &&
		strcmp (layer_name, closed_layers[closed_count].name) == 0)
	    {
		found_flag = 1;
		break;
		
	    }
	}
	if (found_flag)
	{
		/* CLOSE AN OPEN FILE */
		open_count = find_highest_status();
		dxf_close_layer(open_count);
	    
		/* OPEN A CLOSED FILE */
		dxf_reopen_layer (type, closed_count, open_count);


		return (layers[open_count].fd);
	}


    /* CHECK TO SEE IF THE MAX NUM OF OPEN FILES HAS BEEN REACHED */
    if (num_open_layers == MAX_FILES ) 
    {

	/* CLOSE AN OPEN FILE */
	open_count = find_highest_status();
	dxf_close_layer(open_count);

	/* OPEN A NEW FILE AND DEFINE THE STRUCTURE ELEMENTS*/
	layers[open_count].name = G_store(layer_name);
	layers[open_count].type = type;
	layer_fd = dxf_open_layer(type,open_count);
	if (layer_fd == NULL)
	    return (0);
	layers[open_count].fd = layer_fd;


	set_status(open_count);
	return (layer_fd);
    }


    /*  IF ALL PREVIOUS IFs WERE FALSE OPEN A NEW FILE */
    /* AND DEFINE STRUCTURE ELEMENTS*/
    layers[num_open_layers].name = G_store(layer_name);
    layers[num_open_layers].type = type;
    layer_fd = dxf_open_layer(type,num_open_layers);
    if (layer_fd == NULL)
	return (0);
    layers[num_open_layers].fd = layer_fd;


    set_status(num_open_layers);
    num_open_layers++;
    return(layer_fd);
}

/* file status is incremented with most recently used file's status set to 0 */
set_status(count)
    int count;
{
    int	t; /*LOOPING VARIABLE */

    layers[count].status = 0; /* sets to most recent status */
    for (t=0; t< num_open_layers; t++)
	layers[t].status++;
}

/* the file with the highest .status is closed if necessary */
int
find_highest_status()
{
    int count;
    int highest = 0;

    for (count = 1; count < MAX_FILES; count++)
    {
	if (layers[highest].status < layers[count].status)
	    highest = count;
    }
    return (highest);
}
