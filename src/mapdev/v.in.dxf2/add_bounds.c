/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */
/* revised by J Moorman 7/23/90
** original by Chuck Ehschlaeger
*/

#include "dxf2vect.h"


/* NOTE.  This does not seem to be currently used! */


#ifdef FOO
int 
dxf_add_boundaries (void)
{
    int		count;
    char	filename[300];
    int		afd;
    FILE	*fp;

	if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
	{
		dxf_head.W = w; 
		dxf_head.E = e;
		dxf_head.S = s;
		dxf_head.N = n;
	}

    for (count = 0; count < num_open_layers; count++)
    {
	if (layers[count].type != DXF_ASCII)
	    continue;

        /* Current opened layers opened using G_fopen_append to find lines
         * and append info.
         * However, G_fopen_modify should be used to update header infomation
         * which is located starting part of a file.
         */ 
	sprintf (filename, "%s.%s", base_name, layers[count].name);

	if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
	{
	    fclose(layers[count].Map->dig_fp);
	    layers[count].Map->dig_fp = G_fopen_modify ("dig", filename);

	    if (layers[count].Map->dig_fp == NULL)
	    {
		fprintf(stderr,"error: unable to open dig file\n");
		exit (-1);
	    }
	}
	else
	{
	    fclose(layers[count].fd);
	    layers[count].fd = G_fopen_modify ("dig_ascii", filename);

	    if (layers[count].fd == NULL)
	    {
		fprintf(stderr,"error: unable to open dig file\n");
		exit (-1);
	    }
	}

	if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
			 dig_write_head_binary(layers[count].fd,&dxf_head);
	else
	{
		fseek (layers[count].fd, e_off, 0);
		fprintf (layers[count].fd, "%-60.2lf", XMAX);
		fseek (layers[count].fd, n_off, 0);
		fprintf (layers[count].fd, "%-60.2lf", YMAX);
		fseek (layers[count].fd, s_off, 0);
		fprintf (layers[count].fd, "%-60.2lf", YMIN);
		fseek (layers[count].fd, w_off, 0);
		fprintf (layers[count].fd, "%-60.2lf", XMIN);
	}		
	fclose (layers[count].fd);
    }

	/* NOW FOR CLOSED LAYERS */
    for (count = 0; count < num_closed_layers; count++)
    {
#ifdef DEBUG
	fprintf(stderr,"%d closed_layers.name %s\n",closed_layers[count].name);
#endif
		if (closed_layers[count].status < 0)
			continue;
		if (closed_layers[count].type != DXF_ASCII)
			continue;

		/* temporarily reopen file */
		sprintf (filename, "%s.%s", base_name, closed_layers[count].name);
		if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
			fp = G_fopen_modify ("dig", filename);

		else
			fp = G_fopen_modify ("dig_ascii", filename);

		if (fp == NULL)
		{
			fprintf(stderr,"error: unable to open dig file\n");
			exit (-1);
		}
		/* print the extents to this file */
		if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
			 dig_write_head_binary(fp,&dxf_head);
		
		else /*FOR USE IN ASCII FILE */
		{
		/* print the extents to this file */
			fseek (fp, e_off, 0);
			fprintf (fp, "%-60.2lf", XMAX);
			fseek (fp, n_off, 0);
			fprintf (fp, "%-60.2lf", YMAX);
			fseek (fp, s_off, 0);
			fprintf (fp, "%-60.2lf", YMIN);
			fseek (fp, w_off, 0);
			fprintf (fp, "%-60.2lf", XMIN);
		}
		fclose (fp);
    }
}
#endif
