#include "dxf2vect.h"


dxf_add_extents ()
{
    int		count;
    char	filename[300];
    int		afd;
    FILE    *fp;

    if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
    {
	dxf_head.W = w; 
	dxf_head.E = e;
	dxf_head.S = s;
	dxf_head.N = n;
    }
    for (count = 0; count < num_open_layers; count++)
    {
#ifdef DEBUG
	fprintf(stderr,"%d open_layers %s\n",count,layers[count].name);
#endif
	if (layers[count].type != DXF_ASCII)
	    continue;

	if (!ascii_flag->answer) /* FOR USE IN BINARY FILE */
	{
	    Vect_copy_head_data (&dxf_head, &(layers[count].Map->head));
	    Vect_close (layers[count].Map);
	}
	else /*FOR USE IN ASCII FILE */
	{
	    fseek (layers[count].fd, e_off, 0);
	    fprintf (layers[count].fd, "%-60.2lf", e);

	    fseek (layers[count].fd, n_off, 0);
	    fprintf (layers[count].fd, "%-60.2lf", n);

	    fseek (layers[count].fd, s_off, 0);
	    fprintf (layers[count].fd, "%-60.2lf", s);

	    fseek (layers[count].fd, w_off, 0);
	    fprintf (layers[count].fd, "%-60.2lf", w);

	/* GEE, this must be where the files get closed ... */
	    fclose (layers[count].fd);
	}
    }
    for (count = 0; count < num_closed_layers; count++)
    {
	if (closed_layers[count].type != DXF_ASCII)
	    continue;
	if (closed_layers[count].status < 0)
	    continue;

#ifdef DEBUG
	fprintf(stderr,"open_layers %s\n",closed_layers[count].name);
#endif
	sprintf (filename, "%s.%s", basename, closed_layers[count].name);

	/* temporarily reopen file */
	if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
	{
	    fp = G_fopen_append ("dig", filename);
	    closed_layers[count].Map->dig_fp = fp;
	}
	else
	    fp = G_fopen_append ("dig_ascii", filename);


	if (fp == NULL)
	{
	    fprintf(stderr,"error: unable to open dig file %s\n",filename);
	    exit (-1);
	}

	/* print the extents to this file */
	if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
	{
	    Vect_copy_head_data (&dxf_head, &(closed_layers[count].Map->head));
	    Vect_close (closed_layers[count].Map);
	}
	else /*FOR USE IN ASCII FILE */
	{
	    fseek (fp, e_off, 0);
	    fprintf (fp, "%-60.2lf", e);
	    fseek (fp, n_off, 0);
	    fprintf (fp, "%-60.2lf", n);
	    fseek (fp, s_off, 0);
	    fprintf (fp, "%-60.2lf", s);
	    fseek (fp, w_off, 0);
	    fprintf (fp, "%-60.2lf", w);

	    fclose (fp);
	}
    }
}
