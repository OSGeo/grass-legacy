#include "Vect.h"
#include "dxf2vect.h"
/* JUST FOR REFERENCE */
/*  struct dig_head
    char organization[30] ;
    char date[20] ;
    char your_name[20] ;
    char map_name[41] ;
    char source_date[11] ;
    long  orig_scale ;
    char line_3[73] ;
    int plani_zone ;
    double W, E, S, N ;
    double digit_thresh ;
    double map_thresh ;
*/


int 
dxf_make_header (DXF_DIG *Layer)
{
    char *date; 
    char *name;

	/* either print ascii(will do piecemeal to permit use of ftell or will print
	** binary file using the dig_head structure and the write_head_binary func*/

    /* CALCULATE TODAY'S DATE */

    date = G_date();
    /* DETERMINE USER'S NAME */
    name = G_whoami();

	
    /* ORGANIZATION NAME DEFAULT IS USED */
	if(!ascii_flag->answer) /* FOR USE  IN BINARY FILE */
	{
		strcpy(dxf_head.organization," US Army Const. Eng. Rsch. Lab\n"); 
		strcpy(dxf_head.date,date);
		strcpy(dxf_head.your_name,name);
		strcpy(dxf_head.map_name,dxf_file);
		dxf_head.source_date[0] = 0;
		dxf_head.line_3[0] = 0;
		/*strcpy(dxf_head.source_date, );  MAP DATE
		**strcpy(dxf_head.line_3, ); OTHER INFO
		*/	
		dxf_head.orig_scale = 2400;
		dxf_head.plani_zone = 0;
		dxf_head.W = 0;
		dxf_head.E = 0;
		dxf_head.S = 0;
		dxf_head.N = 0;
		dxf_head.digit_thresh = 0.;
		
		dxf_head.map_thresh = 0.0;
		Vect_copy_head_data (&dxf_head, &(Layer->Map->head));
	}
	else /* FOR USE IN ASCII FILE */
	{
		fprintf (Layer->fd, "ORGANIZATION: US Army Const. Eng. Rsch. Lab\n");
		if (date !=NULL)
			fprintf (Layer->fd, "DIGIT DATE:   %s\n",date);
/* IF G_date RETURNS A NULL */
		else 
			fprintf (Layer->fd, "DIGIT DATE:     \n"); 
		if (name != NULL)
			fprintf (Layer->fd, "DIGIT NAME:   %s\n",name);
/* IF G_whoami returns a NULL */			
		else
			fprintf (Layer->fd, "DIGIT NAME:     \n");
		
		fprintf (Layer->fd, "MAP NAME:     %s\n",dxf_file);
		fprintf (Layer->fd, "MAP DATE:      \n");
		fprintf (Layer->fd, "MAP SCALE:     2400\n");
		fprintf (Layer->fd, "OTHER INFO:    \n");
		fprintf (Layer->fd, "ZONE:        0  \n");
		fprintf (Layer->fd, "WEST EDGE:    ");
		w_off = ftell (Layer->fd);
		fprintf (Layer->fd, "%-60s\n", "0");
		fprintf (Layer->fd, "EAST EDGE:    ");
		e_off = ftell (Layer->fd);
		fprintf (Layer->fd, "%-60s\n", "0");
		fprintf (Layer->fd, "SOUTH EDGE:   ");
		s_off = ftell (Layer->fd);
		fprintf (Layer->fd, "%-60s\n", "0");
		fprintf (Layer->fd, "NORTH EDGE:   ");
		n_off = ftell (Layer->fd);
		fprintf (Layer->fd, "%-60s\n", "0");
		fprintf (Layer->fd, "MAP THRESH:   00.00\n");
		fprintf (Layer->fd, "VERTI:         \n");
	}

	return 0;
}
