/****************************************************************/
/*								*/
/*	store_landpattern.c	in	~/src/i_range		*/
/*								*/
/*	This function stores the pattern in its final position 	*/
/*	as decided by the user in a dlg-3 file.	The binary	*/
/*	dlg-3 file is stored under the database element		*/
/*	"landpatterns" whereas the orientation of the vertical	*/
/*	axis of the pattern and the location of the origin	*/
/*	is stored under the database element "landpat_info"	*/
/*								*/
/****************************************************************/


#include "dlg.h"
#include "gis.h"

#define         PUT(x,y)        fwrite(x,y,1,fpw)

struct box *store_landpattern(box)
	struct box *box;
{
	FILE *fpw, *fopen();
	extern FILE *fpattern;
	extern struct dlg dlg;
	extern double **line_coor_info, angle_vert_axis;
        extern double origin_x, origin_y;
	extern double *node_coor_info, *area_coor_info;
	extern txt_a,txt_b;
	extern char buf[];
	char landpattern_file[25];
	int i,j,n;

	while(1)	/*	loop infinitely			*/
	{
	erase_in_poly(box);

	/* read in the name of the landpattern from text line	*/
	sprintf(buf, "Enter name of the landpattern : ");
	throw_text(buf,txt_a,txt_b);
	input();
	sprintf(landpattern_file,buf);

	if(!G_find_file("landpatterns",landpattern_file,G_mapset())) 
	break;		/* if unique name, continue		*/

	else		/* othewise, warn and get new name	*/
		{
	erase_in_poly(box);
	sprintf(buf,
		"Landpattern already exists. Provide unique name.");

	throw_text(buf,txt_a,txt_b);
	sleep(4);
		}
	}	/*	end of while				*/
	

	/*	open landpattern file for writing		*/
	fpw = G_fopen_new("landpatterns",landpattern_file);

	/*	write header info in the dlg-3 file		*/
        dlg_write_header(fpw, &dlg);

	/*	write node info					*/
	for (i=1; i<= dlg.max_nodes ; i++)
        {
	dlg_read_node(fpattern,&dlg,i);

        PUT("N", sizeof(char));
        PUT( &i , sizeof(i));

	dlg.node.x = *(node_coor_info + 2*i - 2);
	dlg.node.y = *(node_coor_info + 2*i -1);

        dlg_write_node(fpw,&dlg,i);
        }

	/*	write area info					*/
	for (i=1;i<= dlg.max_areas ; i++)
        {
        dlg_read_area(fpattern,&dlg,i);

        PUT("A", sizeof(char));
        PUT(&i , sizeof(i));

	dlg.area.x = *(area_coor_info + 2*i -2);
	dlg.area.y = *(area_coor_info + 2*i -1);

        dlg_write_area(fpw,&dlg,i);
        }


	/*	write line info					*/
	for (i=1; i<= dlg.max_lines ; i++)
        {
        dlg_read_line(fpattern,&dlg,i);
 
        PUT("L", sizeof(char));
        PUT(&i , sizeof(i));

	for(j=1;j<= dlg.line.n_coors ;j++){
         
        *(dlg.line.coors+2*j-2)= *(*(line_coor_info+i-1)+2*(j-1));
        *(dlg.line.coors+2*j-1)= *(*(line_coor_info+i-1)+2*j-1)  ;
                                          }

        dlg_write_line(fpw,&dlg,i);
        }

	
	fclose(fpw);
	fclose(fpattern);

	/* storing the location of the origin and the 		*/
	/* orientation of the vertical axis of pattern		*/
	G__file_name(buf,"landpat_info","",G_mapset());
	sprintf(buf,"%s/%s",buf,landpattern_file);
	fpw = fopen(buf,"w");
	fprintf(fpw,"%lf %lf %lf",angle_vert_axis,origin_x,origin_y);
	fclose(fpw);
 
        return(box);

}

/************* END OF FUNCTION "STORE_LANDPATTERN" **************/
	



