#include <stdio.h>
#include <math.h>
#include "gis.h"

int main(int argc, char *argv[]) 
{
    char *mapset;
    struct Cell_head window;
    CELL *cell, value,value2 ,min, max, *tmpcell,*outcell;
    struct Range range;
    double drow, dcol;
    int elev, output, zeros;
    char buf[1024];
    char buf1[100], buf2[100];
    char **ptr;
    double G_northing_to_row();
    double G_easting_to_col();
    struct Option *opt1, *opt2, *opt3, *opt4, *opt5;
    struct Flag *flag1;
    char *name, *name2;
    double dazi, dalti,zmult;
    double azi, alti;
    double nstep,estep;
    double hight1,hight2,maxh;
    double east, east1, north, north1;
    int row1, col1;
    int row, col;
    char OK;
    char *new_name;


    opt1 = G_define_option() ;
    opt1->key        = "elev" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->multiple   = NO ;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "Name of elevation raster map" ;

    opt2 = G_define_option() ;
    opt2->key        = "output" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = YES ;
    opt2->multiple   = NO ;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->description= "Output raster map having shadows" ;

    opt3 = G_define_option() ;
    opt3->key        = "altitude" ;
    opt3->type       = TYPE_DOUBLE ;
    opt3->required   = YES ;
    opt3->options    = "0-89.999";
    opt3->description= "altitude of the sun above horizon, degrees" ;

    opt4 = G_define_option() ;
    opt4->key        = "azimuth" ;
    opt4->type       = TYPE_DOUBLE ;
    opt4->required   = YES ;
    opt4->options    = "0-360";
    opt4->description= "azimuth of the sun from the north, degrees" ;
    
    opt5 = G_define_option() ;
    opt5->key        = "z-mult" ;
    opt5->type       = TYPE_DOUBLE ;
    opt5->required   = NO;
    opt5->answer     = "1.0";
    opt5->description= "Multiplier for elevation" ;

    flag1 = G_define_flag();
    flag1->key         = 'z' ;
    flag1->description = "Zero is a real elevation" ;

    G_gisinit (argv[0]);
    
    if (G_parser(argc, argv))
      exit(-1);
	
    
    cell = G_allocate_cell_buf();
    outcell = G_allocate_cell_buf();
    tmpcell = G_allocate_cell_buf();
    zeros = flag1->answer;

    G_get_window (&window);
    
    sscanf(opt3->answer,"%lf",&dalti);
    sscanf(opt4->answer,"%lf",&dazi);
    sscanf(opt5->answer,"%lf",&zmult);
    name = opt1->answer;
    name2= opt2->answer;


    if(NULL == (mapset = G_find_cell2 (name, "")))
      die (name, " - not found");
    if(0 > (elev = G_open_cell_old (name, mapset)))
      die ("can't open", name);

 
    if((output = G_open_cell_new(new_name=name2)) == NULL) 
      die ("cannot open output file ");

/*
    if ((G_read_range(name, mapset,&range))<0)
	die("cannot open range file for ",name);
    G_get_range_min_max(range,&min,&max);
*/
    get_range (name, mapset, &min, &max);

    azi=2*3.1415926*dazi/360;
    alti=2*3.1415926*dalti/360;
    nstep=cos(azi)*window.ns_res;
    estep=sin(azi)*window.ew_res;
	row1=0;

	while (row1 < window.rows) 
	  {
/*printf("%d\n",row1);*/
fprintf(stderr," %d %c complete\r",(int)100*row1/window.rows,'%');
	    col1=0;
	    drow=-1;
	    if (G_get_map_row(elev, cell, row1) < 0)
	      die (argv[1], " - can't read",elev);
	    while (col1<window.cols)
	      {
		value = cell[col1];
		outcell[col1]=1;
		OK=1;
		east=G_col_to_easting(col1+0.5,&window);
		north=G_row_to_northing(row1+0.5,&window);
		east1=east;
		north1=north;
		if (value==0 && !zeros) OK=0;
		while (OK==1)

			{
			east+=estep;
			north+=nstep;
			if(north>window.north || north < window.south 
			   || east>window.east || east < window.west)
				OK=0;
			else
				{
				maxh=tan(alti)*
				     sqrt((north1-north)*(north1-north)+
					  (east1-east)*(east1-east));
				if ((maxh*zmult) > (max-value))
					OK=0;
				else
				  {
				  dcol=G_easting_to_col(east,&window);
				  if(drow!=G_northing_to_row(north,&window))
					{
					drow=G_northing_to_row(north,&window);
	    				G_get_map_row(elev, tmpcell,(int) drow);
					}
				  value2=tmpcell[(int)dcol];
				  if ((value2-value)>(maxh*zmult))
					{
					OK=0;
					outcell[col1]=0;
					}
				  }
				}
			}	
		col1+=1;
	      }
	    G_put_map_row(output,outcell);
	    row1+=1;
	  }
    
G_close_cell(output);
G_close_cell(elev);    
}
