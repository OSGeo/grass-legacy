#include <stdio.h>
#include <math.h>
#include "gis.h"

/*
 * r.sunmask:
 *   Calculates the cast shadow areas from a DEM
 *
 * input: DEM (int, float, double)
 * output: binary shadow map
 *    no shadow: null()
 *    shadow:    1
 *
 * Author: Janne Soimasuo, Finland 1994
 *
 * GPL >= 2
 *
 * MN 2/2001: attempt to update to FP
 * Huidae Cho 3/2001: FP update done
 * 		      but it's somewhat slow with non-CELL maps
 *
 */
 
/*
#define	RASTER_VALUE_FUNC
*/


union RASTER_PTR
{
    void	*v;
    CELL	*c;
    FCELL	*f;
    DCELL	*d;
};


#ifdef	RASTER_VALUE_FUNC

double raster_value(union RASTER_PTR buf, int data_type, int col);

#else

#define	raster_value(buf, data_type, col)	((double)(data_type == CELL_TYPE ? buf.c[col] : (data_type == FCELL_TYPE ? buf.f[col] : buf.d[col])))

#endif


int main(int argc, char *argv[]) 
{
    char *mapset;
    struct Cell_head window;
    union RASTER_PTR elevbuf, tmpbuf, outbuf;
    CELL min, max;
    DCELL dvalue, dvalue2, dmin, dmax;
    RASTER_MAP_TYPE data_type;
    struct Range range;
    struct FPRange fprange;
    double drow, dcol;
    int elev_fd, output_fd, zeros;
    char buf[1024];
    char buf1[100], buf2[100];
    char **ptr;
    double G_northing_to_row();
    double G_easting_to_col();
    struct Option *opt1, *opt2, *opt3, *opt4;
    struct Flag *flag1;
    struct GModule *module;
    char *name, *outname;
    double dazi, dalti;
    double azi, alti;
    double nstep,estep;
    double hight1,hight2,maxh;
    double east, east1, north, north1;
    int row1, col1;
    int row, col;
    char OK;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
            "Calculates cast shadow areas from sun position and DEM.";

    opt1 = G_define_option();
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
    
    flag1 = G_define_flag();
    flag1->key         = 'z' ;
    flag1->description = "Zero is a real elevation" ;

    
    if (G_parser(argc, argv))
      exit(-1);
	
    zeros = flag1->answer;

    G_get_window (&window);
    
    sscanf(opt3->answer,"%lf",&dalti);
    sscanf(opt4->answer,"%lf",&dazi);
    name = opt1->answer;
    outname= opt2->answer;


    mapset = G_mapset();
    
    /* Search for output layer in all mapsets ? 
       G_find_cell2 () */

    if((elev_fd = G_open_cell_old (name, mapset)) < 0)
    {
      sprintf (buf,"can't open %s", name);
      G_fatal_error(buf);
    }
    if((output_fd = G_open_cell_new(outname)) < 0)
    {
      sprintf (buf,"can't open %s", outname);
      G_fatal_error(buf);
    }

    data_type = G_raster_map_type(name, mapset);
    elevbuf.v = G_allocate_raster_buf(data_type);
    tmpbuf.v  = G_allocate_raster_buf(data_type);
    outbuf.v  = G_allocate_raster_buf(CELL_TYPE); /* binary map */

    if(data_type == CELL_TYPE)
    {
       if ((G_read_range(name, mapset,&range))<0)
       {
         sprintf (buf,"can't open range file for %s",name);
         G_fatal_error(buf);
       }
       G_get_range_min_max(&range,&min,&max);
       dmin = (double) min;
       dmax = (double) max;
    }
    else
    {
        G_read_fp_range(name, mapset, &fprange);
        G_get_fp_range_min_max(&fprange,&dmin,&dmax);
    }

    azi=2*3.1415926*dazi/360;
    alti=2*3.1415926*dalti/360;
    nstep=cos(azi)*window.ns_res;
    estep=sin(azi)*window.ew_res;
    row1=0;

    while (row1 < window.rows) 
	  {
            G_percent(row1, window.rows, 2);
	    col1=0;
	    drow=-1;
	    if (G_get_raster_row(elev_fd, elevbuf.v, row1, data_type) < 0)
	      G_fatal_error("can't read row in input elevation map");

	    while (col1<window.cols)
	      {
		dvalue=raster_value(elevbuf, data_type, col1);
/*		outbuf.c[col1]=1;*/
		G_set_null_value(&outbuf.c[col1],1,data_type);
		OK=1;
		east=G_col_to_easting(col1+0.5,&window);
		north=G_row_to_northing(row1+0.5,&window);
		east1=east;
		north1=north;
		if (dvalue==0.0 && !zeros) OK=0;
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
				if ((maxh) > (dmax-dvalue))
					OK=0;
				else
				  {
				  dcol=G_easting_to_col(east,&window);
				  if(drow!=G_northing_to_row(north,&window))
					{
					drow=G_northing_to_row(north,&window);
	    				G_get_raster_row(elev_fd, tmpbuf.v,(int) drow, data_type);
					}
				  dvalue2=raster_value(tmpbuf, data_type, (int)dcol);
				  if ((dvalue2-dvalue)>(maxh))
					{
					OK=0;
					outbuf.c[col1]=1;
					}
				  }
				}
			}	
		col1+=1;
	      }
	    G_put_raster_row(output_fd, outbuf.c, CELL_TYPE);
	    row1+=1;
	  }
    
    G_close_cell(output_fd);
    G_close_cell(elev_fd);

    exit(0);
}

#ifdef	RASTER_VALUE_FUNC
double raster_value(union RASTER_PTR buf, int data_type, int col)
{
	double	retval;

	switch(data_type){
		case CELL_TYPE:
			retval = (double) buf.c[col];
			break;
		case FCELL_TYPE:
			retval = (double) buf.f[col];
			break;
		case DCELL_TYPE:
			retval = (double) buf.d[col];
			break;
	}

	return retval;
}
#endif

