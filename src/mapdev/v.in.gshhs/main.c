/*
 * $ID$
 *
 * PROGRAM:     v.in.gshhs (based on gshhstograss.c)
 * AUTHOR:      Simon Cox (simon@ned.dem.csiro.au) & (gshhstograss.c)
 * 		Paul Wessel (wessel@soest.hawaii.edu) & (gshhstograss.c)
 *		Bob Covill <bcovill@tekmap.ns.ca> (v.in.gshhs)
 * DATE:	December 22, 2000
 * PURPOSE:	To extract GRASS binary vector data from binary gshhs
 *		shoreline data as described in the Wessel, P., and W.H.F.
 * 		High-resolution Shoreline Database, J. Geophys. Res., 101,
 *		8741-8743, 1999.
 *		http://www.soest.hawaii.edu/wessel/hshhs/gshhs.html
 */

/*uncomment next line to get debug output*/
/*
#define DEBUG
*/

#include "gshhs.h"
#include <unistd.h>
#include <string.h>
#include <time.h>

#include "gis.h"
#include "Vect.h"
#include "projects.h"


main (argc, argv)
int     argc;
char **argv;

{
        double w, e, s, n, area, lon, lat;
	double *X, *Y, UX, UY;
        double minx = -180., maxx = 180., miny = -90., maxy = 90.;
	double minx_u, maxx_u, miny_u, maxy_u;
	char *dataname, *outname, *progname ;
	char errmsg[200] ;
        char buf[64], source, dig_name[24], att_name[24], cats_name[24];
        static char *slevel[] = { "null" , "land" , "lake" , "island_in_lake" , "pond_in_island_in_lake" };
        FILE    *fp, *dig_ascii, *dig_att;
        int     k, i, m, max = 270000000;
	int cnt=1 ;
        struct  POINT_1 p;
        struct GSHHS h;
	struct pj_info info_in;
	struct pj_info info_out;
	char parms_in[256];
	char *parms;
	struct Key_Value *in_proj_keys, *in_unit_keys;
	struct Key_Value *out_proj_keys, *out_unit_keys;
        int c, meridian = 0 ;
	char dummy[2];
        time_t tloc;
	int    day, yr;
	char  date[25], mon[4];
	int type, project=0, zone;
	double *xarray, *yarray ;
	struct line_pnts *Points;
	struct Map_info VectMap;
	struct Cell_head region;
	struct Categories cats;
	char AttText[512] ;
        double a, e2 ;
	double lat_nw, lon_nw, lat_ne, lon_ne ;
	double lat_sw, lon_sw, lat_se, lon_se ;
	union {
	int testWord;
	char testByte[4];
	} endianTest;
	int swapFlag;
	struct
	{
	struct Option *input, *output, *n, *s, *e, *w ;
	} parm;
	struct
	{
	struct Flag *g, *support, *a;
	} flag;
	

        progname = argv[0];

	G_gisinit(argv[0]);

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->gisprompt  = "old,cell,raster" ;
    parm.input->description= "Name of Binned Shoreline File" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->gisprompt  = "old,cell,raster" ;
    parm.output->description= "Name for Output Vector" ;

    parm.n= G_define_option() ;
    parm.n->key        = "n";
    parm.n->type       = TYPE_DOUBLE;
    parm.n->required   = NO;
    parm.n->description= "North Limit" ;

    parm.s= G_define_option() ;
    parm.s->key        = "s";
    parm.s->type       = TYPE_DOUBLE;
    parm.s->required   = NO;
    parm.s->description= "South Limit" ;

    parm.e= G_define_option() ;
    parm.e->key        = "e";
    parm.e->type       = TYPE_DOUBLE;
    parm.e->required   = NO;
    parm.e->description= "East Limit" ;

    parm.w= G_define_option() ;
    parm.w->key        = "w";
    parm.w->type       = TYPE_DOUBLE;
    parm.w->required   = NO;
    parm.w->description= "West Limit" ;

    flag.g = G_define_flag();
    flag.g->key = 'g';
    flag.g->description = "Get Coordinates from Current GRASS Region";

    flag.support = G_define_flag();
    flag.support->key = 's';
    flag.support->description = "Automatically run \"v.support\" on vector file";

    flag.a= G_define_flag();
    flag.a->key = 'a';
    flag.a->description = "Import shoreline as line type Area (default = line)";

    if (G_parser(argc, argv))
        exit (1);

/* Get Parameters */
dataname = parm.input->answer;
outname =  parm.output->answer;

G_get_window (&region);
zone = region.zone;

/* In Info */
pj_zero_proj(&info_in);
parms_in[0] = '\0';
pj_get_string(&info_in, parms_in);


/* Out Info */
out_proj_keys = G_get_projinfo();
out_unit_keys = G_get_projunits();
if (pj_get_kv(&info_out,out_proj_keys,out_unit_keys) < 0) {
exit (0);
}

if (flag.g->answer) {
/* Get Coordinates from Current Region */
if (G_projection() != PROJECTION_LL) {
fprintf(stderr, "Using Other Proj\n");

minx_u = region.west;
maxx_u = region.east;
miny_u = region.south;
maxy_u = region.north;

/* NW */
lon_nw = region.west;
lat_nw = region.north;
if(pj_do_proj(&lon_nw,&lat_nw,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}
/* NE */
lon_ne = region.east;
lat_ne = region.north;
if(pj_do_proj(&lon_ne,&lat_ne,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}
/* SE */
lon_se = region.east;
lat_se = region.south;
if(pj_do_proj(&lon_se,&lat_se,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}
/* SW */
lon_sw = region.west;
lat_sw = region.south;
if(pj_do_proj(&lon_sw,&lat_sw,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}

/*Get North Max */
if ( lat_nw > lat_ne )
        maxy = lat_nw ;
        else
        maxy = lat_ne ;
/*Get South Min*/
        if (lat_sw < lat_se)
        miny = lat_sw ;
        else
        miny = lat_se ;
/*Get East Max*/
        if ( lon_ne > lon_se)
        maxx = lon_ne ;
        else
        maxx = lon_se ;
/*Get West Min*/
        if ( lon_nw < lon_sw)
        minx = lon_nw ;
        else
        minx = lon_sw ;

} else {
/* Lat Long Projection */
minx = region.west ;
maxx = region.east ;
miny = region.south ;
maxy = region.north ;

}
/* Get Coordinates from command line */
} else {
/* Lat Long Projection */
if (!parm.w->answer || !parm.e->answer || !parm.s->answer || !parm.n->answer) {
sprintf(errmsg, "%s:  Missing region parameters ...\nMust supply n, s, e, w values\n", progname);
        G_fatal_error (errmsg);
	}

sscanf (parm.w->answer, "%lf", &minx);
sscanf (parm.e->answer, "%lf", &maxx);
sscanf (parm.s->answer, "%lf", &miny);
sscanf (parm.n->answer, "%lf", &maxy);

/* UTM projection */
if (G_projection() != PROJECTION_LL) {

minx_u = minx;
maxx_u = maxx;
miny_u = miny;
maxy_u = maxy;

/* NW */
lon_nw = region.west;
lat_nw = region.north;
if(pj_do_proj(&lon_nw,&lat_nw,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}
/* NE */
lon_ne = region.east;
lat_ne = region.north;
if(pj_do_proj(&lon_ne,&lat_ne,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}
/* SE */
lon_se = region.east;
lat_se = region.south;
if(pj_do_proj(&lon_se,&lat_se,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}
/* SW */
lon_sw = region.west;
lat_sw = region.south;
if(pj_do_proj(&lon_sw,&lat_sw,&info_out,&info_in)<0) {
fprintf(stderr,"Error in pj_do_proj\n");
exit(0);
}

/*Get North Max */
if ( lat_nw < lat_ne )
        maxy = lat_nw ;
        else
        maxy = lat_ne ;
/*Get South Min*/
        if (lat_sw > lat_se)
        miny = lat_sw ;
        else
        miny = lat_se ;
/*Get East Max*/
        if ( lon_ne < lon_se)
        maxx = lon_ne ;
        else
        maxx = lon_se ;
/*Get West Min*/
        if ( lon_nw > lon_sw)
        minx = lon_nw ;
        else
        minx = lon_sw ;
}
}

fprintf(stderr, "Using Bounds N=%f S=%f E=%f W=%f\n", maxy, miny, maxx, minx);

/* Open Binned Shoreline for Reading */
        if ((fp = fopen (dataname, "rb")) == NULL ) {
	sprintf(errmsg, "%s:  Could not find file %s.\n", progname, dataname);
	G_fatal_error (errmsg);
        }

/* Check Coordinate Accuracy */
	if( minx > maxx ){
	sprintf(errmsg, "%s:  minx %f > maxx %f.\n", progname, minx, maxx);
	G_fatal_error (errmsg);
        }

        if( miny > maxy ){
	sprintf(errmsg, "%s:  miny %f > maxy %f.\n", progname, miny, maxy);
	G_fatal_error (errmsg);
        }

/* Open GRASS Vector File(s) */
	/* Open Binary Vect File */
	if (0 > Vect_open_new (&VectMap, outname)) {
	sprintf(errmsg, "Cannot Open Vector File %s\n", outname);
	G_fatal_error (errmsg);
	}
	/* Open Vect Att File */
	if ((dig_att = G_fopen_new ("dig_att", outname)) == NULL) {
	sprintf(errmsg, "Cannot Open Vector Att File %s\n", outname);
	G_fatal_error (errmsg);
	}
	
#ifdef DEBUG
        fprintf(stderr,"ORGANIZATION: \n");
        time(&tloc);
        fprintf(stderr,"DIGIT DATE:   %s",ctime(&tloc));
        fprintf(stderr,"DIGIT NAME:   %s\n",cuserid(buf));
        fprintf(stderr,"MAP NAME:     Global Shorelines\n");
        fprintf(stderr,"MAP DATE:     1996\n");
        fprintf(stderr,"MAP SCALE:    1\n");
        fprintf(stderr,"OTHER INFO:   \n");
        fprintf(stderr,"ZONE:         0\n");
        fprintf(stderr,"WEST EDGE:    %f\n",minx);
        fprintf(stderr,"EAST EDGE:    %f\n",maxx);
        fprintf(stderr,"SOUTH EDGE:   %f\n",miny);
        fprintf(stderr,"NORTH EDGE:   %f\n",maxy);
        fprintf(stderr,"MAP THRESH:   0.0001\n");
        fprintf(stderr,"VERTI:\n");
#endif


/* Check Endian State of Host Computer*/
    endianTest.testWord = 1;
    if (endianTest.testByte[0] == 1) {
	fprintf(stderr, "Swapping Enabled \n");
        swapFlag = 1; /*true: little endian */
    } else {
        swapFlag = 0; /*false: big endian */
    }


	/* set Vector Line type */
	if (flag.a->answer) {
	/* Import shoreline are area */
	type = AREA;
	} else {
	type = LINE;
	}

        /* Initialize Vect struct */
        Points = Vect_new_line_struct();

	/* Initialize category array - dig_cats */
	 G_init_cats( (CELL)0, "", &cats);

/* Start Reading Binned Shoreline */
/* Read Data Header Record and get # records */

while (fread((void *)&h, (size_t)sizeof (struct GSHHS), (size_t)1, fp) == 1) {

	if (swapFlag == 1) {
                h.id = swabi4 ((unsigned int)h.id);
                h.n = swabi4 ((unsigned int)h.n);
                h.level = swabi4 ((unsigned int)h.level);
                h.west = swabi4 ((unsigned int)h.west);
                h.east = swabi4 ((unsigned int)h.east);
                h.south = swabi4 ((unsigned int)h.south);
                h.north = swabi4 ((unsigned int)h.north);
                h.area = swabi4 ((unsigned int)h.area);
                h.greenwich = swabi2 ((unsigned int)h.greenwich);
                h.source = swabi2 ((unsigned int)h.source);
		}
                w = h.west  * 1.0e-6;
                e = h.east  * 1.0e-6;
                s = h.south * 1.0e-6;
                n = h.north * 1.0e-6;
                source = (h.source == 1) ? 'W' : 'C';
                area = 0.1 * h.area;

	xarray = (double *) dig_falloc(h.n, sizeof(double)) ;
        yarray = (double *) dig_falloc(h.n, sizeof(double)) ;

	X = xarray ;
	Y = yarray ;

i = 0;

/* Got Header now read in data structure */
                for (k = 0; k < h.n; k++) {
if (fread ((void *)&p, (size_t)sizeof(struct POINT_1), (size_t)1, fp) != 1)
	G_fatal_error("v.in.gshhs:  Error reading data.\n");

		if (swapFlag == 1) {
                        p.x = swabi4 ((unsigned int)p.x);
                        p.y = swabi4 ((unsigned int)p.y);
		}
	lon = (h.greenwich && p.x > max) ? p.x * 1.0e-6 -360.0 : p.x * 1.0e-6;
	if (lon > 180.) lon = lon - 360 ;
	lat = (p.y * 1.0e-6) ;

	/* Start test loop for coords inside region */
	if (( lon < maxx && lon > minx ) && ( lat < maxy && lat > miny)) {

        if (G_projection() != PROJECTION_LL) {
	if(pj_do_proj(&lon,&lat,&info_in,&info_out)<0) {
	fprintf(stderr,"Error in pj_do_proj\n");
	exit(0);
	}
        if(i == 1) {
                fprintf(dig_att,"L  %-12f  %-12f  %-8d \n", lon, lat,cnt);
                sprintf(AttText, "%s", slevel[h.level]);
                G_set_cat(cnt, AttText, &cats);
                }
        X[i] = lon;
        Y[i] = lat;
	} else {
	/* Lat Long Projection */
	if(i == 1) {
		fprintf(dig_att,"L  %-12f  %-12f  %-8d \n",lon, lat, cnt);
		sprintf(AttText, "%s", slevel[h.level]);
		G_set_cat(cnt, AttText, &cats);
		}
	X[i] = lon;
	Y[i] = lat;
	}
	i++;
	} /* Done Min Max */
	else {
	/* Line goes out of bounds */
	/* Write out segment and reset counter */
	if ( i > 1) {
        /* Write if within limits */
        if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, i))
        G_fatal_error ("Out of memory");

        Vect_write_line (&VectMap, type, Points);
	cnt++;
	i = 0;
        }
	}
		} /* Done with Line */
	 if ( i > 1) {
	/* Write if within limits */
        if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, i))
        G_fatal_error ("Out of memory");

        Vect_write_line (&VectMap, type, Points);
	cnt++;
        }
/*                max = 180000000;   */     /* Only Eurasiafrica needs 270 */
        } /* Done with File */

	G_write_vector_cats(outname, &cats);

/* Write out Vect Header info */
    VectMap.head.orig_scale = 100000l;
/* Format date entry */
    G_strncpy( date, G_date(), 24);
    sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
    if (yr < 2000) yr = yr - 1900;
    else yr = yr - 2000;
    sprintf(date,"%s %d %d",mon,day,yr);

strcpy(VectMap.head.organization," ");
strcpy(VectMap.head.date, date);
strcpy(VectMap.head.your_name, G_whoami() );
strcpy(VectMap.head.map_name, outname);
strcpy(VectMap.head.line_3,"Binned shoreline imported by v.in.gshhs" );

if (G_projection() != PROJECTION_LL) {
    VectMap.head.W = minx_u;
    VectMap.head.S = miny_u;
    VectMap.head.E = maxx_u;
    VectMap.head.N = maxy_u;
    VectMap.head.plani_zone = zone;
} else {
/* Lat Long Proj */
    VectMap.head.W = minx;
    VectMap.head.S = miny;
    VectMap.head.E = maxx;
    VectMap.head.N = maxy;
    VectMap.head.plani_zone = 0;
}

	Vect_close (&VectMap);

        fclose(fp);
	fclose(dig_att);

	fprintf(stderr, "Import Complete !\n");

        /* If "-s" flag is passed as argument then run "v.support" on */
        /* newly created vector file (output).                        */
        if (flag.support->answer)
            {
             fprintf(stderr, "Running v.support to build topology...");
             sprintf(buf,"%s/bin/v.support map=%s", G_gisbase(), outname);
             G_system(buf);
             fprintf(stderr, "Done .\n");
            }
        else
	fprintf(stderr, "Run v.support to build topology\n");

        exit (0);
}
