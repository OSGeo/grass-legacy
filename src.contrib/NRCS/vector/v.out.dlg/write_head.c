/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include "digit.h"
#include "dig_head.h"
#include "gis.h"
#include "export_dlg.h"
#include <sys/types.h>
#include <time.h>

#define LINE1_FMT  "%30.30s %20.20s %20.20s\n"
#define LINE2_FMT  "%40.40s %10.10s%c%8ld            \n"
#define LINE3_FMT  "Produced by GRASS Export_DLG                                            \n"
#define LINE4_FMT  "%6d%6d%6d%6d%-18s%6d%6d%6d%6d%3s   \n"
#define LINE5_FMT  "%-24s%-24s%-24s\n"
#define LINE10_FMT "%18s%18s%18s%18s\n"
#define LINE11_FMT "%2s    %12.6lf%12.6lf      %12.2lf%12.2lf            \n"
#define LINE15_FMT "%20s%4d%6d%6d %c%c %6d%6d %c%c%c%6d%6d   %c\n"

/*
#define LINE4_FMT  "%6d%6d%6d%6d%18.11le%6d%6d%6d%6d      \n"
#define LINE5_FMT  "%24.15le%24.15le%24.15le\n"
#define LINE10_FMT "%18.11le%18.11le%18.11le%18.11le\n"
#define LINE11_FMT "%2s    %12.6lf%12.6lf      %12.2lf%12.2lf            \n"
#define LINE15_FMT "%20s%4d%6d%6d %c%c %6d%6d %c%c%c%6d%6d   %c\n"
*/


char *dtype ();
write_dlg_head (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    line1 (map, d_head, fp);
    line2 (map, d_head, fp);
    line3 (map, d_head, fp);
    line4 (map, d_head, fp);
    line5_9 (map, d_head, fp);
    line10 (map, d_head, fp);
    line11_14 (map, d_head, fp);
    line15 (map, d_head, fp);
}

line1 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    char timebuf[20];
    struct tm *mytime;
    time_t clock;
    time_t tloc;

clock =time(&tloc);
/*printf("tim= %ld\n", clock);*/
mytime = localtime(&clock);
/*printf("%d-%d-%d\n", mytime->tm_mon + 1, mytime->tm_mday,mytime->tm_year);*/
    switch (dlgtype){
  	case 1:
	case 5:
	case 6:
	case 7:
	case 8:
    		fprintf (fp, "USDA-NRCS DLG DATA - CHARACTER FORMAT - %d-%d-%d VERSION\n", mytime->tm_mon + 1, mytime->tm_mday,mytime->tm_year);
		break;
	default:
    		fprintf (fp, LINE1_FMT, d_head->organization, d_head->source_date, d_head->your_name);
		break;
	}
}

line2 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    switch (dlgtype){
	case 5:
	case 6:
	case 7:
	case 8:
	case 1:
    		if (G_projection() != 0 && G_projection() != 3 )
    			fprintf (fp, "%-40.40s %10.10s%c%8ld            \n",
			d_head->map_name, d_head->source_date,
			' ', d_head->orig_scale);
		else
    			fprintf (fp, "%-40.40s %10.10s%c%8ld            \n",
			d_head->map_name, d_head->source_date,
			' ', 1);
		break;
	default:
    		if (G_projection() != 0 && G_projection() != 3 )
	    		fprintf (fp, LINE2_FMT, d_head->map_name,
			 	d_head->date, ' ', d_head->orig_scale);
		else
			fprintf (fp, LINE2_FMT, d_head->map_name,
				d_head->date, ' ', 1);
		break;
	}		

}

line3 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    char *datum;
    char path[100], buf[50];
    int i;
    /*
    fprintf (fp, LINE3_FMT);
    */
    switch (dlgtype){
	case 5:
	case 6:
	case 7:
	case 8:
    		datum = G_find_key_value("ellps", in_proj_keys);
		if (strcmp(datum, "clark66") == 0)
			fprintf (fp, "USDA/NRCS SSURGO DATA; NAD27\n");
		else if (strcmp(datum, "grs80") == 0)
			fprintf (fp, "USDA/NRCS SSURGO DATA; NAD83\n");
		break;
	case 1:
		path[0] = '\0';
        	for (i=1; i<=in_proj_keys->nitems-1; i++) {
			if (!strcmp(in_proj_keys->key[i], "es"))
				continue;
			if (!strcmp(in_proj_keys->key[i], "a"))
				continue;
          		sprintf(buf,"%s=%s ",
          		in_proj_keys->key[i],in_proj_keys->value[i]);
          		strcat(path,buf);
          		if (strlen(path) > 72) {
                		path[72] = '\0';
/*        	     		sprintf(path,"%s",in_proj_keys->value[0]);*/
             			break;
             		}
          	}
		fprintf (fp, "%-72.72s\n", path);
		break;
	default:
    		if (strlen (d_head->line_3) > 72)
			d_head->line_3[73] = 0;
   	 	fprintf (fp, "%72s\n", d_head->line_3);
		break;

    }
}

#define DLG3_CODE 	3

#define OTH_CODE 	0
#define UTM_CODE 	1
#define SP_CODE 	2  /* guess */

#define METERS_CODE 	2
#define FEET_CODE 	1  /* guess */

#define NUM_TRANSFORMS	4
#define NUM_SIDES	4
#define NUM_CATS	1
#define DIG_UNITS	0.001


line4 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    double resolution;
    int units, misc, proj;
    char NAD_CODE[1];

    switch (G_projection ()) {
	case 0:				/* XY */
	    proj = OTH_CODE;
	    break;
	case 1:				/* UTM */
	    proj = UTM_CODE;
	    break;
	case 2:				/* State Plane */
	    proj = SP_CODE;
	    break;
	default:
#ifdef NRCS_MODS
            if (strcmp(G_find_key_value("proj", in_proj_keys), "aea") == 0) proj = 3;
            if (strcmp(G_find_key_value("proj", in_proj_keys), "lcc") == 0) proj = 5;
            if (strcmp(G_find_key_value("proj", in_proj_keys), "merc") == 0) proj = 6;
            if (strcmp(G_find_key_value("proj", in_proj_keys), "tmerc") == 0) proj = 7;
            if (proj == 0) proj = PROJECTION_OTHER;
#else
	    proj = UTM_CODE;
#endif
	    break;
    }

    switch (G__projection_units (G_projection())) {
	case 0:
	    units = OTH_CODE;
	    break;
	case METERS:
	    units = METERS_CODE;
	    break;
	case FEET:
	    units = FEET_CODE;
	    break;
	default:
	    units = METERS_CODE;
	    break;
    }
    misc = 0;
    resolution = d_head->orig_scale * dig_unit_conversion () * DIG_UNITS;

    if (dlgnad == 1){
	sprintf(NAD_CODE,"%d",dlgnad);
    }else{
	sprintf(NAD_CODE," ");
    }



    fprintf (fp, LINE4_FMT, 
	DLG3_CODE, 		/* DLG-3 format */
	proj, 			/* Projection (UTM) */
	d_head->plani_zone,	/* UTM ZONE */
	units, 			/* Units */
	dtype (resolution, 18, 11), 		/* resolution */
	NUM_TRANSFORMS, 	/* # of tranformation params */
	misc, 			/* # of accuracy/misc records */
	NUM_SIDES, 		/* # of sides to window polygon */
	NUM_CATS,		/* # of DLG categories */
	NAD_CODE);		/* blank=nad27 1=nad83 */
}

line5_9 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    register int i;

    for (i = 5 ; i <= 9 ; i++)
    {
	switch (i) {
	    case 5:
		fprintf (fp, LINE5_FMT, dtype(0.0, 14, 15), dtype(0.0, 14, 15), dtype(0.0, 14, 15));
		break;
	    default:
		fprintf (fp, LINE5_FMT, dtype(0.0, 14, 15), dtype(0.0, 14, 15), dtype(0.0, 14, 15));
		break;
	}
    }
}

line10 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    fprintf (fp, LINE10_FMT, dtype(1.0, 18, 11), dtype(0.0, 18, 11), dtype(0.0, 18, 11), dtype(0.0, 18, 11));
}

line11_14 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    register int i;
    double x, y, lat, lon;
    int num_lat, num_lon;
 
    /* find middle */
    x = (map->Area[1].E - map->Area[1].W) / 2  + map->Area[1].W; 
    y = (map->Area[1].N - map->Area[1].S) / 2  + map->Area[1].S; 
    /* find lat/lon of middle*/
    lat = y;
    lon = x;
    pj_do_proj(&lon, &lat, &info_in, &info_out);
    /* find SE corner */
    /* added qformat to calculate corner
    num_lat = lat / .125;
    num_lon = lon / .125; 
    lat = num_lat * .125;
    lon = num_lon * .125;
    */
    num_lat = lat / dlgsize_n ;
    num_lon = lon / dlgsize_e; 
    lat = num_lat *  dlgsize_n;
    lon = num_lon *  dlgsize_e;
/*printf("se=%f %f\n",lon,lat);DEBUG*/
    
    switch (dlgtype){
    case 5:
    case 6:
    case 7:
    case 8:
	y = lat;
/* .125 is for full quad format set this in a command line option */
/* will now read variable dlgsize_e and dlgsize_n set from command line. */ 
/* This defaults to .125 if option is not set                  */

	x = lon - dlgsize_e;

/*printf("x=%f lon=%f dlgsize_e=%f\n",x,lon,dlgsize_e);DEBUG*/

    	pj_do_proj(&x, &y, &info_out, &info_in);
    	fprintf (fp, LINE11_FMT, "SW", lat, lon - dlgsize_e, x, y);
	y = lat + dlgsize_n;
	x = lon - dlgsize_e;
    	pj_do_proj(&x, &y, &info_out, &info_in);
    	fprintf (fp, LINE11_FMT, "NW", lat + dlgsize_n, lon - dlgsize_e, x, y);
	y = lat + dlgsize_n;
	x = lon;
    	pj_do_proj(&x, &y, &info_out, &info_in);
    	fprintf (fp, LINE11_FMT, "NE", lat + dlgsize_n, lon, x, y);
	y = lat;
	x = lon;
    	pj_do_proj(&x, &y, &info_out, &info_in);
    	fprintf (fp, LINE11_FMT, "SE", lat, lon, x, y);
	break;
    case 1:	
	lon=map->Area[1].W;
	lat=map->Area[1].S;
	pj_do_proj(&lon, &lat, &info_in, &info_out);	
    	fprintf (fp, LINE11_FMT, "SW", lat, lon, map->Area[1].W, map->Area[1].S);
	lon=map->Area[1].W;
	lat=map->Area[1].N;
	pj_do_proj(&lon, &lat, &info_in, &info_out);	
    	fprintf (fp, LINE11_FMT, "NW", lat, lon, map->Area[1].W, map->Area[1].N);
	lon=map->Area[1].E;
	lat=map->Area[1].N;
	pj_do_proj(&lon, &lat, &info_in, &info_out);	
    	fprintf (fp, LINE11_FMT, "NE", lat, lon, map->Area[1].E, map->Area[1].N);
	lon=map->Area[1].E;
	lat=map->Area[1].S;
	pj_do_proj(&lon, &lat, &info_in, &info_out);	
    	fprintf (fp, LINE11_FMT, "SE", lat, lon, map->Area[1].E, map->Area[1].S);
	break;
    default:
    	fprintf (fp, LINE11_FMT, "SW", 0.0, 0.0, map->Area[1].W, map->Area[1].S);
    	fprintf (fp, LINE11_FMT, "NW", 0.0, 0.0, map->Area[1].W, map->Area[1].N);
    	fprintf (fp, LINE11_FMT, "NE", 0.0, 0.0, map->Area[1].E, map->Area[1].N);
    	fprintf (fp, LINE11_FMT, "SE", 0.0, 0.0, map->Area[1].E, map->Area[1].S);
	break;
     }
}

line15 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    char buf[100];

    switch (dlgtype){
    case 5:
	G_strcpy(buf, "SOILS");
	break;
    case 6:	
	G_strcpy(buf, "SPECIAL_FEATURES");
	break;
    case 7:	
	G_strcpy(buf, "HYDROGRAPHY");
	break;
    case 8:	
	G_strcpy(buf, "CULTURE");
	break;
    }
    if (dlgtype >= 5){
        fprintf (fp, "%-20.20s%4d%6d%6d %c%c %6d%6d %c%c%c%6d%6d   %c\n",
	buf,			/* Category name */
	0, 			/* Default formating Maj/Min pairs */
	map->n_nodes,		/* # of nodes in file */
	map->n_nodes,		/* # of nodes in file */
	'0',			/* node-area links */
	'1',			/* node-line links */
	map->n_areas + 1,	/* # of areas in file + UNIV*/
	map->n_areas + 1,	/* # of areas in file + UNIV*/
	'0',			/* area-node links */
	'1',			/* area-line links */
	'0',			/* area-coord lists */
	map->n_lines,		/* # of lines in file */
	map->n_lines,		/* # of lines in file */
	'1');			/* line-coord lists */
    }else{
        fprintf (fp, LINE15_FMT, 
	"",			/* Category name */
	0, 			/* Default formating Maj/Min pairs */
	map->n_nodes,		/* # of nodes in file */
	map->n_nodes,		/* # of nodes in file */
	'0',			/* node-area links */
	'1',			/* node-line links */
	map->n_areas,		/* # of areas in file */
	map->n_areas,		/* # of areas in file */
	'0',			/* area-node links */
	'1',			/* area-line links */
	'0',			/* area-coord lists */
	map->n_lines,		/* # of lines in file */
	map->n_lines,		/* # of lines in file */
	'1');			/* line-coord lists */
     }
}

/* uses a circular buffer of 200 bytes */
char *
dtype(value, width, deci)
        double value ;
        int width ;
        int deci ;
{
    static char buff[200];
    static char *p = NULL;
    char *ret;
    char form[64] ;
    char *strchr() ;

    if (p == NULL)
	p = buff;
    if ((buff+200-1) - p < width+1)
	p = buff;
    
    {
	sprintf(form,"%%%d.%dle",width, deci) ;
	sprintf(p,form,value) ;
	*(strchr(p,'e')) = 'D' ;
    }
    ret = p;
    p += strlen (p) + 1;
    return (ret);
}
