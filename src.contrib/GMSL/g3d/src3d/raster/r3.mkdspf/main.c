/* This program implements the marching cubes surface tiler described by
 * Lorenson & Cline in the Siggraph 87 Conference Proceedings.
 *
 * This program reads in data from a grid3 formatted file containing 3D data
 * and creates a display file.  
 * 
 * The display file consists of cell_info structures.
 * The cell_info structure:
 *	threshold (specified on commandline)
 *    	number of polygons 
 *	polygon vertice coordinates
 * 	surface or vertex normals (depending on lighting model specified)
 * 
 * The user must specify the data file name and the thresholds and lighting
 * model desired.  
 *
 * Based on a program outline written by Mike Krogh, NCSA, Feb.,1990
 *
 * written by Jan Moorman for D. Lawrance, Mednet NCSA.
 * rewritten for CERL, August 1990
 *
 * rewritten by Bill Brown
 *     for UI Geographic Modeling Systems Laboratory February 1996
 *     to use new GRASS 3dgrid data files & API
*/

#define MAIN
#include "vizual.h"
#include "gis.h"
#include "G3d.h"
#include <math.h>

char *check_get_any_dspname();


int main(int argc, char *argv[])
{
    char	*dspout, buff[160], element[160];
    int i; /* counter */
    void *g3map;
    G3D_Region g3reg;
    char *mapset;
    double dmin, dmax;

    struct Option *levels;
    struct Option *out;
    struct Option *min;
    struct Option *max;
    struct Option *step;
    struct Option *tnum;
    struct Option *name;

    struct Flag *shade;
    struct Flag *quiet;

    G_gisinit (argv[0]);

    name=G_define_option () ;
    name->key        = "grid3";
    name->type       = TYPE_STRING;
    name->required   = YES;
    name->gisprompt  = "old,grid3,3dcell";  
    /* should still find the DIRECTORY */
    name->description= "Name of an existing 3dcell map" ;

    out=G_define_option () ;
    out->key        = "dspf";
    out->type       = TYPE_STRING;
    out->required   = YES;
    out->description= "Name of output display file" ;

    levels=G_define_option () ;
    levels->key        = "levels";
    levels->type       = TYPE_DOUBLE;
    levels->required   = NO;
    levels->multiple   = YES;
    levels->description= "List of thresholds for isosurfaces" ;
    
    min=G_define_option () ;
    min->key        = "min";
    min->type       = TYPE_DOUBLE;
    min->required   = NO;
    min->description= "Minimum isosurface level" ;

    max=G_define_option () ;
    max->key        = "max";
    max->type       = TYPE_DOUBLE;
    max->required   = NO;
    max->description= "Maximum isosurface level" ;

    step=G_define_option () ;
    step->key        = "step";
    step->type       = TYPE_DOUBLE;
    step->required   = NO;
    step->description= "Positive increment between isosurface levels" ;

    tnum=G_define_option () ;
    tnum->key        = "tnum";
    tnum->type       = TYPE_INTEGER;
    tnum->required   = NO;
    tnum->answer     = "7";
    tnum->description= "Number of isosurface threshold levels" ;

    quiet = G_define_flag() ;
    quiet->key        = 'q';
    quiet->description = "Suppress progress report & min/max information" ;

    shade = G_define_flag() ;
    shade->key        = 'f';
    shade->description = "Use flat shading rather than gradient" ;

    if (G_parser(argc, argv))
	exit (-1);

    G3d_initDefaults();

    G3d_getWindow (&g3reg);
fprintf(stderr,"Region from getWindow: %d %d %d\n",g3reg.rows,g3reg.cols,g3reg.depths);

    if(NULL == (dspout = check_get_any_dspname
		(out->answer, name->answer, G_mapset())))
	exit (-1);

    G3d_setErrorFun (G3d_printError);

    /* open g3 file for reading and writing */
    if(NULL == (mapset = G_find_file2 ("grid3", name->answer, ""))){
        sprintf(buff,"Not able to find grid3 file for [%s]", name->answer);
        G_fatal_error(buff);
    }

    g3map = G3d_openCellOld (name->answer, mapset, &g3reg,
			     G3D_TILE_SAME_AS_FILE,
			     G3D_USE_CACHE_DEFAULT);
/*
    g3map = G3d_openCellOld (name->answer, mapset, G3D_DEFAULT_WINDOW,
			     G3D_TILE_SAME_AS_FILE,
			     G3D_USE_CACHE_DEFAULT);
*/

    if(NULL == g3map){
        sprintf(buff,"Error opening grid3 file [%s]", name->answer);
	G_fatal_error (buff);
    }

    if(0 == G3d_range_load(g3map)){
        sprintf(buff,"Error reading range for [%s]", name->answer);
	G_fatal_error (buff);
    }
    /* TODO: look at this - should use current 3dregion rather than
    region represented by original 3dgrid file */
/*
    G3d_getRegionStructMap (g3map, &g3reg);
*/

    /* DONT USE Headfax any more ?
    g3read_header(&Headfax);
    */
    G3d_range_min_max (g3map, &dmin, &dmax);
    viz_make_header(&Headfax, dmin, dmax, &g3reg);

    /* puts command line options into cmndln_info structure */
    /* need to send it answers */
    viz_calc_tvals(&Headfax.linefax, levels->answers, min->answer,
		   max->answer, step->answer, tnum->answer, 
		   quiet->answer?1:0);

    if (shade->answer){
	Headfax.linefax.litmodel=1; /* flat */
    }
    else
	Headfax.linefax.litmodel=2; /* gradient */

    /* open display file for writing */
    sprintf(element,"grid3/%s/dsp", name->answer);
    if((Headfax.dspfoutfp = G_fopen_new(element,dspout)) == NULL)
    {
        sprintf(buff,"Error opening display file [%s]", dspout);
	G_fatal_error (buff);
    }

/* write display file header info */
/* have to adjust dimensions  -dpg */
    {
	Headfax.xdim -= 1;
	Headfax.ydim -= 1;
	Headfax.zdim -= 1;
fprintf(stderr,"DSPF DIMS: %d %d %d\n", Headfax.ydim, Headfax.xdim, Headfax.zdim);
	if(dfwrite_header(&Headfax) < 0)
	{
	   fclose(Headfax.dspfoutfp);
	   exit(-1);
	}
	Headfax.xdim += 1;
	Headfax.ydim += 1;
	Headfax.zdim += 1;
    }
    
    if(!quiet->answer)
	fprintf(stderr,"Writing %s from %s...", dspout, name->answer);

    viz_iso_surface(g3map, &g3reg, &Headfax.linefax, quiet->answer?1:0);

    if(!quiet->answer)
	fprintf(stderr,"\n");

    /* tries to write a header! */ 
    G3d_closeCell(g3map);   

    fclose(Headfax.dspfoutfp);
    exit(1);
}
