/* Written by Bill Brown, USA-CERL, NCSA.
 * December 28, 1992
 */

/* Use to convert HDF scientific data set to grass raster file.
 * Currently, creates a raster file with southwest origin at 
 * 0 East and 0 North, resolution 1.  So to view GRASS raster file
 * after creation, need to set g.region rast=newfile.  If the 
 * SDS is geo-referenced, you need to manually change the new GRASS
 * cellhd file to the correct region.  - hope to fix later
*/

#include <strings.h>

#include "gis.h"
#include "df.h"
#include "hdf.h"

typedef int FILEDESC;
double atof();
FILEDESC init_rasterfile();

#define MAXDIMS   8
#define FMT_SDS	111
#define FMT_R8	222
#define FMT_R24	333
#define FMT_UNK	444
#define NOT_HDF  -1

#define MAXDSETS 30

main(argc, argv)
    int argc;
    char *argv[];
{

    struct Option 	*vrefs, *hdf_file, *rast, *mult;
    struct Flag         *shh, *readall, *dolist;
    FILEDESC    	cellfile = NULL;
    CELL		*cellbuf; 
    char 		nextfile[200], errbuf[200];
    double 		mply;
    int32 		dims[MAXDIMS];
    int			hdf_fmt, i, ret, done = 0, dsnum = 1;
    uint8 		pal[768];
    uint16 		*validrefs, refnums[MAXDSETS];
    int 		nvalid;


    G_gisinit (argv[0]);

    hdf_file = G_define_option();
    hdf_file->key                    = "input";
    hdf_file->type                   = TYPE_STRING;
    hdf_file->required               = YES;
    hdf_file->multiple               = NO;
    hdf_file->description            = "HDF file to be converted.";

    rast = G_define_option();
    rast->key            	   = "output";
    rast->type           	   = TYPE_STRING;
    rast->required     	           = NO;
    rast->answer    		   = "hdf.rast";
    rast->gisprompt    		   = "new,cell,raster";
    rast->description  		   = "Name of new raster file.";

    mult = G_define_option();
    mult->key                    = "mult";
    mult->type                   = TYPE_DOUBLE;
    mult->required               = NO;
    mult->multiple               = NO;
    mult->description = 
	"Floating point multiplier. (rastfile = (int)(file.hdf * multiplier))";

    vrefs = G_define_option();
    vrefs->key                    = "dsets";
    vrefs->type                   = TYPE_INTEGER;
    vrefs->required               = NO;
    vrefs->multiple               = YES;
    vrefs->description 		  = 
	"List of reference numbers of data sets to be extracted (see -l Flag)";

    shh = G_define_flag ();
    shh->key = 'q';
    shh->description = "Run quietly";

    readall = G_define_flag ();
    readall->key = 'a';
    readall->description = "Convert ALL data sets in the HDF file";

    dolist = G_define_flag ();
    dolist->key = 'l';
    dolist->description = "Only list contents of HDF file (no conversion)";

    if (G_parser (argc, argv))
	exit (-1);
    
    if(mult->answer)
	mply = atof(mult->answer);
    else mply = 1.0;

    if(!shh->answer && !dolist->answer)
	fprintf(stderr,"\nmultiplier = %.4lf\n", mply);
    
    if(rast->answer)
	strcpy(nextfile, rast->answer);
    else
	strcpy(nextfile, "hdf.rast");

    nvalid=0;
    if( vrefs->answers ){
	int tmp;

	for( ; vrefs->answers[nvalid] && nvalid < MAXDSETS; nvalid++){
	    sscanf(vrefs->answers[nvalid],"%d", &tmp);
	    *(refnums + nvalid) = tmp;
	}
	if(!shh->answer)
	    fprintf(stderr,
		    "Attempting to create %d new raster map%c.\n", 
		    nvalid, nvalid == 1? '\0': 's');

    }
    validrefs = nvalid? refnums: NULL;

    hdf_fmt = get_hdf_fmt(hdf_file->answer);

    switch(hdf_fmt){

    case FMT_SDS:
    {
    float32 	*data;
	
	if(dolist->answer){
	    list_hdf_objects(hdf_file->answer, DFTAG_NDG);
	    exit(0);
	}
	while (!done){
	    if(readall->answer || nvalid > 1){
		sprintf(nextfile, "%s%02d", 
			rast->answer? rast->answer: "hdf.rast", dsnum);
	    }
	    else done=1;

	    ret = read_sds(hdf_file->answer, &data, dims, !shh->answer,
		  validrefs, nvalid);

	    if(ret == 1){
		write_rasterfile(nextfile, data, dims, mply, !shh->answer);
		if(!shh->answer)
		    fprintf(stderr,"Writing history file...\n");
		do_history(nextfile, hdf_file->answer, DFTAG_NDG, 
							DFSDlastref());
	    }

	    else if(ret == -1){  /* sds exists, but BIG, convert piecewise */
		cellfile = init_rasterfile(nextfile, dims, 
					   &data, &cellbuf, !shh->answer);
		for(i=0; i<dims[0]; i++){
		    if(!shh->answer)
			G_percent(i, dims[0] - 1, 10);
		    read_sds_row(hdf_file->answer, i, data, dims);
		    write_raster_nextrow(cellfile, dims, data, cellbuf, mply);
		}
		free(cellbuf);
		G_close_cell(cellfile);
		if(!shh->answer)
		    fprintf(stderr,"Writing history file...\n");
		do_history(nextfile, hdf_file->answer, DFTAG_NDG, 
							DFSDlastref());
	    }

	    else done = 1;


	    free(data);
	    dsnum++;
	}
	break;
    }

    case FMT_R8:
    {
    uint8	*data;

	if(dolist->answer){
	    list_hdf_objects(hdf_file->answer, DFTAG_RIG);
	    exit(0);
	}

	while (!done){
	    if(readall->answer || nvalid > 1){
		sprintf(nextfile, "%s%02d", 
			rast->answer? rast->answer: "hdf.rast", dsnum);
	    }
	    else done=1;

	    ret = read_r8(hdf_file->answer, &data, dims, !shh->answer, pal,
		          validrefs, nvalid);

	    if(ret){
		write_rasterfile_r8(nextfile, data, dims, mply, !shh->answer);
		if(!shh->answer)
		    fprintf(stderr,"Writing history file...\n");
		do_history(nextfile, hdf_file->answer, 
					DFTAG_RIG, DFR8lastref());
	    }
	    else done = 1;

	    if(ret == 2)
		write_colorfile_r8(nextfile, pal, mply, !shh->answer);
	    

	    free(data);
	    dsnum++;
	}
	break;

    }

    case FMT_R24:
    sprintf(errbuf, "HDF 24 bit raster image files not yet supported\n");
    G_fatal_error(errbuf);
    break;

    case NOT_HDF:
    sprintf(errbuf,"[%s] not a valid HDF file!\n",hdf_file->answer);
    G_fatal_error(errbuf);
    break;

    case FMT_UNK:
    default:
    sprintf(errbuf,"[%s] not a known HDF format!\n",hdf_file->answer);
    G_fatal_error(errbuf);
    break;

    }


    if(!shh->answer)
	fprintf(stderr,"\nDone.\n"); 
    
    return(1);
}



get_hdf_fmt(filename)
char *filename;
{
int w, h, i, rank, dims[MAXDIMS];

    if(-1 == DFishdf(filename)){
	return(NOT_HDF);
    }
    if(-1 != DFR8getdims(filename, &w, &h, &i)){
	DFR8restart();
	return(FMT_R8);
    }
    if(-1 != DF24getdims(filename, &w, &h, &i)){
	DF24restart();
	return(FMT_R24);
    }
    if(-1 != DFSDgetdims(filename, &rank, dims, MAXDIMS)){
	DFSDrestart();
	return(FMT_SDS);
    }
    return(FMT_UNK);

}


#define MEG16  16000000
#define MEG8   8000000
#define MEG4   4000000
#define MEG2   2000000
#define HIMEMTEST   200
/*
#define HIMEM  HIMEMTEST
*/
#define HIMEM  MEG8


/* get data from HDF file */

read_sds(hdf_file, data, dims, verbose, valid_refs, nvalid)
char *hdf_file;
float32 **data;
int32 *dims; 
uint16 *valid_refs; /* NULL if all valid or first only */
int nvalid;
int verbose;
{
int rank, size;
int lastref; 
static int nextref=0;
    
    if(verbose)
	fprintf(stderr,"Reading HDF file \"%s\"",hdf_file);

    if(valid_refs){
	if(nextref < nvalid){
	    if(FAIL == DFSDreadref(hdf_file, valid_refs[nextref])){
		fprintf(stderr,"Error: Unable to read reference %d\n",
			valid_refs[nextref]); 
		return(0);
	    }
	    if(verbose)
		fprintf(stderr,", dataset reference %d", valid_refs[nextref]);
	    nextref++;
	}
	else
	    return(0);
    }
    if(verbose)
	fprintf(stderr,"\n");

    if(-1 == DFSDgetdims(hdf_file, &rank, dims, MAXDIMS)){
	if(verbose){
	    fprintf(stderr, "No more data sets in HDF file.\n");
	}
	return(0);
    }
    if (2 != rank){
	fprintf(stderr,"%s has %d dimensions, must be 2d for conversion.\n",
				hdf_file, rank);
	exit(1);
    }
    if(verbose)
	fprintf(stderr,"rows = %d, cols = %d\n", dims[0], dims[1]);

    size = dims[0] * dims[1] * sizeof(float32);
    if(size > HIMEM){
	if(verbose)
	    fprintf(stderr, "Large file - reading row by row.\n");
	return(-1);
    }
    
    if((*data = (float32 *)malloc(size))==NULL){
	fprintf(stderr,"malloc failed\n");
	exit(1);
    }

    if(-1 == get_converted_sds_data(hdf_file, 2, dims, *data)){
	if(verbose)
	    fprintf(stderr, "No more data sets in HDF file.\n");
	exit(1);
    }

    return(1);

}

read_r8(hdf_file, data, dims, verbose, palette, valid_refs, nvalid)
char *hdf_file;
uint8 **data;
int32 *dims;
int verbose;
uint8 palette[768];
uint16 *valid_refs; /* NULL if all valid or first only */
int nvalid;
{
int size; 
intn needpalette=0;
int lastref; 
static int nextref=0;
    
    if(verbose)
	fprintf(stderr,"Reading HDF file \"%s\"",hdf_file);

    if(valid_refs){
	if(nextref < nvalid){
	    if(FAIL == DFR8readref(hdf_file, valid_refs[nextref])){
		fprintf(stderr,"Error: Unable to read reference %d\n",
			valid_refs[nextref]); 
		return(0);
	    }
	    if(verbose)
		fprintf(stderr,", dataset reference %d", valid_refs[nextref]);
	    nextref++;
	}
	else
	    return(0);
    }
    if(verbose)
	fprintf(stderr,"\n");

    if(-1 == DFR8getdims(hdf_file, &(dims[1]), &(dims[0]), &needpalette)){
	if(verbose){
	    fprintf(stderr, "No more data sets in HDF file.\n");
	}
	return(0);
    }

    if(verbose)
	fprintf(stderr,"rows = %d, cols = %d\n", dims[0], dims[1]);

    size = dims[0] * dims[1] * sizeof(char);
    
    if((*data = (uint8 *)malloc(size))==NULL){
	fprintf(stderr,"malloc failed - image too large for current memory\n");
	exit(1);
    }

    if(-1 == DFR8getimage(hdf_file, *data, dims[1], dims[0], palette)){
	if(verbose)
	    fprintf(stderr, "No more data sets in HDF file.\n");
	return(0);
    }

    return(needpalette? 2: 1);

}

/* write to new GRASS raster file */

write_rasterfile(rastfile, data, dims, mult, verbose)
char *rastfile;
float32 *data;
int32 *dims;
double mult;
int verbose;
{ 
struct Cell_head	w;
FILEDESC    		cellfile = NULL;
CELL			*cellbuf; 
char 			errbuf[100];
int			row, col;

    if(verbose)
	fprintf(stderr,"Writing GRASS output file: %s\n", rastfile); 

    w.zone = G_zone();
    w.proj = G_projection();
    w.rows = w.north = dims[0];
    w.cols = w.east = dims[1];
    w.south = w.west = 0.0;
    w.ns_res = w.ew_res = 1;
    
    if(G_set_window (&w) < 0)
	exit(2);
    

    if ((cellfile = G_open_cell_new(rastfile)) == -1) 
    {
	sprintf(errbuf,"Unable to create cellfile for [%s]",rastfile);
	G_fatal_error(errbuf);
    }

    cellbuf = G_allocate_cell_buf();
    {	
	int row_off;
	float32 *tf;

	for (row = 0; row < dims[0]; row++) {

	    if(verbose)
		G_percent(row, dims[0] - 1, 10);

	    row_off = row * dims[1];
	    tf = &(data[row_off]);
	    for(col=0; col < w.cols; col++)
		cellbuf[col] = (int)(*tf++ * mult);
/* maybe use this for better rounding?
		cellbuf[col] = (int)(*tf++ * mult + .5);
*/
	    G_put_map_row(cellfile, cellbuf); 
	}
    }
    G_close_cell(cellfile);
    free(cellbuf);
}


write_rasterfile_r8(rastfile, data, dims, mult, verbose) 
char *rastfile;
uint8 *data;
int32 *dims;
double mult;
int verbose;
{ 
struct Cell_head	w;
FILEDESC    		cellfile = NULL;
CELL			*cellbuf; 
char 			errbuf[100];
int			row, col;

    if(verbose)
	fprintf(stderr,"Writing GRASS output file: %s\n", rastfile); 

    w.zone = G_zone();
    w.proj = G_projection();
    w.rows = w.north = dims[0];
    w.cols = w.east = dims[1];
    w.south = w.west = 0.0;
    w.ns_res = w.ew_res = 1;
    
    if(G_set_window (&w) < 0)
	exit(2);
    

    if ((cellfile = G_open_cell_new(rastfile)) == -1) 
    {
	sprintf(errbuf,"Unable to create cellfile for [%s]",rastfile);
	G_fatal_error(errbuf);
    }

    cellbuf = G_allocate_cell_buf();
    {	
	int row_off;
	uint8 *tc;

	for (row = 0; row < dims[0]; row++) {

	    if(verbose)
		G_percent(row, dims[0] - 1, 10);

	    row_off = row * dims[1];
	    tc = &(data[row_off]);
	    for(col=0; col < w.cols; col++)
		cellbuf[col] = (int)(*tc++ * mult);
	    G_put_map_row(cellfile, cellbuf); 
	}
    }
    G_close_cell(cellfile);
    free(cellbuf);
}

/* Would like to use color rules here, but HDF doesn't provide any info
   about palette continuity, so have to have larger color file */

write_colorfile_r8(nextfile, pal, mply, verbose)
char* nextfile;
uint8 pal[768];
double mply;
int verbose;
{
struct Colors col;
int i, r, g, b;
CELL cat;

    if(verbose){
	fprintf(stderr,"HDF image has color information.\n");
	fprintf(stderr,"Writing color file for: %s\n", nextfile);
    }

    G_init_colors(&col);
    for(i=0; i<256; i++ ){
	cat = (CELL)i*mply;
	r = (int)pal[i*3];
	g = (int)pal[i*3+1];
	b = (int)pal[i*3+2];
	G_set_color(cat, r, g, b, &col); 
/*
fprintf(stderr, "colors written > %d:%d:%d:%d\n", cat,r,g,b);
*/
    }
    G_write_colors(nextfile, G_mapset(), &col);
    G_free_colors(&col);

}



FILEDESC
init_rasterfile(rastfile, dims, data, cellbuf,verbose)
char		*rastfile;
int32 		*dims;
float32 	**data;
CELL		**cellbuf; 
int 		verbose;
{
struct Cell_head	w;
FILEDESC    		cellfile = NULL;
char 			errbuf[100];

    if(verbose)
	fprintf(stderr,"Writing GRASS output file: %s\n", rastfile); 

    w.zone = G_zone();
    w.proj = G_projection();
    w.rows = w.north = dims[0];
    w.cols = w.east = dims[1];
    w.south = w.west = 0.0;
    w.ns_res = w.ew_res = 1;
    
    if(G_set_window (&w) < 0)
	exit(2);
    

    if ((cellfile = G_open_cell_new(rastfile)) == -1) 
    {
	sprintf(errbuf,"Unable to create cellfile for [%s]",rastfile);
	G_fatal_error(errbuf);
    }

    if(NULL==(*data = (float32 *)malloc(dims[1]*sizeof(float32)))){
	sprintf(errbuf,"malloc failed for %ld bytes\n", dims[1]*sizeof(float32));
	G_fatal_error(errbuf);
    }
    *cellbuf = G_allocate_cell_buf();
    return(cellfile);

}


write_raster_nextrow(cellfile, dims, data, cellbuf, mply)
FILEDESC    	cellfile;
int32 		*dims;
float32 	*data;
CELL		*cellbuf; 
double 		mply;
{
float32 *tf;
int col;

    tf = data;


    for(col=0; col < dims[1]; col++){
	cellbuf[col] = (int)(*tf++ * mply);
    }
    G_put_map_row(cellfile, cellbuf); 


}


read_sds_row(filename, row, data, dims)
char		*filename;
int 		row;
float32 	*data;
int32 		*dims;
{
int32 winst[2], windims[2], slicedims[2];

    winst[0]=row+1;
    winst[1]=1;
    windims[0] = slicedims[0] = 1;
    windims[1] = slicedims[1] = dims[1];

    get_converted_sds_dataslice(filename, winst, windims, data, slicedims);


}


