/****************************************************************************
*
* MODULE:       r.out.gdal2
* AUTHOR(S):    Vytautas Vebra <olivership@gmail.com>
* PURPOSE:      exports GRASS raster to many GDAL suported formats;
*               based on GDAL library.
*               Should replace r.out.gdal script based on gdal_translate
*               executable.
* COPYRIGHT:    (C) 2006 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdlib.h>
#include <unistd.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/imagery.h>
#include <grass/gprojects.h>
#include <grass/glocale.h>
#include <grass/dbmi.h>

#include "gdal.h"

void supported_formats (char** formats) {
/* Code taken from r.in.gdal */
	
	int iDr;

	dbString gdal_formats;

	db_init_string (&gdal_formats);

	if (*formats)
		fprintf(stdout, "Supported Formats:\n" );

	for (iDr = 0; iDr < GDALGetDriverCount(); iDr++ ) {
	
		GDALDriverH hDriver = GDALGetDriver(iDr);
		const char *pszRWFlag;

		if( GDALGetMetadataItem( hDriver, GDAL_DCAP_CREATE, NULL)) pszRWFlag = "rw+";
		else if( GDALGetMetadataItem( hDriver, GDAL_DCAP_CREATECOPY, NULL)) pszRWFlag = "rw";
		else pszRWFlag = "ro";
		
		if (*formats)
			fprintf(stdout, "  %s (%s): %s\n", GDALGetDriverShortName( hDriver ), pszRWFlag, GDALGetDriverLongName( hDriver ) );
		else {
			if (GDALGetMetadataItem( hDriver, GDAL_DCAP_CREATE, NULL) ||
			    GDALGetMetadataItem( hDriver, GDAL_DCAP_CREATECOPY, NULL)) {
				if (db_sizeof_string (&gdal_formats) > 0)
					db_append_string (&gdal_formats, ",");
				
				db_append_string (&gdal_formats, (char*) GDALGetDriverShortName( hDriver ));
			}
		}
	}

	if (db_sizeof_string (&gdal_formats) > 0)
	  *formats = G_store (db_get_string (&gdal_formats));

	return;
}

int import_band (GDALDatasetH hMEMDS, int band, char* name, char* mapset, struct Cell_head* cellhead) {

	int fd; 
	RASTER_MAP_TYPE maptype;
	int cols=cellhead->cols;
	int rows=cellhead->rows;

	/* Open GRASS raster */
	fd = G_open_cell_old(name, mapset); if (fd < 0) { G_warning(_("Cannot open cell file [%s]"), name); return -1; }
	maptype = G_raster_map_type(name, mapset);

	/* Get raster band  */
	GDALRasterBandH hBand = GDALGetRasterBand( hMEMDS, band); if (hBand==NULL) { G_warning(_("Cannot get raster band.")); return -1; }
	
	/* Set color interpretation. TODO: Implement this better ... */
	GDALSetRasterColorInterpretation(hBand, ((band + 3) % 4 + GCI_RedBand) );

	/* Create GRASS raster buffer */
	void *bufer = G_allocate_raster_buf(maptype); if (bufer==NULL) { G_warning(_("Cannot allocate buffer for reading GRASS raster.")); return -1; }
	char *nulls = (char*) G_malloc(cols); if (nulls==NULL) { G_warning(_("Cannot allocate buffer for reading null values.")); return -1; }

	/* Copy data form GRASS raster to memory raster */
	int row, col; 	
	if (maptype==FCELL_TYPE) {
		
		/* Source datatype understandible by GDAL */
		GDALDataType datatype=GDT_Float32;
		/* I'd better use large negative finite value as null value for float and double instead of nan or infinity 
			Otherwise I get problem when displaying GTiff raster with QGIS  */
		FCELL fnullval=-1E+37;
		GDALSetRasterNoDataValue(hBand, (double) fnullval);
		for (row=0; row<rows; row++) {

			if ( G_get_raster_row(fd, bufer, row, maptype) <0) { G_warning(_("Cannot read GRASS raster.")); return -1; }
			G_get_null_value_row(fd, nulls, row);
			for (col=0; col<cols; col++) if (nulls[col]) ((FCELL*)bufer)[col] = fnullval;
			
			if ( GDALRasterIO( hBand, GF_Write, 0, row, cols, 1, bufer, cols, 1, datatype, 0, 0 ) >=CE_Failure) 
				{ G_warning(_("Cannot write GDAL raster.")); return -1; }
			G_percent (row, rows, 10);
		}
	}
	else if (maptype==DCELL_TYPE) {

		GDALDataType datatype=GDT_Float64; 
		DCELL dnullval=-1E+37;
		GDALSetRasterNoDataValue(hBand, (double) dnullval);
		for (row=0; row<rows; row++) {

			if ( G_get_raster_row(fd, bufer, row, maptype) <0) { G_warning(_("Cannot read GRASS raster.")); return -1; }
			G_get_null_value_row(fd, nulls, row);
			for (col=0; col<cols; col++) if (nulls[col]) ((DCELL*)bufer)[col] = dnullval;

			if ( GDALRasterIO( hBand, GF_Write, 0, row, cols, 1, bufer, cols, 1, datatype, 0, 0 ) >=CE_Failure) 
				{ G_warning(_("Cannot write GDAL raster.")); return -1; }
			G_percent (row, rows, 10);
		}
	}		
	else {
		
		GDALDataType datatype=GDT_Int32; 
		CELL inullval; G_set_null_value (&inullval, 1, maptype);
		GDALSetRasterNoDataValue(hBand, (double) inullval);
		for (row=0; row<rows; row++) {

			if ( G_get_raster_row(fd, bufer, row, maptype) <0) { G_warning(_("Cannot read GRASS raster.")); return -1; }
			/* G_get_null_value_row(fd, nulls, row);
			for (col=0; col<cols; col++) if (nulls[col]) ((CELL*)bufer)[col] = inullval; */

			if ( GDALRasterIO( hBand, GF_Write, 0, row, cols, 1, bufer, cols, 1, datatype, 0, 0 ) >=CE_Failure) 
				{ G_warning(_("Cannot write GDAL raster.")); return -1; }
			G_percent (row, rows, 10);
		}
	}

	return 0;
}


int main (int argc, char *argv[]) {

	struct GModule *module;
	struct Flag *flag_l;
	struct Option *input, *format, *type, *output, *createopt, *metaopt;
	
	struct Cell_head cellhead;
	struct Ref ref;
	char *mapset, *gdal_formats = NULL;
	RASTER_MAP_TYPE maptype;

	G_gisinit (argv[0]);

	/* Init GDAL */
	GDALAllRegister();	

	module = G_define_module();    
	module->description = _("Exports GRASS raster into GDAL supported formats");
	module->keywords = _("raster, export");

	flag_l = G_define_flag();
	flag_l->key = 'l';
	flag_l->description = _("list supported output formats");

	input = G_define_standard_option(G_OPT_R_INPUT);
	input->required = NO;

	
	supported_formats(&gdal_formats);

	format = G_define_option();
	format->key = "format";
	format->type = TYPE_STRING;	
	format->description = _("GIS format to write (case sensitive, see also -l flag)");
	/* format->options = "AAIGrid,BMP,BSB,DTED,ELAS,ENVI,FIT,GIF,GTiff,HFA,JPEG,MEM,MFF,MFF2,NITF,PAux,PNG,PNM,VRT,XPM"; */
	if (gdal_formats)
		format->options = G_store (gdal_formats);
	else
		G_fatal_error (_("Unknown GIS formats"));
		
	format->answer = "GTiff";
	format->required = NO;

	type = G_define_option();
	type->key = "type";
	type->type = TYPE_STRING;	
	type->description = _("file type (ignored)");
	type->options = "Byte,Int16,UInt16,UInt32,Int32,Float32,Float64,CInt16,CInt32,CFloat32,CFloat64";
	type->required = NO;

	output = G_define_standard_option(G_OPT_R_OUTPUT);
	output->required = NO;

	createopt = G_define_option();
	createopt->key = "createopt";
	createopt->type = TYPE_STRING;	
	createopt->description = _("creation option to the output format driver. Multiple options may be listed (ignored)");
	createopt->multiple = YES;
	createopt->required = NO;

	metaopt = G_define_option();
	metaopt->key = "metaopt";
	metaopt->type = TYPE_STRING;	
	metaopt->description = _("metadata key passed on the output dataset if possible (ignored)");
	metaopt->multiple = YES;
	metaopt->required = NO;

	if (G_parser(argc,argv)) exit(EXIT_FAILURE);

	if(flag_l->answer) {
		
		supported_formats(&gdal_formats);
		exit(EXIT_SUCCESS);
	}

	/* Try to open input GRASS raster.. */	
	mapset = G_find_cell2(input->answer, ""); 

	if (mapset != NULL) {
		/* Add input to "group". "Group" whith 1 raster (band) will exist only in memory. */
		I_init_group_ref(&ref); 
		I_add_file_to_group_ref(input->answer, mapset, &ref);		
	} else {		
		/* Maybe input is group. Try to read group file */
		if (I_get_group_ref(input->answer, &ref) != 1)	G_fatal_error(_("Raster map or group [%s] not found"), input->answer);
	}
	if (ref.nfiles == 0) G_fatal_error(_("No raster maps in group [%s]"), input->answer);

	/* Read project and region data */	
	struct Key_Value *projinfo = G_get_projinfo();
	struct Key_Value *projunits = G_get_projunits(); 	
	char *srswkt = GPJ_grass_to_wkt(projinfo, projunits, 0, 0);
	G_get_window(&cellhead);
	
	/* Try to create raster data drivers. If failed - exit. */
	GDALDriverH hDriver=NULL, hMEMDriver=NULL;
	hDriver = GDALGetDriverByName(format->answer); if (hDriver==NULL) G_fatal_error(_("Cannot create [%s] driver"), format->answer);
	/* Does driver support GDALCreate ? */
	if( GDALGetMetadataItem( hDriver, GDAL_DCAP_CREATE, NULL) ==0 ) {
		/* If not - create intermediate MEM driver */
		hMEMDriver = GDALGetDriverByName("MEM"); if (hMEMDriver==NULL) G_fatal_error(_("Cannot create memory raster driver"));
	}
	
	/* Determine raster data type from first GRASS raster */
	maptype = G_raster_map_type(ref.file[0].name, ref.file[0].mapset);
	GDALDataType datatype;

	/* struct FPRange range; double dmin, dmax; int isminmax=0;
	if( G_read_fp_range( ref.file[0].name, ref.file[0].mapset ) >=0 ) {
		G_get_fp_range_min_max( &range, &dmin, &dmax );
		isminmax=1;
	} */

	/*TODO: implement more datatypes (determine it from user input or GRASS raster type) */
	if (maptype==FCELL_TYPE) {
		datatype=GDT_Float32; 
	}
	else if (maptype==DCELL_TYPE) {
		datatype=GDT_Float64;
	}		
	else {
		datatype=GDT_Int32; 
	}
		
	/* Create dataset. TODO: implement creation options.  */
	char **papszOptions = NULL;
	GDALDatasetH hCurrDS, hMEMDS, hDstDS;
	if (hMEMDriver) {
		hMEMDS = GDALCreate( hMEMDriver, "", cellhead.cols, cellhead.rows, ref.nfiles, datatype, papszOptions );  
		if (hMEMDS==NULL) G_fatal_error(_("Cannot create dataset using memory raster driver."));
		hCurrDS=hMEMDS;
	} else {
		hDstDS = GDALCreate( hDriver, output->answer, cellhead.cols, cellhead.rows, ref.nfiles, datatype, papszOptions );  
		if (hDstDS==NULL) G_fatal_error(_("Cannot create [%s] dataset using [%s] driver."), output->answer, format->answer);
		hCurrDS=hDstDS;
	}

	/* Set Geo Transform  */
	double adfGeoTransform[6];
	adfGeoTransform[0] = cellhead.west;
	adfGeoTransform[1] = cellhead.ew_res;
	adfGeoTransform[2] = 0.0;
	adfGeoTransform[3] = cellhead.north;
	adfGeoTransform[4] = 0.0;
	adfGeoTransform[5] = -1*cellhead.ns_res;
	if ( GDALSetGeoTransform( hCurrDS, adfGeoTransform ) >=CE_Failure) G_warning(_("Couldn't set geo transform ...\n"));

	/* Set Projection  */
	if ( GDALSetProjection( hCurrDS, srswkt ) >=CE_Failure) G_warning(_("Couldn't set projection ...\n"));

	/* Export to GDAL raster */
	int band;
	for (band=0; band<ref.nfiles; band++) {
		if (import_band (hCurrDS, band+1, ref.file[band].name, ref.file[band].mapset, &cellhead) <0 ) 
			G_warning(_("Cannot export [%s] raster file."), ref.file[band].name);
	}

	/* Finaly create user required raster format from memory raster if in-memory driver was used*/
   	if (hMEMDriver) {
		hDstDS = GDALCreateCopy( hDriver, output->answer, hMEMDS, FALSE, papszOptions, NULL, NULL );
		if (hDstDS==NULL) G_fatal_error(_("Cannot create [%s] raster file using [%s] driver."), output->answer, format->answer);
	}
	
	GDALClose( hDstDS );
	if (hMEMDriver) GDALClose( hMEMDS );
	G_message(_("Done!"));
   	exit(EXIT_SUCCESS);
}

