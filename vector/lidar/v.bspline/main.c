/***********************************************************************
 *									
 * MODULE:       v.bspline						
 * 									
 * AUTHOR(S):    Roberto Antolin & Gonzalo Moreno			
 *               							
 * PURPOSE:      Spline Interpolation					
 *               							
 * COPYRIGHT:    (C) 2006 by Politecnico di Milano - 			
 *			     Polo Regionale di Como			
 *									
 *               This program is free software under the 		
 *               GNU General Public License (>=v2). 			
 *               Read the file COPYING that comes with GRASS		
 *               for details.						
 *									
 **************************************************************************/

/*INCLUDES*/
#include <stdlib.h> 
#include <string.h> 

#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>
#include <grass/config.h>

#include <grass/PolimiFunct.h>

/*Variable's declaration*/
int nsply, nsplx;
double passoN, passoE;

/*-------------------------------------------------------------------------------------------*/
int
main (int argc,char *argv[])
{

/* Variables' declarations */
    int nsply, nsplx, nlines, nrows, ncols, dim_vect, raster, nparameters, BW;
    int last_row, last_column, bilin, grid, flag_ext, flag_auxiliar = FALSE; 	/* booleans */
    double passoN, passoE, lambda, mean;		
    
    char *mapset, *dvr, *db, *vector, *map, table_name[1024];			/* */
    
    int *lineVect;				/* Vector restoring primitive's ID*/
    double **raster_matrix;			/* Matrix to store the auxiliar raster values*/
    double *TN, *Q, *parVect;			/* Interpolating and least-square vectors*/
    double **N, **obsVect;			/* Interpolation and least-square matrix*/
    
/* Structs' declarations */
    struct Map_info In, In_ext, Out;
    
    struct GModule *module;
    struct Option *in_opt, *in_ext_opt, *out_opt, *out_map_opt, *dbdriver, *dbdatabase, \
    			*passoE_opt, *passoN_opt, *lambda_f_opt, *type; 
    struct History history;
    
    struct Cell_head elaboration_reg, original_reg;
    struct Reg_dimens dims;
    BOUND_BOX general_box, overlap_box;
    
    struct Point *observ;
    struct line_pnts *points;
    struct line_cats *categories;
    
    dbDriver *driver;
    
/*-------------------------------------------------------------------------------------------*/
/* Options' declaration */
    module = G_define_module();
    module->keywords = _("vector, interpolation");
    module->description = _("Bicubic or bilineal interpolation with Tykhonov regularization");

    in_opt = G_define_option () ;
    	in_opt->key 		= "input";
    	in_opt->type 		= TYPE_STRING;
    	in_opt->key_desc    	= "name";
	in_opt->required    	= YES;
	in_opt->gisprompt    	= "old,vector,vector";
	in_opt->description  	= _("Name of input vector map");
	
    in_ext_opt = G_define_option () ;
    	in_ext_opt->key 	= "input_ext";
	in_ext_opt->type 	= TYPE_STRING;
    	in_ext_opt->required 	= NO;
    	in_ext_opt->key_desc    = "name";
	in_ext_opt->gisprompt   = "old,vector,vector";
	in_ext_opt->description = _("Name of input vector map of sparse points");
	
    out_opt = G_define_option () ;
    	out_opt->key 		= "output";
    	out_opt->type 		= TYPE_STRING;
    	out_opt->key_desc       = "name";
	out_opt->required       = NO;
	out_opt->gisprompt      = "new,vector,vector";
	out_opt->description    = _("Name of output vector map");
	
    out_map_opt = G_define_option () ;
    	out_map_opt->key 	  = "raster_out";
    	out_map_opt->type 	  = TYPE_STRING;
    	out_map_opt->key_desc     = "name";
	out_map_opt->gisprompt 	  = "new,cell,raster";
	out_map_opt->required 	  = NO;
	out_map_opt->description  = _("Name of output raster map");
    
    dbdatabase = G_define_option() ;
    	dbdatabase->key        	= "database" ;
    	dbdatabase->type       	= TYPE_STRING ;
    	dbdatabase->required   	= NO ;
    	dbdatabase->multiple   	= NO ;
    	dbdatabase->description	= _("Database name");
    if ( (db=G__getenv2("DB_DATABASE",G_VAR_MAPSET)) )
	    dbdatabase->answer = G_store ( db );
    	
    dbdriver = G_define_option() ;
   	dbdriver->key         = "driver" ;
    	dbdriver->type        = TYPE_STRING ;
    	dbdriver->options     = db_list_drivers();
    	dbdriver->required    = NO  ;
    	dbdriver->multiple    = NO ;
    	dbdriver->description = _("Driver name");
    if ( (dvr = G__getenv2("DB_DRIVER",G_VAR_MAPSET)) )
	    dbdriver->answer = G_store ( dvr );
    
    passoE_opt = G_define_option();
        passoE_opt->key		= "sie";
	passoE_opt->type	= TYPE_DOUBLE;
        passoE_opt->required	= NO;
	passoE_opt->answer 	= "4";
	passoE_opt->description	= _("Interpolation spline step value in east direction");
	
    passoN_opt = G_define_option();
        passoN_opt->key		= "sin";	
        passoN_opt->type	= TYPE_DOUBLE;
        passoN_opt->required	= NO;
        passoN_opt->answer 	= "4";
	passoN_opt->description	= _("Interpolation spline step value in north direction");		

    type = G_define_option();
	type->key	  = "type";	
	type->type	  = TYPE_STRING;	
	type->required	  = NO;	
        type->description = _("Spline type of interpolation");	
	type->options	  = "bilinear,bicubic";
	type->answer	  = "bilinear";
	
    lambda_f_opt = G_define_option();
	lambda_f_opt->key    	  = "lambda_i";
	lambda_f_opt->type	  = TYPE_DOUBLE;
	lambda_f_opt->required	  = NO;
        lambda_f_opt->description =_("Thychonov regularization weigth");
	lambda_f_opt->answer	  = "1";

/* Parsing */	
    G_gisinit(argv[0]);
    if (G_parser (argc, argv))
	exit(EXIT_FAILURE); 

    if (!strcmp(type->answer,"bilinear"))
	bilin = P_BILINEAR;
    else
    	bilin = P_BICUBIC;
	
    passoN = atof (passoN_opt->answer);
    passoE = atof (passoE_opt->answer);
    lambda = atof (lambda_f_opt->answer);

    vector = out_opt->answer;
    map = out_map_opt->answer;

    if (vector && map) 
	G_fatal_error (_("Not both vector and raster output are possible"));

    if (!vector && !map)
	G_fatal_error (_("No raster nor vector output"));

/* Open input vector */
    if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) 
	 G_fatal_error ( _("Could not find input map <%s>"), in_opt->answer);

    Vect_set_open_level (1); 		/* WITHOUT TOPOLOGY */
    if (1 > Vect_open_old (&In, in_opt->answer, mapset)) {
    	Vect_close (&In);
	G_fatal_error (_("Vector <%s> could not be open at the topological level"), in_opt->answer);
    }

/* Open input ext vector */
    if (!in_ext_opt->answer){
    	flag_ext = FALSE;
    	G_warning ( _("No vector map to interpolate. Interpolation will be done with <%s> vector map"), in_opt->answer);
    } else {
	flag_ext = TRUE;
    	G_warning (_("<%s> vector map will be interpolated"), in_ext_opt->answer);
	
    	if ((mapset = G_find_vector2 (in_ext_opt->answer, "")) == NULL) 
	    G_fatal_error ( _("Could not find input map <%s>"), in_ext_opt->answer);

    	Vect_set_open_level (1); 		/* WITHOUT TOPOLOGY */
    	if (1 > Vect_open_old (&In, in_opt->answer, mapset)) {
    	    Vect_close (&In);
	    G_fatal_error (_("Vector <%s> could not be open at the topological level"), in_opt->answer);
    	}
    }

/* Open output vector or raster*/
    if (vector && !map) {
    	Vect_check_input_output_name (in_opt->answer, out_opt->answer, GV_FATAL_EXIT);
    	if (0 > Vect_open_new (&Out, out_opt->answer, WITH_Z)) {
	    Vect_close (&In);
	    G_fatal_error (_("Vector <%s> could not be open"), out_opt->answer);
    	}

    /* Copy vector Head File */
    	if (flag_ext == FALSE) {
    	    Vect_copy_head_data (&In, &Out);
    	    Vect_hist_copy (&In, &Out);
	} else {
    	    Vect_copy_head_data (&In_ext, &Out);
    	    Vect_hist_copy (&In_ext, &Out);
    	} 
	Vect_hist_command (&Out);
    }

    raster = -1;
    G_set_fp_type (DCELL_TYPE);
    if (!vector && map) {
	grid = TRUE;
	if (G_find_cell (out_map_opt->answer, G_mapset()) != NULL) 
	    G_fatal_error (_("Raster <%s> already exist."), out_map_opt->answer);

	if ((raster = G_open_fp_cell_new (out_map_opt->answer)) < 0) 
	    G_fatal_error (_("Raster <%s> could not be open."), out_map_opt->answer);
	
	G_short_history (out_map_opt->answer, "raster", &history);
	G_write_history (out_map_opt->answer, &history);
    }

    driver = db_start_driver_open_database (dbdriver->answer, dbdatabase->answer);
    if (driver == NULL)
	    G_fatal_error( _("No database connection for driver <%s> defined. Run db.connect"), dbdriver->answer);

/* Setting auxiliar table's name */
    sprintf (table_name, "%s_aux", out_opt->answer);

/* Setting regions and boxes */    
    G_debug (0, _("Setting regions and boxes"));
    G_get_window (&original_reg);
    G_get_window (&elaboration_reg);
    Vect_region_box (&elaboration_reg, &overlap_box);
    Vect_region_box (&elaboration_reg, &general_box);

    nrows = G_window_rows ();
    ncols = G_window_cols ();
    raster_matrix = G_alloc_matrix (nrows, ncols);

/* Fixxing parameters of the elaboration region */

    P_zero_dim (&dims);					/* Set to zero the dim struct*/
    dims.latoE = NSPLX_MAX * passoE;
    dims.latoN = NSPLY_MAX * passoN;
    dims.overlap = OVERLAP_SIZE * original_reg.ew_res;
    P_get_orlo (bilin, &dims, passoE, passoN);		/* Set the last two dim elements*/
 
/* Creating line and categories structs */   
    points = Vect_new_line_struct ();
    categories = Vect_new_cats_struct ();
    nlines = Vect_get_num_lines (&In);
    
/*---------------------------------------------------------------------------------------------------------------
  | Subdividing and working with tiles: 									|
  | Each original_region will be divided into several subregions. Each one will be overlaped by its neibourgh	|
  | subregions. The overlaping was calculated as a fixed OVERLAP_SIZE times the east-west resolution		|
  ---------------------------------------------------------------------------------------------------------------*/

    elaboration_reg.south = original_reg.north;
    
    last_row = FALSE;
    while (last_row == FALSE){		/* For each row */
		
	P_set_regions(&elaboration_reg, &general_box, &overlap_box, dims, GENERAL_ROW);
	
	if (elaboration_reg.north > original_reg.north) {		/* First row */

	    P_set_regions(&elaboration_reg, &general_box, &overlap_box, dims, FIRST_ROW);
	    nsply = ceil((elaboration_reg.north - elaboration_reg.south)/passoN)+1;
	    G_debug (0, _("nsply = %d"), nsply);
	    if (nsply > NSPLY_MAX) 
		nsply = NSPLY_MAX;
	}
	
	if (elaboration_reg.south <= original_reg.south) {		/* Last row */
	    
	    P_set_regions(&elaboration_reg, &general_box, &overlap_box, dims, LAST_ROW);
	    nsply=ceil((elaboration_reg.north - elaboration_reg.south)/passoN)+1;
	    last_row = TRUE;
	    G_debug (0, _("nsply = %d"), nsply);
	    if (nsply > NSPLY_MAX) 
		nsply = NSPLY_MAX;
	}
		
	elaboration_reg.east = original_reg.west;
	last_column = FALSE;
	
	while (last_column == FALSE){	/* For each column */
	    int npoints = 0;
	    
	    P_set_regions(&elaboration_reg, &general_box, &overlap_box, dims, GENERAL_COLUMN);
	    
	    if (elaboration_reg.west < original_reg.west)  {		/* First column */
		
		P_set_regions(&elaboration_reg, &general_box, &overlap_box, dims, FIRST_COLUMN);
		nsplx=ceil((elaboration_reg.east - elaboration_reg.west)/passoE)+1;
		G_debug (0, _("nsplx = %d"), nsplx);
		if (nsplx > NSPLX_MAX) 
		    nsplx = NSPLX_MAX;
	    }
	
	    if (elaboration_reg.east >= original_reg.east) {		/* Last column */
		
		P_set_regions(&elaboration_reg, &general_box, &overlap_box, dims, LAST_COLUMN);
		last_column = TRUE;
		nsplx=ceil((elaboration_reg.east - elaboration_reg.west)/passoE)+1;
		G_debug (0, _("nsplx = %d"), nsplx);
		if (nsplx > NSPLX_MAX) 
		    nsplx = NSPLX_MAX;
	    }
	    
	/*Setting the active region*/
	    dim_vect = nsplx * nsply;
	    observ = P_Read_Vector_Region_Map (&In, &elaboration_reg, &npoints, dim_vect);
	    G_debug (3, _("Points number in <elaboration_box> is %d"), npoints);
		    
	    if (npoints > 0) {				/*  */
	        int i;
		nparameters = nsplx * nsply;
		BW = P_get_BandWidth (bilin, nsply);
		
	    /*Least Squares system*/
		N = G_alloc_matrix (nparameters, BW);		/* Normal matrix */
		TN = G_alloc_vector (nparameters);		/* vector */
		parVect = G_alloc_vector (nparameters);		/* Parameters vector */
		obsVect = G_alloc_matrix (npoints + 1, 3);	/* Observation vector */
		Q = G_alloc_vector (npoints + 1);		/* "a priori" var-cov matrix */
		
		lineVect = G_alloc_ivector (npoints + 1);	/*  */
		
		mean = P_Mean_Calc (&elaboration_reg, observ, npoints);
		for (i=0; i<npoints; i++) {		/* Setting obsVect vector & Q matrix */
			obsVect[i][0] = observ[i].coordX;
		    	obsVect[i][1] = observ[i].coordY;
		    	obsVect[i][2] = observ[i].coordZ - mean;
			lineVect[i] = observ[i].lineID;
			Q[i] = 1;					/* Q=I */
		}
		
		G_free (observ);
		
		if (bilin) {		/* Bilinear interpolation */
		    	normalDefBilin (N, TN, Q, obsVect, passoE, passoN, nsplx, nsply, elaboration_reg.west, 
						elaboration_reg.south, npoints, nparameters, BW);
		    	nCorrectGrad (N, lambda, nsplx, nsply, passoE, passoN);
		} 
		else{	
		    	normalDefBicubic(N, TN, Q, obsVect, passoE, passoN, nsplx, nsply, elaboration_reg.west, 
						elaboration_reg.south, npoints, nparameters, BW);
		    	nCorrectGrad(N, lambda, nsplx, nsply, passoE, passoN);
		}
		
		tcholSolve(N, TN, parVect, nparameters, BW);
		
		G_free_matrix (N);
		G_free_vector (TN);
		G_free_vector (Q);
		
		if (grid == FALSE) {		/*OBSERVATION POINTS INTERPOLATION*/
		/* Auxiliar table creation */
		    if (flag_auxiliar == FALSE) {
			if ((flag_auxiliar = P_Create_Aux_Table (driver, table_name)) == FALSE) {
			    Vect_close (&In);
			    #ifdef notdef
			    if (flag_ext != FALSE) G_fatal_error (_(""));
			    #endif
			}
		    }
			
		    if (flag_ext == FALSE) {
			P_Sparse_Points (&Out, &elaboration_reg, general_box, overlap_box, obsVect, parVect, lineVect, passoE, passoN, \
				dims.overlap, nsplx, nsply, npoints, bilin, categories, driver, mean, table_name);
			G_free_matrix (obsVect);
			G_free_vector (parVect);
		    }
			    		
		    else {
			int npoints_ext, *lineVect_ext=NULL;
			double **obsVect_ext, mean_ext = .0;
			struct Point *observ_ext;
			
			observ_ext = P_Read_Vector_Region_Map (&In_ext, &elaboration_reg, &npoints_ext, dim_vect);
			
			obsVect_ext = G_alloc_matrix (npoints_ext + 1, 3);	/* Observation vector_ext */
			lineVect_ext = G_alloc_ivector (npoints_ext + 1);
		
			for (i=0; i<npoints_ext; i++) {		/* Setting obsVect_ext vector & Q matrix */
			    obsVect_ext[i][0] = observ_ext[i].coordX;
		    	    obsVect_ext[i][1] = observ_ext[i].coordY;
		    	    obsVect_ext[i][2] = observ_ext[i].coordZ;
			    lineVect_ext[i] = observ_ext[i].lineID;
			}
			
			G_free (observ_ext);
			P_Sparse_Points (&Out, &elaboration_reg, general_box, overlap_box, obsVect_ext, parVect, lineVect_ext, passoE,
				passoN,	dims.overlap, nsplx, nsply, npoints, bilin, categories, driver, mean_ext, table_name);
			
			G_free_matrix (obsVect_ext);
			G_free_vector (parVect);
			G_free_ivector (lineVect_ext);
		    }		/* END FLAG_EXT == TRUE */
		}		/* END IF GRID == FLASE */
		
		else {			/*GRID INTERPOLATION ==> INTERPOLATION INTO A RASTER*/ 
		    G_free_matrix (obsVect);
		    flag_auxiliar = TRUE;
 		    raster_matrix = P_Regular_Points (&elaboration_reg, general_box, overlap_box, raster_matrix, parVect, \
		    					dims.overlap, nsplx, mean, nrows, ncols);	
		    G_free_vector (parVect);
		}
	    }
	    G_free_ivector (lineVect);	
	
	} /*! END WHILE; last_column = TRUE*/
    } /*! END WHILE; last_row = TRUE*/
    
/* Dropping auxiliar table */
    if (flag_auxiliar == TRUE) {
    
    	if (grid == FALSE) {
            if (flag_ext == FALSE)
    	    	P_Aux_to_Vector (&In, &Out, driver, table_name);
    	    else
	    	P_Aux_to_Vector (&In_ext, &Out, driver, table_name);
  
    	/* Dropping auxiliar table */
	    G_debug (3, _("Dropping <s>"), table_name);
	    if (P_Drop_Aux_Table (driver, table_name) != DB_OK)
	    	G_fatal_error(_("Auxiliar Table could not be drop"));
    	}
    	else {
    	    P_Aux_to_Raster (raster_matrix, raster);
    	    G_free_matrix (raster_matrix);
    	}
    }
    
    db_close_database_shutdown_driver (driver);
     
    Vect_close (&In);
    if (flag_ext != FALSE) Vect_close (&In_ext);
    if (!map) 
    	Vect_close (&Out);
    
    if (!vector) G_close_cell (raster);

    G_done_msg("");
    exit(EXIT_SUCCESS);
}	/*END MAIN*/
