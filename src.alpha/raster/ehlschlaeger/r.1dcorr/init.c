/* init.c                                                               */

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

init (argc, argv, MapName)
	int     argc;
	char    *argv[], **MapName;
{
    struct Option       *LagNameOption, *InputOption, *LagOption;
    struct Option       *MinOption, *MaxOption;
    struct Cell_head    Region;
    int                 row, col, NumCov, k, i, j, NumWeight, FDM, FDC,
			NumDist, NumExp;
    char                *Name, *Number, String[80];
    double              Northing, Easting, NewMinDist, NewMaxDist,
			MinRes, SumWeight;
    CELL		*ARow;
    

    FUNCTION(Init);
    InputOption              = G_define_option() ;
    InputOption->key         = "input";
    InputOption->type        = TYPE_STRING;
    InputOption->required    = YES;
    InputOption->multiple    = NO;
    InputOption->description = "Map to be analyzed";
    InputOption->gisprompt   = "old,cell,raster" ;
    
    LagOption             = G_define_option() ;
    LagOption->key        = "lags";
    LagOption->type       = TYPE_DOUBLE;
    LagOption->required   = YES;
    LagOption->multiple   = YES;
    LagOption->description=
    "Input value(s): Distance of correlation lag(s) (value(s) > 0.0)";

    LagNameOption              = G_define_option() ;
    LagNameOption->key         = "outfile";
    LagNameOption->type        = TYPE_STRING;
    LagNameOption->required    = YES;
    LagNameOption->multiple    = YES;
    LagNameOption->description = "Name(s) of correlation file(s)";
    
    MinOption             = G_define_option() ;
    MinOption->key        = "min_distance";
    MinOption->type       = TYPE_DOUBLE;
    MinOption->required   = NO;
    MinOption->multiple   = NO;
    MinOption->description=
    "Input value: Minimum distance of correlations (default: 0)";

    MaxOption             = G_define_option() ;
    MaxOption->key        = "max_distance";
    MaxOption->type       = TYPE_DOUBLE;
    MaxOption->required   = NO;
    MaxOption->multiple   = NO;
    MaxOption->description=
    "Input value: Maximum distance of correlations (default: entire map)";

    Verbose                     = G_define_flag() ;
    Verbose->key                = 'q' ;
    Verbose->description        = "Quiet running" ;

    if (G_parser(argc, argv))
	exit (1);

    FUNCTION(Init post parser);
    Rs = G_window_rows();
    Cs = G_window_cols();
    G_get_set_window(&Region);
    EW = Region.ew_res;
    NS = Region.ns_res;
    if( EW < NS)        MinRes = EW;
    else                MinRes = NS;
    MinDist = MinDistSq = 0.0;
    MaxDistSq = EW * EW * Cs * Cs + NS * NS * Rs * Rs;
    MaxDist = sqrt( MaxDistSq);
    if( MinOption->answer) {
     	sscanf( MinOption->answer, "%lf", &NewMinDist);
      	if( NewMinDist <= 0.0) 
		G_fatal_error( "min_distance must be >= 0.0");
	if( NewMinDist < MaxDist) {
		MinDist = NewMinDist;
		MinDistSq = MinDist * MinDist;
	}
	else G_fatal_error( "min_distance greater than extents of region.");
    }
    if( MaxOption->answer) {
     	sscanf( MaxOption->answer, "%lf", &NewMaxDist);
      	if( NewMaxDist <= 0.0) 
		G_fatal_error( "max_distance must be greater than 0.0");
	if( NewMaxDist < MaxDist) {
		MaxDist = NewMaxDist;
		MaxDistSq = MaxDist * MaxDist;
	}
	else G_warning(
        "max_distance greater than extents of region.\nregion distance used.");
    }
    if( Rs * Cs < 2) {
	sprintf( Buf, "%s: Map Rows[%d] times Map Columns[%d] must be >= 2",
		G_program_name(), Rs, Cs);
	G_fatal_error( Buf);
    }   
    if( ! InputOption->answer) {
	sprintf( Buf, "%s: must contain input map", G_program_name());
        G_fatal_error( Buf);
    }
    FUNCTION(Init pre lagoption);
    NumLags = 0;
    Lags = (LAG *) G_malloc( sizeof( LAG));
    if( LagOption->answers && LagNameOption->answers) {
	for (i = 0; Name = LagNameOption->answers[i]; i++) {
	    Lags = (LAG *) G_realloc( Lags, ++NumLags * sizeof( LAG));
	    Number = LagOption->answers[i];
	    if( Number == NULL) {
                sprintf( Buf,
		    "%s: must contain as many lag numbers as lag names",
			G_program_name());
                G_fatal_error( Buf);
	    }
	    sscanf( Number, "%lf", &(Lags[ i].Lag));
	    if( Lags[ i].Lag <= 0.0) {
		sprintf( Buf, "%s: lag value[%d]: [%lf] must be > 0.0",
				G_program_name(), i, Lags[ i].Lag);
		G_fatal_error( Buf);
	    }
	    IsLegal( Name);
	    strcpy( Lags[ i].Name, Name);
	}
    }
    Name = InputOption->answer;
    Mapset = G_find_cell2 (Name, "");
    if (!Mapset) {
        sprintf (Buf,"%s: [%s] not found", G_program_name(), Name);
        G_fatal_error (Buf);
    }
    FDC = G_open_cell_old (Name, Mapset);
    if( FDC < 0) {
            sprintf( Buf, "%s: unable to open [%s] raster map",
                    G_program_name(), Name);
            G_fatal_error (Buf);
    }
    strcpy( Name, *MapName);
    FUNCTION(Init post lagoption);
    ARow = G_allocate_cell_buf();
    if (NULL == G_find_file( "cell", "MASK", G_mapset())) {
	Count = Rs * Cs;
	Cells = (CELLS *) G_malloc( Count * sizeof( CELLS));
	Count = 0;
	for (row = 0; row < Rs; row++)     {
		Northing = G_row_to_northing( row + .5, &Region);
		G_get_map_row_nomask( FDC, ARow, row);
                for( col = 0; col < Cs; col++)     {
			Cells[ Count].North = Northing;
			Easting=G_col_to_easting(col + .5,&Region);
			Cells[ Count].East = Easting;
			Cells[ Count].Value = ARow[ col];
			Count++;
                }
	}
	G_close_cell ( FDC);
    } else {
        if ((FDM = G_open_cell_old( "MASK", G_mapset())) < 0) {
                G_fatal_error(" unable to open MASK");
                exit( -1);
        } else {
		Count = 0;
		CellBuffer = G_allocate_cell_buf();
                for (row = 0; row < Rs; row++)     {
                        G_get_map_row_nomask( FDM, CellBuffer, row);
                        for( col = 0; col < Cs; col++)     {
                                if( CellBuffer[ col]) {
					Count++;
                                }
                        }
                }
		FUNCTION(Init pre G_malloc Cells);
		Cells = (CELLS *) G_malloc( Count * sizeof( CELLS));
		FUNCTION(Init post G_malloc Cells);
		Count = 0;
                for (row = 0; row < Rs; row++)     {
			Northing = G_row_to_northing( row + .5, &Region);
                        G_get_map_row_nomask( FDM, CellBuffer, row);
			G_get_map_row_nomask( FDC, ARow, row);
                        for( col = 0; col < Cs; col++)     {
                                if( CellBuffer[ col]) {
				    Cells[ Count].North = Northing;
				    Easting=G_col_to_easting(col + .5,&Region);
				    Cells[ Count].East = Easting;
				    Cells[ Count].Value = ARow[ col];
				    Count++;
                                }
                        }
                }
                G_close_cell ( FDM);
		G_close_cell ( FDC);
        }
    }
    free( ARow);
    FUNCTION(end Init);
}

IsLegal (Name)
char *Name;
{
	if (G_legal_filename (Name) == -1) {
		sprintf (Buf, "%s: map name [%s] not legal for GRASS\n",
			G_program_name(), Name);
		G_fatal_error (Buf);
		exit (1);
	}
}
