/*
 * $Id$
 */

/* init.c								*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

FLAG *FlagCreate();

Init (argc, argv)
	int	argc;
	char    *argv[];
{
    struct Option 	*SeedStuff;
    struct Cell_head	Region;
    int			Count, NumCov, k, i, j, NumWeight, NumDist, NumExp;
    int			comp_array();
    int			FD, row, col;
    char		*Name, *Number, String[80];
    double		MinRes, SumWeight, GasDev();
    FUNCTION(Init);

    Output 		= G_define_option() ;
    Output->key       	= "output";
    Output->type      	= TYPE_STRING;
    Output->required  	= YES;
    Output->multiple  	= NO;
    Output->description	= "Name of indepent cells map";
    Output->gisprompt  	= "new,cell,raster" ;

    Distance             = G_define_option() ;
    Distance->key        = "distance";
    Distance->type       = TYPE_DOUBLE;
    Distance->required   = YES;
    Distance->multiple   = NO;
    Distance->description=
    "Input value: max. distance of spatial correlation (value(s) >= 0.0)";

    SeedStuff             = G_define_option() ;
    SeedStuff->key        = "seed";
    SeedStuff->type       = TYPE_INTEGER;
    SeedStuff->required   = NO;
    SeedStuff->description=
   "Input value: random seed (SEED_MIN >= value >= SEED_MAX), default [random]";

    if (G_parser(argc, argv))
        exit (1);

    Rs = G_window_rows();
    Cs = G_window_cols();
    G_get_set_window(&Region);
    EW = Region.ew_res;
    NS = Region.ns_res;
    if( EW < NS)	MinRes = EW;
    else		MinRes = NS;
    CellBuffer = G_allocate_cell_buf();
    /* Out = FlagCreate( Rs, Cs); */
    Out = (CELL **) G_malloc( sizeof(CELL *) * Rs);
    for( row = 0; row < Rs; row++) {
	Out[ row] = G_allocate_cell_buf();
	G_zero_cell_buf( Out[ row]);
    }
    Cells = FlagCreate( Rs, Cs);
    CellCount = 0;
    if (NULL != G_find_file( "cell", "MASK", G_mapset())) {
        if ((FD = G_open_cell_old( "MASK", G_mapset())) < 0) {
                G_fatal_error(" unable to open MASK");
                exit( -1);
        } else {
                for (row = 0; row < Rs; row++)     {
                        G_get_map_row_nomask( FD, CellBuffer, row);
                        for( col = 0; col < Cs; col++)     {
                                if( CellBuffer[ col]) {
                                        FLAG_SET( Cells, row, col);
					CellCount++;
                                }
                        }
                }
                G_close_cell ( FD);
        }
    } else {
	for (row = 0; row < Rs; row++)     {
                for( col = 0; col < Cs; col++)     {
                       FLAG_SET( Cells, row, col);
                }
	}
	CellCount = Rs * Cs;
    }
    if( ! Output->answer) {
        G_fatal_error("There should be an output map");
    } else {
        IsLegal( Output->answer);
    }
    sscanf( Distance->answer, "%lf", &MaxDist);
    if( MaxDist < 0.0) {
	G_fatal_error( "distance must be >= 0.0");
    }
    DOUBLE(MaxDist);
    MaxDistSq = MaxDist * MaxDist;
    if(! SeedStuff->answer){
		Seed = (int) getpid();
    } else {
       	sscanf( SeedStuff->answer, "%d", &(Seed));
    }
    if (Seed > SEED_MAX){
       	    Seed = Seed % SEED_MAX;
   } else if (Seed < SEED_MIN){
       	    while( Seed < SEED_MIN) 
			Seed += SEED_MAX - SEED_MIN;
    }
    DoNext = (CELLSORTER *) G_malloc( CellCount * sizeof(CELLSORTER));
    Count = 0;
    for( row = 0; row < Rs; row++) {
	for( col = 0; col < Cs; col++)     {
		if( 0 != FlagGet( Cells, row, col)) {
			DoNext[ Count].R = row;
			DoNext[ Count].C = col;
			DoNext[ Count].Value = GasDev();
			if( ++Count == CellCount) {
				row = Rs;
				col = Cs;
			}
		}
	}
    }
    qsort( DoNext, CellCount, sizeof(CELLSORTER), comp_array);
}

comp_array( p1, p2)
CELLSORTER *p1, *p2;
{
	if( p1->Value < p2->Value)
		return( -1);
	if( p2->Value < p1->Value)
		return( 1);
	return( 0);
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
