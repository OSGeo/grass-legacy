/* ****************************************************************************
 *
 * MODULE:       v.in.dgn 
 * AUTHOR(S):    Radim Blazek
 * PURPOSE:      Import DGN files    
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/
#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "dgnlibp.h"

struct Map_info Map;
DGNHandle         hDGN;
DGNElemCore       *psElement;
int    arcInterp = 0;
int    curveInterp = 0;
int    startcat;
int    use_col, colors[256], startcat;
int    use_lev, levels[100];
struct line_pnts *Points; 
struct Flag    *c_flag;
struct Flag    *t_flag;
int    cat;
struct Categories Cats;    
char   ctype;
int    type, entity;
    
int main( int argc, char ** argv )

{
    int    i, np, itmp, slen, ecount;
    char   *mapset;
    struct Option  *out_opt, *dgn_opt, *type_opt;
    struct Option  *lev_opt, *col_opt, *startcat_opt, *entnum_opt;
    struct GModule *module; 
    struct dig_head d_head; 
    char   errmsg[2000], buf[1000]; 
    char   att_file[2000]; 
    int    bUpdate;

    G_gisinit("Import DGN"); 

    module = G_define_module();
    module->description = "Import Microstation DGN file."; 

    dgn_opt = G_define_option();
    dgn_opt->key = "dgn";
    dgn_opt->description = "dgn file";
    dgn_opt->type = TYPE_STRING;
    dgn_opt->required = YES; 

    lev_opt = G_define_option();
    lev_opt->key = "level";
    lev_opt->description = "input level";
    lev_opt->type = TYPE_INTEGER;
    lev_opt->multiple = YES;    
    lev_opt->answer = "-1";
    lev_opt->required = NO; 
    
    col_opt = G_define_option();
    col_opt->key = "color";
    col_opt->description = "input color";
    col_opt->type = TYPE_INTEGER;
    col_opt->multiple = YES;    
    col_opt->answer = "-1";
    col_opt->required = NO; 

    startcat_opt = G_define_option();
    startcat_opt->key = "startcat";
    startcat_opt->description = "first category number";
    startcat_opt->type = TYPE_INTEGER;
    startcat_opt->multiple = NO;    
    startcat_opt->answer = "1";
    startcat_opt->required = NO; 

    entnum_opt = G_define_option();
    entnum_opt->key = "entity";
    entnum_opt->description = "Entitynum (entity number) from mscatalog. MS link values of this "
	                      "entity are used as categories.";
    entnum_opt->type = TYPE_INTEGER;
    entnum_opt->multiple = NO;    
    entnum_opt->answer = "0";
    entnum_opt->required = NO; 

    out_opt = G_define_option();
    out_opt->key = "output";
    out_opt->description = "dig file";
    out_opt->type = TYPE_STRING;
    out_opt->required = YES;  

    type_opt = G_define_option();
    type_opt->key = "type";
    type_opt->description = "Type: area, line";
    type_opt->type = TYPE_STRING;
    type_opt->required = NO;
    type_opt->multiple = NO;
    type_opt->answer = "line";
    type_opt->options = "line,area";  

    c_flag = G_define_flag();
    c_flag->key          = 'c';
    c_flag->description  = "Use texts as categories instead of labels."; 

    t_flag = G_define_flag();
    t_flag->key          = 't';
    t_flag->description  = "Write point for each text."; 

    /*  check args and set flags  */
    if(G_parser (argc, argv))
        exit (1); 

    mapset = G_store (G_mapset ()); 
    G__make_mapset_element("dig_att") ;
    G__make_mapset_element("dig_cats") ;
    
    G__file_name (att_file, "dig_att", out_opt->answer, mapset);
    
    use_lev = FALSE;
    for (i=0; i < 64; i++) levels[i] = FALSE;
    if ( atoi (lev_opt->answers[0]) > 0 ) {
	use_lev = TRUE;
	i = 0;
	while (lev_opt->answers[i])
        {
	    itmp = atoi ( lev_opt->answers[i] );
	    if ( itmp > 0 && itmp < 64 ) 
		levels[itmp] = TRUE;
	    else
    		fprintf(stderr, "Level %d is out of 1-63\n", itmp) ;	    

	    i++;
	} 
    }
    
    use_col = FALSE;
    for (i=0; i < 256; i++) colors[i] = FALSE;
    if ( atoi (col_opt->answers[0]) >= 0 ) {
	use_col = TRUE;
	i = 0;
	while (col_opt->answers[i])
        {
	    itmp = atoi ( col_opt->answers[i] );
	    if ( itmp >= 0 && itmp < 256 ) 
		colors[itmp] = TRUE;
	    else
    		fprintf(stderr, "Color %d is out of 0-255\n", itmp) ;	    

	    i++;
	} 
    }

    startcat = atoi ( startcat_opt->answer );
    entity = atoi ( entnum_opt->answer );

    /* bUpdate should the file be opened with read+update (rb+) mode instead
       of rb */
    bUpdate =0;
    hDGN = DGNOpen( dgn_opt->answer, bUpdate);
    if( hDGN == NULL )
        G_fatal_error ( "Not able to open dgn file <%s>\n", dgn_opt->answer) ;

    if (0 > Vect_open_new (&Map, out_opt->answer))
    {
        DGNClose( hDGN );       
        G_fatal_error ( "Not able to open vector file <%s>\n", out_opt->answer ) ;
    }  

    if ( (Map.att_fp = fopen(att_file, "w")) == NULL )
    {
        DGNClose( hDGN );           
	Vect_close (&Map); 
        G_fatal_error ( "Cannot open dig_att file '%s' for write\n", Map.att_file);
    }   


    G_init_cats ( 0, "", &Cats); 

    type = LINE ; 
    ctype = 'L';
    if ( type_opt->answer[0] == 'a' ){
	type = AREA ;
	ctype = 'A';
    }

    Points = Vect_new_line_struct ();
    cat = startcat - 1;

    DGNGetElementIndex ( hDGN, &ecount );
    /* Read all elements */
    i = 1;
    while( (psElement=DGNReadElement(hDGN)) != NULL )
    {
	/* fprintf ( stderr, "element = %d\n", i); */
	G_percent(i, ecount , 1); 
        read_element ( psElement );
        DGNFreeElement( hDGN, psElement );
	i++;
    }

    if ( arcInterp > 0 ) G_warning ( "%d Arcs/ellipses stroken\n", arcInterp);
    if ( curveInterp > 0 ) G_warning ( "%d Curves stroken\n", curveInterp);

    Vect_destroy_line_struct ( Points ) ;

    DGNClose( hDGN );
    Vect_close (&Map); 
    fclose (Map.att_fp);

    if (G_write_vector_cats (out_opt->answer, &Cats) == -1)
        G_warning("Unable to open category labes file. Not writing category labels.\n");
    
    G_free_cats (&Cats);  

    exit (0);
}

int
read_element ( DGNElemCore *psElement ) 
{    
    int i, slen, link, tmpcat;
    char tctype;
    DGNElemMultiPoint *psLine;
    DGNElemArc        *psArc;
    DGNElemText       *psText;
    DGNPoint          *dgnPoints = NULL;
    int                ndgnPoints, adgnPoints = 0;
    double twidth, theight, x, y;
    int iLink, nLinkType, nEntityNum=0, nMSLink=0, nLinkSize;
    unsigned char *pabyData;

    if( psElement->deleted ) return 0;    
    if( use_lev && !levels[psElement->level] ) return 0;
    if( use_col && !colors[psElement->color] ) return 0;

    Vect_reset_line ( Points );
    
    link = 0;
    if( psElement->attr_bytes > 0 ) {
        /* fprintf( stderr, "Attributes (%d bytes):\n", psElement->attr_bytes ); */

        for( iLink = 0; 1; iLink++ ) {
            pabyData = DGNGetLinkage( hDGN, psElement, iLink, &nLinkType,
	                                      &nEntityNum, &nMSLink, &nLinkSize );
            if( pabyData == NULL ) break;

	    if ( entity == nEntityNum ) link = nMSLink;
	
            /*	    
            fprintf( stderr, "Type=0x%04x", nLinkType );
            if( nMSLink != 0 || nEntityNum != 0 )
                    fprintf( stderr, ", EntityNum=%d, MSLink=%d", nEntityNum, nMSLink );
            fprintf( stderr, "\n  0x" );
	    
            for( i = 0; i < nLinkSize; i++ ) fprintf( stderr, "%02x", pabyData[i] );
            fprintf( stderr, "\n" );
	    */
        }
    }
    /* fprintf( stderr, "link = %d\n", link ); */
    
    switch( psElement->type )
    {   	
	/* nongraphic */
	case DGNT_TCB:
	case DGNT_CELL_LIBRARY:
	case DGNT_DIGITIZER_SETUP:
	case DGNT_LEVEL_SYMBOLOGY:	    
	case DGNT_APPLICATION_ELEM:
	case DGNT_GROUP_DATA:	    
	    break;

	/* OK */
	case DGNT_LINE: 	    	    
	case DGNT_LINE_STRING:	    
	case DGNT_SHAPE:	    
	    psLine = (DGNElemMultiPoint *) psElement;
	    for( i=0; i < psLine->num_vertices; i++ ){
		Vect_append_point ( Points, 
			psLine->vertices[i].x, psLine->vertices[i].y );
	    }
	    Vect_write_line (&Map,  (unsigned int) type, Points);
	    break; 

	case DGNT_ELLIPSE:	    
	case DGNT_ARC:	    
	    psArc = (DGNElemArc *) psElement;
	    
	    ndgnPoints = (int) fabs( psArc->sweepang) / 5 + 1;
	    if ( ndgnPoints < 2 ) ndgnPoints = 2;
	    /* G_warning ( "Arc/elipse interpolated by %d points\n", ndgnPoints); */
	    if ( ndgnPoints > adgnPoints ) {
		adgnPoints = ndgnPoints;
		dgnPoints = (DGNPoint *) G_realloc( dgnPoints, sizeof(DGNPoint) * adgnPoints);
	    }

	    DGNStrokeArc( hDGN, psArc, ndgnPoints, dgnPoints );
	    
	    for( i=0; i < ndgnPoints; i++ ){
		Vect_append_point ( Points, dgnPoints[i].x, dgnPoints[i].y );
	    }
	    Vect_write_line (&Map,  (unsigned int) type, Points);
	    arcInterp++;
	    break; 
	    
	case DGNT_CURVE: 
	    psLine = (DGNElemMultiPoint *) psElement;
	    
	    ndgnPoints = 5 * psLine->num_vertices ;
	    /* G_warning ( "Curve interpolated by %d points\n", ndgnPoints); */
	    if ( ndgnPoints > adgnPoints ) {
		adgnPoints = ndgnPoints;
		dgnPoints = (DGNPoint *) G_realloc( dgnPoints, sizeof(DGNPoint) * adgnPoints);
	    }

	    DGNStrokeCurve( hDGN, psLine, ndgnPoints, dgnPoints );
	    
	    for( i=0; i < ndgnPoints; i++ ){
		Vect_append_point ( Points, dgnPoints[i].x, dgnPoints[i].y );
	    }
	    Vect_write_line (&Map,  (unsigned int) type, Points);
	    curveInterp++;
	    break; 
	    
	case DGNT_TEXT:
	    psText = (DGNElemText *) psElement;  
	    if ( c_flag->answer )
		cat = atoi ( psText->string );
	    else
		cat++;		

	    theight = psText->height_mult;
	    slen = strlen ( psText->string );

	    /* Bug: twidth calculated below differ from the value in Microstation */
	    twidth = psText->length_mult * ( 2 * slen + (slen-1) ) / 3;

	    x = psText->origin.x;
	    y = psText->origin.y;

	    /* Is it v dgnlib.h correct for DGNJ_* constatnts ? */
	    switch ( psText->justification )
	    {
		/* case DGNJ_LEFT_TOP: */
		case 0:
		    y += theight;
		    break;
		/* case DGNJ_LEFT_CENTER: */
		case 1:
		    y += theight / 2;
		    break;
		/* case DGNJ_LEFT_BOTTOM: */
		case 2:
		    break;		
		/* case DGNJ_CENTER_TOP: */
		case 6:
		    x += twidth / 2;
		    y += theight;
		    break;		
		/* case DGNJ_CENTER_CENTER: */
		case 7:
		    x += twidth / 2;
		    y += theight / 2;
		    break;
		/* case DGNJ_CENTER_BOTTOM: */
		case 8:
		    x += twidth / 2;		    
		    break;
		/* case DGNJ_RIGHT_TOP: */
		case 12:
		    x += twidth;		    
		    y += theight;
		    break;
		/* case DGNJ_RIGHT_CENTER: */
		case 13:
		    x += twidth;		    
		    y += theight / 2;
		    break;
		/* case DGNJ_RIGHT_BOTTOM: */
		case 14:
		    x += twidth;
		    break;
	    }

	    if ( t_flag->answer ) {
		Vect_append_point ( Points, x, y );
		Vect_append_point ( Points, x, y ); /* Because point is written as 0 length line */
	        Vect_write_line (&Map,  (unsigned int) DOT, Points);
		tctype = 'P';
	    } else {
		tctype = ctype;
	    }

	    if ( entity > 0 ) { 
		tmpcat = link; 
	    } else {
		tmpcat = cat;
		if ( tmpcat > 0 ) G_set_cat( cat, psText->string, &Cats); 		
	    }

	    if ( tmpcat > 0 ) {
  	        fprintf ( Map.att_fp,"%c    %10.3f   %10.3f    %6d\n", tctype, x, y, tmpcat );
	        fflush(Map.att_fp) ;
	    }
	    break; 

	/* Not fully supported */    
	case DGNT_COMPLEX_CHAIN_HEADER:
	case DGNT_COMPLEX_SHAPE_HEADER:
	    G_warning ( "Complex element header ignored, complex element parts written\n" );
	    break;

	case DGNT_CELL_HEADER:
	    G_warning ( "Unshared cell header ignored, cell parts written without transformation.\n" );
	    break;

	/* Not allowed */
	case DGNT_TEXT_NODE:
	case DGNT_BSPLINE:
	    G_warning("Element type not supported: %d %s\n", psElement->type, DGNTypeToName( psElement->type ));
	    break; 

	default:
	    G_warning("Unknown element type: %d %s\n", psElement->type, DGNTypeToName( psElement->type ));
	    break; 	    

    }	 

    return 1;
}
