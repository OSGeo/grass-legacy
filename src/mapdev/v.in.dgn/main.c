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

int main( int argc, char ** argv )

{
    int    i, np, type, cat, ncat, use_lev, levels[100], itmp, slen;
    int    use_col, colors[256], startcat;
    double twidth, theight, x, y;
    char   *mapset;
    struct Option  *out_opt, *dgn_opt, *type_opt;
    struct Option  *lev_opt, *col_opt, *startcat_opt;
    struct Flag    *c_flag;
    struct GModule *module; 
    struct Map_info Map;
    struct dig_head d_head; 
    char   errmsg[2000], buf[1000]; 
    struct line_pnts *Points; 
    char   att_file[2000]; 
    struct Categories Cats;    
    char   ctype;
    
    DGNHandle         hDGN;
    DGNElemCore       *psElement;
    DGNElemMultiPoint *psLine;
    DGNElemText       *psText;

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

    hDGN = DGNOpen( dgn_opt->answer );
    if( hDGN == NULL )
    {
        sprintf(errmsg, "Not able to open dgn file <%s>\n", dgn_opt->answer) ;
        G_fatal_error (errmsg);
    }      

    if (0 > Vect_open_new (&Map, out_opt->answer))
    {
        DGNClose( hDGN );       
        sprintf(errmsg, "Not able to open vector file <%s>\n", out_opt->answer) ;
        G_fatal_error (errmsg);
    }  

    if ( (Map.att_fp = fopen(att_file, "w")) == NULL )
    {
        DGNClose( hDGN );           
	Vect_close (&Map); 
        sprintf(errmsg,  "Cannot open dig_att file '%s' for write\n", Map.att_file);
        G_fatal_error (errmsg);
    }   

    Points = Vect_new_line_struct ();

    G_init_cats ( 0, "", &Cats); 

    type = LINE ; 
    ctype = 'L';
    if ( type_opt->answer[0] == 'a' ){
	type = AREA ;
	ctype = 'A';
    }

    cat = startcat - 1;
    ncat = 0;
    while( (psElement=DGNReadElement(hDGN)) != NULL )
    {
       if( psElement->deleted ) continue;    

       if( use_lev && !levels[psElement->level] )
       {
            DGNFreeElement( hDGN, psElement );
            continue;
       }

       if( use_col && !colors[psElement->color] )
       {
            DGNFreeElement( hDGN, psElement );
            continue;
       }       
       
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
		Vect_reset_line ( Points );
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

		fprintf ( Map.att_fp,"%c    %10.3f   %10.3f    %6d\n", 
			    ctype, x, y, cat );
		fflush(Map.att_fp) ;

                G_set_cat( cat, psText->string, &Cats); 		

		ncat++;
		break; 

	    /* not allowed */
	    case DGNT_ARC: 
	    case DGNT_CURVE:
	    case DGNT_TEXT_NODE:
	    case DGNT_CELL_HEADER:
	    case DGNT_COMPLEX_CHAIN_HEADER:
	    case DGNT_COMPLEX_SHAPE_HEADER:
	    case DGNT_ELLIPSE:
	    case DGNT_BSPLINE:
		fprintf( stderr, "Element type not supported: %d %s\n", psElement->type, DGNTypeToName( psElement->type ));
		break; 

	    default:
		fprintf( stderr, "Unknown element type: %d %s\n", psElement->type, DGNTypeToName( psElement->type ));
		break; 	    

	}	 
        DGNFreeElement( hDGN, psElement );
    }

    Vect_destroy_line_struct ( Points ) ;

    DGNClose( hDGN );
    Vect_close (&Map); 
    fclose (Map.att_fp);

    if (G_write_vector_cats (out_opt->answer, &Cats) == -1)
    {
        G_warning("Unable to open category labes file. Not writing category labels.\n");
	exit(0);
    }
    G_free_cats (&Cats);  

    exit (0);
}

