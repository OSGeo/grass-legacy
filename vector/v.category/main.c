/* ***************************************************************
 * *
 * * MODULE:       v.category
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Category manipulations
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdlib.h> 
#include "gis.h"
#include "Vect.h"

#define O_ADD  1
#define O_DEL  2
#define O_REP  3

#define FRTYPES 6  /* number of field report types */

#define FR_POINT    0
#define FR_LINE     1
#define FR_BOUNDARY 2
#define FR_CENTROID 3
#define FR_AREA     4
#define FR_ALL      5

typedef struct {
    int field;
    int count[FRTYPES];
    int min[FRTYPES], max[FRTYPES];
} FREPORT;
    
int 
main (int argc, char *argv[])
{
	struct Map_info In, Out;
	static struct line_pnts *Points;
	struct line_cats *Cats;
        int    i, j, ret, option, otype, type, oarea, with_z, step;
	char   *mapset, errmsg[200];
	int    cat, ocat, field;
	struct GModule *module;
	struct Option *in_opt, *out_opt, *option_opt, *type_opt;
	struct Option *cat_opt, *field_opt, *step_opt;
	FREPORT **freps;
	int nfreps, rtype, fld;

	module = G_define_module();
	module->description = 
		"Attach, delete or report vector categories.";

	in_opt = G_define_standard_option(G_OPT_V_INPUT);
	out_opt = G_define_standard_option(G_OPT_V_OUTPUT);
	type_opt = G_define_standard_option(G_OPT_V_TYPE) ;
	
	option_opt = G_define_option();
	option_opt->key = "option";
	option_opt->type =  TYPE_STRING;
	option_opt->required = NO;
	option_opt->multiple = NO;
	option_opt->options = "add,del,report";
	option_opt->answer = "add";
        option_opt->description = "Action to be done";

	cat_opt = G_define_standard_option(G_OPT_V_CAT);
	cat_opt->answer = "1";
	
	field_opt = G_define_standard_option(G_OPT_V_FIELD);
	field_opt->answer = "1";

	step_opt = G_define_option();
	step_opt->key = "step";
	step_opt->type =  TYPE_INTEGER;
	step_opt->required = NO;
	step_opt->multiple = NO;
	step_opt->answer = "1";
        step_opt->description = "Category increment";
	
	G_gisinit(argv[0]);
        if (G_parser (argc, argv))
	    exit(-1); 
	
	/* read options */
        switch ( option_opt->answer[0] )
          {
	    case ( 'a' ):
		option = O_ADD;
		break;
	    case ( 'd' ):
		option = O_DEL;
		break;
	    case ( 'r' ):
		option = O_REP;
		break;
	  }

	cat = atoi( cat_opt->answer );
	field = atoi( field_opt->answer );
	step = atoi( step_opt->answer );
	
        
	i = 0;
        otype = 0; oarea = FALSE;
	while (type_opt->answers[i])
	  {
	    switch ( type_opt->answers[i][0] )
	      {
	        case 'p':
	            otype |= GV_POINT;
		    break;
	        case 'l':
	            otype |= GV_LINE;
		    break;
	        case 'b':
	            otype |= GV_BOUNDARY;
		    break;
	        case 'c':
	            otype |= GV_CENTROID;
		    break;
	        case 'a':
	            oarea = TRUE;
		    break;
	      }
	    i++;
	  }
	
	if ( (option != O_REP) && (out_opt->answer == NULL) )
	  {
	    sprintf (errmsg, "Output vector wasn't entered.\n");
	    G_fatal_error (errmsg);
	  }
	
        Points = Vect_new_line_struct ();
	Cats = Vect_new_cats_struct ();
	
	/* open input vector */
        if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
	     sprintf (errmsg, "Could not find input %s\n", in_opt->answer);
	     G_fatal_error (errmsg);
	}
	
        Vect_set_open_level (1); 
	Vect_open_old (&In, in_opt->answer, mapset); 

	/* open output vector if needed */
	if (option == O_ADD || option == O_DEL)
          {
	    with_z = In.head.with_z;
	
	    Vect_set_fatal_error (GV_FATAL_PRINT);
	    if (0 > Vect_open_new (&Out, out_opt->answer, with_z)) {
	         Vect_close (&In);
	         exit (1);
	    }

	    Vect_copy_head_data (&In, &Out);
          }


        switch ( option)
	  {	
	    case (O_ADD):	  
	        while ( (type = Vect_read_next_line (&In, Points, Cats)) > 0)
	          {
	            if ( type & otype )
	               {
                         if( (Vect_cat_get (Cats, field, &ocat)) == 0)
	                   {
                             Vect_cat_set (Cats, field, cat);
	                     cat += step;
	                   }
	               }	   
	            Vect_write_line ( &Out, type, Points, Cats );  
	          }
		break;
		
	    case (O_DEL):	  
	        while ( (type = Vect_read_next_line (&In, Points, Cats)) > 0)
	          {
	            if ( type & otype )
	               {
                         ret = Vect_cat_del (Cats, field);
	               }	   
	            Vect_write_line ( &Out, type, Points, Cats );  
	          }
		break;
		
	    case (O_REP):	  
		nfreps = 0;
		freps = NULL;
	        while ( (type = Vect_read_next_line (&In, Points, Cats)) > 0)
	          {
                    switch (type)
		      {
                        case (GV_POINT):
			    rtype = FR_POINT;
			    break;
                        case (GV_LINE):
			    rtype = FR_LINE;
			    break;
                        case (GV_BOUNDARY):
			    rtype = FR_BOUNDARY;
			    break;
                        case (GV_CENTROID):
			    rtype = FR_CENTROID;
			    break;
		      }
		    
		    for (i=0; i < Cats->n_cats; i++)
                      {
                        field = Cats->field[i];
                        cat = Cats->cat[i];
                        
			
			ret = FALSE;
		        for (j=0; j < nfreps; j++)
			  {
                            if ( freps[j]->field == field )
			      {
				fld = j;      
				ret = TRUE;      
				break;
			      }
			  }
			if ( !ret ) /* field report doesn't exist */
		          {
			    nfreps++; 	    
                            freps = (FREPORT **) realloc ( freps, nfreps * sizeof (FREPORT *));
                            fld = nfreps - 1;
                            freps[fld] = (FREPORT *) calloc ( 1, sizeof (FREPORT));
                            freps[fld]->field = field;
			  }
			
                        freps[fld]->count[rtype]++;
                        freps[fld]->count[FR_ALL]++;
			
			if ( (freps[fld]->min[rtype] == 0) || freps[fld]->min[rtype] > cat)
		            freps[fld]->min[rtype] = cat;
			
			if ( (freps[fld]->max[rtype] == 0) || freps[fld]->max[rtype] < cat)
		            freps[fld]->max[rtype] = cat;
			
			if ( (freps[fld]->min[FR_ALL] == 0) || freps[fld]->min[FR_ALL] > cat)
		            freps[fld]->min[FR_ALL] = cat;
			
			if ( (freps[fld]->max[FR_ALL] == 0) || freps[fld]->max[FR_ALL] < cat)
		            freps[fld]->max[FR_ALL] = cat;
		      }
	          }
                for (i=0; i < nfreps; i++)
		  {
		    fprintf (stdout, "FIELD %d:\n", freps[i]->field);
		    fprintf (stdout, "type       count        min        max\n");
		    fprintf (stdout, "point    %7d %10d %10d\n", 
				    freps[i]->count[FR_POINT],
				    freps[i]->min[FR_POINT],
				    freps[i]->max[FR_POINT]);
		    fprintf (stdout, "line     %7d %10d %10d\n", 
				    freps[i]->count[FR_LINE],
				    freps[i]->min[FR_LINE],
				    freps[i]->max[FR_LINE]);
		    fprintf (stdout, "boundary %7d %10d %10d\n", 
				    freps[i]->count[FR_BOUNDARY],
				    freps[i]->min[FR_BOUNDARY],
				    freps[i]->max[FR_BOUNDARY]);
		    fprintf (stdout, "centroid %7d %10d %10d\n", 
				    freps[i]->count[FR_CENTROID],
				    freps[i]->min[FR_CENTROID],
				    freps[i]->max[FR_CENTROID]);
		    fprintf (stdout, "area     %7d %10d %10d\n", 
				    freps[i]->count[FR_AREA],
				    freps[i]->min[FR_AREA],
				    freps[i]->max[FR_AREA]);
		    fprintf (stdout, "all      %7d %10d %10d\n", 
				    freps[i]->count[FR_ALL],
				    freps[i]->min[FR_ALL],
				    freps[i]->max[FR_ALL]);

		  }
		break;
	  }
	
	Vect_close (&In);
	if (option == O_ADD || option == O_DEL) {
	    Vect_build (&Out, stdout);
	    Vect_close (&Out);
	}

	exit(0) ;
}


