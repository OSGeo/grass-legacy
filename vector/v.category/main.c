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
	int    cat, field;
	struct GModule *module;
	struct Option *in_opt, *out_opt, *option_opt, *type_opt;
	struct Option *cat_opt, *field_opt, *step_opt;
	FREPORT **freps;
	int nfreps, rtype, fld;

	module = G_define_module();
	module->description = 
		"Attach, delete or report vector categories.";

	in_opt = G_define_option();
	in_opt->key = "input";
	in_opt->type =  TYPE_STRING;
	in_opt->required = YES;
	in_opt->multiple = NO;
	in_opt->gisprompt = "old,dig,vector";
	in_opt->description  = "Name of input vector";
	
	out_opt = G_define_option();
	out_opt->key = "output";
	out_opt->type =  TYPE_STRING;
	out_opt->required = NO;
	out_opt->multiple = NO;
	out_opt->gisprompt = "new,dig,vector";
        out_opt->description = "Name of resulting vector";

	type_opt = G_define_option() ;
	type_opt->key        = "type" ;
	type_opt->type       = TYPE_STRING ;
	type_opt->required   = NO ;
	type_opt->multiple   = YES ;
	type_opt->answer     = "line" ;
	type_opt->options    = "point,line,boundary,centroid,area";
	type_opt->description= "Select type: point, line, boundary, centroid or area" ;
	
	option_opt = G_define_option();
	option_opt->key = "option";
	option_opt->type =  TYPE_STRING;
	option_opt->required = NO;
	option_opt->multiple = NO;
	option_opt->options = "add,del,report";
	option_opt->answer = "add";
        option_opt->description = "Action to be done";

	cat_opt = G_define_option();
	cat_opt->key = "cat";
	cat_opt->type =  TYPE_INTEGER;
	cat_opt->required = NO;
	cat_opt->multiple = NO;
	cat_opt->answer = "1";
        cat_opt->description = "Category value";
	
	field_opt = G_define_option();
	field_opt->key = "field";
	field_opt->type =  TYPE_INTEGER;
	field_opt->required = NO;
	field_opt->multiple = NO;
	field_opt->answer = "1";
        field_opt->description = "Field value";

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
	            otype |= DOT;
		    break;
	        case 'l':
	            otype |= LINE;
		    break;
	        case 'b':
	            otype |= BOUNDARY;
		    break;
	        case 'c':
	            otype |= CENTROID;
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
        if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL)
	  {
	     sprintf (errmsg, "Could not find input %s\n", in_opt->answer);
	     G_fatal_error (errmsg);
	  }
	
        Vect_set_open_level (1); 
	if (1 > Vect_open_old (&In, in_opt->answer, mapset) )
	  {
	     sprintf (errmsg, "Could not open input\n");
	     G_fatal_error (errmsg);
	  }

	/* open output vector if needed */
	if (option == O_ADD || option == O_DEL)
          {
	    with_z = In.head.with_z;
	
	    if (0 > Vect_open_new (&Out, out_opt->answer, with_z))
	      {
	         sprintf (errmsg, "Could not open output\n");
	         Vect_close (&In);
	         G_fatal_error (errmsg);
	      }

	    Vect_copy_head_data (&(In.head), &(Out.head));
          }


        switch ( option)
	  {	
	    case (O_ADD):	  
	        while ( (type = Vect_read_next_line (&In, Points, Cats)) > 0)
	          {
	            if ( (type & ELEMENT_TYPE_LIVE) && (type & otype) )
	               {
                         if( (Vect_cat_get (Cats, field, &cat)) == 0)
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
	            if ( type & ELEMENT_TYPE_DEAD ) continue;
		    
                    switch (type)
		      {
                        case (DOT):
			    rtype = FR_POINT;
			    break;
                        case (LINE):
			    rtype = FR_LINE;
			    break;
                        case (BOUNDARY):
			    rtype = FR_BOUNDARY;
			    break;
                        case (CENTROID):
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
	if (option == O_ADD || option == O_DEL)
	    Vect_close (&Out);

	exit(0) ;
}


