/* example vector module does something like:
 * v.llabel -i map=m1 value=1
 * but the new map is written instead of update of the old one */

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "glocale.h"

int 
main (int argc, char *argv[])
{
	struct Map_info In, Out;
	static struct line_pnts *Points;
	struct line_cats *Cats;
        int    i, type, cat;
	char   *mapset;
	struct Option *old, *new;

	old = G_define_option();
	old->key = "input";
	old->type =  TYPE_STRING;
	old->required = YES;
	old->multiple = NO;
	old->gisprompt = "input vector";
	old->description  = _("name of input vector file");
	
	new = G_define_option();
	new->key = "output";
	new->type =  TYPE_STRING;
	new->required = YES;
	new->multiple = NO;
	new->gisprompt = "output vector";
        new->description = _("name of resulting vector file");

	G_gisinit(argv[0]);
        if (G_parser (argc, argv))
	    exit(-1); 
	
        Points = Vect_new_line_struct ();
	Cats = Vect_new_cats_struct ();
	
        if ((mapset = G_find_vector2 (old->answer, "")) == NULL)
	     G_fatal_error ( _("Could not find input %s"), old->answer);
	     
        Vect_set_open_level (2); 
	if (1 > Vect_open_old (&In, old->answer, mapset) )
	     G_fatal_error ( _("Could not open input") );
	
	if (0 > Vect_open_new (&Out, new->answer, WITHOUT_Z))
	{
	     Vect_close (&In);
	     G_fatal_error ( _("Could not open output") );
	}

	Vect_copy_head_data (&In, &Out);
	Vect_hist_copy (&In, &Out);
	Vect_hist_command ( &Out );

	i=1;
	while ( (type = Vect_read_next_line (&In, Points, Cats)) > 0)
	  {
	    if ( type == GV_LINE )
	       {
                 if( Vect_cat_get (Cats, 1, &cat) == 0)
	           {
                      Vect_cat_set (Cats, 1, i);
	              i++;
	           }
	       }	   
	    Vect_write_line ( &Out, type, Points, Cats );  
	  }
	  
	Vect_build (&Out, stdout );
	Vect_close (&In);
	Vect_close (&Out);

	exit(0) ;
}


