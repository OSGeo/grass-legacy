/* example vector module does something like:
 * v.llabel -i map=m1 value=1
 * but the new map is written instead of update of the old one */

#include "gis.h"
#include "Vect.h"

int 
main (int argc, char *argv[])
{
	struct Map_info Map1, Map2;
	static struct line_pnts *Points;
	struct line_cats *Cats;
        int    i, type, cat;
	char   *mapset, errmsg[200];
	struct Option *old, *new;

	old = G_define_option();
	old->key = "input";
	old->type =  TYPE_STRING;
	old->required = YES;
	old->multiple = NO;
	old->gisprompt = "input vector";
	old->description  = "name of input vector file";
	
	new = G_define_option();
	new->key = "output";
	new->type =  TYPE_STRING;
	new->required = YES;
	new->multiple = NO;
	new->gisprompt = "output vector";
        new->description = "name of resulting vector file";

	G_gisinit(argv[0]);
        if (G_parser (argc, argv))
	    exit(-1); 
	
        Points = Vect_new_line_struct ();
	Cats = Vect_new_cats_struct ();
	
        if ((mapset = G_find_vector2 (old->answer, "")) == NULL)
	  {
	     sprintf (errmsg, "Could not find input %s\n", old->answer);
	     G_fatal_error (errmsg);
	  }
	     
        Vect_set_open_level (1); 
	if (1 > Vect_open_old (&Map1, old->answer, mapset) )
	  {
	     sprintf (errmsg, "Could not open input\n");
	     G_fatal_error (errmsg);
	  }
	
	if (0 > Vect_open_new (&Map2, new->answer, WITHOUT_Z))
	  {
	     sprintf (errmsg, "Could not open output\n");
	     Vect_close (&Map1);
	     G_fatal_error (errmsg);
	  }

	Vect_copy_head_data (&(Map1.head), &(Map2.head));

	i=1;
	while ( (type = Vect_read_next_line (&Map1, Points, Cats)) > 0)
	  {
	    if ( type == LINE )
	       {
                 if( Vect_cat_get (Cats, 1, &cat) == 0)
	           {
                      Vect_cat_set (Cats, 1, i);
	              i++;
	           }
	       }	   
	    Vect_write_line ( &Map2, type, Points, Cats );  
	  }
	  
	Vect_close (&Map1);
	Vect_close (&Map2);

	exit(0) ;
}


