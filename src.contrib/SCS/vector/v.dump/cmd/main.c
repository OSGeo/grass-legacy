/*  @(#)main.c     1.1  6/03/91   
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector maps, areas, lines, islands, etc.
 *
 * snoop [-falicne] map=name[,name,...]
 *                  num=ent number
 *
 *  flags:    -f(default)   full reports:   All areas, lines, islands, etc.\n");
 *            -a   areas only report
 *            -l   lines only report
 *            -i   islands only report
 *            -c   categories only report
 *            -n   nodes only report
 *            -e   modifies header records
 */

#include <stdio.h>
#include  "gis.h"
#include "Vect.h"
#include "V_.h"
#include "dump.h"

struct Map_info Map;
char  buf[1024] ;
struct Categories cats;

main (argc,argv)
int argc;
char *argv[];

{
	register int area_num, line_num, isle_num, node_num, i;
	int vect_read, got_cats;
	char *input, *mapset;

	struct
	    {
		struct Option *mapopt;
		struct Option *entopt;
	} parms;
	struct
	    {
		struct Flag *f;
		struct Flag *a;
		struct Flag *l;
		struct Flag *i;
		struct Flag *c;
		struct Flag *n;
		struct Flag *e;
	} flags;

        G_gisinit(argv[0]);

        parms.entopt = G_define_option();
        parms.entopt->key                = "num";
        parms.entopt->type               = TYPE_INTEGER;
        parms.entopt->required           = NO;                 
        parms.entopt->multiple           = NO; 
        parms.entopt->gisprompt          = "entity number";
        parms.entopt->description        = "Unique number of an area,line,site,category, or node";

        parms.mapopt = G_define_option();
        parms.mapopt->key                = "map";
        parms.mapopt->type               = TYPE_STRING;
        parms.mapopt->required           = YES;                 
        parms.mapopt->multiple           = YES; 
        parms.mapopt->gisprompt          = "old,dig,vector"; 
        parms.mapopt->description        = "vector map(s)";

        flags.f = G_define_flag();
        flags.f->key              = 'f';
        flags.f->description      = "full report [default]";

        flags.a = G_define_flag();
        flags.a->key              = 'a';
        flags.a->description      = "area report";

        flags.l = G_define_flag();
        flags.l->key              = 'l';
        flags.l->description      = "line report";

        flags.i = G_define_flag();
        flags.i->key              = 'i';
        flags.i->description      = "island report";

        flags.c = G_define_flag();
        flags.c->key              = 'c';
        flags.c->description      = "categories report";

        flags.n = G_define_flag();
        flags.n->key              = 'n';
        flags.n->description      = "node report";

        flags.e = G_define_flag();
        flags.e->key              = 'e';
        flags.e->description      = "edit header";

	if (G_parser(argc, argv)) exit(-1);

	Full = Area = Line = Isle = Attr = Node = Edit = 0;

        if (!flags.f->answer)
          {
          if (flags.a->answer)
	     {
	     Area = 1;
	     if (parms.entopt->answer)
	        sscanf(parms.entopt->answer,"%d",&aArea);
	     else aArea = 0;
             }
          if (flags.l->answer)
	     {
	     Line = 1;
	     if (parms.entopt->answer)
	        sscanf(parms.entopt->answer,"%d",&aLine);
	     else aLine = 0;
             }
          if (flags.i->answer)
	     {
	     Isle = 1;
	     if (parms.entopt->answer)
	        sscanf(parms.entopt->answer,"%d",&aIsle);
	     else aIsle = 0;
             }
          if (flags.n->answer)
	     {
	     Node = 1;
	     if (parms.entopt->answer)
	        sscanf(parms.entopt->answer,"%d",&aNode);
	     else aNode = 0;
             }
          if (flags.c->answer)
	     {
	     Attr = 1;
	     if (parms.entopt->answer)
	        sscanf(parms.entopt->answer,"%d",&aAttr);
	     else aAttr = 0;
             }
          if (flags.e->answer) Edit = 1;
	  if (!Area && !Line && !Isle && !Node && !Attr && !Edit) Full = 1;
	  }
         else Full = 1;
	    
                    /* -- input file name(s) -- */    
         i = 0;
	 while (parms.mapopt->answers[i])
	    {
            input = parms.mapopt->answers[i++];
            if ((mapset = G_find_vector2 (input, "")) == NULL)
               {                                                  
               sprintf (buf, "Could not find vector file <%s>\n", input);
               G_fatal_error (buf);
               }                       
            fprintf(stderr,"\nLoading <%s> vector information.\n",input);

                     /* Do initial read of input DIGIT file */
            if ((vect_read = Vect_open_old(&Map,input, mapset)) < 0 )
               G_fatal_error("Reading input file.") ;
            if (vect_read < 2)
               G_fatal_error("You must run v.support on this file.") ;

	    if (Edit)
	       {                  /* Read and write header info */
               get_head_info(&(Map.head));
               Vect__write_head_binary(Map, &(Map.head));
	       continue;
	       }

		     /* read in categories */
            G_suppress_warnings(1);
	    got_cats = G_read_vector_cats (input, mapset, &cats);
	    G_suppress_warnings(0);
	    if (got_cats != 0)
	       G_fatal_error("Category support for vector file missing or invalid");
           

            fprintf(stderr,"\nProcessing ..... \n");
            printf("\n\t\tREPORT FOR '%s'\n\n",input);


            if (Full || Area || Isle || Attr)
               {
                     /* Cycle through all areas */
	       for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
		  do_area(&Map, &cats, area_num);
               }

            if (Full || Line)
               {
               for (line_num=1; line_num <= Map.n_lines; line_num++)
		  do_line(&Map, &cats, line_num);
               } 

            if (Full || (Isle && Map.n_isles))
               {
               for (isle_num=0; isle_num < Map.n_isles; isle_num++)
		  { 
		  }
               } 

            if (Full || Node)
               {
               for (node_num=1; node_num <= Map.n_nodes; node_num++)
		  {
		  } 
               }

	    Vect_close (&Map);
            fprintf(stderr,"\nFinished <%s>\n",input);
            }
       exit(0);
}
