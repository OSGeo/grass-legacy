/* getAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicating that he
                  is providing am input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/
//------------------------A.Sh 13.09.2000
#include "gis.h"
getAllOpts(argc, argv)
        int argc;
        char **argv;

{

        struct Option *key, *col, *lab, *where, *tab, *input, *output;
        int retval;


	retval = 0 ;

        key = G_define_option() ;
        key->key        = "key" ;
        key->type       = TYPE_STRING ;
        key->required   = YES  ;
        key->multiple   = NO ;
        key->description= "Column corresponding to cats in raster map [input]" ;

        tab = G_define_option() ;
        tab->key        = "tab" ;
        tab->type       = TYPE_STRING ;
        tab->required   = YES  ;
        tab->multiple   = NO ;
        tab->description= "Table containing [col]." ;


        col = G_define_option() ;
        col->key        = "col" ;
        col->type       = TYPE_STRING ;
        col->required   = YES  ;
        col->multiple   = NO ;
        col->description= "Column to base reclass on." ;
	
	lab = G_define_option() ;
        lab->key        = "lab" ;
        lab->type       = TYPE_STRING ;
        lab->required   = NO  ;
        lab->multiple   = NO ;
        lab->description= "Column to use as labels (optional)." ;

        where = G_define_option() ;
        where->key        = "where" ;
        where->type       = TYPE_STRING ;
        where->required   = NO  ;
        where->multiple   = NO ;
        where->description= "Where clause for query (ie. where col='paved'). " ;

 
        input = G_define_option() ;
	input->gisprompt  = "old,cell,raster" ;
        input->key        = "input" ;
        input->type       = TYPE_STRING ;
        input->required   = YES  ;
        input->multiple   = NO ;
        input->description= "Name of existing raster file.";

        output = G_define_option() ;
	output->gisprompt  = "new,cell,raster" ;
        output->key        = "output" ;
        output->type       = TYPE_STRING ;
        output->required   = NO  ;
        output->multiple   = NO ;
        output->description= "Name of for new reclass file.";

        /* Invoke parser */
        if (G_parser(argc, argv)) {
	   system("d.rast.pg -s help");
            exit(-1);
	  }

        if (! (G_find_cell(input->answer,"")))  {
             fprintf(stderr,"Raster file %s not found.\n",input->answer);
             exit(-1);
        }


		retval = buildInfxQry(key->answer,col->answer,lab->answer,
		tab->answer,where->answer,
                input->answer, output->answer);


	return(retval) ;
}
