/* getAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicating that he
                  is providing am input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/

#include "gis.h"
getAllOpts(argc, argv)
        int argc;
        char **argv;

{

        struct Option *key, *col, *where, *tab, *input, *output, *join  ;
        int colr, stat;
        char *mapset, *buildSQL(), *tmpfile_out;

	stat = 0 ;

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

/* Note where option supercedes col above but provides more user flexability  
        where = G_define_option() ;
        where->key        = "where" ;
        where->type       = TYPE_STRING ;
        where->required   = YES  ;
        where->multiple   = NO ;
        where->description= "Where clause for query (ie. where col='paved'). " ;

	Would need to add an additional option [ORDERBY] to work with this module!
*/
 
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

        join = G_define_option() ;
        join->key        = "join" ;
        join->type       = TYPE_STRING ;
        join->required   = NO  ;
        join->multiple   = NO ;
        join->key_desc   = "tab,tabkey,pkey" ;
        join->description= "JOIN rules (eg. table,key,primekey). ";


        /* Invoke parser */
        if (G_parser(argc, argv)) {
	   system("d.rast.inf -s help");
            exit(-1);
	  }

        if (! (G_find_cell(input->answer,"")))  {
             fprintf(stderr,"Raster file %s not found.\n",input->answer);
             exit(-1);
        }

/*************** INFX driver code begins ***************/

/*
        buildInfxQry(key->answer,col->answer,tab->answer,
                input->answer, output->answer, join->answers, 1);
*/

        tmpfile_out=buildSQL(key->answer,col->answer,tab->answer,
                input->answer, output->answer, join->answers);
	if (*tmpfile_out > 0)
        	stat = runSQL( tmpfile_out, input->answer, output->answer, key->answer, col->answer,1);
	  else	{
		fprintf(stderr, "SQL error.\n"); exit(-1);
		}


	return(stat) ;
}
