/* getAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicating that he
                  is providing am input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/
/*		alex shevlakov, jan'00, libpq update
*/

#include "gis.h"
getAllOpts(argc, argv)
        int argc;
        char **argv;

{

        struct Option *key, *col, *where, *tab, *input, *output, *vtype;
	struct Flag *disolve;
        int retval;

	retval = 0 ;
	
	disolve = G_define_flag();
        disolve->key     = 'd';
        disolve->description = "Dissolve common boundaries (default is no)." ;
	
        key = G_define_option() ;
        key->key        = "key" ;
        key->type       = TYPE_STRING ;
        key->required   = YES  ;
        key->multiple   = NO ;
        key->description= "Column corresponding to cats in vector map [input]" ;

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


        where = G_define_option() ;
        where->key        = "where" ;
        where->type       = TYPE_STRING ;
        where->required   = NO  ;
        where->multiple   = NO ;
        where->description= "Where clause for query (ie. where col='paved'). " ;

 
        input = G_define_option() ;
	input->gisprompt  = "old,dig,vector" ;
        input->key        = "input" ;
        input->type       = TYPE_STRING ;
        input->required   = YES  ;
        input->multiple   = NO ;
        input->description= "Name of existing vector file.";

        output = G_define_option() ;
	output->gisprompt  = "new,dig,vector" ;
        output->key        = "output" ;
        output->type       = TYPE_STRING ;
        output->required   = NO  ;
        output->multiple   = NO ;
        output->description= "Name of new reclass file.";
	
	vtype = G_define_option() ;
        vtype->key        = "type" ;
        vtype->type       = TYPE_STRING ;
        vtype->required   = YES  ;
        vtype->multiple   = NO ;
	vtype->options    =  "area,line,site";
        vtype->description= "Select area, line or site.";
	

        /* Invoke parser */
        if (G_parser(argc, argv)) {
	   system("v.reclass.pg -s help");
            exit(-1);
	  }

        if (! (G_find_vector2(input->answer,"")))  {
             fprintf(stderr,"Vector file %s not found.\n",input->answer);
             exit(-1);
        }

		retval = buildInfxQry(key->answer,col->answer,
		tab->answer,where->answer,
                input->answer, output->answer, 
		vtype->answer, disolve->answer);


	return(retval) ;
}
