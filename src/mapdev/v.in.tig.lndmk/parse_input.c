
#include "gis.h"
#include "globals.h"

parse_input(argc,argv)
int argc; char *argv[];
{
int i;
/* Define the different options */

    opt1 = G_define_option() ;
    opt1->key        = "t1";
    opt1->type       = TYPE_STRING;
    opt1->required   = YES;
    opt1->answer     = NULL;
    opt1->description= "Path/name of Type 1 Tiger File";

    opt2 = G_define_option() ;
    opt2->key        = "t2";
    opt2->type       = TYPE_STRING;
    opt2->required   = NO;
    opt2->answer     = NULL;
    opt2->description= "Path/name of Type 2 Tiger File";
/*
    opt3 = G_define_option() ;
    opt3->key        = "t3";
    opt3->type       = TYPE_STRING;
    opt3->required   = NO;
    opt3->answer     = NULL;
    opt3->description= "Path/name of Type 3 Tiger File";
*/
    opt7 = G_define_option() ;
    opt7->key        = "t7";
    opt7->type       = TYPE_STRING;
    opt7->required   = NO;
    opt7->answer     = NULL;
    opt7->description= "Path/name of Type 7 Tiger File";

    opt8 = G_define_option() ;
    opt8->key        = "t8";
    opt8->type       = TYPE_STRING;
    opt8->required   = NO;
    opt8->answer     = NULL;
    opt8->description= "Path/name of Type 8 Tiger File";
/*
    optA = G_define_option() ;
    optA->key        = "tA";
    optA->type       = TYPE_STRING;
    optA->required   = NO;
    optA->answer     = NULL;
    optA->description= "Path/name of Type A Tiger File";
*/
    optI = G_define_option() ;
    optI->key        = "tI";
    optI->type       = TYPE_STRING;
    optI->required   = NO;
    optI->answer     = NULL;
    optI->description= "Path/name of Type I Tiger File";

    optP = G_define_option() ;
    optP->key        = "tP";
    optP->type       = TYPE_STRING;
    optP->required   = NO;
    optP->answer     = NULL;
    optP->description= "Path/name of Type P Tiger File";

    optin = G_define_option() ;
    optin->key        = "input";
    optin->type       = TYPE_STRING;
    optin->required   = NO;
    optin->answer     = NULL;
    optin->description= "file path/name for input commands" ;

    optv = G_define_option() ;
    optv->key        = "vect";
    optv->type       = TYPE_STRING;
    optv->required   = NO;
    optv->answer     = NULL;
    optv->description= "Name of vector map to create";

    opts = G_define_option() ;
    opts->key        = "site";
    opts->type       = TYPE_STRING;
    opts->required   = NO;
    opts->answer     = NULL;
    opts->description= "Name of site map to create";

  if (proj == PROJECTION_UTM){
    sprintf(t1buf,"%d",G_zone() );
    optz = G_define_option() ;
    optz->key        = "zone";
    optz->type       = TYPE_INTEGER;
    optz->required   = NO;
    optz->answer     = t1buf;
    optz->options    = "1-60";
    optz->description= "UTM zone number; default is location zone";

    optsph = G_define_option() ;
    optsph->key        = "spheroid";
    optsph->type       = TYPE_STRING;
    optsph->required   = NO;
    optsph->answer     = "clark66";
    optsph->description= "Spheroid for LL to UTM conversion; see m.gc.ll";
  }


/* Define the different flags */

    flag1 = G_define_flag() ;
    flag1->key         = 'i' ;
    flag1->description = "Force interactive mode" ;

    flag2 = G_define_flag() ;
    flag2->key         = 's' ;
    flag2->description = "Input commands from stdin" ;


    if (G_parser(argc, argv) < 0)
	exit(-1);

/* initialize tiger structures */
for (i=0; i<=N_TIGERS; i++) {
  tiger[i].fp   = NULL;
  tiger[i].file = NULL;
}
tiger[0].type  = '0';
tiger[1].type  = '1'; tiger[1].file  = opt1->answer;
tiger[2].type  = '2'; tiger[2].file  = opt2->answer;
tiger[3].type  = '3';
tiger[4].type  = '4';
tiger[5].type  = '5';
tiger[6].type  = '6';
tiger[7].type  = '7'; tiger[7].file  = opt7->answer;
tiger[8].type  = '8'; tiger[8].file  = opt8->answer;
tiger[9].type  = 'A';
tiger[10].type = 'I'; tiger[10].file = optI->answer;
tiger[11].type = 'P'; tiger[11].file = optP->answer;
tiger[12].type = 'R';

} /* end of parse_input() */

