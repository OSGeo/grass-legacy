/*  s.db.rim parse command and run
 *
 */
#define MAIN
#include "gis.h"

main (argc, argv)
    int argc;
    char *argv[];
{
    struct Option *opt1;
    char cmd[1000];
    opt1 = G_define_option() ;
    opt1->key        = "database" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = NO ;
    opt1->multiple   = NO ;
    opt1->description= "Name of data base to open" ;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
        exit(1);
    sprintf(cmd,"%s/etc/rim/s.db.rim.cmd %s",G_gisbase(),opt1->answer);
    G_system(cmd);
}
