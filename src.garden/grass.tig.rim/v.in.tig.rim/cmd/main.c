#include "gis.h"
main(argc,argv) char *argv[];
{
    char command[1024];
    int zonenum;

    struct Option *dbname, *in1, *in2, *zone, *spheroid;

    G_gisinit(argv[0]);

    dbname = G_define_option();
    dbname->key="dbname";
    dbname->type=TYPE_STRING;
    dbname->required=YES;
    dbname->description="Vector/rim data name (max 7 chars long)";
    dbname->multiple=NO;

    in1 = G_define_option();
    in1->key="in1file";
    in1->type=TYPE_STRING;
    in1->required=YES;
    in1->description="Tiger type1 input filename";
    in1->multiple=NO;

    in2 = G_define_option();
    in2->key="in2file";
    in2->type=TYPE_STRING;
    in2->required=YES;
    in2->description="Tiger type2 input filename";
    in2->multiple=NO;

    zone = G_define_option();
    zone->key="zone";
    zone->type=TYPE_INTEGER;
    zone->required=YES;
    zone->description="UTM zone for this data";
    zone->multiple=NO;
    zone->options="-60-60";

/*
    spheroid = G_define_option();
    spheroid->key="spheroid";
    spheroid->type=TYPE_STRING;
    spheroid->required=NO;
    spheroid->description="Name of spheroid to use";
    spheroid->answer="clark66";
    spheroid->multiple=NO;

*/

    if (G_parser(argc,argv))
	exit(1);

    if (strlen(dbname->answer) > 7)
    {
        fprintf(stderr,"\nError: Name for rim database/vector file must be <= 7 chars\n");
        exit(2);
    }

    sscanf(zone->answer,"%d",&zonenum);
    if (zonenum == 0)
    {
        fprintf(stderr,"\nError: Must have a non-zero zone number.\n");
        fprintf(stderr," If you aren't sure of the zone number for the \n");
        fprintf(stderr," type 1 input data, run tiger.region to obtain it.\n");
        exit(3);
    }

    sprintf (command, "%s/etc/v.in.tg.rm.sh %s %s %s %d \n", G_gisbase(),  dbname->answer, in1->answer, in2->answer, zonenum);
    return system(command);
}

