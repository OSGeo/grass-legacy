#include "gis.h"

main(argc,argv)
int argc;
char *argv[];
{
	char weapons_file[200], ammunition_file[200];
	char buff[128] ;
	struct Flag *ammo, *weap ;

	sprintf(weapons_file, "%s/etc/weapon_data/weapons",	G_gisbase());
	sprintf(ammunition_file, "%s/etc/weapon_data/ammunition", G_gisbase());

	ammo             = G_define_flag() ;
	ammo->key        = 'a' ;
	ammo->description= "Print list of ammunition" ;

	weap             = G_define_flag() ;
	weap->key        = 'w' ;
	weap->description= "Print list of weapons" ;

	/* gis database initialization  */
	G_gisinit (argv[0]);

	if (G_parser(argc, argv))
		exit(-1);

	if(weap->answer == 1)
		show_weapons(weapons_file) ;
	if(ammo->answer == 1)
		show_ammo(ammunition_file) ;
	if(!ammo->answer && !weap->answer)
		G_usage() ;
}

show_weapons(weapons_file)
	char *weapons_file ;
{
	FILE *fpr, *fopen() ;
	double high_angle, low_angle ;
	char buf[128] ;

	fpr = fopen(weapons_file, "r");
	if(fpr == NULL)
	{
		G_fatal_error("can't open weapons file");
	}

	printf("\n%10s %12s %12s\n", "weapon", "high angle", "low angle") ;
	while (fscanf(fpr, "%10s%10lf%10lf", 
		    buf, &high_angle, &low_angle) != EOF)
		printf("%10s %12.1lf %12.1lf\n", buf, high_angle, low_angle) ;
	
	fclose(fpr);
}

show_ammo(ammunition_file)
	char *ammunition_file ;
{
	FILE *fpr, *fopen() ;
	double DIAMETER, MASS, vel_initial ;
	char buf[128] ;

	fpr = fopen(ammunition_file, "r");
	if(fpr == NULL)
	{
		G_fatal_error("can't open ammunition file");
	}

	printf("\n%10s %12s %12s %12s\n", "ammo", "diameter", "mass", "velocity") ;
	while(fscanf(fpr, "%10s%10lf%10lf%10lf",
		    buf, &DIAMETER, &MASS, &vel_initial) != EOF)
		printf("%10s %12.1lf %12.1lf %12.1lf\n", buf, DIAMETER, MASS, vel_initial) ;
	
	fclose(fpr) ;
}
