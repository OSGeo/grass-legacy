#include <stdio.h>

digit_header(fp, name, rank)
	FILE *fp;
	char *name ;
{
	fprintf (fp, "ORGANIZATION: CIA\n") ;
	fprintf (fp, "DIGIT DATE:   10 January 1987\n") ;
	fprintf (fp, "DIGIT NAME:   CIA\n") ;
	fprintf (fp, "MAP NAME:     %s %02d\n", name, rank) ;
	fprintf (fp, "MAP DATE:     unknown\n") ;
	fprintf (fp, "MAP SCALE:    unknown\n") ;
	fprintf (fp, "OTHER INFO:                              \n") ;
	fprintf (fp, "ZONE:         %d\n", 0) ;
	fprintf (fp, "WEST EDGE:    %.2lf\n", (double)-180) ;
	fprintf (fp, "EAST EDGE:    %.2lf\n", (double)180) ;
	fprintf (fp, "SOUTH EDGE:   %.2lf\n", (double)-90) ;
	fprintf (fp, "NORTH EDGE:   %.2lf\n", (double)90) ;
	fprintf (fp, "MAP THERSH:   %lf\n", 1./3600.) ;
	fprintf (fp, "VERTI:             \n") ;
}
