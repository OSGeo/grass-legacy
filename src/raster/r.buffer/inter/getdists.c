/*
 *            Get distances from user
 */

#include <stdio.h>

#define MAXDIST 60

static char *valid_units[] = {"meters", "kilometers", "feet", "miles", NULL};

double *
getdists(ndist, units) 
    int *ndist ;
    char *units;
{
    double dist[MAXDIST];
    static double distances[MAXDIST];
    int i ;
    char junk[256];

    strcpy (units, "meters");
    for(i=0; i<MAXDIST; i++)
	dist[i] = 0 ;

    V_clear() ;
    V_line (  0, "              ENTER MAXIMUM DISTANCE FOR EACH ZONE DESIRED") ;
    V_line (  1, "                   All values in:");
    V_line (  2, "ZONE  1:               ZONE 21:               ZONE 41:") ;
    V_line (  3, "ZONE  2:               ZONE 22:               ZONE 42:") ;
    V_line (  4, "ZONE  3:               ZONE 23:               ZONE 43:") ;
    V_line (  5, "ZONE  4:               ZONE 24:               ZONE 44:") ;
    V_line (  6, "ZONE  5:               ZONE 25:               ZONE 45:") ;
    V_line (  7, "ZONE  6:               ZONE 26:               ZONE 46:") ;
    V_line (  8, "ZONE  7:               ZONE 27:               ZONE 47:") ;
    V_line (  9, "ZONE  8:               ZONE 28:               ZONE 48:") ;
    V_line ( 10, "ZONE  9:               ZONE 29:               ZONE 49:") ;
    V_line ( 11, "ZONE 10:               ZONE 30:               ZONE 50:") ;
    V_line ( 12, "ZONE 11:               ZONE 31:               ZONE 51:") ;
    V_line ( 13, "ZONE 12:               ZONE 32:               ZONE 52:") ;
    V_line ( 14, "ZONE 13:               ZONE 33:               ZONE 53:") ;
    V_line ( 15, "ZONE 14:               ZONE 34:               ZONE 54:") ;
    V_line ( 16, "ZONE 15:               ZONE 35:               ZONE 55:") ;
    V_line ( 17, "ZONE 16:               ZONE 36:               ZONE 56:") ;
    V_line ( 18, "ZONE 17:               ZONE 37:               ZONE 57:") ;
    V_line ( 19, "ZONE 18:               ZONE 38:               ZONE 58:") ;
    V_line ( 20, "ZONE 19:               ZONE 39:               ZONE 59:") ;
    V_line ( 21, "ZONE 20:               ZONE 40:               ZONE 60:") ;

    V_ques(units, 's', 1, 35, 12);
    V_ques(&dist[ 0], 'd',  2, 10, 10) ;
    V_ques(&dist[ 1], 'd',  3, 10, 10) ;
    V_ques(&dist[ 2], 'd',  4, 10, 10) ;
    V_ques(&dist[ 3], 'd',  5, 10, 10) ;
    V_ques(&dist[ 4], 'd',  6, 10, 10) ;
    V_ques(&dist[ 5], 'd',  7, 10, 10) ;
    V_ques(&dist[ 6], 'd',  8, 10, 10) ;
    V_ques(&dist[ 7], 'd',  9, 10, 10) ;
    V_ques(&dist[ 8], 'd', 10, 10, 10) ;
    V_ques(&dist[ 9], 'd', 11, 10, 10) ;
    V_ques(&dist[10], 'd', 12, 10, 10) ;
    V_ques(&dist[11], 'd', 13, 10, 10) ;
    V_ques(&dist[12], 'd', 14, 10, 10) ;
    V_ques(&dist[13], 'd', 15, 10, 10) ;
    V_ques(&dist[14], 'd', 16, 10, 10) ;
    V_ques(&dist[15], 'd', 17, 10, 10) ;
    V_ques(&dist[16], 'd', 18, 10, 10) ;
    V_ques(&dist[17], 'd', 19, 10, 10) ;
    V_ques(&dist[18], 'd', 20, 10, 10) ;
    V_ques(&dist[19], 'd', 21, 10, 10) ;
    V_ques(&dist[20], 'd',  2, 33, 10) ;
    V_ques(&dist[21], 'd',  3, 33, 10) ;
    V_ques(&dist[22], 'd',  4, 33, 10) ;
    V_ques(&dist[23], 'd',  5, 33, 10) ;
    V_ques(&dist[24], 'd',  6, 33, 10) ;
    V_ques(&dist[25], 'd',  7, 33, 10) ;
    V_ques(&dist[26], 'd',  8, 33, 10) ;
    V_ques(&dist[27], 'd',  9, 33, 10) ;
    V_ques(&dist[28], 'd', 10, 33, 10) ;
    V_ques(&dist[29], 'd', 11, 33, 10) ;
    V_ques(&dist[30], 'd', 12, 33, 10) ;
    V_ques(&dist[31], 'd', 13, 33, 10) ;
    V_ques(&dist[32], 'd', 14, 33, 10) ;
    V_ques(&dist[33], 'd', 15, 33, 10) ;
    V_ques(&dist[34], 'd', 16, 33, 10) ;
    V_ques(&dist[35], 'd', 17, 33, 10) ;
    V_ques(&dist[36], 'd', 18, 33, 10) ;
    V_ques(&dist[37], 'd', 19, 33, 10) ;
    V_ques(&dist[38], 'd', 20, 33, 10) ;
    V_ques(&dist[39], 'd', 21, 33, 10) ;
    V_ques(&dist[40], 'd',  2, 55, 10) ;
    V_ques(&dist[41], 'd',  3, 55, 10) ;
    V_ques(&dist[42], 'd',  4, 55, 10) ;
    V_ques(&dist[43], 'd',  5, 55, 10) ;
    V_ques(&dist[44], 'd',  6, 55, 10) ;
    V_ques(&dist[45], 'd',  7, 55, 10) ;
    V_ques(&dist[46], 'd',  8, 55, 10) ;
    V_ques(&dist[47], 'd',  9, 55, 10) ;
    V_ques(&dist[48], 'd', 10, 55, 10) ;
    V_ques(&dist[49], 'd', 11, 55, 10) ;
    V_ques(&dist[50], 'd', 12, 55, 10) ;
    V_ques(&dist[51], 'd', 13, 55, 10) ;
    V_ques(&dist[52], 'd', 14, 55, 10) ;
    V_ques(&dist[53], 'd', 15, 55, 10) ;
    V_ques(&dist[54], 'd', 16, 55, 10) ;
    V_ques(&dist[55], 'd', 17, 55, 10) ;
    V_ques(&dist[56], 'd', 18, 55, 10) ;
    V_ques(&dist[57], 'd', 19, 55, 10) ;
    V_ques(&dist[58], 'd', 20, 55, 10) ;
    V_ques(&dist[59], 'd', 21, 55, 10) ;

    while(1)
    {
	V_intrpt_ok();
	if(!V_call()) exit(0) ;
	for (i = 0; valid_units[i]; i++)
	    if (strcmp(units, valid_units[i]) == 0)
		break;
	if (valid_units[i])
	    break;
	printf ("\n** units=%s - invalid **\nChoices are:", units);
	for (i = 0; valid_units[i]; i++)
	    printf ("%s%s", i?",":" ", valid_units[i]);
	printf ("\nHit RETURN -->");
	if (!gets(junk)) exit(0);
    }

/* Extract the distances */
    *ndist = 0;
    for (i = 0; i < MAXDIST; i++)
	if (dist[i] > 0.0)
	    distances[(*ndist)++] = dist[i];
    return distances;
}
