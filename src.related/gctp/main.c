/* Philip Thompson (phils@athena.mit.edu), 21/1/91.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */
#include <stdio.h>
#include <sys/file.h>
#include "lib/gctp.h"

#define BOGUS_INT   -999999

/* reference systems suported bye GCTP/II */
/* code number of input coordinate system */
#define LAT_LONG                0   /* GEOGRAPHIC */
#define UTM                     1
#define STATE_PLANE             2
#define ALBERS_CONICAL          3   /* EQUAL-AREA */
#define LAMBERT_CONFORMAL_CONIC 4 
#define MERCATOR                5 
#define POLAR_STEREOGRAPHIC     6 
#define POLYCONIC               7 
#define EQUIDISTANT_CONIC       8 
#define TRANSVERSE_MERCATOR     9 
#define STEREOGRAPHIC           10 
#define LAMBERT_AZIMUTHAL       11  /* EQUAL-AREA */
#define AZIMUTHAL_EQUIDISTANT   12 
#define GNOMONIC                13 
#define ORTHOGRAPHIC            14 
#define GENERAL_VERTICAL        15  /* NEAR-SIDE PERSPECTIVE */
#define SINUSOIDAL              16 
#define EQUIRECTANGULAR         17 
#define MILLER_CYLINDRICAL      18  /* P0227*/
#define VAN_DER_GRINTEN_I       19 
#define OBLIQUE_MERCATOR        20  /* (HOTINE) */
/* code number of units of measure for input coords */
#define RADIANS         0
#define FEET            1
#define METERS          2
#define SECONDS_OF_ARC  3
#define DEGREES_OF_ARC  4
#define PACKED_DMS      5

double GetDouble();
char *calloc();

/* ARGSUSED */
main(argc, argv)
int argc;
char *argv[];
{
    int xflag = 0, found = 0, west_flag = 0;
    double *crdin, *tparin, *crdio, *tpario;
    int *indef, *iodef, iflg = 0;
    char line[80];
	FILE *ipfile = stdout;

    fprintf(stderr, "**********************************************\n");
    fprintf(stderr, "              Welcome to the\n");
    fprintf(stderr, " General Cartographic Transformation Package\n");
	fprintf(stderr, " (MIT modified for State Plane in north-west\n");
    fprintf(stderr, " hemisphere projections. Positive values only)\n");
    fprintf(stderr, "**********************************************\n");
    crdin = (double*)calloc(2, sizeof(double));
    crdio = (double*)calloc(2, sizeof(double));
    tparin = (double*)calloc(13, sizeof(double));
    tpario = (double*)calloc(13, sizeof(double));
    indef = (int*)calloc(3, sizeof(int));
    iodef = (int*)calloc(3, sizeof(int));
    do {
        fprintf(stderr, "\nReference System Conversion codes:\n");
        fprintf(stderr, "0) Lat. Long.    1) UTM    2) State Plane\n");
        fprintf(stderr, "Enter conversion codes (src dst): ");
        (void)gets(line);
    } while (sscanf(line, "%d %d", &indef[0], &iodef[0]) != 2);

    switch(indef[0]) {
    case LAT_LONG:
        west_flag = 1;
        break;
    case UTM:
        while ((indef[1] = GetInteger("Enter source UTM zone code"))
                == BOGUS_INT);
        fprintf(stderr,"\nDefinitions of source UTM projection sytem\n");
        if (indef[1] == 0)
            xflag = 1;
        Get_UTM(tparin, xflag);
        break;
    case STATE_PLANE:
        do {
        found = 0;
        while ((indef[1] = GetInteger(
            "Enter source State Plane zone code (or 99 for a list) "))
                == BOGUS_INT);
        if (indef[1] == 99)
            ZoneListing(stderr);
        else
            found = 1;
        } while (!found);
        fprintf(stderr,"\nDefinitions of source State Plane projection sytem\n");
        Get_StatePlane(tparin);
        break;
    default:
        GctpError("bad \"source\" reference system code");
    }
    switch(iodef[0]) {
    case LAT_LONG:
        break;
    case UTM:
        while ((iodef[1] = GetInteger("\nEnter target UTM zone code"))
                == BOGUS_INT);
        fprintf(stderr,"\nDefinitions of target UTM projection sytem\n");
        if (iodef[1] <= 0)
            xflag = 1;
        Get_UTM(tpario, xflag);
        break;
    case STATE_PLANE:
        do {
        found = 0;
        while ((iodef[1] = GetInteger(
            "\nEnter target State Plane zone code (or 99 for a list) "))
                == BOGUS_INT);
        if (iodef[1] == 99)
            ZoneListing(stderr);
        else
            found = 1;
        } while (!found);
        fprintf(stderr,"\nDefinitions of target State Plane projection sytem\n");
        Get_StatePlane(tpario);
        break;
    default:
        GctpError("bad \"target\" reference system code");
    }
    do {
        fprintf(stderr, "\nUnits of Measure codes:\n");
        fprintf(stderr, "0) radians    1) Feet    2) Meters\n");
        fprintf(stderr, "3) seconds of arc    4) degrees of arc\n");
        fprintf(stderr, "5) packed dms (+-dddmmss.ssss)\n");
        fprintf(stderr, "Enter measure conversion codes (src dst): ");
        (void)gets(line);
    } while (sscanf(line, "%d %d", &indef[2], &iodef[2]) != 2);

    fprintf(stderr, "\nEnter source Coordinates (x y): ");
    while (gets(line)) {
        if (sscanf(line, "%lf %lf", &crdin[0], &crdin[1]) != 2) {
            fprintf(stderr, "\nEnter source Coordinates (x y): ");
            continue;
        }
        if (west_flag)
            crdin[0] = -crdin[0];
        (void)gtrnz0_(crdin, indef, tparin, crdio, iodef, tpario,
                ipfile, &iflg);
        if (iflg == 0)
            printf("\nTarget Coordinates: %lf  %lf\n", crdio[0],
                    crdio[1]);
        else
            fprintf(stderr,"gtrnz0_: error in return (%d)\n", iflg);
        fprintf(stderr, "\nEnter source Coordinates (x y): ");
    }
    exit(0);
}


Get_UTM(par, xflag)
double par[];
int xflag;
{
    par[0] = GetDouble("Semimajor axis of ellipsoid [default]");
    par[1] = GetDouble("Eccentricity squared of ellipsoid [default]");
    if (xflag) {
        par[2] = GetDouble(
                "Longitude of any point within UTM zone");
        par[3] = GetDouble(
                "Latitude of any point within UTM zone");
        par[2] = -par[2];    /* neg. for west hemis. */
    }
}


Get_StatePlane(par)
double par[];
{
    par[0] = GetDouble("Semimajor axis of ellipsoid [default]");
    par[1] = GetDouble("Eccentricity squared of ellipsoid [default]");
}


double GetDouble(prompt)
char *prompt;
{
    double dtmp;
    char line[80];

    fprintf(stderr, "%s: ", prompt);
    (void)gets(line);
    if (sscanf(line, "%lf", &dtmp) == 1)
        return dtmp;
    else
        return 0.0;
}

int GetInteger(prompt)
char *prompt;
{
    int itmp;
    char line[80];

    fprintf(stderr, "%s: ", prompt);
    (void)gets(line);
    if (sscanf(line, "%d", &itmp) == 1)
        return itmp;
    else
        return BOGUS_INT;
}

GctpError(s1)
char *s1;
{
    extern int errno, sys_nerr;
    extern char *sys_errlist[];

    fprintf(stderr,"%c>>%c  ", 7, 7);
    fprintf(stderr, s1);
    if ((errno > 0) && (errno < sys_nerr))
        fprintf(stderr, "  (%s)", sys_errlist[errno]);
    fprintf(stderr, "\n");
    exit(1);
}
/*** end main.c ***/
