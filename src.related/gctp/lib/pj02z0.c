/* pj02z0.f -- translated by f2c (version of 11 May 1990  14:38:39). 
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include "gctp.h"

#define MAXLINE     82
#define TMERCAT     1
#define LAMBERT     2
#define POLYCONIC   3
#define OBLIQUE     4

typedef struct nad_rec {
    char name[33];
    int proj;
    char type[6];
    int zone;
    double fields[9];
} NadRec;

/* these headers need to follow the above typedef (they initialize it)*/
#include "nad27.h"
#include "nad83.h"


/******************************************************************/
/* NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL  */
/* GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986  */
/******************************************************************/
/* STATE PLANE  */
/******************************************************************/

/* assumes geog input is in latitude & longitude in radians
 * proj output is in easting & northing in meters */
int pj02z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double atof();
    NadRec *recs;
    double table[9], buffl[13];
    int izone, id = 0, ind;
    static int switch_ = 0, itype;

    /* Parameter adjustments */
    if (data)
        --data;
    if (geog)
        --geog;
    if (proj)
        --proj;
    /* Function Body */
    switch (n__) {
    case 1:
        /* INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  */
        *iflg = 0;
        if (switch_ != 0 && switch_ == *zone)
            return 0;
        if (*zone <= 0) {
            *iflg = 201;
            return 0;
        }
        izone = *zone;
        recs = Nad27;
        for (ind = 1; ind <= 135; ++ind) {
            if (izone == recs[ind - 1].zone) {
                table[0] = recs[ind - 1].fields[0];
                table[1] = recs[ind - 1].fields[1];
                table[2] = recs[ind - 1].fields[2];
                table[3] = recs[ind - 1].fields[3];
                table[4] = recs[ind - 1].fields[4];
                table[5] = recs[ind - 1].fields[5];
                table[6] = recs[ind - 1].fields[6];
                table[7] = recs[ind - 1].fields[7];
                table[8] = recs[ind - 1].fields[8];
                id = recs[ind - 1].proj;
                break;
            }
        }
        if (ind >= 136) {
            fprintf(ipfile, 
                    "pj02z0_0_: didn't find state zone %d\n", izone);
            *iflg = 201;
            return 0;
        }
        if (id <= 0) {
            *iflg = 201;
            return 0;
        }
        itype = id;
        if (data[1] != 0.0)
            table[0] = data[1];
        if (data[2] != 0.0)
            table[1] = data[2];
        buffl[0] = table[0];
        buffl[1] = table[1];

        switch (itype) {
        case 1:
            /* TRANSVERSE MERCATOR PROJECTION */
            buffl[2] = table[3];
            buffl[4] = table[2];
            buffl[5] = table[6];
            buffl[6] = table[7];
            buffl[7] = table[8];
            is09z0_(zone, buffl, ipfile, iflg);
            break;
        case 2:
            /* LAMBERT CONFORMAL PROJECTION */
            buffl[2] = table[5];
            buffl[3] = table[4];
            buffl[4] = table[2];
            buffl[5] = table[6];
            buffl[6] = table[7];
            buffl[7] = table[8];
            is04z0_(zone, buffl, ipfile, iflg);
            break;
        case 3:
            /* POLYCONIC PROJECTION */
            buffl[4] = table[2];
            buffl[5] = table[3];
            buffl[6] = table[4];
            buffl[7] = table[5];
            is07z0_(zone, buffl, ipfile, iflg);
            break;
        case 4:
            /* OBLIQUE MERCATOR PROJECTION */
            buffl[2] = table[3];
            buffl[3] = table[5];
            buffl[4] = table[2];
            buffl[5] = table[6];
            buffl[6] = table[7];
            buffl[7] = table[8];
            buffl[12] = (float) 1.;
            is20z0_(zone, buffl, ipfile, iflg);
        }
        if (*iflg == 0)
            switch_ = *zone;
        break;
    case 2:
        /* .  FORWARD TRANSFORMATION  . */
        *iflg = 0;
        if (switch_ != 0) {
            switch (itype) {
            case 1:
                /* TRANSVERSE MERCATOR PROJECTION */
                pf09z0_(&geog[1], &proj[1], iflg);
                break;
            case 2:
                /* LAMBERT CONFORMAL PROJECTION */
                pf04z0_(&geog[1], &proj[1], iflg);
                break;
            case 3:
                /* POLYCONIC PROJECTION */
                pf07z0_(&geog[1], &proj[1], iflg);
                break;
            case 4:
                /* OBLIQUE MERCATOR PROJECTION */
                pf20z0_(&geog[1], &proj[1], iflg);
                break;
            }
            return 0;
        }
        *iflg = 200;
        fprintf(ipfile, "pj02z0_0_: Unitialized transformation.\n");
        break;
    case 3:
        /* .  INVERSE TRANSFORMATION  . */
        *iflg = 0;
        if (switch_ != 0) {
            switch (itype) {
            case 1:
                /* TRANSVERSE MERCATOR PROJECTION */
                pi09z0_(&proj[1], &geog[1], iflg);
                break;
            case 2:
                /* LAMBERT CONFORMAL PROJECTION */
                pi04z0_(&proj[1], &geog[1], iflg);
                break;
            case 3:
                /* POLYCONIC PROJECTION */
                pi07z0_(&proj[1], &geog[1], iflg);
                break;
            case 4:
                /* OBLIQUE MERCATOR PROJECTION */
                pi20z0_(&proj[1], &geog[1], iflg);
                break;
            }
            return 0;
        }
        *iflg = 200;
        fprintf(ipfile, "pj02z0_0_: Unitialized transformation.\n");
        break;
    }
    return 0;
} /* pj02z0_ */

int pj02z0_()
{
    return pj02z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is02z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj02z0_0_(1, zone, data, ipfile, iflg,(double*)0,(double*)0);
}

int pf02z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj02z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi02z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj02z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

ZoneListing(fp)
FILE *fp;
{
    int i, num_states;
    NadRec *rec;

    num_states = sizeof(Nad27) / sizeof(NadRec);
    fprintf(fp, "%-32s   %4s   %5s   %s\n", "ZONE NAME", "CODE",
            "DATUM", "PROJECTION");
    for (i = 0, rec = Nad27; i < num_states; i++, rec++) {
        fprintf(fp, "%32s:  %4d   %5s   %s\n", rec->name, rec->zone,
                rec->type, rec->proj == LAMBERT ? "LAMBERT" :
                (rec->proj == OBLIQUE ? "OBLIQUE" : (rec->proj == 
                     POLYCONIC ? "POLYCONIC" : "TRANSVERSE MERCATOR")));
    }
}

/*** end pj02z0.c ***/
