#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "site.h"
#include <stdio.h>
#include <libpq-fe.h>
#include "dbsite.h"
#include "glocale.h"
/*taken from 7.1.3 pg_type.h*/
#define BOOLOID                 16
#define BYTEAOID                17
#define CHAROID                 18
#define NAMEOID                 19
#define INT8OID                 20
#define INT2OID                 21
#define INT2VECTOROID   22
#define INT4OID                 23
#define REGPROCOID              24
#define TEXTOID                 25
#define OIDOID                  26
#define TIDOID          27
#define XIDOID 28
#define CIDOID 29
#define OIDVECTOROID    30
#define POINTOID                600
#define LSEGOID                 601
#define PATHOID                 602
#define BOXOID                  603
#define POLYGONOID              604
#define LINEOID                 628
#define FLOAT4OID 700
#define FLOAT8OID 701
#define ABSTIMEOID              702
#define RELTIMEOID              703
#define TINTERVALOID    704
#define UNKNOWNOID              705
#define CIRCLEOID               718
#define CASHOID 790
#define INETOID 869
#define CIDROID 650
#define ACLITEMSIZE 8
#define BPCHAROID               1042
#define VARCHAROID              1043
#define DATEOID                 1082
#define TIMEOID                 1083
#define TIMESTAMPOID    1184
#define INTERVALOID             1186
#define TIMETZOID               1266
#define ZPBITOID         1560
#define VARBITOID         1562
#define NUMERICOID              1700


int runPg(SQL_stmt, map, plotargs)
     char *SQL_stmt, *map, *plotargs[];
{
    FILE *fpout = NULL;
    int i, nflds, err;
    int retval;
    int color;
    int size;
    char *icon;
    char buf1[QRY_LENGTH];
    char buf2[QRY_LENGTH];
    char *buf3;
    float x;
    float y;
    PGconn *pg_conn;
    PGresult *res;
    char *pghost;
    Site *site;
    int ftype = 0;

    memset(buf1, '\0', sizeof(buf1));
    memset(buf1, '\0', sizeof(buf1));

    i = 1;
    retval = 0;
    buf3 = G_malloc(QRY_LENGTH * sizeof(char));
    err = 1;

    color = D_translate_color(plotargs[0]);
    icon = plotargs[1];
    size = atoi(plotargs[2]);

    R_open_driver();
    D_setup(0);
    R_standard_color(color);

    if (map) {

	if ((fpout = G_fopen_sites_new(map)) == NULL)
	{
	    sprintf(buf1, _("Cannot open %s"), map);
	    G_fatal_error(buf1);
	}

    }

#ifdef VERBOSE
    printf("\n\nExecuting\n%s;\n\n", SQL_stmt);
#endif

    pghost = G__getenv("PG_HOST");

    pg_conn = PQsetdb(pghost, NULL, NULL, NULL, G_getenv("PG_DBASE"));
    if (PQstatus(pg_conn) == CONNECTION_BAD) {
	printf(_("Error: connect Postgres:%s\n"), PQerrorMessage(pg_conn));
	PQfinish(pg_conn);
	R_close_driver();
	exit(-1);
    }

    res = PQexec(pg_conn, SQL_stmt);
    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	printf(_("Error: select Postgres:%s\n"),
	       PQerrorMessage(pg_conn));
	PQclear(res);
	PQfinish(pg_conn);
	R_close_driver();
	exit(-1);
    }

    nflds = PQnfields(res);

    if (nflds == 3) {
	ftype = PQftype(res, 2);
	switch (ftype) {

	case BPCHAROID:
	case VARCHAROID:
	case CHAROID:
	case TEXTOID:
	case INT8OID:
	case INT2OID:
	case INT4OID:
	case FLOAT4OID:
	case FLOAT8OID:
	    break;
	default:
	    printf("Can not use the field type id %d for category\n", ftype);
	}
    }

    for (i = 0; i < PQntuples(res); i++) {
	strcpy(buf1, PQgetvalue(res, i, 0));
	strcpy(buf2, PQgetvalue(res, i, 1));
	x = atof(buf1);
	y = atof(buf2);
	/* use ascii buffer to Postgres until MSB / LSB issues can be determined */
	/* cfa 11/98    */

	retval = plotsite(x, y, icon, size);
	if (retval != 0) {
	    printf(_("Display error(exiting..)\n"));
	    exit(-1);
	}
	if (map) {
	    if (ftype) {

		strcpy(buf3, PQgetvalue(res, i, 2));

		switch (ftype) {

/*char, bpchar,varchar,text*/
		case CHAROID:
		case BPCHAROID:
		case VARCHAROID:
		case TEXTOID:
		    site = G_site_new_struct(-1, 2, 1, 0);
		    if (strlen(buf3) == 0)
			strcpy(buf3, "no data");
		    G_strncpy(site->str_att[0], buf3, strlen(buf3));
		    break;
/*int4, int2, int8*/
		case INT8OID:
		case INT2OID:
		case INT4OID:
		    site = G_site_new_struct(CELL_TYPE, 2, 0, 0);
		    sscanf(buf3, "%d", &site->ccat);
		    break;
/*float4, float8*/
		case FLOAT4OID:
		case FLOAT8OID:
		    site = G_site_new_struct(FCELL_TYPE, 2, 0, 0);
		    sscanf(buf3, "%f", &site->fcat);
		    break;

		default:
		    site = G_site_new_struct(-1, 2, 0, 0);
		}
	    }
	    else
		site = G_site_new_struct(-1, 2, 0, 0);

	    site->east = x;
	    site->north = y;
	    G_site_put(fpout, site);
	    G_site_free_struct(site);
	}
    }

    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */

    PQfinish(pg_conn);
    /* close connection to database */

    if (map)
	fclose(fpout);

    R_close_driver();

    return (0);
}
