/* updated 10/99 Morten Hulden for GRASS 5*/
#ident "  @(#)geo_func.c    1.0  3/18/91  "
/*
   **  created by R.L.Glenn
   **  USDA Soil Conservation Service, CGIS Division
   *
   *   set of re-usables for geo
 */
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <ctype.h>
#include "gis.h"
#include "geo.h"
#include "local_proto.h"

int get_enz(void)
{
	for (;;) {
		/*
		   G_clear_screen();
		 */
		fprintf(stderr, "                   Coordinate Conversions\n\n");
		fprintf(stderr, "                 Coord->Lat/Long Conversion\n\n");
		fprintf(stderr, "\n\n");
		if (!get_easting()) {
			if (!ier)
				return (0);
			else
				continue;
		}
		if (!get_northing()) {
			if (!ier)
				return (0);
			else
				continue;
		}
		break;
	}
	return (1);
}

int get_easting(void)
{
	answer[0] = '\0';
	fprintf(stderr, "\n    Enter Easting  : ");
	G_gets(answer);
	if (strlen(answer) == 0) {
		ier = 0;
		return (0);
	}
	get_num(answer, 0);
	if (ier)
		exit(0);
	return (1);
}

int get_northing(void)
{
	answer[0] = '\0';
	fprintf(stderr, "\n    Enter Northing : ");
	G_gets(answer);
	if (strlen(answer) == 0) {
		ier = 0;
		return (0);
	}
	get_num(answer, 1);
	if (ier)
		return (0);
	return (1);
}

int get_float(int in, double *used, int proj_index, int index)
{
	double num;
	char io[16];

	if (in == 1)
		sprintf(io, "INPUT ");
	else if (in == 2)
		sprintf(io, "OUTPUT ");
	else
		io[0] = '\0';


	if (TABLE[proj_index][index].def_exists == 1) {
		if ((index > INTLOW) && (index < INTHIGH))
			fprintf(stderr, "\n    Enter %s%s (%d) : ", io, DESC[index], (int) TABLE[proj_index][index].deflt);
		else
			fprintf(stderr, "\n    Enter %s%s (%f) : ", io, DESC[index], TABLE[proj_index][index].deflt);
		G_gets(answer);
		if (strlen(answer) == 0) {
			*used = (TABLE[proj_index][index].deflt);
			return (1);
		}
	} else {
		fprintf(stderr, "\n    Enter %s%s : ", io, DESC[index]);
		G_gets(answer);
		if (strlen(answer) == 0) {
			ier = 0;
			return (0);
		}
	}
	for (ptr = answer; *ptr; ptr++) {
		if (*ptr == '\n') {
			*ptr = 0;
			break;
		}
		if (!isdigit(*ptr))
			if (*ptr != '\056' && *ptr != '\055') {
				fprintf(stderr, "  *** INVALID entry *** \n");
				sleep(2);
				ier = 1;
				return 0;

			}
	}
	sscanf(answer, "%lf", &num);
	*used = num;
	return (1);
}

int get_ll(void)
{
	for (;;) {
		fprintf(stderr, "                   Coordinate Conversions\n\n");
		fprintf(stderr, "                 Lat/Long->Coord Conversion\n\n");
		fprintf(stderr, "\n\n");
		if (!get_lat()) {
			if (!ier)
				return (0);
			else
				continue;
		}
		if (!get_lon()) {
			if (!ier)
				return (0);
			else
				continue;
		}
		break;
	}
	return (1);
}

int get_lat(void)
{
	answer[0] = '\0';
	fprintf(stderr, "\n    Enter Latitude [%s]: ", G_lat_format_string());
	G_gets(answer);
	if (strlen(answer) == 0) {
		ier = 0;
		return (0);
	}
	LAT = 0.0;
	if (!get_deg(answer, &LAT, 1)) {
		ier = 1;
		return (0);
	}
	return (1);
}

int get_lon(void)
{
	answer[0] = '\0';
	fprintf(stderr, "\n    Enter Longitude [%s]: ", G_lon_format_string());
	G_gets(answer);
	if (strlen(answer) == 0) {
		ier = 0;
		return (0);
	}
	LON = 0.0;
	if (!get_deg(answer, &LON, 0)) {
		ier = 1;
		return (0);
	}
	return (1);
}


/* Get the Prime Meridian value and std parallel value  */
int get_LL_stuff(int in, double *used, int lat, int proj_index, int index)
{
	char lat_strng[48], lon_strng[48], answer[48];
	double deg;
	char io[16];

	if (in == 1)
		sprintf(io, "INPUT ");
	else if (in == 2)
		sprintf(io, "OUTPUT ");
	else
		io[0] = '\0';

	answer[0] = '\0';
	if (TABLE[proj_index][index].def_exists == 1) {
		if (lat == 1) {
			G_format_northing(TABLE[proj_index][index].deflt, lat_strng, PROJECTION_LL);
			fprintf(stderr, "\n  Enter %s%s (%s) : ", io, DESC[index], lat_strng);
		} else {
			G_format_easting(TABLE[proj_index][index].deflt, lon_strng, PROJECTION_LL);
			fprintf(stderr, "\n  Enter %s%s (%s) : ", io, DESC[index], lon_strng);
		}
		G_gets(answer);
		if (strlen(answer) == 0) {
			*used = TABLE[proj_index][index].deflt;
			return (1);
		}
	} else {
		fprintf(stderr, "\n  Enter %s%s : ", io, DESC[index]);
		G_gets(answer);
		if (strlen(answer) == 0) {
			ier = 1;
			return (0);
		}
	}
	if (lat == 1) {
		if (!get_deg(answer, &deg, 1)) {
			ier = 1;
			return (0);
		}
	} else {
		if (!get_deg(answer, &deg, 0)) {
			ier = 1;
			return (0);
		}
	}
	*used = deg;
	return (1);
}

int get_num(char *strng, int swt)
{
	double num;

	for (ptr = strng; *ptr; ptr++) {
		if (*ptr == '\n') {
			*ptr = 0;
			break;
		}
		if (!isdigit(*ptr))
			if (*ptr != '\056' && *ptr != '\055') {
				fprintf(stderr, "  *** INVALID entry *** \n");
				sleep(2);
				ier = 1;
				return 0;
			}
	}
	sscanf(strng, "%lf", &num);
	if (swt == 0)
		EAS = num;
	if (swt == 1)
		NOR = num;
	if (swt == 6)
		radius_in = num;
	if (swt == 7)
		radius_out = num;
	if (swt == 10)
		unit_fact_in = num;
	if (swt == 11)
		unit_fact_out = num;
	ier = 0;
	return 0;
}


int DMS(int in)
{
	/*  convert lat/lon to dd mm ss.ss */
	float Q;
	double ltt, lng;

	if (in == 1) {
		ltt = LAT;
		lng = LON;
	} else {		/* in == 2 */
		ltt = LAT_res;
		lng = LON_res;
	}
	if (lng > 0)
		IDEG = lng;
	else
		IDEG = lng * -1.0;
	Q = IDEG;
	if (lng > 0)
		Q = (lng - Q) * 60.;
	else
		Q = ((lng * -1.0) - Q) * 60.;
	IMIN = Q;
	XSEC = (Q - IMIN) * 60.;
	if (XSEC >= 59.99) {
		IMIN++;
		XSEC = XSEC - 60.;
	}
	if (IMIN >= 60) {
		IDEG++;
		IMIN = IMIN - 60;
	}
	if (XSEC < 0.0)
		XSEC = 0.0;

	if (ltt > 0)
		JDEG = ltt;
	else
		JDEG = ltt * -1.0;
	Q = JDEG;
	if (ltt > 0)
		Q = (ltt - Q) * 60.;
	else
		Q = ((ltt * -1.0) - Q) * 60.;
	JMIN = Q;
	YSEC = (Q - JMIN) * 60.;
	if (YSEC >= 59.99) {
		JMIN++;
		YSEC = YSEC - 60.;
	}
	if (JMIN >= 60) {
		JDEG++;
		JMIN = JMIN - 60;
	}
	if (YSEC < 0.0)
		YSEC = 0.0;
	if (lng < 0)
		IDEG = -IDEG;
	if (ltt < 0)
		JDEG = -JDEG;

	return 0;
}

int init_used_table(void)
{
	int i;
	for (i = 0; i < NOPTIONS; i++) {
		USED_in[i].was = 0;
		USED_out[i].was = 0;
	}

	return 0;
}

