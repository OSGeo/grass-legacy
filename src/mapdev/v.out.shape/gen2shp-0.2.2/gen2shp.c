/* Jan-Oliver Wagner	$Date$
 * $Id$
 *
 * Copyright (C) 1999 by Jan-Oliver Wagner
 * 
 *    This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License
 *   as published by the Free Software Foundation; either version 2
 *   of the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Log$
 * Revision 1.1  2000-03-08 14:32:23  markus
 * contribution from Jan Wagner
 *
 * Revision 1.5  1999/11/05  08:02:40  jwagner
 * Added CASE_INSENSITIVE_STR_CMP
 *
 * Revision 1.4  1999/11/05  07:13:31  jwagner
 * test for "end" now case-insensitive.
 *
 * Revision 1.3  1999/09/16  08:44:57  jwagner
 * Just a typo and new revision.
 *
 * Revision 1.2  1999/04/22 15:30:25  jwagner
 * Added further geodata types: lines and polygons.
 * Rearrangement of procedures, some facelifting for readability.
 *
 * Revision 1.1  1999/04/21  16:01:31  jwagner
 * Initial revision
 *
 */

#include <shapefil.h>	/* from shapelib */

#include "utils.h"

#define VERSION "0.2.2 (RCS-$Revision$)"

/* Error codes for exit() routine: */
#define	ERR_USAGE	1
#define ERR_TYPE	2
#define ERR_FORMAT	3
#define ERR_OBJECTTYPE	4
#define ERR_ALLOC	5

#define ERR_DBFCREATE	10
#define ERR_DBFADDFIELD	11
#define ERR_DBFOPEN	12
#define	ERR_DBFWRITEINTEGERATTRIBUTE	13

#define ERR_SHPOPEN	20

/* Object Type codes used in main(): */
#define OBJECTTYPE_NONE		0
#define OBJECTTYPE_POINT	1
#define OBJECTTYPE_LINE		2
#define OBJECTTYPE_POLYGON	3

/* minimum number of coordinates allocated blockwise */
#define COORDS_BLOCKSIZE	100

/* maximum length for read strings,
 * if input lines with more characters appear,
 * errors are likely to occur */
#define STR_BUFFER_SIZE		300

#ifdef USE_STRICMP
#define CASE_INSENSITIVE_STR_CMP	stricmp
#else
#define CASE_INSENSITIVE_STR_CMP	strcasecmp
#endif

void print_version(FILE *file)
{
	fprintf(file,"gen2shp version " VERSION "\n"); 
	fprintf(file,"Copyright (C) 1999 by Jan-Oliver Wagner.\n"
		"The GNU GENERAL PUBLIC LICENSE applies. "
		"Absolutly No Warranty!\n");
#ifdef DEBUG
	fprintf(file,"compiled with option: DEBUG\n"); 
#endif
}

static DBFHandle LaunchDbf (	const char *fname ) {
	DBFHandle	hDBF;
	char		dbffname[STR_BUFFER_SIZE];
	char		fieldname[STR_BUFFER_SIZE];

	sprintf(dbffname, "%s.dbf", fname);
	sprintf(fieldname, "%s-id", fname);

	hDBF = DBFCreate( dbffname );
	if( hDBF == NULL ) {
		fprintf(stderr, "DBFCreate(%s) failed.\n", fname );
		exit(ERR_DBFCREATE);
	}

	if (DBFAddField( hDBF, fieldname, FTInteger, 11, 0 ) == -1) {
		fprintf(stderr, "DBFAddField(hDBF,%s,FTInteger,11,0) failed.\n", fieldname);
		exit(ERR_DBFADDFIELD);
	}

	DBFClose( hDBF );

	hDBF = DBFOpen( dbffname, "r+b" );
	if( hDBF == NULL ) {
		fprintf(stderr, "DBFOpen(%s,\"r+b\") failed.\n", dbffname );
		exit(ERR_DBFOPEN);
	}

	return hDBF;
}

static SHPHandle LaunchShp(	const char *fname,
				int ObjectType ) {
	SHPHandle	hSHP;
	SHPObject	*psShape;
	char		shpfname[STR_BUFFER_SIZE];

	sprintf(shpfname, "%s.shp", fname);

	switch (ObjectType) {
		case OBJECTTYPE_POINT:
			hSHP = SHPCreate( shpfname, SHPT_POINT );
			break;
		case OBJECTTYPE_LINE:
			hSHP = SHPCreate( shpfname, SHPT_ARC );
			break;
		case OBJECTTYPE_POLYGON:
			hSHP = SHPCreate( shpfname, SHPT_POLYGON );
			break;
		default:
			fprintf(stderr, "internal error: "
				"unknown ObjectType=%d\n", ObjectType);
			exit(ERR_OBJECTTYPE);
	}

	if( hSHP == NULL ) {
		fprintf(stderr, "SHPOpen(%s, shape_type) failed.\n", shpfname );
		exit(ERR_SHPOPEN);
	}

	return hSHP;
}

static void WriteDbf (	DBFHandle hDBF,
			int rec,
			int id ) {
	if (! DBFWriteIntegerAttribute(hDBF, rec, 0, id)) {
		fprintf(stderr, "DBFWriteIntegerAttribute(hDBFs,%d,1,%d) failed.\n", rec, id );
		exit(ERR_DBFWRITEINTEGERATTRIBUTE);
	}
}

static void WritePoint(	SHPHandle hSHP,
			int rec,
			double x,
			double y ) {
	SHPObject	*psShape;

	psShape = SHPCreateObject( SHPT_POINT, rec, 0, NULL, NULL,
                               1, &x, &y, NULL, NULL );
	SHPWriteObject( hSHP, -1, psShape );
	SHPDestroyObject( psShape );
}

static void WriteLine(	SHPHandle hSHP,
			int rec,
			int coords,
			double * x,
			double * y ) {
	SHPObject	*psShape;

	psShape = SHPCreateObject( SHPT_ARC, rec, 0, NULL, NULL,
		coords, x, y, NULL, NULL );
	SHPWriteObject( hSHP, -1, psShape );
	SHPDestroyObject( psShape );
}

static void WritePolygon(	SHPHandle hSHP,
				int rec,
				int coords,
				double * x,
				double * y ) {
	SHPObject	*psShape;

	psShape = SHPCreateObject( SHPT_POLYGON, rec, 0, NULL, NULL,
		coords, x, y, NULL, NULL );
	SHPWriteObject( hSHP, -1, psShape );
	SHPDestroyObject( psShape );
}

/* read from fp and generate point shapefile to hDBF/hSHP */
static void GeneratePoints (	FILE *fp,
				DBFHandle hDBF,
				SHPHandle hSHP ) {
	char linebuf[STR_BUFFER_SIZE];	/* buffer for line-wise reading from file */
	int id;			/* ID of point */
	double x, y;		/* coordinates of point */
	char * str;		/* tmp variable needed for assertions */
	int rec = 0;		/* Counter for records */

	while (getline(fp, linebuf) != EOF) {
		if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
#ifdef DEBUG
			fprintf(stderr, "debug output: 'end' detected\n");
#endif
			break;
		}
		if ((str = dtok(linebuf, ',')) == NULL) {
			fprintf(stderr, "format error in line %d\n", rec + 1);
			exit(ERR_FORMAT);
		}
		id = atoi((const char *)str);

		if ((str = dtok(NULL, ',')) == NULL) {
			fprintf(stderr, "format error in line %d\n", rec + 1);
			exit(ERR_FORMAT);
		}
		x = atof((const char *)str);

		if ((str = dtok(NULL, ',')) == NULL) {
			fprintf(stderr, "format error in line %d\n", rec + 1);
			exit(ERR_FORMAT);
		}
		y = atof((const char *)str);

#ifdef DEBUG
		fprintf(stderr, "debug output: id=%d, x=%f, y=%f\n", id, x, y);
#endif

		WriteDbf(hDBF, rec, id);
		WritePoint(hSHP, rec, x, y);
		rec ++;
	}
}

/* read from fp and generate line/arc shapefile to hDBF/hSHP */
static void GenerateLines (	FILE *fp,
				DBFHandle hDBF,
				SHPHandle hSHP ) {
	char linebuf[STR_BUFFER_SIZE];	/* buffer for line-wise reading from file */
	int id;			/* ID of point */
	double	* x = NULL,
		* y = NULL;	/* coordinates arrays */
	int vector_size = 0;	/* current size of the vectors x and y */
	char * str;		/* tmp variable needed for assertions */
	int rec = 0;		/* Counter for records */
	int coord = 0;		/* Counter for coordinates */

	/* loop lines */
	while (getline(fp, linebuf) != EOF) {
		if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
#ifdef DEBUG
			fprintf(stderr, "debug output: final 'end' detected\n");
#endif
			break;
		}

		/* IDs are in single lines */
		id = atoi((const char *)linebuf);

#ifdef DEBUG
		fprintf(stderr, "debug output: id=%d\n", id);
#endif

		coord = 0;

		/* loop coordinates of line 'id' */
		while (getline(fp, linebuf) != EOF) {
			if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
#ifdef DEBUG
				fprintf(stderr, "debug output: a lines "
					"'end' detected\n");
#endif
				break;
			}

			/* allocate coordinate vectors if to small */
			if (vector_size <= coord) {
				vector_size += COORDS_BLOCKSIZE;
				x = realloc(x, vector_size * sizeof(double));
				y = realloc(y, vector_size * sizeof(double));
				if (x == NULL || y == NULL) {
					fprintf(stderr, "memory allocation failed\n");
					exit(ERR_ALLOC);
				}
			}

			if ((str = dtok(linebuf, ',')) == NULL) {
				fprintf(stderr, "format error for line with "
					"id=%d\n", id);
				exit(ERR_FORMAT);
			}
			x[coord] = atof((const char *)str);

			if ((str = dtok(NULL, ',')) == NULL) {
				fprintf(stderr, "format error for line with "
					"id=%d\n", id);
				exit(ERR_FORMAT);
			}
			y[coord] = atof((const char *)str);

#ifdef DEBUG
			fprintf(stderr, "debug output: x=%f, y=%f\n",
				x[coord], y[coord]);
#endif

			coord ++;
		}
		WriteDbf(hDBF, rec, id);
		WriteLine(hSHP, rec, coord, x, y);
		rec ++;
	}

	free(x);
	free(y);
}

/* read from fp and generate polgon shapefile to hDBF/hSHP */
static void GeneratePolygons (	FILE *fp,
				DBFHandle hDBF,
				SHPHandle hSHP ) {
	char linebuf[STR_BUFFER_SIZE];	/* buffer for line-wise reading from file */
	int id;			/* ID of point */
	double	* x = NULL,
		* y = NULL;	/* coordinates arrays */
	int vector_size = 0;	/* current size of the vectors x and y */
	char * str;		/* tmp variable needed for assertions */
	int rec = 0;		/* Counter for records */
	int coord = 0;		/* Counter for coordinates */

	/* loop polygons */
	while (getline(fp, linebuf) != EOF) {
		if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
#ifdef DEBUG
			fprintf(stderr, "debug output: final 'end' detected\n");
#endif
			break;
		}

		/* IDs are in single lines */
		id = atoi((const char *)linebuf);

#ifdef DEBUG
		fprintf(stderr, "debug output: id=%d\n", id);
#endif

		coord = 0;

		/* loop coordinates of polygon 'id' */
		while (getline(fp, linebuf) != EOF) {
			if (CASE_INSENSITIVE_STR_CMP(linebuf, "end") == 0) {
#ifdef DEBUG
				fprintf(stderr, "debug output: a polygons "
					"'end' detected\n");
#endif
				break;
			}

			/* allocate coordinate vectors if to small */
			if (vector_size <= coord) {
				vector_size += COORDS_BLOCKSIZE;
				x = realloc(x, vector_size * sizeof(double));
				y = realloc(y, vector_size * sizeof(double));
				if (x == NULL || y == NULL) {
					fprintf(stderr, "memory allocation failed\n");
					exit(ERR_ALLOC);
				}
			}

			if ((str = dtok(linebuf, ',')) == NULL) {
				fprintf(stderr, "format error for polygon with "
					"id=%d\n", id);
				exit(ERR_FORMAT);
			}
			x[coord] = atof((const char *)str);

			if ((str = dtok(NULL, ',')) == NULL) {
				fprintf(stderr, "format error for polygon with "
					"id=%d\n", id);
				exit(ERR_FORMAT);
			}
			y[coord] = atof((const char *)str);

#ifdef DEBUG
			fprintf(stderr, "debug output: x=%f, y=%f\n",
				x[coord], y[coord]);
#endif

			coord ++;
		}
		WriteDbf(hDBF, rec, id);
		WritePolygon(hSHP, rec, coord, x, y);
		rec ++;
	}

	free(x);
	free(y);
}

int main(	int argc,
		char ** argv ) {
	DBFHandle hDBF;		/* handle for dBase file */
	SHPHandle hSHP;		/* handle for shape files .shx and .shp */
	int ObjectType = OBJECTTYPE_NONE;

	if (argc != 3) {
		print_version(stderr);
		fprintf(stderr, "usage: %s outfile type < infile\n", argv[0]);
		fprintf(stderr, "\treads stdin and creates outfile.shp, "
			"outfile.shx and outfile.dbf\n"
			"\ttype must be one of these: points lines polygons\n"
			"\tinfile must be in 'generate' format\n");
		exit(ERR_USAGE);
	}

	/* determine Object Type: */
	if (strcmp(argv[2], "points") == 0) ObjectType = OBJECTTYPE_POINT;
	if (strcmp(argv[2], "lines") == 0) ObjectType = OBJECTTYPE_LINE;
	if (strcmp(argv[2], "polygons") == 0) ObjectType = OBJECTTYPE_POLYGON;
	if (ObjectType == OBJECTTYPE_NONE) {
		fprintf(stderr, "type '%s' unknown, use one of these: "
			"points lines polygons.", argv[2]);
		exit(ERR_TYPE);
	}

#ifdef DEBUG
	fprintf(stderr, "debug output: outfile=%s\n", argv[1]);
	fprintf(stderr, "debug output: type=%s\n", argv[2]);
#endif

	/* Open and prepare output files */
	hDBF = LaunchDbf(argv[1]);
	hSHP = LaunchShp(argv[1], ObjectType);

	/* Call generate function */
	switch (ObjectType) {
		case OBJECTTYPE_POINT:
			GeneratePoints(stdin, hDBF, hSHP);
			break;
		case OBJECTTYPE_LINE:
			GenerateLines(stdin, hDBF, hSHP);
			break;
		case OBJECTTYPE_POLYGON:
			GeneratePolygons(stdin, hDBF, hSHP);
			break;
		default:
			fprintf(stderr, "internal error: "
				"unknown ObjectType=%d\n", ObjectType);
			exit(ERR_OBJECTTYPE);
	}

	/* Finish output files */
	DBFClose( hDBF );
	SHPClose( hSHP );

	/* success */
	exit(0);
}
