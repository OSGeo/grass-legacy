#include "gis.h"

#define QRY_LENGTH 1024

struct Sql
{
	double centX	;   /* x coordinate 			*/
	double centY	;   /* y coordinate			*/
	double permX	;   /* permiter easting			*/
	double permY	;   /* perimeter north			*/
	double rad2	;
	double distance	;
	double maxY	;   /* northing				*/
	double minY	;   /* south				*/
	double minX	;   /* west				*/
	double maxX	;   /* east				*/
};

int getAllOpts (int, char **);
int getSelectOpts (int, char **);
int runPg(char *, char *, char *);
int runqry(char *, struct Sql *, char *);
char* buildPg(struct Option *,struct Option *, struct Option *);
int getArea (struct Sql *);
