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


struct Stats
{
	int count	;  /* count aggregate			*/
	float sum	;  /* sum column aggregate		*/
	float avg	;  /* average agregate			*/
	float min	;  /* min column value			*/
	float max	;  /* max column value			*/
	float freq	;  /* freq not used currently		*/
	float mode	;  /* mode for column value		*/
};
