/*      dig2dxf PATH FILENAME DXF_FILENAME
        will convert vector files into autocad files.  This program is
        a small demo and not to be taken seriously.                     */

/*      written by Chuck Ehlschlaeger                                   */
/*      improved March 15, 1989:  take it a little more seriously, but
        not too much.                                                   */
/*      at this stage of the game, this program will combine a vector
	file and an attribute file into a dxf file.  The layer names
   	(AV_..., LV_..., AC_..., LC_... will be based on the filename in 
	the ascii and att subdirectories.                               */

/*      PATH is parent directory location for the directories /dig_ascii
	and /dig_att which will contain the file FILENAME.  DXF_FILENAME
	is the path and filename of the resulting dxf file.             */

#include "to.dxf.h"

/* convert meters to inches						*/
#define MULTIPLY	3.281 * 12.0 
/* size of text compared to screen=1					*/
#define TEXT_SIZE	.015
#define CENTERED	4

FILE	*fpvect,	/* file pointer to vector file                  */
	*fpcatt;   	/* file pointer to attribute file               */
char    layer[80];	/* char string of shortened vector file name    */
char	lv_layer[33];
char	av_layer[33];
char	ac_layer[33];
char	lc_layer[33];

main(argc,argv)
int argc;
char *argv[];
{
	double textsize;
	double do_limits();

	if(argc != 4)   {
		fprintf(stderr,"ERROR, command line format is:\n");	
		fprintf(stderr,"%s PATH FILENAME", argv[0]);
		fprintf(stderr," DXF_FILENAME\n");
		exit(1);
	}
	init_vars(argv);          /* sets file pointers                  */
	textsize = do_limits();   /*does header in fpdxf                 */
	make_layername(argv[2]);  /* replaces . with _ in layername      */
	dxf_entities();
	add_plines();             /* puts plines in fpdxf                */
	add_text(textsize);       /*puts categories as text in fpdxf     */
	dxf_endsec();
	do_eof();                 /* puts final stuff in fpdxf, closes file */
	exit(0);
}

init_vars(argv)
char *argv[];
{
	FILE *fopen();
	char *ascii = "/dig_ascii/";
	char *att = "/dig_att/";
	char dig_ascii[120];
	char dig_att[120];
	char name[40];

	strcpy(dig_att, *++argv);
	strcpy(dig_ascii, *argv);
	strcpy(name, *++argv);
	strcat(dig_att, att);
	strcat(dig_ascii, ascii);
	strcat(dig_att, name);
	strcat(dig_ascii, name);
 	if((fpvect = fopen(dig_ascii, "r")) == NULL)      {
		fprintf(stderr,"ERROR, vector file name:%s cannot be opened\n",
				dig_ascii);
		exit(1);
	}
	if((fpcatt = fopen(dig_att, "r")) == NULL)        {
		fprintf(stderr,"ERROR, attribute file:%s cannot be opened\n",
				dig_att);
		exit(1);
	}
	*++argv;
	dxf_open(*argv);
}

double
do_limits()
{
	double west, east, south, north, textsize;
	char onechar, oneline[90];

	while(getc(fpvect) != 'W')
		fgets(oneline, 90,fpvect);
	fscanf(fpvect,"EST EDGE: %lf",&west);
	west *= MULTIPLY;
	while(getc(fpvect) !=':');
	fscanf(fpvect,"%lf",&east);
	east *= MULTIPLY;
	while(getc(fpvect) != ':');
	fscanf(fpvect,"%lf",&south);
	south *= MULTIPLY;
	while(getc(fpvect) != ':');
	fscanf(fpvect,"%lf",&north);
	north *= MULTIPLY;
	fgets(oneline, 90, fpvect);
	fgets(oneline, 90, fpvect);

	dxf_header();
	dxf_limits(north,south,east,west);
	dxf_endsec();

	if((east - west) >= (north - south))
		textsize = (east - west) * TEXT_SIZE;
	else	textsize = (north - south) * TEXT_SIZE;
	return(textsize);
}


make_layername(filename)
char filename[];
{
	int countfile = 0;
	int countlayer = 3;

	while(filename[countfile] != '\0')	{
		if(filename[countfile] == '/')
			countlayer = 3;
		else if(filename[countfile] == '.')
			layer[countlayer++] = '_';
		else if(islower(filename[countfile]))
			layer[countlayer++] = toupper(filename[countfile]);
		else	layer[countlayer++] = filename[countfile];
		countfile++;
	}
	layer[countlayer] = '\0';

	dxf_tables();
	dxf_linetype_table(1);
	dxf_solidline();
	dxf_endtable();
	dxf_layer_table(5);
	dxf_layer0();

	layer[0] = 'A';
	layer[1] = 'V';
	layer[2] = '_';
	strcpy(av_layer,layer);
	dxf_layer(layer,1,"CONTINUOUS",0);
	layer[0] = 'L';
	strcpy(lv_layer,layer);
	dxf_layer(lv_layer,2,"CONTINUOUS",0);
	layer[1] = 'C';
	strcpy(lc_layer,layer);
	dxf_layer(lc_layer,3,"CONTINUOUS",0);
	layer[0] = 'A';
	strcpy(ac_layer,layer);
	dxf_layer(ac_layer,4,"CONTINUOUS",0);

	dxf_endtable();
	dxf_endsec();
}

add_plines()
{
	char onechar;
	int numlines;
	double y, x, firsty, firstx;

	while((onechar = getc(fpvect)) != EOF)   {
		while(onechar != 'A' && onechar != 'L')
			if((onechar = getc(fpvect)) == EOF)
				return(1);
		fscanf(fpvect,"%d",&numlines);
		if(onechar == 'A') 
			dxf_polyline(av_layer);
		else	dxf_polyline(lv_layer);
		while(numlines-- > 0)    {
			fscanf(fpvect,"%lf %lf", &y, &x);
			y *= MULTIPLY;
			x *= MULTIPLY;
			if(onechar == 'A')
				dxf_vertex(av_layer,x,y,(double) 0);
			else    dxf_vertex(lv_layer,x,y,(double) 0);
		}
		if(onechar == 'A')
			dxf_poly_end(av_layer);
		else	dxf_poly_end(lv_layer);
		printf("p");
	}
}

add_text(textsize)
double textsize;
{
	char onechar;
	char cat_num[32];
	double x, y;

	while((onechar = getc(fpcatt)) != EOF)   {
		while(onechar != 'A' && onechar != 'L')
			if((onechar = getc(fpcatt)) == EOF)
				return(1);
		fscanf(fpcatt,"%lf  %lf  %s",&x,&y,cat_num);
		x *= MULTIPLY;
		y *= MULTIPLY;
		if(onechar == 'A')
			dxf_text(ac_layer,x,y,(double)0,textsize,
				CENTERED,cat_num);
		else	dxf_text(lc_layer,x,y,(double)0,textsize,
				CENTERED,cat_num);
		printf("t");
	}
}

do_eof()
{
	fclose(fpvect);
	fclose(fpcatt);
	dxf_eof();
}
