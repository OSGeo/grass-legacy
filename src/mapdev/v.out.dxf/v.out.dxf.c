/*      vect.to.dxf PATH FILENAME DXF_FILENAME
        will convert vector files into autocad files.  This program is
        a small demo and not to be taken seriously.                     

**      written by Chuck Ehlschlaeger                                   
**      improved March 15, 1989:  take it a little more seriously, but
        not too much.                                                   
**      at this stage of the game, this program will combine a vector
	file and an attribute file into a dxf file.  The layer names
   	(AV_..., LV_..., AC_..., LC_... will be based on the filename in 
	the ascii and att subdirectories.                               

**      PATH is parent directory location for the directories /dig_ascii
	and /dig_att which will contain the file FILENAME.  DXF_FILENAME
	is the path and filename of the resulting dxf file.             

**      Revised for Grass4.0- converted for new command parser 1/91 -dks
**         BUT--still needs to be set up for binary input before
	   moved from alpha to main installation.

**      revised usage:

	   v.out.dxf input=filename (ascii and att)  output=[path/]dfxfilename 
**/

#include "gis.h"
#include "to.dxf.h"

/* convert meters to inches						*/
#define MULTIPLY 	1 /* inches to meters 3.281 * 12.0 */
/* size of text compared to screen=1					*/
#define TEXT_SIZE	.003
#define CENTERED	4

FILE	*fpvect; /* file pointer to vector (asciir) file                  */
FILE    *fpcatt;   	/* file pointer to attribute file               */
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
	struct Option *old, *new;
	char *mapset, *dig_name, *dxf_name;
	char errmsg[200];

	G_gisinit (argv[0]);


    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    old->gisprompt		= "old,dig_ascii,dig_ascii";
    old->description		= "vector ascii input file";
    
    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
    new->description		= "dxf output file";


        if (G_parser (argc, argv))
    	exit (-1);
    
        dig_name = old->answer;
        dxf_name = new->answer;
    
        if (!*dig_name  || !*dxf_name )
        {
            fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
    	G_usage();
            exit (-1);
        }
    
        if ((mapset = G_find_file2("dig_ascii", dig_name,"")) == NULL)
        {
    	sprintf (errmsg, "Could not find Vector Ascii file <%s>\n", dig_name);
    	G_fatal_error (errmsg);
        }
    
        if ((mapset = G_find_file2("dig_att", dig_name,"")) == NULL)
        {
    	sprintf (errmsg, "Could not find Vector Attribute file <%s>\n", dig_name);
    	G_fatal_error (errmsg);
        }
    
        if((fpvect = G_fopen_old("dig_ascii", dig_name, mapset)) == NULL)      {
    	    fprintf(stderr,"ERROR, vector file name:%s cannot be opened\n",
    			    dig_name);
    	    exit(-1);
        }
    
        if((fpcatt = G_fopen_old("dig_att", dig_name, mapset)) == NULL)        {
    	    fprintf(stderr,"ERROR, attribute file:%s cannot be opened\n",
    			    dig_name);
    	    exit(-1);
        }
    
        dxf_open(dxf_name);
    

#ifdef OLD	
	if(argc != 4)   {
		fprintf(stderr,"ERROR, command line format is:\n");	
		fprintf(stderr,"vect.to.dxf PATH FILENAME");
		fprintf(stderr," DXF_FILENAME\n");
		exit(1);
	}
	init_vars(argv);          /* sets file pointers                  */
#endif /*OLD*/

	textsize = do_limits();   /*does header in fpdxf                 */
	make_layername(dig_name);  /* replaces . with _ in layername      */
	dxf_entities();
	add_plines();             /* puts plines in fpdxf                */
	add_text(textsize);       /*puts categories as text in fpdxf     */
	dxf_endsec();
	do_eof();                 /* puts final stuff in fpdxf, closes file */
	exit(0);
}



#ifdef OLD 

init_vars(argv) 
    char *argv[]; 
{ 
	FILE *fopen();
	char *ascii = "/dig_ascii/";
	char *att = "/dig_att/";
	char dig_ascii[120];
	char dig_att[120];
	char name[40];

	strcpy(dig_att, *++argv);  /*argv[1]*/
	strcpy(dig_ascii, *argv);  /*argv[1]*/
	strcpy(name, *++argv);     /*argv[2]*/
	strcat(dig_att, att);
	strcat(dig_ascii, ascii);
	strcat(dig_att, name);
	strcat(dig_ascii, name);
	/*DEBUG*/ fprintf (stderr, "dig_att '%s', dig_ascii '%s', name '%s'\n", dig_att, dig_ascii, name);
	exit(-1);
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
#endif /*OLD*/

double
do_limits()
{
	double west, east, south, north, textsize;
	char oneline[90];
	int onechar;

	while(getc(fpvect) != 'W')
		fgets(oneline, 90,fpvect);
	if(fscanf(fpvect,"EST EDGE: %lf",&west)!=1) 
			  G_fatal_error("can't read west limit");
	west *= MULTIPLY;
	while(getc(fpvect) !=':');
	if(fscanf(fpvect,"%lf",&east)!=1) 
			  G_fatal_error("can't read east limit");
	east *= MULTIPLY;
	while(getc(fpvect) != ':');
	if(fscanf(fpvect,"%lf",&south)!=1) 
			  G_fatal_error("can't read south limit");
	south *= MULTIPLY;
	while(getc(fpvect) != ':');
	if(fscanf(fpvect,"%lf",&north)!=1) 
			  G_fatal_error("can't read north limit");
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
	int onechar;
	int numlines;
	double y, x, firsty, firstx;

	while((onechar = getc(fpvect)) != EOF)   {
		while(onechar != 'A' && onechar != 'L')
			if((onechar = getc(fpvect)) == EOF)
				return(1);
		if(fscanf(fpvect,"%d",&numlines)!=1)
			G_fatal_error("error reading number of lines");
		if(onechar == 'A') 
			dxf_polyline(av_layer);
		else	dxf_polyline(lv_layer);
		while(numlines-- > 0)    {
			if(fscanf(fpvect,"%lf %lf", &y, &x)!=2)
			       G_fatal_error("error reading polyline coors");
			y *= MULTIPLY;
			x *= MULTIPLY;
			if(onechar == 'A')
				dxf_vertex(av_layer,x,y,(double) 0);
			else    dxf_vertex(lv_layer,x,y,(double) 0);
		}
		if(onechar == 'A')
			dxf_poly_end(av_layer);
		else	dxf_poly_end(lv_layer);
	/*DEBUG*//*	printf("p");*/
	}
	return (0);
}

add_text(textsize)
double textsize;
{
	int onechar;
	char cat_num[32];
	double x, y;

	while((onechar = getc(fpcatt)) != EOF)   {
		while(onechar != 'A' && onechar != 'L')
			if((onechar = getc(fpcatt)) == EOF)
				return(1);
		if(fscanf(fpcatt,"%lf  %lf  %s",&x,&y,cat_num)!=3)
			G_fatal_error("reading atts");
		x *= MULTIPLY;
		y *= MULTIPLY;
		if(onechar == 'A')
			dxf_text(ac_layer,x,y,(double)0,textsize,
				CENTERED,cat_num);
		else	dxf_text(lc_layer,x,y,(double)0,textsize,
				CENTERED,cat_num);
		/*DEBUG*//*printf("t");*/
	}
	return(0);
}

do_eof()
{
	fclose(fpvect);
	fclose(fpcatt);
	dxf_eof();
}
