/* Memory allocation check added -Alex Shevlakov sixote@yahoo.com 23/03/00*/
/*									*/
/* Updated 8/99 to true 24bit support.
 * The old version was limited to 32767 colors.
 * 
 *  Incorporated color quantization to speed up GRASS.
 *   (GRASS is getting extremely slow with more than
 *     a few thousand categories/colors)
 *    
 *    Stefano Merler
 *    merler@irst.itc.it
*/

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "gis.h"

#include <math.h>

/*#define MAXCOLOR 16777216  - such big -it kills the prog. A.Sh.*/
/*#define MAXCOLOR 65536*/
#define MAXCOLOR 16777216

/*struct mycolor {
	int red;
	int grn;
	int blu;
} ppm_color[MAXCOLOR];
*/
typedef struct {
	int red;
	int grn;
	int blu;
	} mycolor;
	
mycolor * ppm_color;

int knum = 0;
	
int get_ppm_colors(FILE *, int, int, struct Colors *);
int get_ppm_colors2(struct Colors *, int, int *);
CELL lookup_color(int, int, int, int);
CELL lookup_color2(int, int, int, int, int *);
int quantize(int, int *);
int count_colors(FILE *, int, int);

int 
main (int argc, char *argv[])
{
	FILE *infp;
	CELL *cell,*cellptr;
	char *name;
	int outfp;
	int row,col;
	int nrows, ncols;
	int x, c,c1;
	struct Colors colors, *pcolr;
	struct Colors bwcolors, *pbwcolr;
	int red,grn,blu,num_colors;
	struct GModule *module;
	struct Option *inopt, *outopt, *nlevopt;
	int *levels;
	struct Flag *vflag, *bflag;
	int Verbose = 0;
	int nlev;
	int ppm_height, ppm_width, ppm_maxval, ppm_magic, ppm_head;
	struct Cell_head cellhd;
	long ppm_pos;
	int ncolors;
	int maxcolors;
	int Bands = 0;
	int outred, outgrn, outblu;
	char mapred[300], mapgrn[300], mapblu[300];
	CELL *cellr, *cellg, *cellb;
	
	
	
	
	pcolr = &colors;
	pbwcolr=&bwcolors;
	
	G_gisinit (argv[0]);
	module = G_define_module();
	module->description =
		"Converts an ASCII/BINARY PPM image file to "
		"a GRASS raster file.";

	inopt = G_define_option();
	outopt = G_define_option();
	nlevopt = G_define_option();

	inopt->key              = "input";
	inopt->type             = TYPE_STRING;
	inopt->required 	= YES;
	inopt->description      = "Name of existing PPM file.";

	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description     = "Name of new raster file(s).";

	nlevopt->key             = "nlev";
	nlevopt->type   	 = TYPE_INTEGER;
	nlevopt->required        = NO;
	nlevopt->description     = "Max number of levels for R/G/B.";
	nlevopt->answer          = "20 (= 8000 colors)";
	nlevopt->options         = "1-256";
	
	vflag = G_define_flag();
	vflag->key              = 'v';
	vflag->description      = "Verbose mode on.";

	bflag = G_define_flag();
	bflag->key              = 'b';
	bflag->description      = "Create 3 separate raster maps of the (true) R/G/B levels.";

	if(G_parser(argc, argv))
		exit(-1);

	Verbose = vflag->answer;
	Bands = bflag->answer;

	if((infp = fopen(inopt->answer, "r")) == NULL)
		G_fatal_error("Can't open PPM file for read.");
	name = outopt->answer;

	nlev=atoi(nlevopt->answer);
	
	maxcolors=nlev*nlev*nlev;
	G_init_colors(pcolr);
	
	ppm_height = ppm_width = ppm_maxval = ppm_magic = ppm_head = 0;

	while((c = fgetc(infp)) != EOF && !ppm_head){
		switch (c){
		case 'P':
			ppm_magic = fgetc(infp);
			if (Verbose)
				fprintf(stderr, "Magic = P%c\n", (char)ppm_magic);
			break;
		case '#':
			if (Verbose) fprintf(stderr, "Comment = #");
			while((c1 = fgetc(infp)) != EOF && c1 != '\n')
				if (Verbose)
					putchar(c1);
			putchar('\n');
			break;
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		case '0':
			if (!ppm_width || !ppm_height || !ppm_maxval)
				ungetc(c, infp);
			if (!ppm_width){
				fscanf(infp, "%d", &ppm_width);
				if (Verbose)
					fprintf(stderr, "Width = %d\n", ppm_width);
				break;
			}
			if (!ppm_height){
				fscanf(infp, "%d", &ppm_height);
				if (Verbose)
					fprintf(stderr, "Height = %d\n", ppm_height);
				break;
			}
			if (!ppm_maxval){
				fscanf(infp, "%d", &ppm_maxval);
				if (Verbose)
					fprintf(stderr, "Maxval = %d\n", ppm_maxval);
				ppm_head = 1;
				break;
			}
			break;
		default:
			break;
		}
	}
	
	knum = (int) floor(ppm_height*ppm_width/10);
	

	ppm_color = (mycolor*) malloc(knum * sizeof(mycolor));
  
  	if (ppm_color == NULL) {
    		fprintf(stderr,"\nCouldn't allocate space for image.\n Try increase your comp's RAM (256 Mb should do)\n");
    		exit(-1);
  	}

	ppm_pos = ftell(infp);
/*	ncolors=count_colors(infp,ppm_height*ppm_width, ppm_magic); we are not able to alloc */


	ncolors=num_colors=get_ppm_colors(infp, ppm_height*ppm_width, 
				    ppm_magic, pcolr);
	  if (Verbose)fprintf(stderr, "Total used colors = %d\n", num_colors);
	

	fseek(infp, ppm_pos, 0); /* get back where we were */
	
	ppm_pos = ftell(infp);
	if(ncolors > maxcolors){
	  G_warning("Color levels quantization...\n");
	  
	  if ( (levels=(int*)G_calloc(nlev,sizeof(int))) == NULL)
	  	G_fatal_error("Cannot allocate memory");
		
	  quantize(nlev,levels);
	  num_colors=get_ppm_colors2(pcolr,nlev,levels);
	  if (Verbose)fprintf(stderr, "Total used colors = %d\n", num_colors);
	}

	fseek(infp, ppm_pos, 0); /* get back where we were */
	
	nrows = cellhd.rows = ppm_height;
	ncols = cellhd.cols = ppm_width;
	cellhd.proj = G_projection();
	cellhd.zone = G_zone();
	cellhd.ew_res = cellhd.ns_res = 1.0;
	cellhd.north = (double) ppm_height;
	cellhd.east = (double) ppm_width;
	cellhd.west = cellhd.south = 0.0;
	G_set_window(&cellhd);
	G_set_cell_format(0);
	if((outfp = G_open_cell_new (name)) < 0)
	  G_fatal_error("Can't open new raster file.");
	cell = G_allocate_cell_buf();
	if(Bands){
	  sprintf(mapred,"%s.r",name);
	  if((outred = G_open_cell_new (mapred)) < 0)
	    G_fatal_error("Can't open new raster file.");
	  cellr = G_allocate_cell_buf();
	  sprintf(mapgrn,"%s.g",name);
	  if((outgrn = G_open_cell_new (mapgrn)) < 0)
	    G_fatal_error("Can't open new raster file.");
	  cellg = G_allocate_cell_buf();
	  sprintf(mapblu,"%s.b",name);
	  if((outblu = G_open_cell_new (mapblu)) < 0)
	    G_fatal_error("Can't open new raster file.");
	  cellb = G_allocate_cell_buf();
	}
	if (Verbose) fprintf(stderr, "Percent Complete: ");
	for (row = 0; row < nrows; row++)
	{
		cellptr = cell;
		if(Verbose) G_percent(row, nrows, 5);
		for (col = 0; col < ncols; col++){
			switch (ppm_magic){
			case '6':
				red = fgetc(infp);
				grn = fgetc(infp);
				blu = fgetc(infp);

				break;
			case '3':
				fscanf(infp, "%d %d %d", &red, &grn, &blu);
				break;
			}
			if(ncolors>maxcolors)
			  *cellptr++ = lookup_color2(red,grn,blu,nlev,levels);
			else
			  *cellptr++ = lookup_color(red,grn,blu,num_colors);
			if(Bands){
			  cellr[col] = (CELL)red;
			  cellg[col] = (CELL)grn;
			  cellb[col] = (CELL)blu;
			}
		}
		if (G_put_map_row(outfp, cell) < 0 )
			G_fatal_error("Can't write new raster row!!");
		if(Bands){
		  if (G_put_map_row(outred, cellr) < 0 )
		    G_fatal_error("Can't write new raster row!!");
		  if (G_put_map_row(outgrn, cellg) < 0 )
		    G_fatal_error("Can't write new raster row!!");
		  if (G_put_map_row(outblu, cellb) < 0 )
		    G_fatal_error("Can't write new raster row!!");
		}
	}
	if(Verbose) G_percent(nrows,nrows, 5);
	G_close_cell(outfp);
	if(Bands){
	  G_close_cell(outred);
	  G_close_cell(outgrn);
	  G_close_cell(outblu);
	}
	  
	/*G_put_cellhd(outopt->answer, &cellhd);*/
	if (Verbose) fprintf(stderr, "Writing color table for %d values\n", num_colors);
	for(x=0;x<num_colors;x++){
		G_set_color((CELL)x, ppm_color[x].red, ppm_color[x].grn,
			ppm_color[x].blu, pcolr);
	}
	if(G_write_colors(outopt->answer, G_mapset(), pcolr) < 0)
		G_fatal_error("Can't write color table!!");
	if(Bands){
	  G_init_colors(pbwcolr);
	  for(x=0;x<256;x++){
	    G_set_color((CELL)x, x, x,x, pbwcolr);
	  }
	  if(G_write_colors(mapred, G_mapset(), pbwcolr) < 0)
	    G_fatal_error("Can't write color table!!");
	  if(G_write_colors(mapgrn, G_mapset(), pbwcolr) < 0)
	    G_fatal_error("Can't write color table!!");
	  if(G_write_colors(mapblu, G_mapset(), pbwcolr) < 0)
	    G_fatal_error("Can't write color table!!");
	}
	
	exit(0);
}

int get_ppm_colors (FILE *infp, int pixels,
	int ppm_magic, struct Colors *pcolr)
{
	int count,x,maxcolor,match;
	int red, grn, blu;

	match = maxcolor = 0;
	for (count=0; count < pixels; count++){
		match = 0;
		switch (ppm_magic){
		case '6':
			red = fgetc(infp);
			grn = fgetc(infp);
			blu = fgetc(infp);
			break;
		case '3':
			fscanf(infp, "%d %d %d", &red, &grn, &blu);
			break;
		default:
			G_fatal_error("Unknown ppm magic number!!");
			break;
		}
		if ( knum < maxcolor + 1) {
			knum *= 2;
			ppm_color = (mycolor*) realloc((void*) ppm_color,knum * sizeof(mycolor));
  
  			if (ppm_color == NULL) {
    				fprintf(stderr,"\nCouldn't allocate space for image.\n Try increase your comp's RAM (256 Mb should do)\n");
    				exit(-1);
  			}
		}
		
		for (x=0;x<maxcolor;x++){
			if (ppm_color[x].red == red && 
			    ppm_color[x].grn == grn &&
			    ppm_color[x].blu == blu){
				match = 1;
				break;
			}
		}
		if (match == 0){
			ppm_color[maxcolor].red = red;
			ppm_color[maxcolor].grn = grn;
			ppm_color[maxcolor].blu = blu;
			maxcolor++;
			if (maxcolor == MAXCOLOR)
				G_fatal_error("Exceeded maximum colors!!");
		}
	}
	return(maxcolor);
}

int 
get_ppm_colors2 (struct Colors *pcolr, int colors_for_chanell, int *levels)
{
  int i,j,k;
  int actual;
  
  
  actual = 0;
  for(i=0;i<colors_for_chanell;i++)
    for(j=0;j<colors_for_chanell;j++)
      for(k=0;k<colors_for_chanell;k++){
      	if ( knum < actual +1) {
		knum *= 2;
		ppm_color = (mycolor*) realloc((void*) ppm_color,knum * sizeof(mycolor));
  
  		if (ppm_color == NULL) {
    			fprintf(stderr,"\nCouldn't allocate space for image.\n Try increase your comp's RAM (256 Mb should do)\n");
    			exit(-1);
  		}
	}
	ppm_color[actual].red = levels[i];
	ppm_color[actual].grn = levels[j];
	ppm_color[actual].blu = levels[k];
	actual ++;
      }
  return(actual);
}


CELL 
lookup_color (int r, int g, int b, int num)
{
	int x;

	for (x=0;x<num;x++){
		if (ppm_color[x].red == r && 
		    ppm_color[x].grn == g &&
		    ppm_color[x].blu == b){
			break;
		}
	}
	return((CELL)x);
}

CELL 
lookup_color2 (int r, int g, int b, int nlev, int *levels)
{
	int x;
	double mindist, tmpdist;
	int bestlevR;
	int bestlevG;
	int bestlevB;
	int index;
	

	mindist=255.;
	for (x=0;x<nlev;x++){
	  tmpdist=abs((double)(r - levels[x]));
	    if(tmpdist < mindist){
	      mindist = tmpdist;
	      bestlevR = x;
	    }
	}
	
	mindist=255.;
	for (x=0;x<nlev;x++){
	  tmpdist=abs((double)(g - levels[x]));
	  if(tmpdist < mindist){
	    mindist = tmpdist;
	    bestlevG = x;
	  }
	}
	
	mindist=255.;
	for (x=0;x<nlev;x++){
	  tmpdist=abs((double)(b - levels[x]));
	  if(tmpdist < mindist){
	    mindist = tmpdist;
	    bestlevB = x;
	  }
	}
	index = bestlevR*nlev*nlev + bestlevG*nlev + bestlevB;
	return((CELL)index);
	
}

int 
quantize (int colors_for_chanell, int *levels)
     
{
  int i;
  int step;
  
  levels[0] = 0;
  for(i=1;i<colors_for_chanell-1;i++){
    step = (int)((255.-levels[i-1]) / (double)(colors_for_chanell-i));
    levels[i] = levels[i-1] + step;
  }
  levels[colors_for_chanell-1] = 255;

  return 0;
}


int 
count_colors (FILE *infp, int pixels, int ppm_magic)
{
  int i;
  int ncolors;
  int red, grn, blu;
  int *total_color;
  
  if ( (total_color=(int*)G_calloc(MAXCOLOR,sizeof(int))) == NULL)
  	G_fatal_error("Cannot allocate memory");

  for (i=0; i < pixels; i++){
    switch (ppm_magic){
    case '6':
      red = fgetc(infp);
      grn = fgetc(infp);
      blu = fgetc(infp);
      break;
    case '3':
      fscanf(infp, "%d %d %d", &red, &grn, &blu);
      break;
    default:
      G_fatal_error("Unknown ppm magic number!!");
      break;
    }
    total_color[red*256*256+grn*256+blu]=1;
		
  }
  ncolors = 0;
  for(i=0;i<MAXCOLOR;i++)
    ncolors += total_color[i];
  free(total_color);
  return(ncolors);
}
