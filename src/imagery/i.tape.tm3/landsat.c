/*======================================================================
Filename:   landsat.c
Module:	    i.landsat.tm
Author:	    Christopher Lesher

This is the location of main() and Parse().  Parse the command line, read
the first file, and call an import routine depending on the file format.
======================================================================*/

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "landsat.h"

#ifdef _NO_PROTO
static char *Parse();
static int Compare_ints();
#else
static char *Parse(int , char **, Landsat *);
static int Compare_ints(int*, int*);
#endif


int 
main (int argc, char *argv[])
{
   Landsat landsat;
   char *error;
   int k;
   char *header;
   int numread;
   int size;
   int  length;
   int exitval=0;
   long bands_done, bands_notdone, num_notdone;
   G_gisinit(argv[0]);

   if( error = Parse(argc, argv, &landsat)) {
      fprintf(stderr, "%s\n", error);
      exit(1);
   }

   I_must_be_imagery_projection();
   G_want_histogram(1);

   FileSeqInit(&landsat.fileseq, landsat.fileprompt, landsat.infile, "ls",
      landsat.blocksize);
   if(FileSeqNext(&landsat.fileseq, -1))
      goto quit;

   bands_done = 0;
   /* Loop until volume header found and import routine called. */
   while(1) {
      char answer[10];
      /* Read the first file into header.  length = size of first file. */
      size=1025;
      length=0;
      header = malloc(size);
      while(numread = FileSeqRead(&landsat.fileseq, header+size-1024, 1024)) {
	 if(numread == -1)
      	    break;
	 length += numread;
	 size += 1024;
	 header = realloc(header, size);
      }

      /* Determine file format by looking at strings in the header. */
      if(strcmp(Field(header, 17, 28), "CCB-CCT-0002") == 0) {
      	 bands_done = ImportQuadrant(&landsat, header, length);
      	 break;
      }
      if(strcmp(Field(header, 1, 7), "PRODUCT") == 0 ||	    /* Fast ver B */
      	 strcmp(Field(header, 1, 8), "SCENE ID") == 0) {    /* Fast ver A */
      	 bands_done = ImportFast(&landsat, header, length);
      	 break;
      }

      fprintf(stderr, "Volume header not found.\n");
      fprintf(stderr, "The first file read must be a volume header.\n");
      fprintf(stderr, "Continue (y/n) [n]? ");
      fgets(answer, sizeof(answer), stdin);
      if( answer[0] == 'y' || answer[0] == 'Y')
      	 FileSeqNext(&landsat.fileseq, -1);
      else
      	 break;
   }

   /* Print list of bands not imported.  Skip this if files were
      just examined. */
   if( bands_done != -1 && ! landsat.examine) {
      bands_notdone = 0;
      num_notdone = 0;
      for(k=0; k<landsat.numbands; ++k)
	 if( (bands_done & (1 << landsat.bands[k])) == 0) {
	    bands_notdone |= (1 << landsat.bands[k]);
	    ++num_notdone;
	 }
      if( bands_notdone) {
	 fprintf(stderr, "Band%s ", num_notdone>1 ? "s" : "");
	 for(k=0; k<landsat.numbands; ++k)
	    if( bands_notdone & (1 << landsat.bands[k]))
      	       if( landsat.bands[k] == 8)
      	       	  fprintf(stderr, "P ");
      	       else
	       	  fprintf(stderr, "%d ", landsat.bands[k]);
	 fprintf(stderr, "%s not imported.\n", num_notdone>1 ? "were" : "was" );
      }
   }
   exitval = 0;

quit:
   if(header) free(header);
   FileSeqEnd(&landsat.fileseq);
   exit(exitval);
}



/*======================================================================
				Parse

Parse the command line, fill in *landsat's fields.  Return NULL if
successful, else return string containing error message.
======================================================================*/
static char *
Parse (int argc, char *argv[], Landsat *landsat)
{
   struct Option *infile, *group, *outfile, *bands, *rows, *cols, *title;
   struct Option *blocksize;
   struct Flag *eflag, *qflag, *pflag;
   int k;

   infile = G_define_option();
   infile->key = "input";
   infile->type = TYPE_STRING;
   infile->required = NO;
   infile->description = "Filename or name of tape drive device";

   blocksize = G_define_option();
   blocksize->key = "blocksize";
   blocksize->type = TYPE_INTEGER;
   blocksize->required = NO;
   blocksize->description = "Block size of tape device";

   group = G_define_option();
   group->key = "group";
   group->type = TYPE_STRING;
   group->required = NO;
   group->gisprompt   = "any, group, group";
   group->description = "Name of imagery group";

   outfile = G_define_option();
   outfile->key = "output";
   outfile->type = TYPE_STRING;
   outfile->required = NO;
   outfile->description = "Prefix for names of created raster maps";

   bands = G_define_option();
   bands->key = "bands";
   bands->type = TYPE_STRING;
   bands->description = "LandSat TM Bands to import";
   bands->multiple = YES;
   bands->required = NO;
   /**   bands->options  = "1-7"; **/
   bands->key_desc = "band";

   rows = G_define_option();
   rows->key = "rows";
   rows->type = TYPE_INTEGER;
   rows->required = NO;
   rows->multiple = NO;
   rows->description = "First and last rows of extracted region";
   rows->key_desc = "firstrow,lastrow";

   cols = G_define_option();
   cols->key = "cols";
   cols->type = TYPE_INTEGER;
   cols->required = NO;
   cols->multiple = NO;
   cols->description = "First and last columns of extracted region";
   cols->key_desc = "firstcol,lastcol";

   title = G_define_option();
   title->key = "title";
   title->type = TYPE_STRING;
   title->required = NO;
   title->description = "Title for extracted bands";

   eflag = G_define_flag();
   eflag->key = 'e';
   eflag->description = "Only examine (not extract) data";

/***
/***   pflag = G_define_flag();
/***   pflag->key = 'p';
/***   pflag->description = "Display percentage of work done";
***/

   qflag = G_define_flag();
   qflag->key = 'q';
   qflag->description = "Run quietly";

   if(G_parser(argc, argv))
      return("");

   if(infile->answer)
      landsat->infile = G_store(infile->answer);
   else
      landsat->infile = NULL;
   landsat->fileprompt = ! infile->answer;
   G_strncpy(landsat->info.title,
      title->answer ? title->answer : DEFAULT_LANDSAT_TITLE,
      sizeof(landsat->info.title)-1);

   landsat->examine = eflag->answer;

   /** landsat->percent = pflag->answer;  **/
   landsat->percent = 0;

   landsat->quiet = qflag->answer;

   /* If -e given, no other options matter. */
   if(landsat->examine)
      return(NULL);

   if(group->answer == NULL)
      return("You must specify a group");
   if(bands->answers == NULL)
      return("You must specify what bands to import");

   landsat->group = group->answer;
   landsat->outfile = outfile->answer ? outfile->answer : group->answer;
   landsat->bands_to_do = 0;
   k=0;
   while(bands->answers[k]) {
      if( *bands->answers[k] == 'P' || *bands->answers[k] == 'p')
      	 landsat->bands[k] = 8;
      else {
      	 landsat->bands[k] = atoi(bands->answers[k]);
      	 if(landsat->bands[k] < 1 || landsat->bands[k] > 7)
      	    return("Band numbers must be between 1 and 7");
      }
      landsat->bands_to_do |= 1 << landsat->bands[k];
      ++k;
   }
   qsort(landsat->bands, k, sizeof(int), Compare_ints);
   landsat->numbands = k;
   if( landsat->numbands < 1 || landsat->numbands > 7)
      return("You must specify from 1 to 7 bands.");

   landsat->top =    rows->answers  ? atoi(rows->answers[0]) : 0;
   landsat->bottom = rows->answers  ? atoi(rows->answers[1]) : 0;
   landsat->left =   cols->answers  ? atoi(cols->answers[0]) : 0;
   landsat->right =  cols->answers  ? atoi(cols->answers[1]) : 0;

   if( landsat->top < 0 || landsat->bottom < 0 ||
       landsat->left < 0 || landsat->right < 0)
      return("Tape window dimensions must be greater than zero.");
   if( landsat->top > landsat->bottom)
      return("Row range not specified correctly.");
   if( landsat->left > landsat->right)
      return("Column range not specified correctly.");
   if(blocksize->answer) {
      landsat->blocksize = atoi(blocksize->answer);
      if(landsat->blocksize <= 0)
      	 return("Blocksize must be > 0.");
   } else
      landsat->blocksize = DEFAULT_RECORD;
   return(NULL);
}

static int 
Compare_ints (int *a, int *b)
{ return(*a-*b); }


