#define GLOBAL
#include "mkappa.h"

main (argc, argv)
 int argc;
 char *argv[];
{
 int i;
 struct GModule *module;
 struct { 
   struct Option *input, *output, *titles;
 } parms;

 struct { 
   struct Flag *h;
 } flags;

 G_gisinit(argv[0]);

 module = G_define_module();
 module->description =
	"Calculate error matrix and kappa parameter for accuracy "
	"assessment of classification result.";

 parms.input = G_define_option();
 parms.input->key	="input";
 parms.input->type	=TYPE_STRING;
 parms.input->required	=NO;
 parms.input->description	="File name of sites";

 parms.output = G_define_option();
 parms.output->key		="output";
 parms.output->type		=TYPE_STRING;
 parms.output->required		=NO;
 parms.output->description 	="Name of an output file containing of error matrix and kappa";

 parms.titles = G_define_option();
 parms.titles->key		="title";
 parms.titles->type		=TYPE_STRING;
 parms.titles->required		=NO;
 parms.titles->description	="Title for error matrix and kappa";
 parms.titles->answer		="Error matrix and kappa";

 flags.h = G_define_flag();
 flags.h->key = 'h';
 flags.h->description = "no header in the report";

 if (G_parser(argc, argv))
   exit (-1);

 if (parms.input->answer) {
   input = parms.input->answer;
   if (G_legal_filename (input) < 0) {
     fprintf (stderr, "\nERROR: <%s> - illegal input file name\n",
	parms.input->answer);
     G_usage();
     exit(1);
   }
 }
 else
   input=NULL;
 
 if (parms.output->answer) {
   output=parms.output->answer;
   if (G_legal_filename (output) < 0) {
     fprintf (stderr, "\nERROR: <%s> - illegal output file name\n", 
	parms.output->answer);
     G_usage();
     exit(1);
   }
 }
 else
   output=NULL;

 title = parms.titles->answer;

/* read in matrix for calculation */
 readin_data();

/* print header of the output */
 if (!(header = flags.h->answer))
   prn_header();

/* generate the error matrix, kappa and variance */
 calc_kappa();
}
