#include "gis.h" 

main(argc,argv)

  int argc;
  char **argv;
{
  
struct Option *opt;

opt=G_define_option();

opt->key="option";
opt->description="Testing options";
opt->type = TYPE_STRING;
opt->required = NO;

if (G_parser(argc,argv)) exit(-1);

printf("For the option \"%s\" you chose <%s>\n", opt->description, opt->answer);

}
