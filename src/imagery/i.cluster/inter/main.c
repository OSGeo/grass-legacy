#define GLOBAL
#include "global.h"

main(argc,argv)
  char *argv[];
{

  G_gisinit (argv[0]);

  ask_files ();

  ask_parms ();

  run_cluster ();

}
