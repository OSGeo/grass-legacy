#define GLOBAL
#include "global.h"

int main (int argc, char *argv[])
{

  G_gisinit (argv[0]);

  ask_files ();

  ask_parms ();

  run_cluster ();

  return 0;
}
