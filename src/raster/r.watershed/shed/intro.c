#include "watershed.h"

intro ()
{
  printf("%s provides a text-based user-interface to the %s program.\n", G_program_name(), NON_NAME);
  printf("%s also allows the user to prepare a report of map layers for each\n", G_program_name());
  printf("watershed basin determined in %s.\n\n", NON_NAME);

  printf("%s will help the user determine which options to use for the\n", G_program_name());
  printf("%s program.  %s will then ask for map layers that will be\n", NON_NAME, G_program_name());
  printf("divided by basin. %s will then run %s and create the report.\n", G_program_name(), NON_NAME);
  return (0);
}
