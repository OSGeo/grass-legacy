
/****************************************************************************
*
* MODULE:       test.gpde.lib
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert <at> gmx <dot> de
* 		27 11 2006 Berlin
*
* PURPOSE:      Unit and integration tests for the gpde library
*
* COPYRIGHT:    (C) 2006 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/N_pde.h>
#include "test_gpde_lib.h"


/*- Parameters and global variables -----------------------------------------*/
typedef struct
{
  struct Option *unit, *integration;
  struct Flag *full, *testunit, *testint;
} paramType;

paramType param;		/*Parameters */

/*- prototypes --------------------------------------------------------------*/
void set_params ();		/*Fill the paramType structure */

/* ************************************************************************* */
/* Set up the arguments we are expecting ********************************** */
/* ************************************************************************* */
void
set_params ()
{
  param.unit = G_define_option ();
  param.unit->key = "unit";
  param.unit->type = TYPE_STRING;
  param.unit->required = NO;
  param.unit->options = "les,array,solver,assemble";
  param.unit->description = _("Choose the unit tests to run");

  param.integration = G_define_option ();
  param.integration->key = "integration";
  param.integration->type = TYPE_STRING;
  param.integration->required = NO;
  param.integration->options = "gwflow,heatflow,transport";
  param.integration->description = _("Choose the integration tests to run");


  param.testunit = G_define_flag ();
  param.testunit->key = 'u';
  param.testunit->description = _("Run all unit tests");

  param.testint = G_define_flag ();
  param.testint->key = 'i';
  param.testint->description = _("Run all integration tests");

  param.full = G_define_flag ();
  param.full->key = 'a';
  param.full->description = _("Run all unit and integration tests");

}

/* ************************************************************************* */
/* Main function, open the G3D map and create the raster maps ************** */
/* ************************************************************************* */
int
main (int argc, char *argv[])
{
  struct GModule *module;
  int returnstat = 0, i;

  /* Initialize GRASS */
  G_gisinit (argv[0]);

  module = G_define_module ();
  module->keywords = _("test, gpde");
  module->description = _("Performs unit and integration tests for gpde library");

  /* Get parameters from user */
  set_params ();

  /* Have GRASS get pheads */
  if (G_parser (argc, argv))
    exit (EXIT_FAILURE);


  /*Run the unit tests */
  if (param.testunit->answer || param.full->answer)
    {
      returnstat += unit_test_arrays ();
      returnstat += unit_test_assemble ();
      returnstat += unit_test_les_creation ();
      returnstat += unit_test_solvers ();

    }

  /*Run the integration tests */
  if (param.testint->answer || param.full->answer)
    {
      returnstat += integration_test_gwflow ();
    }

  /*Run single tests */
  if (!param.full->answer)
    {
      /*unit tests */
      if (!param.testunit->answer)
	{
	  i = 0;
	  if(param.unit->answers)
	  while (param.unit->answers[i])
	    {
	      if (strcmp (param.unit->answers[i], "assemble") == 0)
		returnstat += unit_test_assemble ();

	      if (strcmp (param.unit->answers[i], "array") == 0)
		returnstat += unit_test_arrays ();

	      if (strcmp (param.unit->answers[i], "les") == 0)
		returnstat += unit_test_les_creation ();

	      if (strcmp (param.unit->answers[i], "solver") == 0)
		returnstat += unit_test_solvers ();

	      i++;
	    }
	}
      /*integration tests */
      if (!param.testint->answer)
	{
	  i = 0;
	  if(param.integration->answers)
	  while (param.integration->answers[i])
	    {
	      if (strcmp (param.integration->answers[i], "gwflow") == 0)
		returnstat += integration_test_gwflow ();

	      if (strcmp (param.integration->answers[i], "heatflow") == 0)
		;		/*nothing to do for now */

	      if (strcmp (param.integration->answers[i], "transport") == 0)
		;		/*nothing to do for now */


	      i++;
	    }

	}
    }

  return (returnstat);
}
