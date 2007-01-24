
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:	Unit and Integration tests
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#ifndef _N_TEST_GPDE_H_
#define _N_TEST_PDE_H_


#define TEST_N_NUM_ROWS 10
#define TEST_N_NUM_COLS 10
#define TEST_N_NUM_DEPTHS 10

/* Array test functions */
int unit_test_arrays ();

/* matrix assembling */
int unit_test_assemble();

/* direct and iterative solvers */
int unit_test_solvers();

/* les creation */
int unit_test_les_creation ();

/*gwflow*/
int integration_test_gwflow ();

#endif
