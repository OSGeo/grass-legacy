#include "include.h"

int set_to_add (void)
{
	analysis_type = ADD ;
	fprintf (stdout,"    Analysis will be done by ADDING weights.\n") ;

	return 0;
}

int set_to_mult (void)
{
	analysis_type = MULT ;
	fprintf (stdout,"    Analysis will be done by MULTIPLYING weights.\n") ;

	return 0;
}
