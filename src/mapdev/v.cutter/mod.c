/**** mod.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


/*
**  Attempt to create a modulus function which works
**   for a ring structure like a polygon, which returns to 
**   its beginning.
**   thus negative numbers wrap around to high positives
*
**  e.g.:
**         2 % 3 == 2
**         1 % 3 == 1
**         0 % 3 == 0
**        -1 % 3 == 2
**        -2 % 3 == 1
**        -3 % 3 == 0
**        -4 % 3 == 2
**
**  It turned out to be a little more tricky than I expected
*/
int
ring_mod (a, b)
    int a, b;
{
    if (a >= 0)
	return a % b;

    if (b < 0)		/* not sure what -b means */
	return a % b;

    a = abs (a) % b;

    if (!a) 
	return 0;
    else
	return b - a;
}
