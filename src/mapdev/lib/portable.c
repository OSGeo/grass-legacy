
/*
**
**   This file is one of two that can be loaded.  The flag CERL_PORTABLE
**   should *NOT* be defined unless you know what you are doing
**   See the file README_386 in this directory for more information.
*/
#ifdef CERL_PORTABLE
#include "./xportable.c"
#else


  /*   nothing */


/* to shut up ranlib, and possibly avert a tragedy down the road: */
static int x; 


#endif
