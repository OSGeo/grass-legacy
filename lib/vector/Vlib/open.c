#include "Vect.h"
/*

   Routines:    
   Vect_open_old (Map, name, mapset)
   Vect_open_new (Map, name)
   Vect_rewind (Map)
   Vect_close (Map)


   These routines all just call the V# equivalents to pass the function
   off to more level-specific code.
 */


#define MAX_OPEN_LEVEL 2


static int 
dummy ()
{
  return 0;
}

static int Open_level = 0;

/* The line below is here so I can be sure that code that requires version 
   **  4.1 of the library won't compile for 4.0
 */
int Vect__four_point_one;

static int (*Open_array[]) () =
{
  dummy, V1_open_old, V2_open_old
};
static int (*Open_update_array[]) () =
{
  dummy, V1__open_update_1, V2__open_update_1
};


/*
   **  Predetermine level you want to open for read at.  If it can't open
   **   that level, the open will fail.  The specified level must be
   **   set before any call to open.  The default is to try to open
   **   the highest level possible, and keep stepping down until success.
   **
   **  This could potentially be used in the future to modify open_new() also
   **
   **  NOTE!!  This should only be used to set when you wish to force
   **    a lower level open.  If you require a higher level, then just
   **    check the return to verify the level instead of forcing it.
   **    This is because future releases will have higher levels which
   **    will be downward compatible and which your programs should 
   **    support by default.
 */
int 
Vect_set_open_level (int level)
{
  Open_level = level;
  if (Open_level < 1 || Open_level > MAX_OPEN_LEVEL)
    {
      fprintf (stderr, "Warning, Programmer requested unknown open_level\n");
      Open_level = 0;
    }

  return 0;
}

/*
   ** returns Level of openness.   [ 1, 2, (3) ]
   **   and -1 on error
 */
int
Vect_open_old (
		struct Map_info *Map,
		char *name,
		char *mapset)
{
  int level;

  if (Open_level)
    {
      level = Open_level;
      if (0 != (*Open_array[Open_level]) (Map, name, mapset))
	level = -1;
    }
  else
    {
      for (level = MAX_OPEN_LEVEL; level; level--)
	if (0 == (*Open_array[level]) (Map, name, mapset))
	  {
	    goto success;
	  }

      level = -1;
    }

success:

  Open_level = 0;
  return level;
}

/*
   **  Returns level  [ 1 ]  or -1 on error 
 */
int 
Vect_open_new (
		struct Map_info *Map,
		char *name,
		int with_z)
{
  if (Open_level == 2)		/* Unsupported */
    {
      Open_level = 0;
      return V2__open_new_1 (Map, name, with_z);
    }

  if (0 > V1_open_new (Map, name, with_z))
    return -1;

  Open_level = 0;
  return 1;
}

/*
   ** returns Level of openness.   [ 1, 2, (3) ]
   **   and -1 on error
   **
   **  This also causes level 3.x files to be updated to 4.0 on the fly
   **
   **  NOT supported!  Do not use.
 */
int 
Vect__open_update_1 (struct Map_info *Map, char *name)
{
  int level;

  if (Open_level)
    {
      level = Open_level;
      if (0 != (*Open_update_array[Open_level]) (Map, name))
	level = -1;
    }
  else
    {
      for (level = MAX_OPEN_LEVEL; level; level--)
	if (0 == (*Open_update_array[level]) (Map, name))
	  {
	    goto success;
	  }

      level = -1;
    }

success:

  Open_level = 0;
  return level;
}
