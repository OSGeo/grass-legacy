#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "G3d_intern.h"
#include "g3dkeys.h"

/*---------------------------------------------------------------------------*/

static int
G3d_readWriteWindow (windowKeys, doRead,
		     proj, zone,
		     north, south, east, west, top, bottom,
		     rows, cols, depths, ew_res, ns_res, tb_res)

     struct Key_Value *windowKeys;
     int doRead;
     int *proj, *zone;
     double *north, *south, *east, *west, *top, *bottom;
     int *rows, *cols, *depths;
     double *ew_res, *ns_res, *tb_res;

{
  int returnVal;
  int (*windowInt) (), (*windowDouble) ();

  if (doRead) {
    windowDouble = G3d_keyGetDouble;
    windowInt = G3d_keyGetInt;
  } else {
    windowDouble = G3d_keySetDouble;
    windowInt = G3d_keySetInt;
  }

  returnVal = 1;
  returnVal &= windowInt (windowKeys, G3D_REGION_PROJ, proj);
  returnVal &= windowInt (windowKeys, G3D_REGION_ZONE, zone);

  returnVal &= windowDouble (windowKeys, G3D_REGION_NORTH, north);
  returnVal &= windowDouble (windowKeys, G3D_REGION_SOUTH, south);
  returnVal &= windowDouble (windowKeys, G3D_REGION_EAST, east );
  returnVal &= windowDouble (windowKeys, G3D_REGION_WEST, west);
  returnVal &= windowDouble (windowKeys, G3D_REGION_TOP, top);
  returnVal &= windowDouble (windowKeys, G3D_REGION_BOTTOM, bottom);

  returnVal &= windowInt (windowKeys, G3D_REGION_ROWS, rows);
  returnVal &= windowInt (windowKeys, G3D_REGION_COLS, cols);
  returnVal &= windowInt (windowKeys, G3D_REGION_DEPTHS, depths);

  returnVal &= windowDouble (windowKeys, G3D_REGION_EWRES, ew_res);
  returnVal &= windowDouble (windowKeys, G3D_REGION_NSRES, ns_res);
  returnVal &= windowDouble (windowKeys, G3D_REGION_TBRES, tb_res);

  if (returnVal) return 1;

  G3d_error ("G3d_readWriteWindow: error writing window");
  return 0;
}

/*---------------------------------------------------------------------------*/

static void
G3d_getFullWindowPath (path, windowName)
     
     char path[1024];
     char *windowName;

{
  char xname[512], xmapset[512];

  if (windowName == NULL) {
    G__file_name (path, "", G3D_WINDOW_ELEMENT, G_mapset ());
    return;
  }

  while (*windowName == ' ') windowName++;
  
  if ((*windowName == '/') || (*windowName == '.')) {
    sprintf (path, windowName);
    return;
  }
    
  if (G__name_is_fully_qualified (windowName, xname, xmapset)) {
    G__file_name (path, G3D_WINDOW_DATABASE, xname, xmapset);
    return;
  }

  G__file_name (path, G3D_WINDOW_DATABASE, windowName, G_mapset ());
}

/*---------------------------------------------------------------------------*/

static void
G3d_getWindowLocation (path, windowName)
     
     char path[1024];
     char *windowName;

{
  char xname[512], xmapset[512];
  char *p, *slash;

  if (windowName == NULL) {
    G__file_name (path, "", "", G_mapset ());
    return;
  }

  while (*windowName == ' ') windowName++;
  
  if ((*windowName != '/') && (*windowName != '.')) {
    if (G__name_is_fully_qualified (windowName, xname, xmapset)) 
      G__file_name (path, G3D_WINDOW_DATABASE, xname, xmapset);
    else
      G__file_name (path, G3D_WINDOW_DATABASE, windowName, G_mapset ());
  } else
    sprintf (path, windowName);
  p = path;
  slash = NULL;
  while (*p != 0) {
    if (*p == '/') slash = p;
    p++;
  }
  if (slash != NULL) *slash = 0;
}

/*---------------------------------------------------------------------------*/

int
G3d_readWindow (window, windowName)

     G3D_Region *window;
     char *windowName;

{
  struct Key_Value *windowKeys;
  char path[1024], msg[1024];
  int status, returnVal;

  G3d_getFullWindowPath (path, windowName);

  if (windowName == NULL) {
    if (access(path, R_OK) != 0) {
      sprintf (msg,"G3d_readWindow: unable to find [%s], using default.", path);
      G_warning (msg);
      
      G__file_name (path, "", G3D_DEFAULT_WINDOW_ELEMENT, G3D_PERMANENT_MAPSET);
      if (access(path, R_OK) != 0) {
	sprintf (msg,"G3d_readWindow: unable to find [%s].", path);
	G_warning (msg);
	
	return 0;
      }
    }
  } else 
    if (access(path, R_OK) != 0) {
      sprintf (msg,"G3d_readWindow: unable to find [%s].", path);
      G_warning (msg);
	
      return 0;
    }
  
  windowKeys = G_read_key_value_file (path, &status);
  if (status != 0) {
    sprintf (msg, "G3d_readWindow: Unable to open %s", path);
    G3d_error (msg);
    return 0;
  }

  if (! G3d_readWriteWindow (windowKeys, 1, 
			     &(window->proj), &(window->zone),
			     &(window->north), &(window->south), 
			     &(window->east), &(window->west), &(window->top),
			     &(window->bottom), &(window->rows), 
			     &(window->cols), &(window->depths),
			     &(window->ew_res), &(window->ns_res), 
			     &(window->tb_res))) {
    sprintf (msg, "G3d_readWindow: error extracting window key(s) of file %s",
	     path);
    G3d_error (msg);
    return 0;
  }

  G_free_key_value (windowKeys);
  return 1;
}

/*---------------------------------------------------------------------------*/
/* modified version of G__make_mapset_element */

static int
G3d_createPath (thePath)

     char *thePath;
     
{
  char command[1024];
  char *path, *p, *pOld;

  if (*thePath == 0) return 0;

  strcpy (path = command, "mkdir ");
  while (*path) path++;
  p = path;

  /* now append element, one directory at a time, to path */
  while (1) {
    if (*thePath == '/') *p++ = *thePath++;
    pOld = p;
    while ((*thePath) && (*thePath != '/')) *p++ = *thePath++;
    *p = 0;

    if (p == pOld) return 1;

    if (access (path, 0) != 0) mkdir (path,0777);
    if (access (path, 0) != 0) system (command);
    if (access (path, 0) != 0) {
      char err[1024];
      sprintf (err, "can't make mapset element %s (%s)", thePath, path);
      G_fatal_error (err);
      exit(1);
    }
  }
}

/*---------------------------------------------------------------------------*/

int
G3d_writeWindow (window, windowName)

     G3D_Region *window;
     char *windowName;

{
  struct Key_Value *windowKeys;
  char path[1024], msg[1024];
  int status;

  windowKeys = G_create_key_value();

  if (! G3d_readWriteWindow (windowKeys, 0, 
			     &(window->proj), &(window->zone),
			     &(window->north), &(window->south), 
			     &(window->east), &(window->west), &(window->top),
			     &(window->bottom), &(window->rows), 
			     &(window->cols), &(window->depths),
			     &(window->ew_res), &(window->ns_res), 
			     &(window->tb_res))) {
    sprintf (msg, "G3d_writeWindow: error adding window key(s) for file %s", 
	     path);
    G3d_error (msg);
    return 0;
  }

  G3d_getWindowLocation (path, windowName);
  G3d_createPath (path);

  G3d_getFullWindowPath (path, windowName);
  G_write_key_value_file (path, windowKeys, &status);

  G_free_key_value(windowKeys);

  if (status == 0) return 1;

  sprintf (msg, "G3d_writeWindow: error writing window file %s", path);
  G3d_error (msg);
  return 0;
}
  
/*---------------------------------------------------------------------------*/

void
G3d_useWindowParams ()

{
  G3d_setWindowParams ();
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
