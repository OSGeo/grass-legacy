static char rcsid[] = "@(#)XGRASS $Id: database.c,v 0.0 1992/05/05 14:56:14 sink Exp sink $";
/*
* File: database.c
*
* Desc: contains code for getting database information
*
* Auth: Eric W. Sink
*
* Date: Tue Nov  5 16:21:47 CST 1991
*
* Modification History:
*
*
*/

#include "xgrass_lib.h"
#include <gis.h>
 
extern char *_XgMalloc();

#ifdef _NO_PROTO
int
_XgHasMaps(mapset,type, user_defined)
char *mapset;
int type;
 char *user_defined;
#else
int
_XgHasMaps(char *mapset,int type, char *user_defined)
#endif
{
  char path[512];

  switch (type) {
    case XG_RASTER:
	(void) sprintf(path, "%s/%s/cell", G_location_path(),mapset);
    break;
    case XG_ASCII_DLG:
	(void) sprintf(path, "%s/%s/dlg", G_location_path(),mapset);
    break;
    case XG_DLG:
	(void) sprintf(path, "%s/%s/bdlg", G_location_path(),mapset);
    break;
    case XG_ASCII_VECTOR:
	(void) sprintf(path, "%s/%s/dig_ascii", G_location_path(),mapset);
    break;
    case XG_VECTOR:
	(void) sprintf(path, "%s/%s/dig", G_location_path(),mapset);
    break;
    case XG_SITE:
	(void) sprintf(path, "%s/%s/site_lists", G_location_path(),mapset);
    break;
    case XG_REGION:
	(void) sprintf(path, "%s/%s/windows", G_location_path(),mapset);
    break;
    case XG_ICON:
	(void) sprintf(path, "%s/%s/icons", G_location_path(),mapset);
    break;
    case XG_LABEL:
	(void) sprintf(path, "%s/%s/paint", G_location_path(),mapset);
    break;
    case XG_GROUP:
	(void) sprintf(path, "%s/%s/group", G_location_path(),mapset);
    break;
    case XG_SEGMENT:
	(void) sprintf(path, "%s/%s/seg", G_location_path(),mapset);
    break;
    case XG_USER_DEFINED:
	(void) sprintf(path, "%s/%s/%s", G_location_path(),mapset,user_defined);
    break;
  }
  return _XgDirectoryNotEmpty(path,0,0);
}

#ifdef _NO_PROTO
int
_XgCountMaps(mapset,type,user_defined)
char *mapset;
int type;
char *user_defined;
#else
int
_XgCountMaps(char *mapset,int type, char *user_defined)
#endif
{
  char path[512];
  char **DirList;
  int DirCount;

  switch (type) {
    case XG_RASTER:
	(void) sprintf(path, "%s/%s/cell", G_location_path(),mapset);
    break;
    case XG_ASCII_DLG:
	(void) sprintf(path, "%s/%s/dlg", G_location_path(),mapset);
    break;
    case XG_DLG:
	(void) sprintf(path, "%s/%s/bdlg", G_location_path(),mapset);
    break;
    case XG_ASCII_VECTOR:
	(void) sprintf(path, "%s/%s/dig_ascii", G_location_path(),mapset);
    break;
    case XG_VECTOR:
	(void) sprintf(path, "%s/%s/dig", G_location_path(),mapset);
    break;
    case XG_SITE:
	(void) sprintf(path, "%s/%s/site_lists", G_location_path(),mapset);
    break;
    case XG_REGION:
	(void) sprintf(path, "%s/%s/windows", G_location_path(),mapset);
    break;
    case XG_ICON:
	(void) sprintf(path, "%s/%s/icons", G_location_path(),mapset);
    break;
    case XG_LABEL:
	(void) sprintf(path, "%s/%s/paint", G_location_path(),mapset);
    break;
    case XG_GROUP:
	(void) sprintf(path, "%s/%s/group", G_location_path(),mapset);
    break;
    case XG_SEGMENT:
	(void) sprintf(path, "%s/%s/seg", G_location_path(),mapset);
    break;
    case XG_USER_DEFINED:
	(void) sprintf(path, "%s/%s/%s", G_location_path(),mapset,user_defined);
    break;
  }
  DirList = _XgDirectoryListing(path, &DirCount, 0, 0, 0);

  if (DirCount)
    _XgFreeDirectoryListing(DirList, DirCount);

  return DirCount;
}

#ifdef _NO_PROTO
int
_XgCountMapsetsWithMaps(type, user_defined)
int type;
char *user_defined;
#else
int
_XgCountMapsetsWithMaps(int type, char *user_defined)
#endif
{
  char **DirList;
  int DirCount;
  int result = 0;
  int i;
  DirList = _XgDirectoryListing(G_location_path(),&DirCount,0,0,1);
  for (i=0;i < DirCount;i++) {
    if (_XgHasMaps(DirList[i],type,user_defined)) result++;
  }
  _XgFreeDirectoryListing(DirList,DirCount);
  return result;
}

char **
#ifdef _NO_PROTO
_XgListMapsetsWithMaps(type, user_defined ,count,sort)
int type;
char *user_defined;
int *count;
int sort;
#else
_XgListMapsetsWithMaps(int type,char *user_defined, int *count,int sort)
#endif
{
  char **DirList;
  char **OutList;
  int OutCount;
  int DirCount;
  int result = 0;
  int i;
  DirList = _XgDirectoryListing(G_location_path(),&DirCount,0,sort,1);
  OutCount = DirCount;
  OutList = (char **) _XgMalloc(sizeof(char *) * OutCount);
  OutCount = 0;
  for (i=0;i < DirCount;i++) {
    if (_XgHasMaps(DirList[i],type,user_defined)) {
      OutList[OutCount++] = _XgStrDup(DirList[i]);
    }
  }
  _XgFreeDirectoryListing(DirList,DirCount);
  *count = OutCount;
  return OutList;
}

#ifdef _NO_PROTO
int
_XgCountMapsets()
#else
int
_XgCountMapsets(void)
#endif
{
  char **DirList;
  int DirCount;
  DirList = _XgDirectoryListing(G_location_path(),&DirCount,0,0,1);
  _XgFreeDirectoryListing(DirList,DirCount);
  return DirCount;
}


