/**********************************************************************
   xgrass_lib.h       - xgrass library header file
 *********************************************************************/
#ifndef _XGRASS_LIB_H
#define _XGRASS_LIB_H

#include "std_incs.h"

#define XG_RASTER 1
#define XG_VECTOR 2
#define XG_SITE 3
#define XG_REGION 4
#define XG_ICON 5
#define XG_LABEL 6
#define XG_GROUP 7
#define XG_ASCII_VECTOR 8
#define XG_ASCII_DLG 9
#define XG_DLG 10
#define XG_SEGMENT 11
#define XG_USER_DEFINED 12


#define XG_SINGLE_SELECT 1
#define XG_MULTI_SELECT 2

#define XG_SINGLE_MAP_SELECT 1
#define XG_MULTI_MAP_SELECT 2

#ifdef _NO_PROTO

/* token.c */
char **_XgTokenize();
void _XgFreeTokens();
int _XgNumberOfTokens();

/* strdup.c */
char *_XgStrDup();

/* dirlist.c */
int _XgDirectoryNotEmpty();
char **_XgDirectoryListing();
void _XgFreeDirectoryListing();
void _XgPutDirectoryInList();

/* stringarray.c */
char **_XgCopyStringArray();
int _XgStringArraySearch();
int _XgStringArrayTotalLength();
void _XgSortStringArray();
void _XgFreeStringArray();
XmStringTable _XgStringArray2XmStringTable();

/* editor.c */
Widget XgEditor();

/* editstr.c */
char *XgEditStringDialog();

/* alloc.c */
void _XgFree();
char *_XgMalloc();
char *_XgCalloc();
char *_XgRealloc();

/* user.c */
char *_XgGetUserName();

/* filebrowser.c */
char **XgSelectFile();

/* mapbrowser.c */
char *XgSelectMap();

/* database.c */
char **_XgListMapsetsWithMaps();

/* warning.c */
void XgWarningDialog();

#else /* _NO_PROTO */

/* token.c */
char **_XgTokenize(char *, char *);
void _XgFreeTokens(char **);
int _XgNumberOfTokens(char **);

/* strdup.c */
char *_XgStrDup(char *);

/* dirlist.c */
int _XgDirectoryNotEmpty(char *, Boolean, int);
char **_XgDirectoryListing(char *, int *, Boolean, int, int);
void _XgFreeDirectoryListing(char **,int);
void _XgPutDirectoryInList(char *,Widget,int);

/* stringarray.c */
char **_XgCopyStringArray(char **,int);
int _XgStringArraySearch(char **,int,char *);
int _XgStringArrayTotalLength(char **,int);
void _XgSortStringArray(char **,int,int);
void _XgFreeStringArray(char **,int);

/* editor.c */
Widget XgEditor(Widget parent, char *text, char *title, char *command, Widget *text_ret);

/* editstr.c */
char *XgEditStringDialog(XtAppContext, Widget, char *, char *);

/* alloc.c */
void _XgFree(char *);
char *_XgMalloc(int);
char *_XgCalloc(int, int);
char *_XgRealloc(char *, int);

/* user.c */
char *_XgGetUserName(void);

/* filebrowser.c */
char **XgSelectFile(/* Fill in proto here */);

/* mapbrowser.c */
char *XgSelectMap(/* Fill in proto here */);

/* database.c */
char **_XgListMapsetsWithMaps(int,int *,int);

/* warning.c */
int XgWarningDialog(Widget,char *);

#endif /* _NO_PROTO */

#endif /* _XGRASS_LIB_H */
