
/*
 * FILE: xgreproto.h
 *
 * PROGRAMMER: David M. Johnson
 *
 */

#ifndef __XGREPROTO_H
#define __XGREPROTO_H

/***************/
/* allocolor.c */
/***************/

int AllocColors(
#ifndef _NO_PROTO
   XgdObject *obj;
#endif
);

/****************/
/* applybrush.c */
/****************/

int ApplyBrush(
#ifndef _NO_PROTO
   int ax,
   int ay
#endif
);

/**************/
/* brushcat.c */
/**************/

void BrushCat(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/********************/
/*** brushcolor.c ***/
/********************/

int BrushColor(
#ifdef _NO_PROTO
#endif
);

/*****************/
/* brushcreate.c */
/*****************/

void BrushCreate(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/******************/
/* brushdefault.c */
/******************/

void BuildDefaultBrush(
#ifdef _NO_PROTO
#endif
);

/***************/
/* brushedit.c */
/***************/

void BrushEditCB(
#ifndef _NO_PROTO
   Widget w, 
   XtPointer cld, 
   XtPointer cad
#endif
);

void BrushEdit(
#ifndef _NO_PROTO
#endif
);

/***************/
/* brushload.c */
/***************/

void BrushLoad(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/***************/
/* brushsave.c */
/***************/

void BrushSave(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/***************/
/* buttonbar.c */
/***************/

Widget CreateButtonBar(
#ifndef _NO_PROTO
   Widget shell
#endif
);

/************/
/* cursor.c */
/************/

void DoBrushCursor(
#ifndef _NO_PROTO
   Widget shell
#endif
);

void DoZoomCursor(
#ifndef _NO_PROTO
   Widget shell;
#endif
);

void UndoCursor(
#ifndef _NO_PROTO
   Widget shell
#endif
);

/**************/
/* editcats.c */
/**************/

void EditCats(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/************/
/* editcb.c */
/************/

void EditLockCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void EditVerticalCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void EditHorizontalCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void EditPolygonRegionCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void EditBoxRegionCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void EditUndoCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/***************/
/* editrange.c */
/***************/

void EditCellAddressRange(
#ifndef _NO_PROTO
   int x1,
   int y1,
   int x2,
   int y2
#endif
);

/**************/
/* editundo.c */
/**************/

int EditUndo(
#ifndef _NO_PROTO
#endif
);

int AddToUndoBuffer(
#ifndef _NO_PROTO
   int row, 
   int col, 
   CELL val
#endif
);

void ClearUndoBuffer(
#ifndef _NO_PROTO
#endif
);

/***************/
/* fileclose.c */
/***************/

void FileClose(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void FileRemove(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/**************/
/* fileopen.c */
/**************/

void FileOpen(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/****************/
/* fileresume.c */
/****************/

void FileResume(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/**************/
/* filesave.c */
/**************/

void FileSaveAs(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/*************/
/* handler.c */
/*************/

void HandleExposeEvents(
#ifndef _NO_PROTO
   Widget w, 
   XtPointer cld, 
   XEvent *event, 
   Boolean *dispatch
#endif
);

void HandleMouseEvents(
#ifndef _NO_PROTO
   Widget w, 
   XtPointer cld, 
   XEvent *event, 
   Boolean *dispatch
#endif
);

/*****************/
/* killdialogs.c */
/*****************/

void KillRasterDialogs();

void KillBrushDialogs();

/************/
/* layout.c */
/************/

void CreateLayout(
#ifndef _NO_PROTO
   Widget shell
#endif
);

/***************/
/* loadimage.c */
/***************/

int LoadImage();

void RedrawImage(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

int HighlightImagePixel(
#ifndef _NO_PROTO
   int px,
   int py
#endif
);

/*************/
/* menubar.c */
/*************/

Widget CreateMenuBar(
#ifndef _NO_PROTO
   Widget shell
#endif
);

void CancelCB(
#ifndef _NO_PROTO
   Widget w, 
   XtPointer cld, 
   XtPointer cad
#endif
);

/*************/
/* r_color.c */
/*************/

int read_colors(
#ifndef _NO_PROTO
   char *element,
   char *name,
   char *mapset,
   Struct Colors *colors;
#endif
);
 
/**************/
/* range_rw.c */
/**************/

int read_range(
#ifndef _NO_PROTO
   char *element,
   char *name,
   char *mapset,
   struct Range *range
#endif
);

int write_range(
#ifndef _NO_PROTO
   char *element,
   char *name,
   char *mapset,
   struct Range *range
#endif
);

/*************/
/* ras2seg.c */
/*************/

int RasterToSegment(
#ifndef _NO_PROTO
#endif
);

/*************/
/* seg2ras.c */
/*************/

int SegmentToRaster(
#ifndef _NO_PROTO
#endif
);

/*************/
/* segfile.c */
/*************/

int OpenSegmentFile();

int CloseSegmentFile();

int RemoveSegmentFile();

/***************/
/* viewindex.c */
/***************/

void ViewIndex(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/**************/
/* viewzoom.c */
/**************/

void ViewZoom(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

/***************/
/* updatemsg.c */
/***************/

void UpdatePositionText(
#ifndef _NO_PROTO
   int x,
   int y
#endif
);

void UpdateCategoryText(
#ifndef _NO_PROTO
   int x,
   int y
#endif
);

void UpdateModeText(
#ifndef _NO_PROTO
   int x,
   int y
#endif
);

void UpdateBrushText(
#ifndef _NO_PROTO
   int x,
   int y
#endif
);

/**************/
/* xgrehelp.c */
/**************/

void HelpGeneral( 
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,cad
#endif
);

void HelpModal( 
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,cad
#endif
);

/*****************/
/* zoomhandler.c */
/*****************/

void zoomHandleExposeEvents(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XEvent *event,
   Boolean *dispatch
#endif
);

void zoomHandleMouseEvents(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XEvent *event,
   Boolean *dispatch
#endif
);

#endif /*  __XGREPROTO_H */

