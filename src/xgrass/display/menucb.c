#include "xgdisp.h"

#define ISSPLINE() (Global.drawType == XGD_OPEN_INTERP_SPLINE || \
		    Global.drawType == XGD_CLOSED_INTERP_SPLINE || \
		    Global.drawType == XGD_OPEN_APPROX_SPLINE || \
		    Global.drawType == XGD_CLOSED_APPROX_SPLINE )

typedef struct _delete_map_data {
    Widget interactor;
    int which;
} DeleteMapData;

void
#ifdef _NO_PROTO
TAct(w, cld, cad)
Widget w;
unsigned int cld;
XtPointer cad;
#else
TAct(Widget w, unsigned int cld, XtPointer cad)
#endif
{
  Widget col;
  struct Categories cats;
  static int saveType;
  
  EndFlash();
  
  if (Global.mode == XGD_MODE_POLYRESHAPE){
    XgdPointList *tmp;
    
    DrawPolyPointHandles(Global.selectedObjects->object, Global.xorGC);
    if (Global.selectedObjects->object->type == XGD_FAKE_POLYLINE){
      XgdUnDrawObject(Global.selectedObjects->object,
		      Global.selectedObjects->object->bg,
		      True);
      Global.selectedObjects->object->type = saveType;
      Global.selectedObjects->object->bbox = NULL;
      if (saveType == XGD_POLYLINE || saveType == XGD_POLYGON)
	tmp = Global.selectedObjects->object->Obj.Polyline.pts;
      else
	tmp = Global.selectedObjects->object->Obj.Spline.points;

      for (; tmp; tmp = tmp->next){
	tmp->x += Global.selectedObjects->object->x;
	tmp->y += Global.selectedObjects->object->y;
      }
      XgdConfigureObject(XgdGetGCOfObject(Global.selectedObjects->object),
			 Global.selectedObjects->object,
			 Global.selectedObjects->object->x,
			 Global.selectedObjects->object->y,
			 Global.selectedObjects->object->width,
			 Global.selectedObjects->object->height,
			 True);
      
      XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		    Global.selectedObjects->object, True, NULL);
    }
    XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
  }
  
/*
 ***************************************************************************
 *                        XGD_MODE_MODIFY_COLORS                           *
 ***************************************************************************
 */
  if (cld == XGD_MODE_MODIFY_COLORS){
    if (Global.selectedObjects == NULL){
      XgWarningDialog(Global.applShell, "No GeoFrame selected.");
      return;
    } else if (SelectedObjectCount() != 1){
      XgWarningDialog(Global.applShell, "Select 1 GeoFrame only.");
      return;
    } else if (Global.selectedObjects->object->type != XGD_GEOFRAME){
      XgWarningDialog(Global.applShell, "Selected object must be a GeoFrame.");
      return;
    } else if (Global.selectedObjects->object->Obj.GeoFrame.colormode != XGD_STANDARD_IMAGE) {
      XgWarningDialog(Global.applShell, "You cannot modify a Dithered Image");
      return;
    } else {
      G_read_cats(Global.selectedObjects->object->Obj.GeoFrame.rname,
		  Global.selectedObjects->object->Obj.GeoFrame.rmapset, &cats);
      
      col = XgCreateColormapEditor(Global.selectedObjects->object, w,
				   "Modify Colors", NULL, 0, &cats);
      XtManageChild(col);
    }
/*
 ***************************************************************************
 *                        XGD_MODE_HIGHLIGHT                               *
 ***************************************************************************
 */
  } else if (cld == XGD_MODE_HIGHLIGHT){
    if (Global.selectedObjects == NULL){
      XgWarningDialog(Global.applShell, "No GeoFrame selected.");
      return;
    } else if (SelectedObjectCount() != 1){
      XgWarningDialog(Global.applShell, "Select 1 GeoFrame only.");
      return;
    } else if (Global.selectedObjects->object->type != XGD_GEOFRAME){
      XgWarningDialog(Global.applShell, "Selected object must be a GeoFrame.");
      return;
    } else if (Global.selectedObjects->object->Obj.GeoFrame.colormode != XGD_STANDARD_IMAGE) {
      XgWarningDialog(Global.applShell, "You cannot modify a Dithered Image");
      return;
    }
/*
 ***************************************************************************
 *                        XGD_MODE_POLYRESHAPE                             *
 ***************************************************************************
 */
  } else if (cld == XGD_MODE_POLYRESHAPE){
    if (Global.selectedObjects == NULL){
      XgWarningDialog(Global.applShell, "No object currently selected.");
      return;
    } else if (SelectedObjectCount() != 1){
      XgWarningDialog(Global.applShell, "Select 1 Poly object only.");
      return;
    }
    XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    if (ISSPLINE()){
      saveType = Global.selectedObjects->object->type;
      XgdUnDrawObject(Global.selectedObjects->object,
		      Global.selectedObjects->object->bg, False);
      Global.selectedObjects->object->type = XGD_FAKE_POLYLINE;
      XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object),
		    Global.selectedObjects->object, False, NULL);
    }
    DrawPolyPointHandles(Global.selectedObjects->object, Global.xorGC);
/*
 ***************************************************************************
 *                        XGD_MODE_QUERY_RASTER                            *
 ***************************************************************************
 */
  } else if (cld == XGD_MODE_QUERY_RASTER) {
    if ( Global.selectedObjects == NULL ) {
	XgWarningDialog(Global.applShell, "No object currently selected."); 
	return;
    } else if (SelectedObjectCount() != 1) {
	XgWarningDialog(Global.applShell, "Select 1 GeoFrame object only.");
	return;
    } else if (Global.selectedObjects->object->type == XGD_GEOFRAME) { 
	if (Global.selectedObjects->object->Obj.GeoFrame.rname == NULL || Global.selectedObjects->object->Obj.GeoFrame.rmapset == NULL) {
	XgWarningDialog(Global.applShell, "No raster map on display in GeoFrame.");
	return;
	}
	if ( Global.selectedObjects->object->Obj.GeoFrame.rname != NULL && Global.selectedObjects->object->Obj.GeoFrame.rmapset != NULL) {
	if (Global.qrastpanel == NULL)
	   Global.qrastpanel = CreateQueryRasterPanel();
	else if (XtIsManaged(Global.qrastpanel) == False)
	{
	   UpdateRasterPanel();
	   XtManageChild(Global.qrastpanel);
	}
    }

  }
  }
  
  Global.mode = cld;
  SetMode(cld);
}

void
#ifdef _NO_PROTO
SaveObjectsCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
SaveObjectsCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget xgi;
    Arg al[15];
    int ac = 0;
    XmString xms;
    XmString xms2;

    xms = XmStringCreateSimple("Please enter name of display script");
    if ( yyfilename ) {
        xms2 = XmStringCreateSimple(yyfilename);
        XtSetArg(al[ac], XmNtextString, xms2); ac++;
    }
    XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
    xgi = XgCreateInteractorPromptDialog(Global.applShell, 
        "Save Dialog", al, ac);
    if ( yyfilename ) {
	XmStringFree(xms2);
    }
    XtAddCallback(xgi, XmNokCallback, SaveObjectsOKCallBack, (XtPointer)xgi);
    XtAddCallback(xgi, XmNcancelCallback, XtUnmanageChild, (XtPointer)xgi);
    XmStringFree(xms);
    XtManageChild(xgi);

}

void
#ifdef _NO_PROTO
SaveObjectsOKCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
SaveObjectsOKCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    InteractorCallbackStruct *xgb = (InteractorCallbackStruct *)cad;
    char *file;
    FILE *fp;
    char name[30], mapset[30];

    XmStringGetLtoR(xgb->value,XmSTRING_DEFAULT_CHARSET,&file);

    if ( file == NULL ) return;
    if ( !_XgNameIsFullyQualified(file,name,mapset) ) {
        printf("NAME %s \n", file);
        strcpy(name, file);
        mapset[0] = '\0';
    }
    printf("NAME %s MAPSET %s\n", name, mapset);
    if ( G_find_file("xgd.scripts", name, mapset) != NULL ) {
        char errorbuf[256];

        sprintf(errorbuf, "[%s] exists, overwrite ?", name);
        if ( !XgYesNo(Global.applShell, errorbuf) ) {
            return;
        }
    }
    if ( mapset[0] == '\0' ) strcpy(mapset,G_mapset());
    if ((fp = G_fopen_new("xgd.scripts", name, mapset)) != NULL ) {
        SaveObjects(fp, Global.objectList);
    } else {
        char errorbuf[256];

        sprintf(errorbuf, "Could not open [%s].", name);
        XgWarningDialog(Global.applShell, errorbuf);
    }
}

void 
#ifdef _NO_PROTO
SaveObjects(fp, list)
    FILE *fp;
    ObjectList list;
#else
SaveObjects(FILE *fp, ObjectList list)
#endif
{
    char units[30];
    int count = 0;

    fprintf(fp, "PAGEWIDTH %lf\n", Global.pageWidth);
    fprintf(fp, "PAGEHEIGHT %lf\n", Global.pageHeight);
    switch(Global.units) {
    case XGD_UNITS_MILLI:
        strcpy(units,"millimeters");
         break;
    case XGD_UNITS_INCHES:
        strcpy(units,"inches");
         break;
    case XGD_UNITS_PIXELS:
        strcpy(units,"pixels");
         break;
    }
    fprintf(fp, "UNITS %s\n\n", units);
    while ( list ) {
        XgdObject *o = list->object;

        switch ( o->type ) {
	  case XGD_SQUARE:
	      fprintf(fp, "<OBJECT %d ", count++);
              fprintf(fp, "SQUARE ");
              fprintf(fp, "%d %d %d %d %d %d %d ", 
                  o->x, o->y, o->width, o->height, 
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s ", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, ">\n"); 
              break;
	  case XGD_RECTANGLE:
	      fprintf(fp, "<OBJECT %d ", count++);
              fprintf(fp, "RECTANGLE ");
              fprintf(fp, "%d %d %d %d %d %d %d ", 
                  o->x, o->y, o->width, o->height, 
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s ", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, ">\n"); 
              break;
	  case XGD_CIRCLE:
	      fprintf(fp, "<OBJECT %d ", count++);
              fprintf(fp, "CIRCLE ");
              fprintf(fp, "%d %d %d %d %d %d %d ", 
                  o->x, o->y, o->width, o->height, 
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s ", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, ">\n"); 
              break;
	  case XGD_ELLIPSE:
	      fprintf(fp, "<OBJECT %d ", count++);
              fprintf(fp, "ELLIPSE ");
              fprintf(fp, "%d %d %d %d %d %d %d ", 
                  o->x, o->y, o->width, o->height, 
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s ", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, ">\n"); 
              break;
	  case XGD_GEOFRAME:
              {
		char buf[1024];
		int i;
                struct Cell_head *cellhd;
  
	        fprintf(fp, "<OBJECT %d ", count++);
                fprintf(fp, "GEOFRAME ");
                fprintf(fp, "%d %d %d %d %d %d %d ", 
                    o->x, o->y, o->width, o->height, 
                    o->fp, o->lp, o->lw);
                fprintf(fp, "%s %s \n", XgdGetVectColorNameByPixel(o->fg),
                                      XgdGetVectColorNameByPixel(o->bg));
                fprintf(fp, "\t<GEOFRAME \n");
  
                /* REGION */
                cellhd = &o->Obj.GeoFrame.region;
  
                fprintf(fp, "\t\t<REGION %d %d ", cellhd->proj, cellhd->zone);
                G_format_resolution(cellhd->ew_res, buf, cellhd->proj);
                fprintf(fp, "%s ", buf);
                G_format_resolution(cellhd->ns_res, buf, cellhd->proj);
                fprintf(fp, "%s ", buf);
                G_format_northing(cellhd->north, buf, cellhd->proj);
                fprintf(fp, "%s ", buf);
                G_format_northing(cellhd->south, buf, cellhd->proj);
                fprintf(fp, "%s ", buf);
                G_format_easting(cellhd->east, buf, cellhd->proj);
                fprintf(fp, "%s ", buf);
                G_format_easting(cellhd->west, buf, cellhd->proj);
                fprintf(fp, "%s ", buf);
                fprintf(fp, "%d %d>\n", cellhd->rows, cellhd->cols);

                /* RASTER */
		if ( o->Obj.GeoFrame.rname ) {
		    fprintf(fp, "\t\t<RASTER %s %s>\n", o->Obj.GeoFrame.rmapset,
		        o->Obj.GeoFrame.rname);
		}

                /* VECTOR */
		if (o->Obj.GeoFrame.vects.vect != NULL ) {
		   XgdVectInfo    *v;

		   v = o->Obj.GeoFrame.vects.vect;
		   while ( v) {
		       fprintf(fp, "\t\t<VECTOR %s %s %s %d %d>\n", 
			   v->vmapset, v->vname, 
			   XgdGetVectColorNameByPixel(v->color), 
			   v->linewidth, v->linepattern);
		       v = v->next;
		   }
		}

                /* SITES */
                if (o->Obj.GeoFrame.sites.site != NULL ) {
                    XgdSiteInfo    *s;

                    s = o->Obj.GeoFrame.sites.site;
                    while ( s ) {
                        fprintf(fp, "\t\t<SITE_MAP %s %s\n",
                            s->sname, s->smapset);
                        switch ( s->type ) {
                        case XGD_SITE_PIXMAP:
                            fprintf(fp, 
                                "\t\t\t<PIXMAPSITE \"%s\">\n", 
                                 s->Site.pixdef.pixmapfile);
                            break;
                        case XGD_SITE_FREEHAND:
                            break;
                        case XGD_SITE_STANDARD:
                            fprintf(fp,"\t\t\t<STANDARDSITE %d %d %d \"%s\">\n",
                                s->Site.def.icontype, s->Site.def.size,
                                s->Site.def.width, 
			        XgdGetVectColorNameByPixel(s->Site.def.color));
                            break;
                        }
                        fprintf(fp,"\t\t>\n");
                        s = s->next;
                    }
                }

                /* GRID */
                if ( o->Obj.GeoFrame.gridOn ) {
                   fprintf(fp, "\t\t<GRID ");
		   fprintf(fp, "%d ", o->Obj.GeoFrame.grid.labelon ? 1:0);
		   fprintf(fp, "%8.2f ", o->Obj.GeoFrame.grid.gridgap);
		   fprintf(fp, "%d ", o->Obj.GeoFrame.grid.spacing);
		   fprintf(fp, "%s ", 
		    XgdGetVectColorNameByPixel(o->Obj.GeoFrame.grid.color));
		   fprintf(fp, "%d ", o->Obj.GeoFrame.grid.linepattern);
		   fprintf(fp, "%d\n", o->Obj.GeoFrame.grid.linewidth);
		   if ( o->Obj.GeoFrame.grid.fontname )
		     fprintf(fp, "\t\t\"%s\"\n",o->Obj.GeoFrame.grid.fontname);
		   else
		     fprintf(fp, "\t\t\"%s\"\n",Global.fontName);
		   fprintf(fp, "\t\t%s>\n", 
		    XgdGetVectColorNameByPixel(o->Obj.GeoFrame.grid.textcolor));
                }

                /* LEGEND */
		/* XXX KAB DO THESE !!!! */

                /* BARSCALE */
                for ( i = 0; i < o->Obj.GeoFrame.numbarscales; i++ ) {
                    XgdObject *bsobj = o->Obj.GeoFrame.barscales[i];
                    XgdBarscale *bs = &bsobj->Obj.Barscale;

                    fprintf(fp, "\t\t<BARSCALE ");
                    fprintf(fp, "%d %d %d %d ", 
			bsobj->x, bsobj->y, bsobj->width, bsobj->height);
		    fprintf(fp, "%d %8.2lf ", bs->style, bs->length); 
		    fprintf(fp, "%d %d ", bs->intervals, bs->unit); 
		    fprintf(fp, "%d %s\n", bs->linewidth, 
			XgdGetVectColorNameByPixel(bs->color)); 
		    if ( bs->fontname )
			fprintf(fp, "\t\t\"%s\"\n",bs->fontname);
		    else
			fprintf(fp, "\t\t\"%s\"\n",Global.fontName);

		    fprintf(fp, "\t\t%s>\n", 
		        XgdGetVectColorNameByPixel(bs->textcolor)); 
                }

                /* END GEOFRAME */
                fprintf(fp, "\t>\n>\n"); 
              }
              break;
	  case XGD_POLYLINE:
            {
              XgdPointList *pts = o->Obj.Polyline.pts;

	      fprintf(fp, "<OBJECT %d ", count++);
              fprintf(fp, "POLYLINE ");
              fprintf(fp, "%d %d %d %d %d %d %d ", 
                  o->x, o->y, o->width, o->height, 
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s \n", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, "\t<POLYLINE \n");
              while ( pts ) {
                fprintf(fp, "\t\t(%d,%d)\n", pts->x + o->x, pts->y + o->y);
                pts = pts->next;
              }
              fprintf(fp, "\tNIL >\n");
              fprintf(fp, ">\n"); 
            }
              break;
	  case XGD_POLYGON:
            {
              XgdPointList *pts = o->Obj.Polyline.pts;

              fprintf(fp, "<OBJECT %d ", count++);
              fprintf(fp, "POLYGON ");
              fprintf(fp, "%d %d %d %d %d %d %d ",
                  o->x, o->y, o->width, o->height,
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s \n", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, "\t<POLYGON \n");
              while ( pts ) {
                fprintf(fp, "\t\t(%d,%d)\n", pts->x + o->x, pts->y + o->y);
                pts = pts->next;
              }
              fprintf(fp, "\tNIL >\n");
              fprintf(fp, ">\n");
            }
              break;
	  case XGD_OPEN_INTERP_SPLINE:
	  case XGD_CLOSED_INTERP_SPLINE:
	  case XGD_OPEN_APPROX_SPLINE:
	  case XGD_CLOSED_APPROX_SPLINE:
            {
              XgdPointList *pts = o->Obj.Spline.points;

              fprintf(fp, "<OBJECT %d ", count++);
              switch(o->type) {
	      case XGD_OPEN_INTERP_SPLINE:
		  fprintf(fp, "OPEN_INTERP_SPLINE ");
		  break;
	      case XGD_CLOSED_INTERP_SPLINE:
		  fprintf(fp, "CLOSED_INTERP_SPLINE ");
		  break;
	      case XGD_OPEN_APPROX_SPLINE:
		  fprintf(fp, "OPEN_APPROX_SPLINE ");
		  break;
	      case XGD_CLOSED_APPROX_SPLINE:
		  fprintf(fp, "CLOSED_APPROX_SPLINE ");
		  break;
              }
              fprintf(fp, "%d %d %d %d %d %d %d ",
                  o->x, o->y, o->width, o->height,
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s \n", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, "\t<SPLINE \n");
              while ( pts ) {
                fprintf(fp, "\t\t(%d,%d)\n", pts->x + o->x, pts->y + o->y);
                pts = pts->next;
              }
              fprintf(fp, "\tNIL >\n");
              fprintf(fp, ">\n");
            }
              break;
	  case XGD_LABEL:
            {
              char **labelstr = o->Obj.Label.lblstr;
              int i;

	      fprintf(fp, "<OBJECT %d ", count++);
              fprintf(fp, "LABEL ");
              fprintf(fp, "%d %d %d %d %d %d %d ", 
                  o->x, o->y, o->width, o->height, 
                  o->fp, o->lp, o->lw);
              fprintf(fp, "%s %s \n", XgdGetVectColorNameByPixel(o->fg),
                                    XgdGetVectColorNameByPixel(o->bg));
              fprintf(fp, "\t<LABEL \n");
              fprintf(fp, "\t\t\"");
              for ( i = 0; i < o->Obj.Label.numlines; i++ )  {
                  fprintf(fp, "%s", labelstr[i]);
                  if ( i != o->Obj.Label.numlines - 1 )
                      fprintf(fp, "\n");
              }
              if ( o->Obj.Label.fontname )
		  fprintf(fp, "\"\n\t\t\"%s\"",o->Obj.Label.fontname);
              else
		  fprintf(fp, "\"\n\t\t\"%s\"",Global.fontName);
              fprintf(fp, " >\n");
              fprintf(fp, ">\n"); 
            }
              break;
        }
        list = list->next;
    }
    fclose(fp);
}

void
#ifdef _NO_PROTO
OpenFileOKCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
OpenFileOKCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
  Widget xgb = (Widget)cld;
  XmString xms;
  char   *script = NULL;

  XtVaGetValues(xgb, XmNresultString, &xms, NULL);
  XmStringGetLtoR(xms,XmSTRING_DEFAULT_CHARSET,&script);

  if (script){
    char name[30], mapset[30];
    extern FILE *yyin;

    _XgNameIsFullyQualified(script,name,mapset);
  
    if ( yyfilename ) XtFree(yyfilename);
    yyfilename = XtNewString(script);
    yyin = G_fopen_old("xgd.scripts",name, mapset);
    if ( yyin != NULL ) {
        ObjectList list = Global.selectedObjects;

XmUpdateDisplay(Global.drawArea);
        for ( ; list; list = list->next) {
            XgdDrawResizeHandles(list->object, Global.xorGC);
            DeleteObjectFromList(&Global.selectedObjects, list->object);
        }
	yyparse();
        fclose(yyin);
    } else {
        XgWarningDialog(Global.applShell, "Error reading input file.");
    }
  }

}

void
#ifdef _NO_PROTO
OpenFileCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
OpenFileCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget xgb;
    XmString xms;
    XmString xms2;
    char buf[256];
    Arg al[15];
    int ac = 0;

    xms = XmStringCreateSimple("Please Select A Display Script To Open");
    xms2 = XmStringCreateSimple("xgd.scripts");
    sprintf(buf,"Display Script Browser");
    XtSetArg(al[ac], XmNnumLists, 1); ac++;
    XtSetArg(al[ac], XmNbrowseMode, XG_USER_DEFINED); ac++;
    XtSetArg(al[ac], XmNuserDBElement, xms2); ac++;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT); ac++;
    XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
    xgb = XgCreateBrowserDialog(Global.applShell, buf, al, ac);
    XtManageChild(XgInteractorGetChild(xgb,XmINTERACT_PROMPT_LABEL));
    XtUnmanageChild(XgInteractorGetChild(xgb,
                                         XmINTERACT_APPLY_BUTTON));
    XtAddCallback(xgb, XmNokCallback, OpenFileOKCallBack, (XtPointer)xgb);
    XtAddCallback(xgb, XmNcancelCallback, BrowserCancelCallBack, NULL);
    XmStringFree(xms);
    XtManageChild(xgb);
}

void
#ifdef _NO_PROTO
SiteStandardCallBack(w, cld, call)
     Widget    w;
     XtPointer cld, call;
#else
SiteStandardCallBack(Widget w, XtPointer cld, XtPointer call)
#endif
{
  Global.setSite = XGD_SITE_STANDARD;
}

void
#ifdef _NO_PROTO
PBAct(w, cld, cad)
     Widget w;
     XtPointer cld, cad;
#else
PBAct(Widget w, XtPointer cld, XtPointer cad)
#endif
{
  if ( !strcmp((char *)cld,"Quit")) exit(0);

  if ( strcmp ((char *)cld,"SitesAttr") == NULL) {
	
    if ( Global.stdsitepl == NULL ) {
      Global.stdsitepl = CreateStdSitesPanel();
      return;
    }
    if ( XtIsManaged(Global.stdsitepl) == False ) {
      XtManageChild(Global.stdsitepl);
      return;
    }
  }
  
  if ( strcmp ((char*)cld, "SitePixmap" ) == NULL){
    Global.setSite = XGD_SITE_PIXMAP;
    XgdSelectPixmap();
    return;
  }
}

void
#ifdef _NO_PROTO
SetHighlightColor(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
SetHighlightColor(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Arg al[15];
    int ac = 0;
    Widget xgp;
    XColor xcolor;
    XmString xms;

    xcolor.pixel = Global.highlight;
    XQueryColor(Global.display, Global.cmap, &xcolor);

    xms = XmStringCreateSimple("Accept");

    XtSetArg(al[ac],XmNcolormap, Global.cmap); ac++;
    XtSetArg(al[ac],XmNxColor,&xcolor); ac++;
    XtSetArg(al[ac],XmNscaleTypeMask,XgRGB); ac++;
    XtSetArg(al[ac],XmNenableWorkAreaStretch,True); ac++;
    XtSetArg(al[ac],XmNautoUnmanage, True); ac++;
    XtSetArg(al[ac],XmNokLabelString, xms); ac++;
    XtSetArg(al[ac],XmNcancelRestore,True); ac++;
    xgp = XgCreatePixelDialog(Global.applShell, "Set Highlight Color",al,ac);
/*
    XtAddCallback(xgp, XmNokCallback, SetHighlightColorOk,(XtPointer) xgp);
*/
    XtManageChild(xgp);
}

void
#ifdef _NO_PROTO
DoToolBox(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
DoToolBox(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( Global.toolbox == NULL ) {
        Global.toolbox = CreateToolBox();
    }
    if ( XtIsManaged(Global.toolbox) == False ) {
        XtManageChild(Global.toolbox);
    }
}


void
#ifdef _NO_PROTO
DoGridBox(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
DoGridBox(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( Global.gridbox == NULL ) {
        Global.gridbox = CreateGridToolBox();
	UpdateGridBox();
	return;
    }
    if ( XtIsManaged(Global.gridbox) == False ) {
	UpdateGridBox();
        XtManageChild(Global.gridbox);
	return;
    }

    if ( XtIsManaged(Global.gridbox) == True) 
	UpdateGridBox();
}


void
#ifdef _NO_PROTO
DoBarBox(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
DoBarBox(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( Global.barbox == NULL ) {
        Global.barbox = CreateBarToolBox();
	UpdateBarBox();
	return;
    }
    if ( XtIsManaged(Global.barbox) == False ) {
	UpdateBarBox();
        XtManageChild(Global.barbox);
	return;
    }

    if ( XtIsManaged(Global.barbox) == True )
	UpdateBarBox();

}







void
#ifdef _NO_PROTO
BrowserCancelCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
BrowserCancelCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XtDestroyWidget(w);
}

void
#ifdef _NO_PROTO
BrowserOkCallBack(w, cld, cad)
Widget w;
int cld;
XtPointer cad;
#else
BrowserOkCallBack(Widget w, int cld, XtPointer cad)
#endif
{
  char name[30], mapset[30];
  char *result;
  XmString xms;
  
  XtVaGetValues(w, XmNresultString, &xms, NULL);
  XmStringGetLtoR(xms,XmSTRING_DEFAULT_CHARSET,&result);
  if ( result == NULL ) {
    XgUndoHourGlass(Global.applShell);
    return;
  }
  _XgNameIsFullyQualified(result,name,mapset);
  XgDoHourGlass(Global.applShell);
  if ( cld == XG_RASTER )
    XgdDrawRaster(Global.selectedObjects->object, name, mapset, NULL); 
  else if ( cld == XG_VECTOR ) {
    XgdObject *obj = Global.selectedObjects->object;
    
    XgdInitVector(obj, name, mapset,
		  Global.foreground, Global.linePattern, Global.lineWidth); 
    XgdDrawVector(obj, name, mapset, NULL); 
    XgdDrawObject(XgdGetGCOfObject(obj), obj, True, NULL);
  } else if ( cld == XG_SITE ) {
    Global.sname = (char *)XtMalloc(sizeof (char)*strlen(name) + 1);
    Global.smapset= (char *)XtMalloc(sizeof (char)*strlen(mapset) + 1);
    strcpy (Global.sname, name);  
    strcpy (Global.smapset, mapset);

    
    XgdInitSite(Global.selectedObjects->object,
		name, mapset, Global.setSite);
    XgdUpdateSiteStd(Global.selectedObjects->object->Obj.GeoFrame.sites.Tail,
		     Global.stdsiteattr.icontype,
		     Global.stdsiteattr.size,
		     Global.stdsiteattr.width,
		     Global.stdsiteattr.color);
    XgdDrawSite(Global.selectedObjects->object,
		XtWindow(Global.drawArea), name, mapset,
		Global.sitefile);
  }
  XgUndoHourGlass(Global.applShell);
}

void
#ifdef _NO_PROTO
MapDisplayCallBack(w, cld, cad)
Widget w;
int cld;
XtPointer cad;
#else
MapDisplayCallBack(Widget w, int cld, XtPointer cad)
#endif
{
    Widget xgb;
    XmString xms;
    char buf[256];
    Arg al[15];
    int ac = 0;

    if ( !(SelectedObjectCount() == 1 && 
         Global.selectedObjects->object->type == XGD_GEOFRAME )) {
        if ( SelectedObjectCount() == 1 ) {
          XgWarningDialog(Global.applShell, "Select 1 GeoFrame Only");
        } else {
          XgWarningDialog(Global.applShell, "No Selected GeoFrame");
        }
        return;
    }

    if ( cld == XG_RASTER ) {
	xms = XmStringCreateSimple("Please Select A Raster Map To Display");
	sprintf(buf,"Raster Browser");
    } else if ( cld == XG_VECTOR ) {
	xms = XmStringCreateSimple("Please Select A Vector Map To Display");
	sprintf(buf,"Vector Browser");
    } else if ( cld == XG_SITE ) {
	xms = XmStringCreateSimple("Please Select A Site List To Display");
	sprintf(buf,"Site List Browser");
    }
    XtSetArg(al[ac], XmNnumLists, 1); ac++;
    XtSetArg(al[ac], XmNbrowseMode, cld); ac++;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT); ac++;
    XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
    xgb = XgCreateBrowserDialog(Global.applShell, buf, al, ac);
    XtManageChild(XgInteractorGetChild(xgb,XmINTERACT_PROMPT_LABEL));
    XtUnmanageChild(XgInteractorGetChild(xgb,
                                         XmINTERACT_APPLY_BUTTON));
    XtAddCallback(xgb, XmNokCallback, BrowserOkCallBack, cld);
    XtAddCallback(xgb, XmNcancelCallback, BrowserCancelCallBack, NULL);
    XmStringFree(xms);
    XtManageChild(xgb);
}

void
#ifdef _NO_PROTO
RegionOkCallback(w, cld, cad)
     Widget w;
     XtPointer cld;
     XtPointer cad;
#else
RegionOkCallback( Widget w, XtPointer cld, XtPointer cad)
#endif
{
  char north[64];
  char south[64];
  char east[64];
  char west[64];
  char ns_res[64];
  char ew_res[64];

  G_set_window(&Global.selectedObjects->object->Obj.GeoFrame.region);
  G_format_northing(Global.selectedObjects->object->Obj.GeoFrame.region.north,
		    north,
		    Global.selectedObjects->object->Obj.GeoFrame.region.proj);
  G_format_northing(Global.selectedObjects->object->Obj.GeoFrame.region.south,
		    south,
		    Global.selectedObjects->object->Obj.GeoFrame.region.proj);
  G_format_easting(Global.selectedObjects->object->Obj.GeoFrame.region.east,
		   east,
		   Global.selectedObjects->object->Obj.GeoFrame.region.proj);
  G_format_easting(Global.selectedObjects->object->Obj.GeoFrame.region.west,
		   west,
		   Global.selectedObjects->object->Obj.GeoFrame.region.proj);

  G_format_resolution(Global.selectedObjects->object->Obj.GeoFrame.region.ns_res, 
    ns_res, Global.selectedObjects->object->Obj.GeoFrame.region.proj);
  G_format_resolution(Global.selectedObjects->object->Obj.GeoFrame.region.ew_res, 
    ew_res, Global.selectedObjects->object->Obj.GeoFrame.region.proj);
/*
  fprintf(stderr,"%s[N] %s[S] %s[E] %s %s[NSRES] %s[EWRES]\n",north, south, east, 
    west, ns_res, ew_res);
*/
  
  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
  XgdUnDrawObject(Global.selectedObjects->object,
		  Global.selectedObjects->object->bg, True);
  XgdConfigureObject(XgdGetGCOfObject(Global.selectedObjects->object),
    Global.selectedObjects->object,  Global.selectedObjects->object->x,
     Global.selectedObjects->object->y, Global.selectedObjects->object->width,
     Global.selectedObjects->object->height, True);
  XgdDrawObject(XgdGetGCOfObject(Global.selectedObjects->object), 
     Global.selectedObjects->object, True, NULL);
  XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
}

void
#ifdef _NO_PROTO
RegionCallBack(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
RegionCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Arg al[15];
    int ac = 0;
    Widget xgp;
    XmString pxms;

    if ( !(SelectedObjectCount() == 1 && 
         Global.selectedObjects->object->type == XGD_GEOFRAME )) {
        if ( SelectedObjectCount() == 1 ) {
          XgWarningDialog(Global.applShell, "Select 1 GeoFrame Only");
        } else {
          XgWarningDialog(Global.applShell, "No Selected GeoFrame");
        }
        return;
    }
    XtSetArg(al[ac],XmNgrid,True);ac++;
    XtSetArg(al[ac],XmNeditDefaultRegion,False);ac++;
    XtSetArg(al[ac],XmNcurrentRegion, 
        &Global.selectedObjects->object->Obj.GeoFrame.region); ac++;
    pxms = XmStringCreateSimple("Set Region For Selected GeoFrame");
    XtSetArg(al[ac],XmNpromptLabelString, pxms);ac++;
    XtSetArg(al[ac],XmNenableWorkAreaStretch,True);ac++;
    XtSetArg(al[ac],XmNallowShellResize,True);ac++;
    /*XtSetArg(al[ac],XmNautoUnmanage,True);ac++;*/
    xgp = XgCreateRegionDialog(Global.applShell,"Modify the Region",al,ac);
    XtManageChild(XgInteractorGetChild(xgp,XmINTERACT_PROMPT_LABEL));
    XtAddCallback(xgp,XmNcancelCallback,BrowserCancelCallBack, NULL);
    XtAddCallback(xgp,XmNokCallback,RegionOkCallback, NULL);
    XtManageChild(xgp);
    XmStringFree(pxms);
}

void
#ifdef _NO_PROTO
DeleteRasterCallBack(w, cli, call)
     Widget     w;
     XtPointer  cli;
     XtPointer  call;
#else
DeleteRasterCallBack(Widget w, XtPointer cli, XtPointer call)
#endif
{
  if (SelectedObjectCount() == 1){
    if (Global.selectedObjects->object->type == XGD_GEOFRAME){

      XgWarningDialog(Global.applShell, "No geoframe is currently selected");
    }
  } else {
    XgWarningDialog(Global.applShell, "Please select only 1 Geoframe");
  }
}
    
void    
#ifdef _NO_PROTO
DeleteMapCallBack(w, cli, call)
     Widget     w;
     int        cli;
     XtPointer  call;
#else
DeleteMapCallBack(Widget w, int cli, XtPointer call)
#endif
{
  Widget       list;
  Widget       xgi;
  Widget       ok;
  XmString     xms;
  XgdVectInfo *tmpv;
  XgdSiteInfo *tmps;
  DeleteMapData *data = (DeleteMapData *) XtMalloc (sizeof(DeleteMapData));

  if ( !(SelectedObjectCount() == 1 && 
         Global.selectedObjects->object->type == XGD_GEOFRAME )) {
    if ( SelectedObjectCount() == 1 ) {
      XgWarningDialog(Global.applShell, "Select 1 GeoFrame Only");
    } else {
      XgWarningDialog(Global.applShell, "No Selected GeoFrame");
    }
    return;
  }

  if (cli == XG_RASTER){
    XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    XgdDeleteRaster(Global.selectedObjects->object);
    if (Global.selectedObjects->object->Obj.GeoFrame.legend){
      XgdUnDrawObject(Global.selectedObjects->object,
		      Global.selectedObjects->object->bg, True);
      DeleteObjectFromList(&Global.objectList,
			   Global.selectedObjects->object->Obj.GeoFrame.legend);
      Global.selectedObjects->object->Obj.GeoFrame.legend = NULL;
    }
    XgdRedrawGeoframe(Global.selectedObjects->object, True, True, NULL);
    XgdDrawResizeHandles(Global.selectedObjects->object, Global.xorGC);
    return;
  }

  xgi = XgCreateInteractorListDialog(w, "DeleteVector", NULL, 0); 

  list = XgInteractorGetChild(xgi, XmINTERACT_LIST);

  switch (cli)
    {
    case XGD_VECTOR:
      if (Global.selectedObjects->object->Obj.GeoFrame.vects.vect == NULL){
	XgWarningDialog(Global.applShell,"No Vector Maps currently displayed");
	return;
      }
      for (tmpv=Global.selectedObjects->object->Obj.GeoFrame.vects.vect;
	                                              tmpv; tmpv = tmpv->next){
	xms = XmStringCreateSimple(tmpv->vname);
	XmListAddItem(list, xms, 0);
	XmStringFree(xms);
      }
      break;
    case XGD_SITE:
      if (Global.selectedObjects->object->Obj.GeoFrame.sites.site == NULL){
	XgWarningDialog(Global.applShell,"No Site Maps currently displayed");
	return;
      }
      for (tmps=Global.selectedObjects->object->Obj.GeoFrame.sites.site;
	                                              tmps; tmps = tmps->next){
	xms = XmStringCreateSimple(tmps->sname);
	XmListAddItem(list, xms, 0);
	XmStringFree(xms);
      }
      break;
    default:
      break;
    }

  ok = XgInteractorGetChild(xgi, XmINTERACT_OK_BUTTON);
  bcopy((char *)&xgi, (char *)&data->interactor, sizeof(Widget));
  data->which = cli;
  XtAddCallback(ok, XmNactivateCallback, DeleteMapOKCallBack, (XtPointer)data);
  
  XtManageChild(xgi);
}

void
#ifdef _NO_PROTO
DeleteMapOKCallBack(w, cli, call)
     Widget     w;
     DeleteMapData     *cli;
     XtPointer  call;
#else
DeleteMapOKCallBack(Widget w, DeleteMapData *cli, XtPointer call)
#endif
{
  Widget text;
  char   *map = NULL;
  
  text = XgInteractorGetChild(cli->interactor, XmINTERACT_LIST_TEXT);
  map = XmTextGetString(text);

  if (map){
    XEvent event;

    event.type = Expose;
    event.xexpose.display = Global.display;
    event.xexpose.window = XtWindow(Global.drawArea);

    XSendEvent(Global.display, XtWindow(Global.drawArea), False, ExposureMask,
	       &event);
    XgdDeleteMap(Global.selectedObjects->object, map, cli->which);
    XSendEvent(Global.display, XtWindow(Global.drawArea), False, ExposureMask,
	       &event);
    XgdRedrawGeoframe(Global.selectedObjects->object, True, True, NULL);
  }
}

void
#ifdef _NO_PROTO
WindowDumpCallBack(w, cld, cad)
     Widget w;
     XtPointer cld, cad;
#else
WindowDumpCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget xgi;
    Arg al[15];
    int ac = 0;
    XmString xms;
    XmString xms2;

    xms = XmStringCreateSimple("Please enter name of dump file");
    if ( yyfilename ) {
        xms2 = XmStringCreateSimple(yyfilename);
        XtSetArg(al[ac], XmNtextString, xms2); ac++;
    }
    XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
    xgi = XgCreateInteractorPromptDialog(Global.applShell, 
        "Window Dump Dialog", al, ac);
    if ( yyfilename ) {
	XmStringFree(xms2);
    }
    XtAddCallback(xgi, XmNokCallback, WindowDumpOKCallBack, (XtPointer)xgi);
    XtAddCallback(xgi, XmNcancelCallback, XtUnmanageChild, (XtPointer)xgi);
    XmStringFree(xms);
    XtManageChild(xgi);
}

void
#ifdef _NO_PROTO
WindowDumpOKCallBack(w, cld, cad)
     Widget w;
     XtPointer cld, cad;
#else
WindowDumpOKCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
  InteractorCallbackStruct *xgb = (InteractorCallbackStruct *)cad;
  char *file;
  FILE *fp;
  char name[30], mapset[30];
  
  XmStringGetLtoR(xgb->value,XmSTRING_DEFAULT_CHARSET,&file);

  if ( file == NULL ) return;
  
  if ((fp = fopen(file, "w")) == NULL){
    XgWarningDialog(Global.applShell, "Unable to open dump file");
    return;
  }

  DoWindowDump(fp);
}

