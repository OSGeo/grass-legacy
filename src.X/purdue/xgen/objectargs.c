#include "xgen.h"
#include "widgets/Table.h"

SetObjectGeometryArgs(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;
	int dpyWidth = DisplayWidth(xgenGD.display,xgenGD.screen),
        dpyHeight = DisplayHeight(xgenGD.display,xgenGD.screen);

	if ( NULL != (resource = IndexResource(object,OBJECT,"x"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if ( abs(resource->val.ival) > dpyWidth ) {
            sprintf(errorbuf, "x value in object [%s] out of range, ",
				object->name);
            sprintf(errorbuf,"%sthe screen is only %d pixels wide",
                errorbuf,dpyWidth);
            XgenWarning("set object geometry",errorbuf);
            resource->val.ival = 0;
        }
        XtSetArg(args[*n],XmNx,resource->val.ival); (*n)++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"y"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if ( abs(resource->val.ival) > dpyHeight ) {
            sprintf(errorbuf, "y value in object [%s] out of range, ",
				object->name);
            sprintf(errorbuf,"%sthe screen is only %d pixels tall",
                errorbuf,dpyHeight);
            XgenWarning("set object geometry",errorbuf);
            resource->val.ival = 0;
        }
        XtSetArg(args[*n],XmNy,resource->val.ival); (*n)++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"width"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[*n],XmNwidth,resource->val.ival); (*n)++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"height"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[*n],XmNheight,resource->val.ival); (*n)++;
    }
}


SetObjectColorArgs(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;
	XColor color;

	if ( object != NULL && 
		 NULL != (resource = IndexResource(object,OBJECT,"foreground"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNforeground,color.pixel); (*n)++;
		} else {

			sprintf(errorbuf,"invalid foreground color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_fg ) {
        XtSetArg(args[*n],XmNforeground,xgenGD.g_fgs.pixel); (*n)++;
    }
	if ( object != NULL &&
		 NULL != (resource = IndexResource(object,OBJECT,"background"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNbackground,color.pixel); (*n)++;
		} else {

			sprintf(errorbuf,"invalid background color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_bg ) {
        XtSetArg(args[*n],XmNbackground,xgenGD.g_bgs.pixel); (*n)++;
    }
}

SetTableObjectColorArgs(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;
	XColor color;

	if ( xgenGD.g_fg ) {
        XtSetArg(args[*n],XmNtitleFontColor,xgenGD.g_fgs.pixel); (*n)++;
        XtSetArg(args[*n],XmNcolumnHeadingFontColor,xgenGD.g_fgs.pixel); (*n)++;
        XtSetArg(args[*n],XmNrowHeadingFontColor,xgenGD.g_fgs.pixel); (*n)++;
    }
}

SetObjectReverseColorArgs(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;
	XColor color;

	if ( object != NULL && 
		 NULL != (resource = IndexResource(object,OBJECT,"foreground"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNbackground,color.pixel); (*n)++;
		} else {

			sprintf(errorbuf,"invalid foreground color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_fg ) {
        XtSetArg(args[*n],XmNbackground,xgenGD.g_fgs.pixel); (*n)++;
    }
	if ( object != NULL &&
		 NULL != (resource = IndexResource(object,OBJECT,"background"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNforeground,color.pixel); (*n)++;
		} else {

			sprintf(errorbuf,"invalid background color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_bg ) {
        XtSetArg(args[*n],XmNforeground,xgenGD.g_bgs.pixel); (*n)++;
    }
}

SetObjectFont(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;
	
	if ( NULL != (resource = IndexResource(object,OBJECT,"font"))) {
        XFontStruct *fs = XLoadQueryFont(xgenGD.display,resource->val.cval);
        if ( fs ) {
            XtSetArg(args[*n],XmNfontList,XmFontListCreate(fs,SDC)); (*n)++;
        } else {
            sprintf(errorbuf,"couldn't load font: %s",resource->val.cval);
            XgenWarning("set object font",errorbuf);
        }
    }
}

SetObjectAlignmentArgs(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;

	if ( NULL != (resource = IndexResource(object,OBJECT,"alignment"))) {
		if ( resource->variable ) ExpandVariable(resource);
        if ( !strcmp(resource->val.cval,"left") ) {
            XtSetArg(args[*n],XmNalignment,XmALIGNMENT_BEGINNING); (*n)++;
        } else if ( !strcmp(resource->val.cval,"right") ) {
            XtSetArg(args[*n],XmNalignment,XmALIGNMENT_END); (*n)++;
        } else if ( !strcmp(resource->val.cval,"center") ) {
            XtSetArg(args[*n],XmNalignment,XmALIGNMENT_CENTER); (*n)++;
        } else {

            sprintf(errorbuf,"invalid alignment in label \"%s\"",object->name);
            XgenWarning("create label object",errorbuf);
        }
    } else {
		XtSetArg(args[*n],XmNalignment,XmALIGNMENT_BEGINNING); (*n)++;
	}
}


SetGlobalArgs(n,fonts)
	int *n;
	int fonts;
{
	if ( fonts && xgenGD.g_font ) {
        XtSetArg(args[*n],XmNfontList,XmFontListCreate(xgenGD.g_fs,SDC));(*n)++;
    }
    if ( xgenGD.g_bgpix ) {
        XtSetArg(args[*n],XmNbackgroundPixmap,xgenGD.g_bgpm); (*n)++;
    }
}



SetShellColorArgs(shell,n)
	Shell *shell;
	int *n;
{
	Resource *resource;
	XColor color;

	if ( shell != NULL && 
		 NULL != (resource = IndexResource(shell,SHELL,"foreground"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {

                sprintf(errorbuf,"couldn't allocate color %s in shell %s",
						resource->val.cval,shell->name);
				XgenWarning("set shell color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNforeground,color.pixel); (*n)++;
		} else {

			sprintf(errorbuf,"invalid foreground color %s in shell %s",
					resource->val.cval,shell->name);
			XgenWarning("set shell color",errorbuf);
		}
    } else if ( xgenGD.g_fg ) {
        XtSetArg(args[*n],XmNforeground,xgenGD.g_fgs.pixel); (*n)++;
    }
	if ( shell != NULL &&
		 NULL != (resource = IndexResource(shell,SHELL,"background"))) {
		if ( resource->variable ) ExpandVariable(resource);
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {

                sprintf(errorbuf,"couldn't allocate color %s in shell %s",
						resource->val.cval,shell->name);
				XgenWarning("set shell color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNbackground,color.pixel); (*n)++;
		} else {

			sprintf(errorbuf,"invalid background color %s in shell %s",
					resource->val.cval,shell->name);
			XgenWarning("set shell color",errorbuf);
		}
    } else if ( xgenGD.g_bg ) {
        XtSetArg(args[*n],XmNbackground,xgenGD.g_bgs.pixel); (*n)++;
    }
}
