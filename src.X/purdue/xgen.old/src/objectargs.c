#include "xgen.h"

SetObjectGeometryArgs(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;

	if ( NULL != (resource = IndexResource(object,OBJECT,"x"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[*n],XmNx,resource->val.ival); (*n)++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"y"))) {
		if ( resource->variable ) ExpandVariable(resource);
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
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {
                char errorbuf[80];

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNforeground,color.pixel); (*n)++;
		} else {
			char errorbuf[80];

			sprintf(errorbuf,"invalid foreground color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_fg ) {
        XtSetArg(args[*n],XmNforeground,xgenGD.g_fgs.pixel); (*n)++;
    }
	if ( object != NULL &&
		 NULL != (resource = IndexResource(object,OBJECT,"background"))) {
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {
                char errorbuf[80];

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNbackground,color.pixel); (*n)++;
		} else {
			char errorbuf[80];

			sprintf(errorbuf,"invalid background color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_bg ) {
        XtSetArg(args[*n],XmNbackground,xgenGD.g_bgs.pixel); (*n)++;
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
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {
                char errorbuf[80];

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNbackground,color.pixel); (*n)++;
		} else {
			char errorbuf[80];

			sprintf(errorbuf,"invalid foreground color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_fg ) {
        XtSetArg(args[*n],XmNbackground,xgenGD.g_fgs.pixel); (*n)++;
    }
	if ( object != NULL &&
		 NULL != (resource = IndexResource(object,OBJECT,"background"))) {
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {
                char errorbuf[80];

                sprintf(errorbuf,"couldn't allocate color %s in object %s",
						resource->val.cval,object->name);
				XgenWarning("set object color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNforeground,color.pixel); (*n)++;
		} else {
			char errorbuf[80];

			sprintf(errorbuf,"invalid background color %s in object %s",
					resource->val.cval,object->name);
			XgenWarning("set object color",errorbuf);
		}
    } else if ( xgenGD.g_bg ) {
        XtSetArg(args[*n],XmNforeground,xgenGD.g_bgs.pixel); (*n)++;
    }
}

SetObjectAlignmentArgs(object,n)
	InterfaceObject *object;
	int *n;
{
	Resource *resource;

	if ( NULL != (resource = IndexResource(object,OBJECT,"alignment"))) {
        if ( !strcmp(resource->val.cval,"left") ) {
            XtSetArg(args[*n],XmNalignment,XmALIGNMENT_BEGINNING); (*n)++;
        } else if ( !strcmp(resource->val.cval,"right") ) {
            XtSetArg(args[*n],XmNalignment,XmALIGNMENT_END); (*n)++;
        } else if ( !strcmp(resource->val.cval,"center") ) {
            XtSetArg(args[*n],XmNalignment,XmALIGNMENT_CENTER); (*n)++;
        } else {
            char errorbuf[80];

            sprintf(errorbuf,"invalid alignment in label \"%s\"",object->name);
            XgenWarning("create label object",errorbuf);
        }
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
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {
                char errorbuf[80];

                sprintf(errorbuf,"couldn't allocate color %s in shell %s",
						resource->val.cval,shell->name);
				XgenWarning("set shell color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNforeground,color.pixel); (*n)++;
		} else {
			char errorbuf[80];

			sprintf(errorbuf,"invalid foreground color %s in shell %s",
					resource->val.cval,shell->name);
			XgenWarning("set shell color",errorbuf);
		}
    } else if ( xgenGD.g_fg ) {
        XtSetArg(args[*n],XmNforeground,xgenGD.g_fgs.pixel); (*n)++;
    }
	if ( shell != NULL &&
		 NULL != (resource = IndexResource(shell,SHELL,"background"))) {
		if (XParseColor(xgenGD.display,xgenGD.cmap,
						resource->val.cval,&color)!=0){
			if(XAllocColor(xgenGD.display,xgenGD.cmap,&color) == 0) {
                char errorbuf[80];

                sprintf(errorbuf,"couldn't allocate color %s in shell %s",
						resource->val.cval,shell->name);
				XgenWarning("set shell color",errorbuf);
            } else
        		XtSetArg(args[*n],XmNbackground,color.pixel); (*n)++;
		} else {
			char errorbuf[80];

			sprintf(errorbuf,"invalid background color %s in shell %s",
					resource->val.cval,shell->name);
			XgenWarning("set shell color",errorbuf);
		}
    } else if ( xgenGD.g_bg ) {
        XtSetArg(args[*n],XmNbackground,xgenGD.g_bgs.pixel); (*n)++;
    }
}
