#include <stdio.h>
#include <math.h>
#include <string.h>
#include "viewstruct.h"
#include "gis.h"

int draw();

typedef struct LOOKUP 
{
	int r,g,b;
	CELL cell;
} lookup;


char *grassdir[NBTOKEN] = 
{
	"meta",
	"dig",
	"dig",
	"dig",
	"site_lists",
	"cell",
	"cell",
	"paint/labels", 
	"",
	"",
	""
};

char *keywords[NBTOKEN] = 
{
	"meta",
	"polyline",
	"polygon",
	"point",
	"site",
	"raster",
	"oraster",
	"label",
	"grid",
	"legend",
	"scale"
};

	
Tk_ConfigSpec configPolyline[] =
{
	{ TK_CONFIG_STRING,	"-name", "name", "Name", "", Tk_Offset(LAYER,name), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING,	"-mapset", "mapset", "Mapset", "", Tk_Offset(LAYER,mapset), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_UID,	"-color", "color", "Color", "white", Tk_Offset(LAYER,color), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_INT,	"-width", "width", "Width", "0", Tk_Offset(LAYER,lineWidth), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING,	"-dash", "dash", "Dash", NULL, Tk_Offset(LAYER,dash), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_BITMAP,	"-fill", "fill", "Fill",  None, Tk_Offset(LAYER,symbol), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-downscale", "downscale", "Downscale", MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};

Tk_ConfigSpec configGrid[] =
{
	{ TK_CONFIG_DOUBLE,	"-unit", "unit", "Unit", "10000.0", Tk_Offset(LAYER,grid), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_UID,	"-color", "color", "Color", "white", Tk_Offset(LAYER,color), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_INT,	"-width", "width", "Width", "0", Tk_Offset(LAYER,lineWidth), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING,	"-dash", "dash", "Dash", NULL, Tk_Offset(LAYER,dash), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_BITMAP,	"-fill", "fill", "Fill",  None, Tk_Offset(LAYER,symbol), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-downscale", "downscale", "Downscale", MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};
	
Tk_ConfigSpec configPolygone[] =
{
	{ TK_CONFIG_STRING,	"-name", "name", "Name" ,"", Tk_Offset(LAYER,name), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING,	"-mapset", "mapset", "Mapset", "", Tk_Offset(LAYER,mapset), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_UID,	"-color", "color", "Color", "white", Tk_Offset(LAYER,color), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_INT,	"-cat", "cat", "Cat", "-1", Tk_Offset(LAYER,cat), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_BITMAP,	"-fill", "fill", "Fill",  None, Tk_Offset(LAYER,symbol), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-downscale", "downscale", "Downscale", MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-upscale", "upscale", "Upscale",  MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};
	
Tk_ConfigSpec configSites[] =
{
	{ TK_CONFIG_STRING,	"-name", "name","Name", "", Tk_Offset(LAYER,name), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING,	"-mapset", "mapset", "Mapset",  "", Tk_Offset(LAYER,mapset), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_UID,	"-color", "color", "Color", "white", Tk_Offset(LAYER,color), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_BITMAP,	"-bitmap", "bitmap", "Bitmap",  "@dot", Tk_Offset(LAYER,symbol), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_FONT,	"-font", "font", "Font", NULL, Tk_Offset(LAYER,font), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-downscale", "downscale", "Downscale", MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};

Tk_ConfigSpec configLegend[] =
{
	{ TK_CONFIG_WINDOW,	"-window", "window","Window", NULL, Tk_Offset(LAYER,window), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_UID,	"-color", "color", "Color", "white", Tk_Offset(LAYER,color), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_FONT,	"-font", "font", "Font", NULL, Tk_Offset(LAYER,font), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-downscale", "downscale", "Downscale", MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};

Tk_ConfigSpec configPoints[] =
{
	{ TK_CONFIG_STRING,	"-name", "name","Name", "", Tk_Offset(LAYER,name), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING,	"-mapset", "mapset", "Mapset",  "", Tk_Offset(LAYER,mapset), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_UID,	"-color", "color", "Color", "white", Tk_Offset(LAYER,color), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_BITMAP,	"-bitmap", "bitmap", "Bitmap",  "@dot", Tk_Offset(LAYER,symbol), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-downscale", "downscale", "Downscale", MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE,	"-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};
	
Tk_ConfigSpec configGeneric[] =
{
	{ TK_CONFIG_STRING, "-name", "name", "Name", "", Tk_Offset(LAYER,name), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING, "-mapset", "mapset", "Mapset", "", Tk_Offset(LAYER,mapset), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE, "-downscale", "downscale", "Downscale",  MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE, "-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};

Tk_ConfigSpec configRaster[] =
{
	{ TK_CONFIG_STRING, "-name", "name", "Name", "", Tk_Offset(LAYER,name), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING, "-mapset", "mapset", "Mapset", "", Tk_Offset(LAYER,mapset), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_INT,	"-mode", "mode", "Mode", "1", Tk_Offset(LAYER,colormode), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE, "-downscale", "downscale", "Downscale",  MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE, "-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};
	
Tk_ConfigSpec configLabel[] =
{
	{ TK_CONFIG_STRING, "-name", "name", "Name", "", Tk_Offset(LAYER,name), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_STRING, "-mapset", "mapset", "Mapset", "", Tk_Offset(LAYER,mapset), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_FONT,	"-font", "font", "Font", "*-Helvetica-medium-r-normal--*-80-*", Tk_Offset(LAYER,font), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_UID,	"-color", "color", "Color", "white", Tk_Offset(LAYER,color), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE, "-downscale", "downscale", "Downscale",  MINSCALE, Tk_Offset(LAYER,scaleDownThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_DOUBLE, "-upscale", "upscale", "Upscale", MAXSCALE, Tk_Offset(LAYER,scaleUpThreshold), 0, (Tk_CustomOption *) NULL },
	{ TK_CONFIG_END,	(char *) NULL, (char *) NULL, (char *) NULL, (char *) NULL, 0, 0, (Tk_CustomOption *) NULL }
};

Tk_ConfigSpec *configLayer[NBTOKEN] =
{
	(Tk_ConfigSpec *) configGeneric,	/* metalayer */
	(Tk_ConfigSpec *) configPolyline,	/* polyline */
	(Tk_ConfigSpec *) configPolygone,	/* polygone */
	(Tk_ConfigSpec *) configPoints,		/* points */
	(Tk_ConfigSpec *) configSites,		/* sites */
	(Tk_ConfigSpec *) configRaster,		/* raster */
	(Tk_ConfigSpec *) configRaster,		/* oraster */
	(Tk_ConfigSpec *) configLabel,		/* labels */
	(Tk_ConfigSpec *) configGrid,		/* grids */
	(Tk_ConfigSpec *) configLegend,		/* legends */
	(Tk_ConfigSpec *) configLegend,		/* scales */

};
		

unsigned long mask = StructureNotifyMask | SubstructureNotifyMask |
                     ButtonPressMask | ButtonReleaseMask | PointerMotionMask;

int
viewWidgetCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
	char *cmd;
	int length;
	VIEW *view = (VIEW *) clientData;

	
	if (argc < 2) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
						argv[0], " command \"", 0);
		return(TCL_ERROR);
		}
	
	cmd = argv[1];
	length = strlen(cmd);
	Tcl_ResetResult(interp);
	
	switch(*cmd) {
	
	case 'a':
		if (strncmp(cmd, "addmeta", length) == 0 && length > 1)
			return( addlayer(view,METALAYER,argc,argv));
		if (strncmp(cmd, "addpolyline", length) == 0 && length > 1)
			return( addlayer(view,POLYLINE,argc,argv));
		if (strncmp(cmd, "addpolygon", length) == 0 && length > 1)
			return( addlayer(view,POLYGONE,argc,argv));
		if (strncmp(cmd, "addpoint", length) == 0 && length > 1)
			return( addlayer(view,DIGPOINT,argc,argv));
		if (strncmp(cmd, "addsites", length) == 0 && length > 1)
			return( addlayer(view,SITE,argc,argv));
		if (strncmp(cmd, "addraster", length) == 0 && length > 1)
			return( addlayer(view,RASTER,argc,argv));
		if (strncmp(cmd, "addoraster", length) == 0 && length > 1)
			return( addlayer(view,ORASTER,argc,argv));
		if (strncmp(cmd, "addlabel", length) == 0 && length > 1)
			return( addlayer(view,LABELS,argc,argv));
		if (strncmp(cmd, "addgrid", length) == 0 && length > 1)
			return( addlayer(view,GRID,argc,argv));
		if (strncmp(cmd, "addlegend", length) == 0 && length > 1)
			return( addlayer(view,LEGEND,argc,argv));
		break;
		
	case 'd':
		if (strncmp(cmd, "draw", length) == 0 && length > 1) {
			return( draw(view) );
			}
		if (strncmp(cmd, "delete", length) == 0 && length > 1) {
			return( deleteLayer(view,argc,argv) );
			}
		break;
		/*		
	case 'g':
		if (strncmp(cmd, "getsetregion", length) == 0 && length > 1)
			return( getsetregion(view));
		break;
		*/

	case 'l':
		if (strncmp(cmd, "list", length) == 0 && length > 1)
			return( listLayer(view,argc,argv));
		break;

	case 'p':
		if (strncmp(cmd, "panx", length) == 0 && length > 1)
			return( panx(view,argc,argv));
		if (strncmp(cmd, "pany", length) == 0 && length > 1)
			return( pany(view,argc,argv));
		break;
		/*
	case 'r':
		if (strncmp(cmd, "region", length) == 0 && length > 1)
			return( region(view,argc,argv));
		break;
		*/
	case 's':
		if (strncmp(cmd, "scale", length) == 0 && length > 1)
			return( scale(view,argc,argv));
		if (strncmp(cmd, "setdefregion", length) == 0 && length > 1)
			return( setdefregion(view,argc,argv));  
		break;
		
	case 'u':
		if (strncmp(cmd, "up", length) == 0 && length > 1)
			return( upLayer(view,argc,argv));
		break;
		/*
	case 'w':
		if (strncmp(cmd, "where", length) == 0 && length > 1)
			return( where(view,argc,argv));
		break;
		*/
	case 'z':
		if (strncmp(cmd, "zoom", length) == 0 && length > 1)
			return( zoom(view,argc,argv));
		if (strncmp(cmd, "zoommode", length) == 0 && length > 1)
			return( zoommode(view,argc,argv));
		break;
	
 	}
	
	Tcl_AppendResult(interp, "unknown view subcommand: \"", cmd, "\"", 0);
	return(TCL_ERROR);
}
 
void
viewDeleteCmd(clientData)
ClientData clientData;
{
	int i;
	VIEW *view = (VIEW *) clientData;
	
	for (i = 0; i < view->nb_layer; ++i)
		deleteALayer(view->layer[i]);
		
	free(view);
}
 
void
viewEventHandler(clientData,event)
ClientData clientData;
XEvent *event;
{
	VIEW *view = (VIEW *) clientData;
	
	switch(event->type) {

	case ConfigureNotify:
		if (!view->updatePending)
			Tk_DoWhenIdle(draw,(ClientData) view);
		view->updatePending = 1;
		break;
	case ButtonPress:
		if (view->zoommode) 
		  changeRegion(view,event);
		break;
	}
}
	
 
int viewCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
	unsigned long valuemask = 0;
	XSetWindowAttributes setwinattr; 
	VIEW *newview;
	Tk_Window tkwinbuf = (Tk_Window) clientData;
	Tk_Window new;

	if (argc != 2) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
						argv[0], " pathName \"", 0);
		return(TCL_ERROR);
		}

	newview = (VIEW *) G_malloc(sizeof(VIEW));
	if (newview == NULL) 
	{
		Tcl_AppendResult(interp, "could not allocate memory ", NULL);
		return(TCL_ERROR);
	}

	newview->interp = interp;
	newview->nb_layer = 0;
	newview->updatePending = 0;
	newview->zoommode = 0;

	new = Tk_CreateWindowFromPath(interp, tkwinbuf, argv[1], NULL);

	if (new == NULL) 
	{
		Tcl_AppendResult(interp, "could not create \"",
			argv[1], "\"", 0);
		 return(TCL_ERROR);
	}

	newview->tkwin = new; 	

	getdefregion(&newview->region);
	
	Tk_SetClass(newview->tkwin, "View");
	
	Tk_GeometryRequest(newview->tkwin, MINWIDTH, MINHEIGHT);

	Tk_MakeWindowExist(newview->tkwin);

	setwinattr.backing_store = Always;
	XChangeWindowAttributes(Tk_Display(newview->tkwin), Tk_WindowId(newview->tkwin),
							CWBackingStore, &setwinattr);
	
	Tk_CreateEventHandler(newview->tkwin,mask,viewEventHandler,
							(ClientData) newview); 

	strcpy(newview->name, argv[1]);
	
	Tcl_CreateCommand(newview->interp, argv[1], viewWidgetCmd, 
						(ClientData) newview, viewDeleteCmd );
	Tcl_SetResult(newview->interp, argv[1], TCL_VOLATILE);
	return(TCL_OK);
}

setdefregion(view,argc,argv)
VIEW *view;
int argc;
char **argv;
{
        getdefregion(&view->region);

	draw(view);

	return(TCL_OK);
}

compab(a,b)
lookup *a;
lookup *b;
{
	double da,db;
	
	da = a->r * 256 * 256 + a->g * 256 + a->b;
	db = b->r * 256 * 256 + b->g * 256 + b->b;

	if (da < db)
		return(-1);
	
	if (da > db)
		return(1);
		
	return(0);
}

addlayer(view,type,argc,argv)
VIEW *view;
int type,argc;
char *argv[];
{
	char *whereMapset;
	LAYER *newlayer;
	
	newlayer = (LAYER *) G_malloc(sizeof(LAYER));
	initLayerWithDefault(newlayer,type);
	
	if (Tk_ConfigureWidget(view->interp, view->tkwin, 
						configLayer[type], argc-2, argv+2, 
						(char *) newlayer, 0) == TCL_OK) 
	{
		if (type < GRID ) 
		{
			if ((whereMapset = G_find_file(grassdir[type],newlayer->name,newlayer->mapset)) == NULL) 
			{
				Tcl_AppendResult(view->interp, newlayer->name," not found in mapset ", newlayer->mapset, 0);
				free(newlayer);
				return(TCL_ERROR);
			}
			free(newlayer->mapset);			
			newlayer->mapset = whereMapset;
		}
		
	
		if (prepareLayer(view,newlayer,type) == TCL_ERROR) {
			free(newlayer);
			return(TCL_ERROR);
		}
	
		view->layer[view->nb_layer] = newlayer;
		view->nb_layer = view->nb_layer + 1;
	
		Tcl_SetResult(view->interp, newlayer->name, TCL_VOLATILE);	
		
                /* Modif A. L. 18/02/94 */
		/* return(drawOneLayer(view,newlayer)); */
		return(TCL_OK);
	}
	else 
	{
		free(newlayer);
		return(TCL_ERROR);
	}
}

deleteLayer(view,argc,argv)
VIEW *view;
int argc;
char **argv;
{
	int i,dl = 0;
	
	if (argc != 3) 
	{
		Tcl_AppendResult(view->interp, "wrong # args: should be \"",
					argv[0], " delete layer_number \"", 0);
		return(TCL_ERROR);
	}
	
	dl = atoi(argv[2]);
	
	if (dl >= view->nb_layer || dl < 0)
	{
		Tcl_AppendResult(view->interp, "not a valid layer", 0);
		return(TCL_ERROR);
	}
	
	deleteALayer(view,view->layer[dl]);
	
	for (i = dl; i < view->nb_layer - 1; ++i)
		view->layer[i] = view->layer[i+1];
			
	view->nb_layer--;

	return(TCL_OK);
}
	

upLayer(view,argc,argv)
VIEW *view;
int argc;
char **argv;
{
	int ul = 0;
	LAYER *layerbuf;
	
	if (argc != 3) 
	{
		Tcl_AppendResult(view->interp, "wrong # args: should be \"",
					argv[0], " delete layer_number \"", 0);
		return(TCL_ERROR);
	}
	
	ul = atoi(argv[2]);
	
	if (ul >= view->nb_layer || ul < 0)
	{
		Tcl_AppendResult(view->interp, "not a valid layer", 0);
		return(TCL_ERROR);
	}
	
	if (ul > 0 && view->nb_layer > 1)
	{
		layerbuf = view->layer[ul-1];
		view->layer[ul-1] = view->layer[ul];
		view->layer[ul] = layerbuf;
	}
	
	return(TCL_OK); 
}

	
deleteALayer(view,layer)
VIEW *view;
LAYER *layer;
{
	int i;
	XColor freecolor;
	
	switch(layer->type)  
	{
	case METALAYER:
		for(i = 0; i < layer->nb_sublayer; ++i) 
			deleteALayer(layer->sublayer[i]);
		break;
	
	case RASTER:
	case ORASTER:
/*		for(i = 0; i < layer->nrcolor; ++i) 
		{
			freecolor.pixel = layer->rcolor[i];
			XQueryColor(Tk_Display(view->tkwin),Tk_Colormap(view->tkwin),&freecolor);
			Tk_FreeColor(&freecolor);
		} */
		free(layer->rcolor);
		break;

	case POLYLINE:
	case POLYGONE:
	case DIGPOINT:
		Vect_close(&(layer->Map));
		break;
	}
	
	Tk_FreeOptions(configLayer[layer->type],layer,Tk_Display(view->tkwin),0);		
	free(layer);
	
}

listLayer(view,argc,argv)
VIEW *view;
int argc;
char **argv;
{
	char **liste,**optionlist,**vargv;
	int i,j,nboption,vargc;
	
	if (argc != 2) 
	{
		Tcl_AppendResult(view->interp, "wrong # args: should be \"",
					argv[0], " list \"", 0);
		return(TCL_ERROR);
	}

	if (view->nb_layer == 0) 
	{
                /* Modif A. L. 18/02/94 */
                /* Tcl_AppendResult(view->interp, "Nothing to list", 0); */
		Tcl_AppendResult(view->interp, 0);
		return(TCL_OK);
	}

	liste = (char **) malloc(sizeof(char *) * view->nb_layer);

	for(i = 0; i < view->nb_layer; ++i)
	{
		Tk_ConfigureInfo(view->interp,view->tkwin,configLayer[view->layer[i]->type],
						view->layer[i], NULL, TK_CONFIG_ARGV_ONLY );
		liste[i] = (char *) malloc(strlen(view->interp->result));
		strcpy(liste[i],view->interp->result);
	}
	
	Tcl_ResetResult(view->interp);

	for(i = 0; i < view->nb_layer; ++i)
	{
		Tcl_AppendResult(view->interp, "{", keywords[view->layer[i]->type], " ", 0);

		Tcl_SplitList(view->interp,liste[i],&nboption,&optionlist);
		
		for (j = 0; j < nboption; ++j)
		{
			Tcl_SplitList(view->interp,optionlist[j],&vargc,&vargv);
			if (strlen(vargv[4]) > 0)
				Tcl_AppendResult(view->interp, vargv[0]," ", vargv[4], " ", 0);
		}		
			
		Tcl_AppendResult(view->interp, "} ", 0);
		free(liste[i]);
	}
	
	free(liste);
	return(TCL_OK);
}
	
initLayerWithDefault(newlayer,type)
LAYER *newlayer;
{
	newlayer->type = type;
	newlayer->nb_sublayer = 0;
	newlayer->name = NULL;
	newlayer->mapset = NULL;
	newlayer->dash = NULL;
	newlayer->nbdash = 0;
	newlayer->color = None;
	newlayer->rcolor = NULL;
	newlayer->nrcolor = 0;
	newlayer->colormode = FLOATCOLOR;
	newlayer->symbol = None;
	newlayer->font = NULL;
	newlayer->grid = 0.0;
	newlayer->window = None;
}

prepareLayer(view,newlayer,type)
VIEW *view;
LAYER *newlayer;
int type;
{
	int i;

	switch(type) 
	{
	case METALAYER:
		if (addAllSubLayer(view,newlayer) != TCL_OK) {
			Tcl_AppendResult(view->interp, "Problem in metalayer ", newlayer->name, 0);
			return(TCL_ERROR);
			}
		break;
	case POLYLINE:
		if (newlayer->dash != NULL) 
			newlayer->nbdash = strlen(newlayer->dash);
	case POLYGONE:
	case DIGPOINT:
		if (2 > Vect_open_old(&(newlayer->Map), newlayer->name, newlayer->mapset)) 
		{
			Tcl_AppendResult(view->interp, "cannot open digfile ",
					newlayer->name, " in mode 2 ", 0);
			return(TCL_ERROR);
		}
		break;
	case RASTER:
	case ORASTER:
		prepareRasterColor(view,newlayer);
		break;
	case GRID:
		if (newlayer->dash != NULL) 
			newlayer->nbdash = strlen(newlayer->dash);
		break;
	}
}

prepareRasterColor(view,newlayer)
LAYER *newlayer;
VIEW *view;
{
	struct Colors colors;
	int red256, green256, blue256;
	int j,inc;
	XColor sendColor, *receiveColor;
	struct Cell_head cellregion;
	CELL min,max;
	lookup *lut;
	int n_levels,multfactor;
	double divfactor;

	G_read_colors(newlayer->name, newlayer->mapset, &colors); 
	G_get_color_range(&min,&max,&colors);
	G_get_cellhd(newlayer->name, newlayer->mapset, &cellregion);
	newlayer->rcolor = (unsigned long *) G_malloc( sizeof(long) * max + 1);			
	newlayer->nrcolor =  max + 1;			

	G_get_color(0, &red256, &green256, &blue256, &colors);
	sendColor.red = red256 << 8;
	sendColor.green = green256 << 8;
	sendColor.blue = blue256 << 8;
/*
	receiveColor = Tk_GetColorByValue(view->interp, view->tkwin,Tk_Colormap(view->tkwin),&sendColor);  
*/
	receiveColor = Tk_GetColorByValue(view->tkwin,&sendColor);  
	newlayer->rcolor[0] = receiveColor->pixel;
	
	if ((max - min) < MAXRASTCOLOR) 
	{
		for (j = min; j <= max; ++j) 
		{
			G_get_color(j, &red256, &green256, &blue256, &colors);
			sendColor.red = red256 << 8;
			sendColor.green = green256 << 8;
			sendColor.blue = blue256 << 8;
/*
			receiveColor = Tk_GetColorByValue(view->interp, view->tkwin,Tk_Colormap(view->tkwin),&sendColor);  
*/
			receiveColor = Tk_GetColorByValue(view->tkwin,&sendColor);  
			newlayer->rcolor[j] = receiveColor->pixel;
		}
	}
	else 
	{
		switch (newlayer->colormode)
		{
		case FLOATCOLOR:
			lut = (lookup *) malloc(sizeof(lookup)*(max-min+1));
			for (j = min; j < max; ++j) 
			{
				G_get_color(j, &lut[j-min].r, &lut[j-min].g, &lut[j-min].b, &colors);
				lut[j-min].cell = j;
			}
			qsort(lut,max-min+1,sizeof(lookup),compab);
			inc = ceil((double) (max-min+1) / MAXRASTCOLOR);
			for (j = 0; j < (max-min+1); ++j) 
			{
				sendColor.red = lut[(j/inc)*inc].r << 8;
				sendColor.green = lut[(j/inc)*inc].g << 8;
				sendColor.blue =lut[(j/inc)*inc].b << 8;
/*
				receiveColor = Tk_GetColorByValue(view->interp, view->tkwin,Tk_Colormap(view->tkwin),&sendColor);  
*/
				receiveColor = Tk_GetColorByValue(view->tkwin,&sendColor);  
				newlayer->rcolor[lut[j].cell] = receiveColor->pixel;
			}	
			break;
			
		case FIXEDCOLOR:
			for (n_levels = 0; n_levels * n_levels * n_levels <= MAXRASTCOLOR; ++n_levels);
			--n_levels;
			divfactor = (double) n_levels / 256.0;
			multfactor = (int) (256.0 / (double) n_levels) * 257;
			for (j = min; j <= max; ++j) 
			{
				G_get_color(j, &red256, &green256, &blue256, &colors);
				sendColor.red = (int) ((double) red256 * divfactor) * multfactor;
				sendColor.green = (int) ((double) green256 * divfactor) * multfactor;
				sendColor.blue = (int) ((double) blue256 * divfactor) * multfactor;
/*
				receiveColor = Tk_GetColorByValue(view->interp, view->tkwin,Tk_Colormap(view->tkwin),&sendColor);  
*/
				receiveColor = Tk_GetColorByValue(view->tkwin,&sendColor);  
				newlayer->rcolor[j] = receiveColor->pixel;
			}
			break;
		}	
	}
		
	G_free_colors(&colors);
}

addAllSubLayer(view,layer)
LAYER *layer;
VIEW *view;
{
	FILE *inputFile;
	char line[255],token[80],*whereMapset;
	LAYER *newlayer;
	int type,i;
	int argc;
	char **argv;

	inputFile = G_fopen_old(grassdir[METALAYER],layer->name, layer->mapset);

	while (fgets(line, 255, inputFile) != NULL)
	{
		if (sscanf(line,"%s", token) == 1)
		{
			type = compare(token);
			if (type < NBTOKEN) 
			{

				Tcl_SplitList(view->interp,line,&argc,&argv);
				
				newlayer = (LAYER *) G_malloc(sizeof(LAYER));				
				initLayerWithDefault(newlayer,type);
				
				if (Tk_ConfigureWidget(view->interp, view->tkwin,configLayer[type],argc-1,argv+1, (LAYER *) newlayer, 0) == TCL_OK) 
				{
					if ((whereMapset = G_find_file(grassdir[type],newlayer->name,newlayer->mapset)) != NULL || type >= GRID) 
					{
						free(newlayer->mapset);			
						newlayer->mapset = whereMapset;
						
						if (prepareLayer(view,newlayer,type) == TCL_ERROR) 
						{
							free(newlayer);
							return(TCL_ERROR);
						}
					
						layer->sublayer[layer->nb_sublayer] = newlayer;
						layer->nb_sublayer++;
					}
					else
					{
						free(newlayer);
						return(TCL_ERROR);
					}
				}
				else 
				{
					free(newlayer);
					return(TCL_ERROR);
				}
			}
		}
	}

	fclose(inputFile);
}
	
compare(token)
char *token;
{
	int i;
	
	for (i = 0; i < NBTOKEN; ++i) 
		if (strncmp(keywords[i], token, strlen(keywords[i])) == 0)
			return(i);
	return(i);
}

scale(view,argc,argv)
VIEW *view;
int argc;
char **argv;
{
	double scale;

	if (argc == 3)
	{
		Tcl_GetDouble(view->interp,argv[2],&scale);
		setViewByScale(view,scale);
		draw(view);
	}
	
	sprintf(view->interp->result,"1 : %d", (int) (view->scale + 0.5));

	return(TCL_OK);
}					
	
calcScale(view)
VIEW *view;
{
	double pixelsize;
	double scaleOnWidth,scaleOnHeight,scaleRatioOnWidth,scaleRatioOnHeight;

	Tk_GetScreenMM(view->interp,view->tkwin,"1",&pixelsize);
	pixelsize /= 1000.0;
	scaleOnWidth =  (view->region.northEastCoordinate.x - view->region.southWestCoordinate.x) / 
							(double) (view->region.D_east - view->region.D_west);
	scaleOnHeight = (view->region.northEastCoordinate.y - view->region.southWestCoordinate.y) / 
							(double) (view->region.D_south - view->region.D_north);
							
	scaleRatioOnWidth = scaleOnWidth / pixelsize;
	scaleRatioOnHeight = scaleOnHeight / pixelsize;
	
	if ( scaleOnWidth > scaleOnHeight )  {
	  view->scale = scaleRatioOnWidth;
	  view->res = scaleOnWidth;
	  }
	else {
	  view->scale = scaleRatioOnHeight;
	  view->res = scaleOnHeight;
	  }
}


setViewByScale(view,newscale)
VIEW *view;
double newscale;
{
	double pixelsize,centrex,centrey,screenWidth,screenHeight,latDistance,longDistance;
	
	Tk_GetScreenMM(view->interp,view->tkwin,"1",&pixelsize);
	pixelsize /= 1000.0;

              centrex = ( view->region.northEastCoordinate.x +
		view->region.southWestCoordinate.x ) / 2.0;
              centrey = ( view->region.northEastCoordinate.y +
		view->region.southWestCoordinate.y ) / 2.0;
	
	screenHeight = pixelsize * (view->region.D_south - view->region.D_north);
	screenWidth = pixelsize * (view->region.D_west - view->region.D_east);
	
	latDistance = (screenHeight * newscale) / 2.0;
	longDistance = (screenWidth * newscale) / 2.0;

	view->region.northEastCoordinate.x = centrex - longDistance;
	view->region.southWestCoordinate.x = centrex + longDistance;
	view->region.northEastCoordinate.y = centrey + latDistance;
	view->region.southWestCoordinate.y = centrey - latDistance;

	calcScale(view);
}


zoom(v,argc,argv)
VIEW *v;
int argc;
char *argv[];
{
	double zoomFactor,factor;
		
	if (argc != 3) 
	{
		Tcl_AppendResult(v->interp, "wrong # args: should be \"",
					argv[0], " zoom zoom_factor \"", 0);
		return(TCL_ERROR);
	}
	
	zoomFactor = atof(argv[2]);
	
	factor = (v->region.northEastCoordinate.x - v->region.southWestCoordinate.x) 
			* (1.0 - zoomFactor) / 2.0;
	
	v->region.northEastCoordinate.x -= factor;
	v->region.southWestCoordinate.x += factor;
	
	factor = (v->region.northEastCoordinate.y - v->region.southWestCoordinate.y) * (1.0 - zoomFactor) / 2.0;
		v->region.northEastCoordinate.y -= factor;
	v->region.southWestCoordinate.y += factor;

	draw(v);
	   
	return(TCL_OK);
}



panx(v,argc,argv)
VIEW *v;
int argc;
char *argv[];
{
	double panFactor,factor;
	
	if (argc != 3) 
	{
	  Tcl_AppendResult(v->interp, "wrong # args: should be \"",
				  argv[0], " panx pan_factor \"", 0);
	  return(TCL_ERROR);
	}
	
	panFactor = atof(argv[2]);
	
	factor = (v->region.northEastCoordinate.x - v->region.southWestCoordinate.x) * panFactor;
	v->region.northEastCoordinate.x += factor;
	v->region.southWestCoordinate.x += factor;

	draw(v);
	return(TCL_OK);
	
}

pany(v,argc,argv)
VIEW *v;
int argc;
char *argv[];
{
	double panFactor,factor;
	
	if (argc != 3) 
	{
	  Tcl_AppendResult(v->interp, "wrong # args: should be \"",
				  argv[0], " panx pan_factor \"", 0);
	  return(TCL_ERROR);
	}
	
	panFactor = atof(argv[2]);
	
	factor = (v->region.northEastCoordinate.y - v->region.southWestCoordinate.y) * panFactor;
	v->region.northEastCoordinate.y += factor;
	v->region.southWestCoordinate.y += factor;

	draw(v);
	
	return(TCL_OK);	
}

zoommode(view,argc,argv)
VIEW *view;
int argc;
char **argv;
{
	if (argc != 3) 
	{
		Tcl_AppendResult(view->interp, "must be  ",
						argv[0], " zoommode [1|0] ", 0);
		return(TCL_ERROR);
	}
	view->zoommode = atoi(argv[2]);

	return(TCL_OK);
}		
		
changeRegion(view,event)
VIEW *view;
XEvent *event;
{
	XEvent report;
	int mouseReleased = 0;
	int x1,y1,x2,y2,xx1,xx2,yy1,yy2;
	XGCValues gcvalues;
	XColor *col;
	double distancePixel,distanceCoord,ratioPixelCoord;
	
	/* verifie ensuite si on a presse le bouton de la souris dans la region courante  */
		
	if (event->xbutton.y < view->region.D_north)
		return(0);
	if (event->xbutton.y > view->region.D_south)
		return(0);
	if (event->xbutton.x < view->region.D_west)
		return(0);
	if (event->xbutton.x > view->region.D_east)
		return(0); 

	x1 = event->xbutton.x;
	y1 = event->xbutton.y;
	x2 = 0;
	y2 = 0;

	view->gc = XCreateGC(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),0, &gcvalues);

	XSetFunction(Tk_Display(view->tkwin),view->gc,GXxor);			
/*
	col = Tk_GetColor(view->interp, view->tkwin,Tk_Colormap(view->tkwin),Tk_GetUid("black"));
*/
	col = Tk_GetColor(view->interp, view->tkwin,Tk_GetUid("black"));
	XSetForeground(Tk_Display(view->tkwin),view->gc,col->pixel);

	while (!mouseReleased) 
	{
		XNextEvent(Tk_Display(view->tkwin),&report);

		switch(report.type) 
		{
		case ButtonRelease:
				mouseReleased = -1;
				break;
				
		case MotionNotify:
				while(XCheckMaskEvent(Tk_Display(view->tkwin), ButtonMotionMask, &report));
				if (x2 != 0)
					dessineRectangle(view,x1,y1,x2,y2);
				x2 = report.xmotion.x;
				y2 = report.xmotion.y;
				dessineRectangle(view,x1,y1,x2,y2);
				break;
		}
	}

	if (x1 <= x2)
	{
		xx1 = x1;
		xx2 = x2;
	}
	else
	{
		xx1 = x2;
		xx2 = x1;
	}

	if (y1 <= y2)
	{
		yy1 = y1;
		yy2 = y2;
	}
	else
	{
		yy1 = y2;
		yy2 = y1;
	}

	dessineRectangle(view,x1,y1,x2,y2);

	XFreeGC(Tk_Display(view->tkwin),view->gc);

	if ( (yy2 - yy1 + 1) > 40 && (xx2 - xx1 + 1) > 40)
	{
		distancePixel = view->region.D_south - view->region.D_north + 1;
		distanceCoord = view->region.northEastCoordinate.y - view->region.southWestCoordinate.y + 1;
		ratioPixelCoord = distanceCoord / distancePixel;
		
		view->region.northEastCoordinate.y -= (yy1 - view->region.D_north) * ratioPixelCoord;
		view->region.southWestCoordinate.y += (view->region.D_south - yy2) * ratioPixelCoord;
	
		distancePixel = view->region.D_east - view->region.D_west + 1;
		distanceCoord = view->region.northEastCoordinate.x - view->region.southWestCoordinate.x + 1;
		ratioPixelCoord = distanceCoord / distancePixel;
	
		view->region.southWestCoordinate.x += (xx1 - view->region.D_west) * ratioPixelCoord;
		view->region.northEastCoordinate.x -= (view->region.D_east - xx2) * ratioPixelCoord;
		draw(view);	
	}
	

	return( TCL_OK );	
}

dessineRectangle(view,x1,y1,x2,y2)
VIEW *view;
int x1,y1,x2,y2;
{
	int xx1,xx2,yy1,yy2;
	int width, height;

	if (x1 <= x2)
	{
		xx1 = x1;
		xx2 = x2;
	}
	else
	{
		xx1 = x2;
		xx2 = x1;
	}

	if (y1 <= y2)
	{
		yy1 = y1;
		yy2 = y2;
	}
	else
	{
		yy1 = y2;
		yy2 = y1;
	}
	
	width = xx2 - xx1 + 1;
	height = yy2 - yy1 + 1;

	XDrawRectangle(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),view->gc,xx1,yy1,width,height);
	
}
