#include "xgdisp.h"

Widget aspop_chvect;
Widget aspop_chsite;
int    typech;

void setlistitem();
void insert_chlistvect();
void insert_chlistsite();

void 
#ifdef _NO_PROTO
  quitchlistvect(w)
Widget w;
#else
  quitchlistvect(Widget w)
#endif
{
  printf ("quitchlistvect \n");
}

void 
#ifdef _NO_PROTO
  quitchlistsite(w)
Widget w;
#else
  quitchlistsite(Widget w)
#endif
{
  printf ("quitchlistsite\n");
}

void 
#ifdef _NO_PROTO
dellistvect (w)
#else
dellistvect (Widget w)
#endif
{
printf (" to delete vector \n");
}


void 
#ifdef _NO_PROTO
dellistsite(w)
#else
dellistsite(Widget w)
#endif
{
printf (" to delete vector \n");
}

void CreateChPanel(type)
int    type;
{
Widget xgi;
Widget form;
Widget listw;
Widget labelw;
XmString xmsdel;
XmString xmsok;
XmString xms;

int    ac;
Arg    al[10];

    xmsok = XmStringCreateSimple("Set");
    xmsdel = XmStringCreateSimple("Delete");
    ac = 0;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
	ac++;
    XtSetArg(al[ac], XmNcancelLabelString, xmsdel); ac++;
    XtSetArg(al[ac], XmNokLabelString, xmsok); ac++;
    xgi = XgCreateInteractorDialog(Global.applShell, 
	"XGRASS Set Panel", al, ac);
    XmStringFree(xmsok);
    XmStringFree(xmsdel);

    switch (type)
    {
    case VECTOR:
/*
     	XtAddCallback(xgi, XmNokCallback, 
		quitchlistvect, NULL);
*/
	XtAddCallback(xgi, XmNcancelCallback,
		dellistvect, NULL);
	xms = XmStringCreateSimple("To set one vector for change");
    break;

    case SITE:
/*
        XtAddCallback(xgi, XmNokCallback, 
		quitchlistsite, NULL);
*/
	XtAddCallback(xgi, XmNcancelCallback,
		dellistsite, NULL);
	xms = XmStringCreateSimple("To set one site file for change");
    break;
    }

    form= XtVaCreateManagedWidget("chlist_form",
	xmFormWidgetClass,xgi,
	NULL);

    labelw = XtVaCreateManagedWidget("label",
	xmLabelWidgetClass, form,
	XmNlabelString, xms,
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_FORM,
	NULL);
    XmStringFree(xms);

    ac=0;
    XtSetArg(al[ac],XmNtopAttachment,XmATTACH_WIDGET);
	ac++;
    XtSetArg(al[ac],XmNtopWidget, labelw);
	ac++;
    XtSetArg(al[ac],XmNleftAttachment,XmATTACH_FORM);
	ac++;
    XtSetArg(al[ac],XmNrightAttachment,XmATTACH_FORM);
	ac++;
    XtSetArg(al[ac],XmNvisibleItemCount, 5);
	ac++;
    listw= XmCreateScrolledList(form, "chlist",al,ac);
    XtManageChild(listw);

   switch (type)
   {
   case VECTOR: 
   	insert_chlistvect(listw,Global.selectedObjects->object->Obj.GeoFrame.vects.vect);
   break;

   case SITE: 
	insert_chlistsite(listw,Global.selectedObjects->object->Obj.GeoFrame.sites.site);
   break;
   }

   XtAddCallback(xgi, XmNokCallback, setlistitem, listw);

   XtManageChild(xgi);

}


void
#ifdef _NO_PROTO 
chlist(w,cld)
Widget w;
XtPointer cld;
#else
chlist(Widget w,
	XtPointer cld)
#endif
{
	if (Global.selectedObjects->object &&
		SelectedObjectCount()==1 &&
		Global.selectedObjects->object->type== XGD_GEOFRAME)
	{
	typech= (int) cld;
	CreateChPanel((int)cld);
	}
}



static void
#ifdef _NO_PROTO
setlistitem(w,cld,cad)
Widget w;
XtPointer cld;
XtPointer cad;
#else
setlistitem(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Widget list= (Widget)cld;
int *pos;
int posCount;
XmString *items;
char name[512], mapset[512];


if ( XmListGetSelectedPos(list,&pos,&posCount) ) {
   char *text;

   XtVaGetValues(list, XmNitems, &items, NULL);
   XmStringGetLtoR(items[pos[0]-1], 
	XmSTRING_DEFAULT_CHARSET,
	&text);
   G__name_is_fully_qualified(text,name,mapset);

   switch (typech)
   {
   case VECTOR:
        Global.vname = (char *)XtMalloc(sizeof (char)*strlen(name) + 1);
        Global.vmapset= (char *)XtMalloc(sizeof (char)*strlen(mapset) + 1);
        strcpy (Global.vname, name);
        strcpy (Global.vmapset, mapset);
   break;

   case SITE:
	free(Global.sname);
	free(Global.smapset);
        Global.sname = (char *)XtMalloc(sizeof (char)*strlen(name) + 1);
        Global.smapset= (char *)XtMalloc(sizeof (char)*strlen(mapset) + 1);
        strcpy (Global.sname, name);
        strcpy (Global.smapset, mapset);
   break;
   }
	
}
}



void
#ifdef _NO_PROTO
insert_chlistvect( list, vect)
Widget list;
XgdVectInfo *vect;
#else
insert_chlistvect( Widget list, XgdVectInfo *vect)
#endif
{
    XmString *sptr;
    XmString at = XmStringCreateSimple("@");
    XgdVectInfo *tptr;
    char *fullname;
    int count,  i;

    count = 0;
    i = 0;

    if (vect == NULL)
	return;

    tptr = vect;
    while (tptr != NULL)
    {
        count ++;
	tptr = tptr->next;
    }

    sptr = (XmString *)_XgCalloc(count, sizeof(XmString));
    tptr = vect;
    while (tptr != NULL)
    {
	fullname= (char*)G_fully_qualified_name(
		tptr->vname,tptr->vmapset);
	sptr[i] = XmStringCreateSimple(fullname);
	tptr = tptr->next;
	i++;
    }
    XtVaSetValues(list, XmNitems, sptr, XmNitemCount, count, NULL);
    for ( i = 0; i < count; i++) {
        XmStringFree(sptr[i]);
    }
    XmStringFree(at);
    _XgFree(sptr);
}



void
#ifdef _NO_PROTO
insert_chlistsite( list, ptr)
Widget list;
XgdSiteInfo *ptr;
#else
insert_chlistsite( Widget list, XgdSiteInfo *ptr )
#endif
{
    XmString *sptr;
    XmString at = XmStringCreateSimple("@");
    XgdSiteInfo *tptr;
    char *fullname;

    int count = 0, i;

    if (ptr == NULL)
	return;

    tptr = ptr;
    while (tptr != NULL) {
	if (tptr->type==XGD_SITE_STANDARD)
           count ++;
	   tptr = tptr->next;
    }

    i = 0;
    sptr = (XmString *)_XgCalloc(count, sizeof(XmString));
    tptr = ptr;
    while (tptr != NULL) 
    {
	if (tptr->type==XGD_SITE_STANDARD) {
	fullname= (char*)G_fully_qualified_name(
		tptr->sname,tptr->smapset);
	sptr[i] = XmStringCreateSimple(fullname);
	i++;
	}
	tptr = tptr->next;
    }
    XtVaSetValues(list, XmNitems, sptr, XmNitemCount, count, NULL);
    for ( i = 0; i < count; i++) {
        XmStringFree(sptr[i]);
    }
    XmStringFree(at);
    _XgFree(sptr);

}
