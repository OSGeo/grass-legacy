#include "xgen.h"
#include <Xm/ScrolledWP.h>

Widget
CreateList(object,widget)
    InterfaceObject *object;
    Widget widget;
{
    static int numlists = 0;
    char listname[80];
    static int numlistEls = 0;
    char listElname[80];
    int n;
    int itemCount = 0;
    int count = 0;
    int vItemCount = 0;
    Widget listW;
    Resource *resource;
    ListData *listData;
    ListData *curList;
    Boolean first = True;
    Boolean vItemSpecd = False;
    ListType listType;
    XmString *items;

    numlists++;
    sprintf(listname,"list%03d",numlists);
    curList = listData = (ListData *)XtMalloc(sizeof(ListData));
    bzero((char *)curList,sizeof(ListData));


    if (NULL != (resource = IndexResource(object,OBJECT,"visibleitems"))) {
		if ( resource->variable ) ExpandVariable(resource);
        vItemCount = resource->val.ival;
        vItemSpecd = True;
    } 
    if (NULL != (resource = IndexResource(object,OBJECT,"listelement"))) {
        while(resource) {
            if ( !strcmp(resource->name,"listelement") ) {
                itemCount++;
            }
            resource =resource->next;
        }
    }

    items = (XmString *)XtCalloc(itemCount,sizeof(XmString));

    if (NULL != (resource = IndexResource(object,OBJECT,"listelement"))) {
        while(resource) {
            if ( !strcmp(resource->name,"listelement") ) {

                if ( !first ) {
                    curList->next =(ListData *)XtMalloc(sizeof(ListData));
                    bzero((char *)curList->next,sizeof(ListData));
                    curList = curList->next;
                }
                first = False;
                curList->item = XmStringLtoRCreate(resource->val.cval,SDC);
                curList->selected = False;
                if ( resource->next && 
                     !strcmp(resource->next->name,"valuestring")) {
                    curList->valueString = 
                        XtMalloc(strlen(resource->next->val.cval) + 1);
                    strcpy(curList->valueString,resource->next->val.cval);
                }

                items[count++] = curList->item;
            }
            resource = resource->next;
        }
    } else {
        XgenFatalError("while creating list object","no list elements");
    }

    n = 0;
    SetGlobalArgs(&n,FONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object,&n);
    SetObjectColorArgs(object,&n);
    XtSetArg(args[n],XmNautomaticSelection,True); n++;
    XtSetArg(args[n],XmNitems,items); n++;
    XtSetArg(args[n],XmNitemCount,itemCount); n++;
    if ( vItemSpecd )
        XtSetArg(args[n],XmNvisibleItemCount,vItemCount); n++;
    if ( NULL != (resource = IndexResource(object,OBJECT,"listtype"))) {
        if ( !strcmp(resource->val.cval,"single") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmSINGLE_SELECT); n++;
            listType = single;
        } else if ( !strcmp(resource->val.cval,"multiple") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmMULTIPLE_SELECT); n++;
            listType = multiple;
        } else if ( !strcmp(resource->val.cval,"extended") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmEXTENDED_SELECT); n++;
            listType = extended;
        } else if ( !strcmp(resource->val.cval,"browse") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmBROWSE_SELECT); n++;
            listType = browse;
        } else 
            XgenFatalError("creating list object","no such selection policy");
    } else {
           XtSetArg(args[n],XmNselectionPolicy,XmSINGLE_SELECT); n++;
           listType = single;
    }
    listW = XmCreateScrolledList(widget,listname,args,n);
    XtManageChild(listW);
    XmAddTabGroup(listW);

    /***************************************************************
     *  don't ask......
     **************************************************************/
    {
        XmScrolledWindowWidget sw = (XmScrolledWindowWidget)listW;

        n = 0;
        SetObjectColorArgs(NULL,&n);
        XtSetValues(sw->core.parent,args,n);
    }


    if ( listType == single ) 
        XtAddCallback(listW,XmNsingleSelectionCallback,
                      SSListChangedCB,(caddr_t)object);
    else if ( listType == multiple ) 
        XtAddCallback(listW,XmNmultipleSelectionCallback,
                      MSListChangedCB,(caddr_t)object);
    else if ( listType == extended ) {
        XtAddCallback(listW,XmNextendedSelectionCallback,
                      ESListChangedCB,(caddr_t)object);
        XtAddCallback(listW,XmNsingleSelectionCallback,
                      SSListChangedCB,(caddr_t)object);
    } else {
        XtAddCallback(listW,XmNbrowseSelectionCallback,
                      BSListChangedCB,(caddr_t)object);
        XtAddCallback(listW,XmNsingleSelectionCallback,
                      SSListChangedCB,(caddr_t)object);
    }
    AddListInfo(object->name,listData);
    return listW;
}

