#include "xgdisp.h"

Boolean
#ifdef _NO_PROTO
IsObjectInList(list, obj)
    ObjectList list;
    XgdObject *obj;
#else
IsObjectInList( ObjectList list, XgdObject *obj)
#endif
{
  for(; list; list = list->next) {
    if ( list->object == obj ) return True;
  }
  return False;
}

void
#ifdef _NO_PROTO
DeleteObjects(widget, cli, call)
     Widget widget;
     caddr_t call;
     caddr_t cli;
#else     
DeleteObjects(Widget widget, caddr_t cli, caddr_t call)
#endif
{
  ObjectList list = Global.selectedObjects;
  
  for(; list; list = list->next) {
    DeleteObjectFromList(&Global.objectList, list->object);

    if (list->object->type==XGD_GEOFRAME) {
      XgdSiteInfo *s;

      EndFlash();
      SetMode(XGD_MODE_SELECT);
      if (list->object->Obj.GeoFrame.rname){
        XgdDeleteRaster(list->object);
      }
      if (list->object->Obj.GeoFrame.legend){
	DeleteObjectFromList(&Global.objectList,
				list->object->Obj.GeoFrame.legend);
	list->object->Obj.GeoFrame.legend = NULL;
      }
      
      if (list->object->Obj.GeoFrame.numbarscales != 0) {
	int i;

	for (i = 0; i < list->object->Obj.GeoFrame.numbarscales; i++) {
          XgdObject *obj = list->object->Obj.GeoFrame.barscales[i];
	  DeleteObjectFromList(&Global.objectList, obj);
	}
	list->object->Obj.GeoFrame.barscales = NULL;
        list->object->Obj.GeoFrame.numbarscales = 0;
      }
      s = list->object->Obj.GeoFrame.sites.site;
      while (s != NULL) {
	if (s->type == XGD_SITE_PIXMAP) {
	  int i;
	  for (i=0; i<s->Site.pixdef.count; i++)
	    XDestroyWindow(list->object->display, s->Site.pixdef.pwin[i]);
	}
	s = s->next;
      }
    }

    /* 
     * if we are deleting a barscale, we must remove the reference to it
     * from the geoframe object 
     */
    if (list->object->type == XGD_BARSCALE) {
      int i = 0;
      int count = -1;
      XgdObject *geoFrame = list->object->Obj.Barscale.gfobj;

      if ( geoFrame == NULL ) continue;

      for ( i = 0; i < geoFrame->Obj.GeoFrame.numbarscales; i++ ) {
          if ( list->object == geoFrame->Obj.GeoFrame.barscales[i] ) {
              count = i;
              break;
          }
      }
      if ( count != -1 ) {
          for ( i = count; (i+1) < geoFrame->Obj.GeoFrame.numbarscales; i++ ) {
              geoFrame->Obj.GeoFrame.barscales[i] = 
                  geoFrame->Obj.GeoFrame.barscales[i + 1];
          }
          geoFrame->Obj.GeoFrame.numbarscales--;
      }
    }
    if (list->object->type == XGD_LEGEND) {
      XgdObject *geoFrame = list->object->Obj.Legend.geoFrame;

      if ( geoFrame == NULL ) continue;

      geoFrame->Obj.GeoFrame.legend = NULL;
    }
    XgdDestroyObject(list->object);
  }
  Global.selectedObjects = NULL;
}

void
#ifdef _NO_PROTO
DeleteObjectFromList(list, obj)
     ObjectList *list;
     XgdObject  *obj;
#else
DeleteObjectFromList(ObjectList *list, XgdObject *obj)
#endif
{
  ObjectList cur, prev;
  ObjectList addlist = NULL, exclude = NULL;
  
  if (list == NULL || obj == NULL)
    return;

  AddObjectToList(&exclude, obj);
  GetBoxAffectedObjects(XgdGetBBoxOfObject(obj), &addlist, &exclude);

  cur = *list;
  prev = *list;

  while (cur != NULL){
    if (cur->object == obj){
      if (prev == cur){
        if ( IsObjectInList(Global.selectedObjects, obj) )
	  XgdDrawResizeHandles(obj, Global.xorGC);
	XgdUnDrawObject(obj, obj->bg, True);
	if (addlist){
	  ReorderObjects(&addlist);
	  DrawObjectsInList(addlist, NULL);
	}
	*list = (*list)->next;
	return;
      }
      prev->next = cur->next;
      if ( IsObjectInList(Global.selectedObjects, obj) )
	XgdDrawResizeHandles(obj, Global.xorGC);
      XgdUnDrawObject(obj, obj->bg, True);
      if (addlist){
	ReorderObjects(&addlist);
	DrawObjectsInList(addlist, NULL);
      }
      return;
    }
    prev = cur;
    cur = cur->next;
  }
}

void
#ifdef _NO_PROTO
AddObjectToList(list, obj)
    ObjectList *list;
    XgdObject *obj;
#else
AddObjectToList(ObjectList *list, XgdObject *obj)
#endif
{
    ObjectList ptr;

    if ( list == NULL ) {
        list = (ObjectList *)XtMalloc(sizeof(ObjectList));
        *list = NULL;
    }

    if ( obj == NULL ) {
        XgdWarning("AddObjectToList: Attempt to add NULL object");
        return;
    }

    if ( *list == NULL ) {
        *list = (ObjectList)XtMalloc(sizeof(ObjectListRec));
        (*list)->object = obj;
        (*list)->next = NULL;
    } else {
      ptr = (ObjectList) XtMalloc(sizeof(ObjectListRec));
      ptr->object = obj;
      ptr->next = *list;
      *list = ptr;
    }
}


void
#ifdef _NO_PROTO
PrintObjectList(list, string)
ObjectList list;
char *string;
#else
PrintObjectList(ObjectList list, char *string)
#endif
{
    ObjectList ptr = list;

    fprintf(stderr, "%s:", string);
    for ( ; ptr; ptr = ptr->next) {
        fprintf(stderr, ":%x",ptr->object);
    }
    fprintf(stderr,"\n");
    
}

void
#ifdef _NO_PROTO
ObjectToFront(list, obj)
    ObjectList *list;
    XgdObject *obj;
#else
ObjectToFront(ObjectList *list, XgdObject *obj)
#endif
{
    ObjectList head;
    ObjectList before;
    ObjectList ptr;

    if (list == NULL)
      return;
    if (*list == NULL)
      return;
    if ((*list)->next == NULL)
      return;
    if ((*list)->object == obj)
      return;
    head = *list;
    before = *list;
    for (ptr = (*list)->next; ptr && ptr->object != obj; before = ptr,ptr = ptr->next);
    if ( ptr == NULL ) {
        XgdError("ObjectToFront: object not in list");
    }
    before->next = ptr->next;
    ptr->next = head;
    *list = ptr;
}

void
#ifdef _NO_PROTO
ObjectToBack(list, obj)
    ObjectList *list;
    XgdObject *obj;
#else
ObjectToBack(ObjectList *list,XgdObject *obj)
#endif
{
  ObjectList before;
  ObjectList ptr;
  ObjectList tail;
  
  if (list == NULL)
    return;
  if (*list == NULL)
    return;
  if ((*list)->next == NULL)
    return;
  before = *list;
  ptr = (*list)->next;
  for (;ptr && ptr->object != obj; before = ptr, ptr = ptr->next);
  if ( ptr == NULL ) {
    if ((*list)->object == obj){
      ptr = *list;
      *list = (*list)->next;
      before->next = ptr;
      ptr->next = NULL;
      return;
    }
    XgdError("ObjectToFront: object not in list");
  }
  before->next = ptr->next;
  for ( tail = *list; tail->next; tail = tail->next);
  tail->next = ptr;
  tail->next->next = NULL;
}

void
#ifdef _NO_PROTO
FreeObjectList(list)
ObjectList list;
#else
FreeObjectList(ObjectList list)
#endif
{
    if ( list == NULL ) return;

    if ( list->next != NULL ) {
        FreeObjectList(list->next);
    }
    XtFree(list);
}

void 
#ifdef _NO_PROTO
GetPointSelectedObjects(list, x, y, newList)
ObjectList list;
int x, y;
ObjectList *newList;
#else
GetPointSelectedObjects(ObjectList list, int x, int y, ObjectList *newList)
#endif
{
    ObjectList ptr = list;

    for (; ptr; ptr = ptr->next ) {
       if ( XgdIsPointInObject(ptr->object, x, y)) {
	    AddObjectToList(newList, ptr->object);
	    return;
       }
    }
}

void
#ifdef _NO_PROTO
GetBoxSelectedObjects(box, newList)
XgdBox *box;
ObjectList *newList;
#else
GetBoxSelectedObjects(XgdBox *box, ObjectList *newList)
#endif
{
    ObjectList ptr = Global.objectList;

    for (; ptr; ptr = ptr->next ) {
       if ( XgdIsObjectInBox(ptr->object, box)) {
	    AddObjectToList(newList, ptr->object);
       }
    }
}

void
#ifdef _NO_PROTO
DrawObjectsInList(list, pix)
     ObjectList list;
     Pixmap     pix;
#else
DrawObjectsInList(ObjectList list, Pixmap pix)
#endif
{
  Boolean pixmap = False;
  XEvent event;
  
  event.type = Expose;
  event.xexpose.display = Global.display;
  event.xexpose.window = XtWindow(Global.drawArea);
  
  if ( list == NULL ) return;
  if ( list->object == NULL ) return;
  
  if ( list->next != NULL ) {
    DrawObjectsInList(list->next, pix);
  }

  if (list->object->type == XGD_GEOFRAME){
    XgdSiteInfo *s = list->object->Obj.GeoFrame.sites.site;

    while(s){
      if (s->type == XGD_SITE_PIXMAP){
	pixmap = True;
	break;
      }
	
      s = s->next;
    }
  }

  XgdDrawObject(XgdGetGCOfObject(list->object), list->object, True, pix);
  if ( IsObjectInList(Global.selectedObjects, list->object) && !pix )
    XgdDrawResizeHandles(list->object, Global.xorGC);

}

void
#ifdef _NO_PROTO
UnDrawObjectsInList(list, doHandles)
     ObjectList list;
     Boolean    doHandles;
#else
UnDrawObjectsInList(ObjectList list, Boolean doHandles)
#endif
{
  if ( list == NULL ) return;
  if ( list->object == NULL ) return;
  
  if ( list->next != NULL ) {
   UnDrawObjectsInList(list->next, doHandles);
  }
  
  if ( IsObjectInList(Global.selectedObjects, list->object ) && doHandles)
    XgdDrawResizeHandles(list->object, Global.xorGC);
  XgdUnDrawObject(list->object, list->object->bg, True);
}


int
#ifdef _NO_PROTO
SelectedObjectCount()
#else
SelectedObjectCount(void)
#endif
{
    ObjectList list = Global.selectedObjects;

    return CountObjects(list);
}
int
#ifdef _NO_PROTO
CountObjects(list)
ObjectList list;
#else
CountObjects(ObjectList list)
#endif
{
    int n = 0;

    while ( list ) {
      n++;
      list = list->next;
    }
    return n;
}

XgdBox *
#ifdef _NO_PROTO
GetBBoxOfObjects(list)
     ObjectList list;
#else
GetBBoxOfObjects(ObjectList list)
#endif
{
  int r = 0, l = BigInt(), b = 0, t = BigInt();

  for ( ; list; list = list->next ) {
    XgdBox *bb = XgdGetBBoxOfObject(list->object);
    
    if ( bb->l < l ) l = bb->l;
    if ( bb->t < t ) t = bb->t;
    if ( bb->b > b ) b = bb->b;
    if ( bb->r > r ) r = bb->r;
  }
  return XgdSetBox(t, b, l, r);
}
  
XgdBox *
#ifdef _NO_PROTO
GetBBoxOfSelectedObjects()
#else
GetBBoxOfSelectedObjects(void)
#endif
{
    ObjectList list = Global.selectedObjects;
    int r = 0, l = BigInt(), b = 0, t = BigInt();

    for ( ; list; list = list->next ) {
        XgdBox *bb = XgdGetBBoxOfObject(list->object);

        if ( bb->l < l ) l = bb->l;
        if ( bb->t < t ) t = bb->t;
        if ( bb->b > b ) b = bb->b;
        if ( bb->r > r ) r = bb->r;
    }
    return XgdSetBox(t, b, l, r);
}

int 
#ifdef _NO_PROTO
BigInt()
#else
BigInt(void)
#endif
{
    int ret = 128, i;
    for ( i = 2; i < sizeof(int); i++) ret *= 256;
    return ret;
}

void 
#ifdef _NO_PROTO
GetAffectedObjects(obj, addlist, exclude)
XgdObject *obj;
ObjectList *addlist;
ObjectList *exclude;
#else
GetAffectedObjects(XgdObject *obj, ObjectList *addlist, ObjectList *exclude)
#endif
{
    XgdBox *obb = XgdGetBBoxOfObject(obj);

    GetBoxAffectedObjects(obb, addlist, exclude);
}

void
#ifdef _NO_PROTO
GetBoxAffectedObjects(obb, addlist, exclude)
     XgdBox *obb;
     ObjectList *addlist;
     ObjectList *exclude;
#else
GetBoxAffectedObjects(XgdBox *obb,  ObjectList *addlist, ObjectList *exclude)
#endif
{
  ObjectList list = Global.objectList;
  ObjectList eptr;
  
  for ( ; list; list = list->next) {
    XgdBox *bb;
    Boolean ignore = False;
    
    if ( exclude != NULL ) {
      for ( eptr = *exclude; eptr; eptr = eptr->next ) {
	if ( eptr->object == list->object ) {
	  ignore = True;
	}
      }
    }
    if ( !ignore ) {
      bb = XgdGetBBoxOfObject(list->object);
      
      if ( XgdBoxesOverlap(*obb, *bb) ) {
	AddObjectToList(addlist, list->object);
	AddObjectToList(exclude, list->object);
	GetBoxAffectedObjects(bb, addlist, exclude);
      }
    }
  }
}

typedef struct _objPos {
    int position;
    ObjectList ptr;
} ObjPos;

void
#ifdef _NO_PROTO
ReorderObjects(list)
     ObjectList *list;
#else
ReorderObjects( ObjectList *list)
#endif
{
  ObjectList glist = Global.objectList;
  ObjectList gptr;
  ObjectList lptr;
  ObjectList ret = NULL;
  int count;
  int i, j = 0;
  int k, l;
  ObjPos *pos;
  
  if (list == NULL)
    return;
  
  if (*list == NULL)
    return;
  
  count = CountObjects(*list);
  pos = (ObjPos *)XtCalloc(count, sizeof(ObjPos));
  bzero((char *) pos, sizeof(ObjPos) * count);
  for ( lptr = *list; lptr; lptr = lptr->next ) {
    for ( i = 0, gptr = glist; gptr; i++, gptr = gptr->next) {
      if ( lptr->object == gptr->object ) {
	pos[j].position = i;
	pos[j].ptr = lptr;
	++j;
      }
    }
  }
  for ( k = 0; k < count - 1; k++ ) {
    for ( l = 0; l < count - k - 1; l++ ) {
      if ( pos[l].position > pos[l + 1].position ) {
	ObjPos tmp;
	
	bcopy((char *)&pos[l], (char *)&tmp, sizeof(ObjPos));
	bcopy((char *)&pos[l+1], (char *)&pos[l], sizeof(ObjPos));
	bcopy((char *)&tmp, (char *)&pos[l+1], sizeof(ObjPos));
      }
    }
  }
  for ( k = count - 1; k >=0 ; k-- ) {
    AddObjectToList(&ret, pos[k].ptr->object);
  }
  *list = ret;
}
