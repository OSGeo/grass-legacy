#ifndef _BBOX_H
#define _BBOX_H



typedef struct _XgdBox {
  int t;        /* top */
  int b;        /* bottom */
  int l;        /* left */
  int r;        /* right */
} XgdBox;

typedef struct _XgdLine {
  int  x1, y1;          /* first point */
  int  x2, y2;          /* end point */
} XgdLine;

#endif
