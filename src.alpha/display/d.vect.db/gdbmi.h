#include <stdio.h>
#include "dbmi.h"

#define API_VERSION "0"

#define GDB_DEFAULT_DEPTH 20

typedef struct _gdbLinkEdgeSet
{
  int            numEdges;
  struct _gdbLinkEdge **edges;
} gdbLinkEdgeSet;

typedef struct _gdbLinkNodeSet
{
  int            numNodes;
  struct _gdbLinkNode **nodes;
} gdbLinkNodeSet;

typedef struct _gdbLinkNode
{
  dbDriver      *driver;
  dbString       driverName;
  dbHandle       handle;
  dbTable        table;
  int            mark;
  gdbLinkEdgeSet edges;
} gdbLinkNode;

typedef struct _gdbLinkEdge
{
  gdbLinkNode   *node;
  gdbLinkNode   *refNode;
  int            numColumns;
  dbString      *columnNames;
  dbString      *refColumnNames;
} gdbLinkEdge;

typedef struct _gdbLink
{
  gdbLinkNode   *R1;
  gdbLinkNode   *R2;
  gdbLinkEdge   *edge;
} gdbLink;

typedef struct _gdbTheme
{
  int            tid;        /* TAG TID - Theme ID */
  dbString       themeName;  /* The filename of the theme file -- 14 char max */
  dbString       themeDesc;  /* Tag TD  - A user description of the theme */
  gdbLinkNode   *linkNode;   /* gdbLinkNode for Link Table */
} gdbTheme;


