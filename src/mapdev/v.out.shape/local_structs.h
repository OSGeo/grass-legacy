#define GET_VAL 0
#define SET_VAL 1

typedef union  {
  char *stringField;
  int intField;
  double doubleField;
} dbfRecElement;


typedef struct {
  int duff;
  int fldSize;
  int fldDec;
  int nRec;
  DBFFieldType fldType;
  char fldName[12];
  dbfRecElement *fldRecs;
} fieldDescriptor;

typedef struct {

  int nlines;
  struct line_pnts **llist;
  int *alive;
} line_repository;
