typedef struct {
  int child [2];
  int priority;
} rsEntryType;

typedef struct {
  int maxn;
  int root;
  int (*GT) ();
  rsEntryType *entry;
  stackType *parentStack;
  int lastX, parentX;
  int xGTparent;
  int xFound;
} rsType;

/* randtree.c */
rsType *rsNew(int, int (*)(int,int));
void rsDispose(rsType *);
void rsReset(rsType *);
void rsDelete(rsType *, int);
void rsInsert(rsType *, int);
void rsPrintTree(rsType *);
int rsLargest(rsType *);
int rsSmallest(rsType *);
int rsNextSmaller(rsType *);
int rsNextLarger(rsType *);
int rsFindInit(rsType *, int);
int testrand(void);
