
extern void * rsNew (/* n, GTfunction */);
extern void rsDispose (/* Tree */);

extern void rsReset (/* Tree */);

extern void rsDelete (/* Tree, x */);
extern void rsInsert (/* Tree, x */);

extern int rsLargest (/* Tree */);
extern int rsSmallest (/* Tree */);

extern int rsFindInit (/* Tree, x */);
extern int rsNextSmaller (/* Tree */);
extern int rsNextLarger (/* Tree */);

extern void rsPrintTree (/* Tree */);











