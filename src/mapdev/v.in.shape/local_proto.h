/* dbfopen.c */
DBFHandle DBFOpen(const char *, const char *);
void DBFClose(DBFHandle);
DBFHandle DBFCreate(const char *);
int DBFAddField(DBFHandle, const char *, DBFFieldType, int, int);
int DBFReadIntegerAttribute(DBFHandle, int, int);
double DBFReadDoubleAttribute(DBFHandle, int, int);
const char *DBFReadStringAttribute(DBFHandle, int, int) = {0};
int DBFGetFieldCount(DBFHandle);
int DBFGetRecordCount(DBFHandle);
DBFFieldType DBFGetFieldInfo(DBFHandle, int, char *, int *, int *);
int DBFWriteDoubleAttribute(DBFHandle, int, int, double);
int DBFWriteIntegerAttribute(DBFHandle, int, int, int);
int DBFWriteStringAttribute(DBFHandle, int, int, const char *);
/* shpopen.c */
SHPHandle SHPOpen(const char *, const char *);
void SHPClose(SHPHandle);
void SHPGetInfo(SHPHandle, int *, int *, double *, double *);
SHPHandle SHPCreate(const char *, int);
void SHPComputeExtents(SHPObject *);
SHPObject *SHPCreateObject(int, int, int, int *, int *, int, double *, double *, double *, double *);
SHPObject *SHPCreateSimpleObject(int, int, double *, double *, double *);
int SHPWriteObject(SHPHandle, int, SHPObject *);
SHPObject *SHPReadObject(SHPHandle, int);
const char *SHPTypeName(int) = {0};
const char *SHPPartTypeName(int) = {0};
void SHPDestroyObject(SHPObject *);
/* shp2dig.c */
void linedDispose( lineList *l1, fieldDescript *cat1, int fieldCount );
int pntsort( const void *pnt1, const void *pnt2 );
int pntsCoincide( pntDescript *p1, pntDescript *p2 );
void thirdPoint( pntDescript *pntArray );
void partCalcFieldsArc( partDescript *partd );
void partCalcFieldsPolygon( partDescript *partd );
void getTotalParts( lineList *L1 );
void getValidParts( lineDescript *line1 );
void locateNewCentroid( double *xpos1, double *xpos2, 
			double xcentre, pntDescript *isects, int nIsects );
double getTheta( double x1, double x2 );
int isIslandOf( partDescript *part1, partDescript *part2 );
int pntInside( partDescript *part1, partDescript *part2, double *maxIsect );
void recalcCentroid( partDescript *part1, double intsect );
int procMapType( int iswitch, int *mtype );
/* dbutils.c */
int vertRegister( BTREE *hDB, partDescript *part1, int pt_indx );
char *calcKeyValue( pntDescript *pnt1, float sr, int decs, double fe, double fn );
int btree_compare( char *key1, char *key2 );
int procSnapDistance( int iswitch, float *sd );
int procMinSubtend( int iswitch, float *sd );
int proc_key_params( int, int *, double *, double * );
/* writelin.c */
int vbase2segd( segmentList *seg0, BTREE *btr0 );
int segLDispose( segmentList *seg0 );
double reverse_angle(double phi0);
int check_terminal_snapback( pntDescript *ppnt1, pntDescript *ppnt2, pntDescript **pmiddle );
void build_half_lines( pntDescript *ppntx, segmentList *segl );
void extract_simple_link( segmentList *seg1, pntDescript *pt1, pntDescript *pt2 );
/* gbtree.c */
int btree_create(BTREE *, int (*)(), int);
int btree_find(BTREE *, char *, void **);
int btree_free(BTREE *);
int btree_rewind(BTREE *);
int btree_update(BTREE *, char *, int, void *, int);
/* cleanup.c */ 
void vector_map_cleanup( char *mapname );
