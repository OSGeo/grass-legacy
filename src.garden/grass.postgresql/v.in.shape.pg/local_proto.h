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
