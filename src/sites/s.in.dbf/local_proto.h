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
