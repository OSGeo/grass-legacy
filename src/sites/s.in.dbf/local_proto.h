/* dbfopen.c */
DBFHandle DBFOpen(const char *, const char *);
void DBFClose(DBFHandle);
DBFHandle DBFCreate(const char *);
int DBFAddField(DBFHandle, const char *, DBFFieldType, int, int);
int DBFReadIntegerAttribute(DBFHandle, int, int);
double DBFReadDoubleAttribute(DBFHandle, int, int);
const char *DBFReadStringAttribute(DBFHandle, int, int);
int DBFGetFieldCount(DBFHandle);
int DBFGetRecordCount(DBFHandle);
DBFFieldType DBFGetFieldInfo(DBFHandle, int, char *, int *, int *);
int DBFWriteDoubleAttribute(DBFHandle, int, int, double);
int DBFWriteIntegerAttribute(DBFHandle, int, int, int);
int DBFWriteStringAttribute(DBFHandle, int, int, const char *);
char * G_strchg(char*, char, char);
char * G_chop (char *);
DBFFieldType DBFGetFieldInfo( DBFHandle psDBF, int iField, char * pszFieldName,int * pnWidth, int * pnDecimals );

/* dump.c */
int DumpFromDBF (char *, char *, struct TimeStamp, int);
int DBFDumpASCII(DBFHandle, FILE *, int);
