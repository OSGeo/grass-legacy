int extract_lines( SHPObject **, struct Map_info *, int *, int *, const int, int );
int extract_points( SHPObject **, struct Map_info *, int *, int *, const int );
int extract_ring( SHPObject **, struct Map_info *, int *, int *, const int );
int load_dbf_from_fieldD( DBFHandle, fieldDescriptor *, const int );
int load_dbf_from_table( DBFHandle, fieldDescriptor *, char *, char * );
int proc_logfile( int, char * );
