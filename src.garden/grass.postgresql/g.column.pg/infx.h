struct Query
{
	char *select	;   /* SELECT portion of db query 	*/
	char *from	;   /* FROM portion of db query 	*/
	char *where	;   /* WHERE clause in db query 	*/
	char *order	;   /* ORDER BY clause in db query	*/
};

struct Sys_tables
{
	int tabid	;   /* table id number in systables	*/
	char *tabname	;   /* table name in systables		*/
};

struct Sys_column
{
	int tabid	;  /* table id in syscolumns		*/
	char *colname	;  /* column name in syscolumns		*/
	int coltype	;  /* data type of column		*/
	int collength	;  /* storage size of column		*/
	char *dataname	;  /* ascii string name for data type 	*/
};

struct Stats
{
	int count	;  /* count aggregate			*/
	float sum	;  /* sum column aggregate		*/
	float avg	;  /* average agregate			*/
	float min	;  /* min column value			*/
	float max	;  /* max column value			*/
	float freq	;  /* freq not used currently		*/
	float mode	;  /* mode for column value		*/
};
