/* SQL Parser */

/* SQL COMMANDS */
#define SQLP_CREATE 1
#define SQLP_DROP   2
#define SQLP_INSERT 3
#define SQLP_SELECT 4
#define SQLP_UPDATE 5
#define SQLP_DELETE 6

/* SQL COMPARISONS */
#define SQLP_EQ 1    /* =  */
#define SQLP_LT 2    /* <  */
#define SQLP_LE 3    /* <= */
#define SQLP_GT 4    /* >  */
#define SQLP_GE 5    /* >= */
#define SQLP_NE 6    /* <> */

/* SQL VALUE TYPES, NOT COLUMN TYPES */
#define SQLP_S 0x01 /* string */ 
#define SQLP_I 0x02 /* integer */
#define SQLP_D 0x04 /* float */

/* SQL COLUMN TYPES */
#define SQLP_VARCHAR 1 
#define SQLP_INTEGER 2 
#define SQLP_DOUBLE  3 

#define SQLP_MAX_TABLE  200  
#define SQLP_MAX_ERR    500  

typedef struct
{
    int    type;
    char   *s;
    int    i;
    double d;
} SQLPVALUE;

typedef struct
{
    char      *stmt;    /* input statement string */
    char      *cur;     /* cursor for parser */
    char      errmsg[ SQLP_MAX_ERR + 1 ];   
    int       command;
    char      table[ SQLP_MAX_TABLE + 1 ];
    SQLPVALUE *Col;       /* column names */
    int	      *ColType;	
    int	      *ColWidth;	 /* length */
    int	      *ColDecim;	 /* decimals */
    int	      aCol;      /* allocated */
    int	      nCol;	/* number of columns */
    SQLPVALUE *Val;    /* values */
    int	      aVal; 	
    int	      nVal;
    int	      aCom;   /* comparisons */
    int	      nCom;
    SQLPVALUE *ComCol; /* comparison columns */
    int	      *ComOpe;    /* comparison operators */
    SQLPVALUE *ComVal; /* comparison values */
} SQLPSTMT;

int	my_yyinput(char *buf, int max_size);
void	yyerror( char *s );
int     yyparse();
int     yywrap();

int sqpSaveStr(SQLPVALUE *st, char *c);

SQLPSTMT *sqpInitStmt( void ); 
int sqpFreeStmt(SQLPSTMT *st);
int sqpPrintStmt(SQLPSTMT *st);
int sqpAllocCol(SQLPSTMT *st, int n );
int sqpAllocVal(SQLPSTMT *st, int n );
int sqpAllocCom(SQLPSTMT *st, int n );
int sqpInitParser(SQLPSTMT *st);

void sqpCommand( int command );
void sqpTable( char *table );
void sqpColumn( char *column );
void sqpColumnDef( char *column, int type, int width, int decimals );
void sqpValue( char *strval, int intval, double dblval, int type );
void sqpAssignment( char *column, char *strval, int intval, double dblval, int type );
void sqpComparison( char *column, char *oper, char *strval, int intval, double dblval, int type );

#ifdef SQLP_MAIN
SQLPSTMT *sqlpStmt;
#else
extern SQLPSTMT *sqlpStmt;
#endif


