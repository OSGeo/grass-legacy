/* SQL Parser */

/* KEYWORD OPS */
#define OP 1
#define AND 2
#define OR   3
#define NOT   4

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

typedef enum NodeTag
{
	T_A_Expr = 700,
	T_Comparison
} NodeTag;

typedef struct Node
{
	NodeTag		type;
} Node;

typedef struct A_Expr
{
	NodeTag		type;
	int		oper;			/* type of operation (OP,OR,AND,NOT) */
	int	        opname;			/* name of operator */
	Node	   *lexpr;			/* left argument */
	Node	   *rexpr;			/* right argument */
} A_Expr;



#define nodeTag(nodeptr)		(((Node*)(nodeptr))->type)

#define makeNode(_type_)		((_type_ *) newNode(sizeof(_type_),T_##_type_))
#define NodeSetTag(nodeptr,t)	(((Node*)(nodeptr))->type = (t))

#define IsA(nodeptr,_type_)		(nodeTag(nodeptr) == T_##_type_)


typedef struct
{
    int    type;
    char   *s;
    int    i;
    double d;
} SQLPVALUE;

typedef struct Comparison
{
	NodeTag		type;
	int		oper;			/* type of operation (should be OP) */
	int	        opname;			/* name of operator */
	SQLPVALUE	 *lexpr;		/* left argument */
	SQLPVALUE	 *rexpr;		/* right argument */
} Comparison;

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
    Node      *upperNodeptr;
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

Node *parseComparison( char *column, char *oper, char *strval, int intval, double dblval, int type );
Node *makeA_Expr(int oper, int opname, Node *lexpr, Node *rexpr);

static Node *newNode(int size, NodeTag tag);
static Node *makeComparison(int oper, int opname, SQLPVALUE *lexpr, SQLPVALUE *rexpr);


#ifdef SQLP_MAIN
SQLPSTMT *sqlpStmt;
#else
extern SQLPSTMT *sqlpStmt;
#endif


