/* SQL Parser */

/* KEYWORD OPS */
#define SQLP_OP   1
#define SQLP_AND  2
#define SQLP_OR   3
#define SQLP_NOT  4

/* SQL COMMANDS */
#define SQLP_CREATE 1
#define SQLP_DROP   2
#define SQLP_INSERT 3
#define SQLP_SELECT 4
#define SQLP_UPDATE 5
#define SQLP_DELETE 6

/* SQL OPERATORS */
#define SQLP_EQ 1    /* =  */
#define SQLP_LT 2    /* <  */
#define SQLP_LE 3    /* <= */
#define SQLP_GT 4    /* >  */
#define SQLP_GE 5    /* >= */
#define SQLP_NE 6    /* <> */
#define SQLP_ADD 7    /* + */
#define SQLP_SUBTR 8    /* - */
#define SQLP_MLTP 9    /* * */
#define SQLP_DIV 10    /* / */
#define SQLP_MTCH 11    /* ~ */

/* SQL VALUE TYPES, NOT COLUMN TYPES -
-do not change these! leval/reval =2 or .5 for int/double compat.*/
#define SQLP_S 0x01 /* string */ 
#define SQLP_I 0x04 /* integer */
#define SQLP_D 0x08 /* float */
#define SQLP_COL 0x0C /* just column */

/* SQL COLUMN TYPES */
#define SQLP_VARCHAR 1 
#define SQLP_INTEGER 2 
#define SQLP_DOUBLE  3 
#define SQLP_DATE    4

#define SQLP_MAX_TABLE  200  
#define SQLP_MAX_ERR    500  

typedef enum NodeTag
{
	T_A_Expr = 700,
	T_ArithmExpr,
	T_ArithmValue
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

typedef struct ArithmExpr
{
	NodeTag		type;
	int		oper;			/* type of operation (OP,OR,AND,NOT) */
	int	        opname;			/* name of operator */
	Node	   *lexpr;			/* left argument */
	Node	   *rexpr;			/* right argument */
} ArithmExpr;

typedef struct ArithmValue
{
	NodeTag		type;
	int		vtype;			/* type of operation (OP,OR,AND,NOT) */
	char 		*s;
	int		i;
	double		d;
} ArithmValue;

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
    char      *orderCol;  /* column name which should be used for sorting (ORDER BY) or NULL (no sorting) */
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
void sqpOrderColumn( char *col );
int translate_Operator( char *oper);

Node *makeA_Expr(int oper, int opname, Node *lexpr, Node *rexpr);
Node *makeArithmExpr(int opname, Node *lexpr, Node *rexpr);
Node *makeArithmValue( char *strval, int intval, double dblval, int type, int factor );

static Node *newNode(int size, NodeTag tag);

#ifdef SQLP_MAIN
SQLPSTMT *sqlpStmt;
#else
extern SQLPSTMT *sqlpStmt;
#endif


