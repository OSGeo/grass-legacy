
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif

{
	int              int_val;
	struct Node     *nod_val;
	char            *str_val;
} YYSTYPE;
extern YYSTYPE yylval;
# define AND_TKN 257
# define OR_TKN 258
# define NOT_TKN 259
# define GRP_TKN 260
# define CATS_TKN 261
# define EXPR_TKN 262
# define RANGE_TKN 263
# define NAM_TKN 264
# define OVR_TKN 265
# define COV_TKN 266
# define WIN_TKN 267
# define BYE_TKN 268
# define ERA_TKN 269
# define HST_TKN 270
# define HLP_TKN 271
# define NAM_STR 272
# define INUMBER 273
# define LP 274
# define RP 275
# define SEMI 276
