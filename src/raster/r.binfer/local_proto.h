/* gram.c */
int yyparse(void);
/* main.c */
int yyerror(char *);
int parse_arglist(int, char **);
/* misc.c */
int nonfatal(char *);
int fatal(char *);
char *strip_quotes(char *);
/* symtab.c */
int init(void);
int yyfatal(char *);
int add_name(char *);
int add_prob_list(char *);
int ExecuteReclass(char *);
/* yywrap.c */
int yywrap(void);
