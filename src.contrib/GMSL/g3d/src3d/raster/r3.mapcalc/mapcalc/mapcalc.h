/* assign.c */
int assignment(FILE *, char *);
/* compare.c */
int eq_i(CELL *, CELL *, int);
int eq_x(double *, double *, int);
int ne_i(CELL *, CELL *, int);
int ne_x(double *, double *, int);
int gt_i(CELL *, CELL *, int);
int gt_x(double *, double *, int);
int ge_i(CELL *, CELL *, int);
int ge_x(double *, double *, int);
int lt_i(CELL *, CELL *, int);
int lt_x(double *, double *, int);
int le_i(CELL *, CELL *, int);
int le_x(double *, double *, int);
/* convert.c */
int buffer_is_double(int);
int convert_to_double(int);
int convert_to_cell(int);
/* evaluate.c */
int evaluate(int, int);
int grow_execute_stack(void);
/* execute.c */
int execute(char *);
int free_execute_stack(void);
/* expression.c */
int push_expression_map(int, int, int, int);
int push_expression_set_variable(int);
int push_expression_get_variable(int);
int push_expression_integer(int, int, int);
int push_expression_double(double, int, int);
int push_expression_op(int);
int push_expression_function(int, int);
int push_expression(int, int, int, double, int, int);
int free_expression_stack(void);
/* fpe.c */
void fpe(int);
/* function.c */
int find_function(char *, int *);
int print_function_names(void);
/* input.c */
int input(char *);
/* logical.c */
int and_i(CELL *, CELL *, int);
int and_x(double *, double *, int);
int or_i(CELL *, CELL *, int);
int or_x(double *, double *, int);
/* maps.c */
int openmap(char *, char *, int *, int);
int configmaps(int);
int closemaps(void);
int readmap(int, CELL *, double *, int, int, int, int);
int initcats(int);
int initcolors(int);
int translate_from_cats(CELL *, double *, int, int);
int translate_from_colors (DCELL *,CELL *,int,int,int);
int map_is_fp(char *, char *);
/* math.c */
int add(CELL *, CELL *, int);
int add_x(double *, double *, int);
int subtract(CELL *, CELL *, int);
int subtract_x(double *, double *, int);
int multiply(CELL *, CELL *, int);
int multiply_x(double *, double *, int);
int divide(CELL *, CELL *, int);
int divide_x(double *, double *, int);
int modulus(CELL *, CELL *, int);
int modulus_x(double *, double *, int);
int power_x(double *, double *, int);
/* min_max.c */
/* percent.c */
int percent(int, int, int);
/* polish.c */
int polish(FILE *, char *);
/* pool.c */
char *get_buffer_from_pool(int *);
int return_buffer_to_pool(int);
/* range.c */
void print_range(char *);
int get_cell_range(char *, char *, long *, long *);
int get_dcell_range(char *, char *, double *, double *);
/* readrow.c */
void set_readrow_for_fp(int);
int readrow(int,char *,int,int);
/* round.c */
CELL round(double);
/* support.c */
int create_support_files(char *, char *);
int getsome(FILE *, char *, int);
/* variables.c */
int create_variable(int);
int copy_cell_to_variable(CELL *, int, int);
int copy_double_to_variable(double *, int, int);
int copy_variable_to_cell(CELL *, int, int);
int copy_variable_to_double(double *, int, int);
int variable_is_double(int);
/* xrand.c */
int g_randseed(void);
