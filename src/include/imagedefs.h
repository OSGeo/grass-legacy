struct ClassSig *I_NewClassSig();
struct SubSig *I_NewSubSig();
struct ClassData *I_AllocClassData ();
char *I_GetClassTitle();
char *I_GetSigTitle();

int *I_ask_bands();
char *I_bandname();
char *I_bandname_prefix();

char *I_malloc();
char *I_realloc();


int *I_alloc_int();
int **I_alloc_int2();
double *I_alloc_double();
double **I_alloc_double2();
double ***I_alloc_double3();

FILE *I_fopen_group_file_old ();
FILE *I_fopen_group_file_new ();
FILE *I_fopen_group_file_append ();
FILE *I_fopen_subgroup_file_old ();
FILE *I_fopen_subgroup_file_new ();

FILE *I_fopen_group_ref_old ();
FILE *I_fopen_group_ref_new ();
FILE *I_fopen_group_ref_append ();
FILE *I_fopen_subgroup_ref_old ();
FILE *I_fopen_subgroup_ref_new ();
FILE *I_fopen_signature_file_new ();
FILE *I_fopen_signature_file_old ();
FILE *I_fopen_sigset_file_new ();
FILE *I_fopen_sigset_file_old ();
int *I_color_conversion_table ();
