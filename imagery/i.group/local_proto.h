/* main.c */
int main(int, char *[]);
int remove_group_files (char [30], char **, int);
int remove_subgroup_files (char [30], char [30], char **, int);
int add_or_update_group (char [30], char **, int);
int add_or_update_subgroup (char [30], char [30], char **, int);
int list_group_files(char [30], FILE *);
int list_subgroup_files(char [30], char [30], FILE *);
