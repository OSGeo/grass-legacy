/**** parse.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/



struct ARGS {
    char *MapA;
    char *MapB;
    char *Out;
    char *Type;
};

extern int output_open;
/* parse.c */
int parse_args(int, char *[], struct ARGS *);
int open_files(struct ARGS *, struct Map_info [2], struct Map_info *);
