#include <stdio.h>

#ifdef MAIN

int pol_flg;
int lin_flg;
int lab_flg;
int txt_flg;
char separator;
int space = 0;

#else

extern int pol_flg;
extern int lin_flg;
extern int lab_flg;
extern int txt_flg;
extern char separator;
extern int space;

#endif
