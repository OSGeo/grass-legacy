/*
**                               
** Filename: main.c
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

#include <stdio.h>
#include "symtab.h"
extern int yylineno;
extern char yytext[];
extern FILE *yyin;
int verbose;
int probabilitymaps;
int combinedmap;
int colortable;
char *result = "binfer";
extern struct symtab table;

main(argc,argv)
int argc;
char **argv;
{
    struct symtab *ptr;
    FILE *fopen();


    probabilitymaps = 1;
    combinedmap = 1;
    colortable = Ramp;

    parse_arglist(argc,argv);
    G_gisinit(argv[0]);
    ptr = (struct symtab *)yyparse();
    fclose(yyin);
    fprintf(stderr,"Creating intermediate maps");
    do_reclass(ptr);
    engine(ptr,result);
    fprintf("Removing intermediate maps\n");
    cleanup_reclass(ptr);
}



yyerror(s)
char *s;
{
    fprintf(stderr,"\n%s: line number %d at or near \"%s\"\n",s,yylineno,yytext);
    exit(0);
}


parse_arglist(argc,argv)
int argc;
char **argv;
{
    char errmsg[80];

    verbose = 0;
    if ( argc > 1 ) {
        if ( !strcmp(argv[1],"-v")) {
            verbose = 1;
            switch ( argc ) {
            case 2:
                usage(argv[0]);
                break;
            case 3:
                if ((yyin = fopen(argv[2],"r")) == NULL ) {
                    sprintf(errmsg,"Couldn't open scriptfile %s\n",argv[2]);
                    fatal(errmsg);
                }
                break;
            case 4:
                if ((yyin = fopen(argv[2],"r")) == NULL ) {
                    sprintf(errmsg,"Couldn't open scriptfile %s\n",argv[2]);
                    fatal(errmsg);
                }
                result = argv[3];
                break;
            default:
                usage(argv[0]);
                break;
            }
        } else {
            switch ( argc ) {
            case 2:
                if ((yyin = fopen(argv[1],"r")) == NULL ) {
                    sprintf(errmsg,"Couldn't open scriptfile %s\n",argv[1]);
                    fatal(errmsg);
                }
                break;
            case 3:
                if ((yyin = fopen(argv[1],"r")) == NULL ) {
                    sprintf(errmsg,"Couldn't open scriptfile %s\n",argv[1]);
                    fatal(errmsg);
                }
                result = argv[2];
                break;
            default:
                usage(argv[0]);
                break;
            }
        }
    }
    else
        usage(argv[0]);
}


usage(me)
    char *me;
{
    fprintf(stderr,"Usgae: %s [-v] scriptfile [outputcellfile]\n",me);
    exit(-1);
}
