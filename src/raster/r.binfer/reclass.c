/*
**                               
** Filename: reclass.c
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

#include "symtab.h"
struct symtab *index_to_att();

do_reclass(ptr)
    struct symtab *ptr;
{
    struct symtab *local = ptr;
    int init = 0;
    char cmd[256];
    FILE *fopen(),*reclassfp;
    char reclassfile[256];
    
    local = index_to_att(local,LayerAttribute,init);
    while(local->s_type  == AttributeSymbol ) {
        if ( local->element.att->a_type != LayerAttribute )  break;
        sprintf(local->reclassname,"r.binferXX%s",local->name);
        sprintf(reclassfile,"/tmp/%sXXX",local->name);
        fprintf(stderr,".");
        sprintf(cmd,"r.reclass input=%s output=r.binferXX%s < /tmp/%sXXX",
                local->element.att->name,local->name,local->name);
        if ((reclassfp = fopen(reclassfile,"w")) == NULL ) {
            fprintf(stderr,"cannot open reclass rule file.\n");
            exit(0);
        }
        local = local->next;
        while(local->s_type == ValueSymbol) {
            fprintf(reclassfp,"%s\n",local->reclass);
            local = local->next;
        }
        fprintf(reclassfp,"end\n");
        fclose(reclassfp);
        system(cmd);
        unlink(reclassfile);
    }
    fprintf(stderr,"\n");
        
}

cleanup_reclass(ptr)
    struct symtab *ptr;
{
    struct symtab *local = ptr;
    int init = 0;
    char cmd[256];

    fprintf(stderr,"Removing intermediate maps");
    while(( local = index_to_att(local,LayerAttribute,init) ) != NULL ) {
        init = 1;
        fprintf(stderr,".");
        sprintf(cmd,"g.remove rast=%s > /dev/null",local->reclassname);
        system(cmd);
    }
    fprintf(stderr,"\n");
}
