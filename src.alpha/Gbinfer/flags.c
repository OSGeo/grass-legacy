/*
**                               
** Filename: flags.c
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

#include <stdio.h>
#include "symtab.h"

extern int verbose;
struct symtab *index_to_att();
struct symtab *index_to_val();

set_context_flags(ptr)
struct symtab *ptr;
{
    struct symtab *local = ptr->next;
    int init = 0;
    int value;

    while((local = index_to_att(local,ContextAttribute,init)) != NULL ) {
        init = 1;
        value = get_value(local);
        fprintf(stderr,"\n");
        set_flags(local,value);
        local = local->next;
    }
}

set_subjective_flags(ptr)
struct symtab *ptr;
{
    struct symtab *local = ptr->next;
    int init = 0;
    int value;

    while((local = index_to_att(local,SubjectiveAttribute,init)) != NULL ) {
        init = 1;
        value = get_value(local);
        fprintf(stderr,"\n");
        set_flags(local,value);
        local = local->next;
    }
}

set_layer_flags(ptr)
struct symtab *ptr;
{
    struct symtab *local = ptr;
    struct symtab *local2;
    int init = 0;
    int init2 = 0;
    int cellval;

    while((local = index_to_att(local,LayerAttribute,init)) != NULL ) {
        init = 1;
        cellval = *local->element.att->cellptr++;
        init2 = 0;
        local2 = local->next;
        while((local2 = index_to_val(local2,init2)) != NULL ) {
            init2 = 1;
            if ( cellval == local2->element.val->desc.layer->cat_num ) {
                local2->element.val->desc.layer->truthflag = 1;
            } else {
                local2->element.val->desc.layer->truthflag = 0;
            }
            if ( cellval == 0 ) {
                local2->element.val->desc.layer->truthflag = 0;
            }
            local2 = local2->next;
        }
        local = local->next;
    }
}

set_flags(ptr,num)
struct symtab *ptr;
int num;
{
    struct symtab *local = ptr->next;
    int init = 0;
    int count = 1;

    while((local = index_to_val(local,init)) != NULL ) {
        init = 1;
        switch(local->element.val->v_type) {
        case ContextAttribute:
            if ( num == count) {
                local->element.val->desc.cont->truthflag = 1;
            } else {
                local->element.val->desc.cont->truthflag = 0;
            }
            break;
        case SubjectiveAttribute:
            if ( num == count) {
                local->element.val->desc.subj->truthflag = 1;
            } else {
                local->element.val->desc.subj->truthflag = 0;
            }
            break;
        }
        count++;
        local = local->next;
    }
}


int
get_value(ptr)
struct symtab *ptr;
{
    int count;
    int done = 0;
    int value;
    char choice[80];
    char temp[80];
    char *qtemp;
    struct symtab *save;

    save = ptr;
    fprintf(stderr,"\n");

    while(!done) {

        count = 0;
        if ( ptr->question == (char *)0 ) {
            sprintf(temp,"What is the value of %s ?",ptr->name);
            ptr->question = strsave(temp);
            ptr->stripped = 1;
        } else if ( !ptr->stripped ) {
            strcpy(ptr->question,strip_quotes(ptr->question));
            ptr->stripped = 1;
        }
        fprintf(stderr,"%s\n",ptr->question);
        ptr = ptr->next;
        while(ptr->s_type != AttributeSymbol) {
            count++;
            if ( ptr->question == (char *)0 ) {
                sprintf(temp,"%s",ptr->name);
                ptr->question = strsave(temp);
                ptr->stripped = 1;
            } else if ( !ptr->stripped ) {
                strcpy(ptr->question,strip_quotes(ptr->question));
                ptr->stripped = 1;
            }
            fprintf(stderr,"%2d) %s\n",count,ptr->question);
            ptr = ptr->next;
        }
        fprintf(stderr,"\nEnter choice -> ");
        gets(choice);
        if (!strcmp(choice,"q")) {
            fprintf(stderr,"bye\n");
            exit(0);
        }
        value = atoi(choice);
        if ( value > 0 && value <= count ) done = 1;
        if ( !done )
            fprintf(stderr,"\nIncorrect response.\n\n");
        ptr = save;

    }

    ptr = save;
    return(value);
}

