/*
**                               
** Filename: symtab.c
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "symtab.h"
#include "y.tab.h"
#include "local_proto.h"

extern struct symtab table;
extern struct names namelist;
extern struct names problist;


int init (void)
{
    table.name = strsave("head_element");
    table.s_type = HeadSymbol;
    table.next = (struct symtab *)malloc(sizeof(struct symtab));
    table.next = (struct symtab *) 0;
    namelist.next = (struct names *)0;
    namelist.name = strsave("list_header");
    problist.next = (struct names *)0;
    problist.name = strsave("list_header");

    return 0;
}

struct symtab *s_create (register char *name, int s_type, int a_type)
{
    struct symtab * new_entry = (struct symtab *)malloc(sizeof(struct symtab));
    struct symtab * ptr;

    for ( ptr = (&table) ; ptr->next != (struct symtab *) 0;  ptr = ptr->next ) {
        if ( !strcmp(ptr->name,name) && (ptr->s_type == AttributeSymbol) ) {
            fprintf(stderr,"\"%s\"",name);
            yyfatal("Already an attribute name");
        }
    }
    if ( new_entry ) {
        ptr->next = new_entry;
        new_entry->next = (struct symtab *) 0;
        new_entry->name = name;
        new_entry->s_type = s_type;
        new_entry->question = (char *) 0;
        strcpy(new_entry->reclass,"");
        strcpy(new_entry->reclassname,"");
        new_entry->stripped = 0;
        new_entry->colortable = Ramp;
        switch(s_type) {
        case AttributeSymbol:
            new_entry->element.att = 
                (struct attribute *)malloc(sizeof(struct attribute));
            new_entry->element.att->name = name;
            new_entry->element.att->a_type = a_type;
            new_entry->element.att->num_vals = 0;
            new_entry->element.att->dets = (struct names *) 0;
            break;
        case ValueSymbol:
            new_entry->element.val = 
                (struct value *)malloc(sizeof(struct value));
            new_entry->element.val->name = name;
            new_entry->element.val->v_type = a_type;
            v_create(new_entry,a_type);
            break;
        }
        return(new_entry);
    }
    yyfatal("No more room for symbols\n");
    /* NOTREACHED */
}

void v_create (struct symtab *entry, int type)
{
    switch(type) {
    case LayerAttribute:
        entry->element.val->desc.layer =
            (struct layer_val *)malloc(sizeof(struct layer_val));
        entry->element.val->desc.layer->first_assigned = 0;
        entry->element.val->desc.layer->cat_num = (CELL)0;
        entry->element.val->desc.layer->truthflag = 0;
        entry->element.val->desc.layer->list = (struct prob *) 0;
        break;
    case ContextAttribute:
        entry->element.val->desc.cont =
            (struct context *)malloc(sizeof(struct context));
        entry->element.val->desc.cont->first_assigned = 0;
        entry->element.val->desc.cont->truthflag = 0;
        entry->element.val->desc.cont->list = (struct prob *) 0;
        break;
    case SubjectiveAttribute:
        entry->element.val->desc.subj =
            (struct subjective *)malloc(sizeof(struct subjective));
        entry->element.val->desc.subj->first_assigned = 0;
        entry->element.val->desc.subj->truthflag = 0;
        entry->element.val->desc.subj->list = (struct prob *) 0;
        break;
    case InferredAttribute:
        entry->element.val->desc.infr =
            (struct inferred *)malloc(sizeof(struct inferred));
        entry->element.val->desc.infr->prior_prob = 0.0;
        entry->element.val->desc.infr->inf_prob = 0.0;
        break;
    }
}

int s_lookup (int yylex)
{
    extern char yytext[];

    yylval.y_sym = strsave(yytext);

    return 0;
}

struct symtab *s_find (char *s)
{
    register struct symtab *ptr;

    for ( ptr = (&table)->next; ptr != (struct symtab *) 0; ptr = ptr->next ) {
        if (!strcmp(ptr->name,s))
            return(ptr);
    }
    return((struct symtab *)0);
}

char *strsave (char *s)
{
    char *cp = (char *)malloc(strlen(s) + 1);

    if ( cp ) {
        strcpy(cp,s);
        return(cp);
    }
    fprintf(stderr,"No more room for strings\n");

    return NULL;
}

int dump_sym_tab (struct symtab *ptr)
{
    static char *type[] = { 
        SYMMAP     };
    struct symtab *temp;
    struct names *tempn;
    struct prob *probl;
    int attno = 0;
    int valno = 0;
    int num = 0;

    for(temp = ptr;temp != (struct symtab *)0;temp = temp->next) {
        switch ( temp->s_type ) {
        case HeadSymbol:
            fprintf (stdout,"Header Symbol\n");
            break;
        case AttributeSymbol:
            fprintf (stdout,"Attribute %3d\n",attno++);
            fprintf (stdout,"  name:  %s\n",temp->name);
            fprintf (stdout,"  type:  %s\n",type[temp->element.att->a_type]);
            fprintf (stdout,"  question: %s\n",temp->question);
            if ( temp->element.att->a_type == InferredAttribute ) {
                fprintf (stdout,"  determinants:\n");
                for (tempn = (&namelist)->next; tempn != (struct names *) 0 ; tempn = tempn->next) {
                    fprintf (stdout,"   #%3d: %s\n",num++,tempn->name);
                }
            }
            valno = 0;
            break;
        case ValueSymbol:
            fprintf (stdout,"\tValue %3d\n",valno++);
            fprintf (stdout,"\t  name: %s\n",temp->name);
            fprintf (stdout,"\t  type: %s\n",type[temp->element.val->v_type]);
            switch(temp->element.val->v_type) {
            case LayerAttribute:
                probl = temp->element.val->desc.layer->list;
                break;
            case ContextAttribute:
                probl = temp->element.val->desc.cont->list;
                break;
            case SubjectiveAttribute:
                probl = temp->element.val->desc.subj->list;
                break;
            }
            if ( temp->element.val->v_type == InferredAttribute )
                fprintf (stdout,"\t  prior: < %4.2f >\n",
                    temp->element.val->desc.infr->prior_prob);
            if ( temp->element.val->v_type != InferredAttribute ) {
                fprintf (stdout,"\t  question: %s\n",temp->question);
                fprintf (stdout,"\t  conditional probabilities:\n");
                for (;probl != (struct prob *)0;probl = probl->next) {
                    fprintf (stdout,"\t\t%5.4f\n",probl->probability);
                }
            }
            break;
        }
    }

    return 0;
}

int 
yyfatal (char *s)
{
    yyerror(s);
    exit(0);
}


int 
add_name (char *name)
{
    struct names *ptr;
    int num;

    ptr = (&namelist);
    if ( name == (char *) 0 )
        return(0);
    for( num = 0; ptr->next != (struct names *) 0;num++,ptr = ptr->next);
    ptr->next = (struct names *)malloc(sizeof(struct names));
    ptr->next->name = strsave(name);
    ptr->next->next = (struct names *) 0;
    return(1);
}

int 
add_prob_list (char *list)
{
    struct names *ptr;
    int num;

    ptr = (&problist);
    if ( list == (char *) 0 )
        return(0);
    for( num = 0; ptr->next != (struct names *) 0;num++,ptr = ptr->next);
    ptr->next = (struct names *)malloc(sizeof(struct names));
    ptr->next->name = strsave(list);
    ptr->next->next = (struct names *) 0;
    return(1);
}

int ExecuteReclass (char *file)
{
    int pid;
    int status;

    if ( (pid = fork()) == 0) {
        execl("/bin/sh","sh",file);
        _exit(1);
    }
    wait(&status);

    return 0;
}
