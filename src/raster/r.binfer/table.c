/*
**                               
** Filename: table.c
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "symtab.h"
#include "gram.h"
#include "local_proto.h"

extern struct symtab table;
extern struct names namelist;
extern struct names problist;



int 
check_table (struct symtab *ptr, int verbose)
{
  struct symtab *temp;
  struct symtab *temp2;
  struct attribute *cur_att;
  struct value *cur_val;
  struct names *tempn;
  double prior_check;
  int layer_flag = 0;
  int valno = 0;

  for(temp = ptr;temp != (struct symtab *)0;temp = temp->next) {
    switch ( temp->s_type ) {
    case HeadSymbol:
      break;
    case AttributeSymbol:
      if (valno) { 
	cur_att->num_vals = valno; 
	if ( valno < 2 )
	  fprintf(stderr,"Warning: attribute \"%s\" has only %d values.\n"
		  ,cur_att->name,valno);
      }
      valno = 0;   
      cur_att = temp->element.att;
      if ( cur_att->a_type == LayerAttribute ) {
	layer_flag = 1;
      } else {
	layer_flag = 0;
      }

      if ( cur_att->a_type == InferredAttribute ) {

	for( tempn = (&namelist)->next;tempn != (struct names *) 0;tempn = tempn->next ) {
	  if (s_find(tempn->name) == (struct symtab *)0 ) {
	    fprintf(stderr,"Error: \"%s\" can't be a determinant. It's not an attribute\n",
		    tempn->name);
	    exit(0);
	  }
	}
	cur_att->dets = (&namelist)->next;

	prior_check = 0.0;
	for(temp2 = temp->next;temp2 != (struct symtab *)0;temp2 = temp2->next ) {
	  prior_check += temp2->element.val->desc.infr->prior_prob;
	}
	if ( ( prior_check < 0.995 ) || ( prior_check > 1.005 ) ) {
	  fprintf(stderr,"Error:Prior probabilities don't sum to 1.0\n");
	  exit(0);
	}

      }
      break;
    case ValueSymbol:
      valno++;
      cur_val = temp->element.val;
      if ( cur_val->v_type != cur_att->a_type ) 
	cur_att->a_type = cur_val->v_type;
      break;
    }
  }
  if ( valno ) {

    cur_att->num_vals = valno;
  }

  map_prob(ptr);
  if(verbose)
    dump_sym_tab(ptr);

  return 0;
}

int 
map_prob (struct symtab *ptr)
{
  struct names *tempn;
  struct symtab *temptab;
  struct symtab *inferred_att;
  struct symtab *inf_val;
  struct prob *tempp;
  struct prob *assign;
  struct prob *string_to_list();
  char *lastassigned;
  char *lastattribute;
  double prob_check = 0.0;
  int first_att = 1;
  int done_checking = 0;
  struct symtab *index_to_att();

  inferred_att = index_to_att(ptr,InferredAttribute,0);
  inf_val = inferred_att->next;
  for(tempn = (&problist)->next;tempn != (struct names *)0;tempn = tempn->next) {
    tempp = string_to_list(tempn->name);
    for(temptab = ptr;temptab != (struct symtab *)0;temptab = temptab->next) {
      if ( tempp == (struct prob *)0 ) {
	fprintf(stderr,"Not enough probabilities.\n");
	fprintf(stderr,"Last one assigned to: \n");
	fprintf(stderr,"\tvalue:%s\n",lastassigned);
	fprintf(stderr,"\tattribute:%s\n",lastattribute);
	exit(0);
      }
      switch ( temptab->s_type ) {
      case AttributeSymbol:
	if ( !first_att && !done_checking ) {
	  if ( prob_check < 0.995 || prob_check > 1.005 ) {
	    fprintf(stderr,"Conditional probabilities (%.3f) should sum to 1.0\n", prob_check);
	    fprintf(stderr,"for inferred attribute [%s] determinant [%s]\n",
		    inf_val->name,lastattribute);
	    exit(0);
	  }
	}
	first_att = 0;
	prob_check = 0.0;
	lastattribute = strsave(temptab->element.att->name);
	break;
      case ValueSymbol:
	prob_check += tempp->probability;
	switch(temptab->element.val->v_type) {
	case LayerAttribute:
	  if ( !temptab->element.val->desc.layer->first_assigned ) {
	    temptab->element.val->desc.layer->first_assigned = 1;
	    assign =
	      temptab->element.val->desc.layer->list =
	      (struct prob *)malloc(sizeof(struct prob));
	    assign->next = (struct prob *)0;
	  }
	  for(assign = temptab->element.val->desc.layer->list;
	      assign->next != NULL;assign = assign->next);
	  assign->next = (struct prob *)malloc(sizeof(struct prob));
	  assign = assign->next;
	  assign->probability = tempp->probability;
	  assign->next = (struct prob *)NULL;
	  tempp = tempp->next;
	  break;
	case ContextAttribute:
	  if ( !temptab->element.val->desc.cont->first_assigned ) {
	    temptab->element.val->desc.cont->first_assigned = 1;
	    assign =
	      temptab->element.val->desc.cont->list =
	      (struct prob *)malloc(sizeof(struct prob));
	    assign->next = (struct prob *)0;
	  }
	  for(assign = temptab->element.val->desc.cont->list;
	      assign->next != NULL;assign = assign->next);
	  assign->next = (struct prob *)malloc(sizeof(struct prob));
	  assign = assign->next;
	  assign->probability = tempp->probability;
	  assign->next = (struct prob *)NULL;
	  tempp = tempp->next;
	  break;
	case SubjectiveAttribute:
	  if ( !temptab->element.val->desc.subj->first_assigned ) {
	    temptab->element.val->desc.subj->first_assigned = 1;
	    temptab->element.val->desc.subj->list =
	      (struct prob *)malloc(sizeof(struct prob));
	    assign->next = (struct prob *)0;
	  }
	  for(assign = temptab->element.val->desc.subj->list;
	      assign->next != NULL;assign = assign->next);
	  assign->next = (struct prob *)malloc(sizeof(struct prob));
	  assign = assign->next;
	  assign->probability = tempp->probability;
	  assign->next = (struct prob *)NULL;
	  tempp = tempp->next;
	  break;
	case InferredAttribute:
	  done_checking = 1;
	  break;
	}
	break;
      }
      if ( temptab->s_type != HeadSymbol )
	lastassigned = strsave(temptab->element.val->name);
    }
    if ( tempp->next != (struct prob *)0 ) {
      fprintf(stderr,"Warning: More probabilities in list than attribute values.\n");
    }
    inf_val = inf_val->next;
  }

  return 0;
}

struct prob *string_to_list (char *s)
{
  char buf[20];
  int i = 0;
  struct prob *t,*start;

  start = t = (struct prob *)malloc(sizeof(struct prob));
  while(1) {
    buf[i] = *s++;
    if( buf[i] == ',' || buf[i] == ';') {
      buf[i+1] = '\0';
      t->probability = atof(buf);
      t->next = (struct prob *)malloc(sizeof(struct prob));
      t = t->next;
      i = 0;
    }
    if ( buf[i] == '\0' ) {
      t->next = (struct prob *)0;
      break;
    }
    i++;
  }
  return(start);
}
