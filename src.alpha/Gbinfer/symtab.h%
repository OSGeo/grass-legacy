/*
**                               
** Filename: symtab.h
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/


#include "gis.h"

#define AttributeSymbol    0
#define ValueSymbol        1
#define HeadSymbol         2


#define UnknownAttribute       0
#define LayerAttribute         1
#define ContextAttribute       2
#define SubjectiveAttribute    3
#define InferredAttribute      4

#define SYMMAP "unknown", "layer", "context", "subjective", "inferred"


#define AspectColors     0
#define GreyScale        1
#define HistoGreyScale   2
#define Rainbow          3
#define Ramp             4
#define Random           5
#define RYG              6
#define Wave             7


struct symtab {
    char *name;             
    int s_type;            
    union _element {
        struct attribute *att;    
        struct value     *val;    
    } element;
    char *question;    
    char reclass[256];    
    char reclassname[256];    
    int stripped;
    int colortable;
    struct symtab *next;        
};

struct attribute {
    char *name;    
    int a_type;    
    int fildes;
    char *mapset;
    CELL *cellbuf;
    CELL *cellptr;
    struct Cell_head window;
    int num_vals;   
    
    struct names *dets;    
};

struct value {
    char *name;    
    int v_type;    
    union _val {    
        struct layer_val *layer;
        struct context *cont;
        struct subjective *subj;
        struct inferred *infr;
    } desc;
};

struct layer_val {
    int truthflag;        
    CELL cat_num;         
    int first_assigned;        
    struct prob *list;    
};

struct context {
    int truthflag;        
    int first_assigned;        
    struct prob *list;    
};

struct subjective {
    int truthflag;        
    int first_assigned;        
    struct prob *list;    
};

struct inferred {
    double prior_prob;    
    double inf_prob;        
    int fildes;
    CELL *cellbuf;
    CELL *cellptr;
    struct Cell_head window;
};

struct list_o_probs {
    struct prob *prob;
    struct list_o_probs *next;
};

struct prob {
    double probability;    
    struct prob *next;    
};

struct names {
    char *name;        
    struct names *next;    
};

struct symtab *s_create(), *s_find();
char * strsave();
char * strip_quotes();
void v_create();


