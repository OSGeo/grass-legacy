/*
**                               
** Filename: binfer.y
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

%{

#include "symtab.h"
#include <stdlib.h>
#include <math.h>
#include "local_proto.h"
    struct symtab table;
    struct symtab *cur_sym;
    struct symtab *cur_att;
    struct names namelist;
    struct names problist;
    char probbuf[256];
    char reclassbuf[256];
    int valno = 1;
    int expected_type;
    int value_type;
    extern int verbose;
    extern int probabilitymaps;
    extern int combinedmap;
    extern int colortable;

%}

%union {
    char *y_sym;
}

%token  <y_sym> Identifier
%token  <y_sym> String
%token  <y_sym> Constant

%token LAYER
%token CONTEXT
%token SUBJECTIVE
%token INFERRED
%token QUESTION
%token THRU
%token NO_COMBINED_MAP
%token NO_PROBABILITY_MAPS
%token COMBINED_MAP
%token ASPECT
%token GREY
%token HISTO
%token RAINBOW
%token RAMP
%token RANDOM
%token REDYELLOWGREEN
%token WAVE

%token '(' 
%token ')' 
%token '[' 
%token ']' 
%token '{' 
%token '}' 
%token '<' 
%token '>' 
%token ',' 
%token '.' 
%token '%' 


%type <y_sym> Name, Input_Cat
%type <y_sym> Question_Attachment, Input_Cat_List, Input_Cat_Range, Reclass_Rule
%type <y_sym> Probability, Probability_List, Conditional_Probability_Table
%%

Program
    :    { init(); }
      Script { 
                 check_table(&table,verbose);
                 if ( verbose ) fprintf(stderr,"\nAll input parsed.\n");
                 return(-1);
             };

Script
    : Output_Options MainScript ;

Output_Options
    : /* no output options */
    | Output_Options Output_Option ;

Output_Option
    : NO_COMBINED_MAP { 
                          combinedmap = 0;
                          fprintf(stderr,"NoCombinedMap option set.\n"); 
                      }

    | NO_PROBABILITY_MAPS
                      { 
                          probabilitymaps = 0;
                          fprintf(stderr,"NoProbabiltyMaps option set.\n"); 
                      }

    | COMBINED_MAP ASPECT
                      { 
                          if ( combinedmap ) {
                              colortable = AspectColors;
                              fprintf(stderr,"Combined map colortable set to aspect colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
    | COMBINED_MAP GREY
                      { 
                          if ( combinedmap ) {
                              colortable = GreyScale;
                              fprintf(stderr,"Combined map colortable set to grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
    | COMBINED_MAP HISTO
                      { 
                          if ( combinedmap ) {
                              colortable = HistoGreyScale;
                              fprintf(stderr,"Combined map colortable set to histogram stretched grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
    | COMBINED_MAP RAINBOW
                      { 
                          if ( combinedmap ) {
                              colortable = Rainbow;
                              fprintf(stderr,"Combined map colortable set to rainbow colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
    | COMBINED_MAP RAMP
                      { 
                          if ( combinedmap ) {
                              colortable = Ramp;
                              fprintf(stderr,"Combined map colortable set to color ramp.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
    | COMBINED_MAP RANDOM
                      { 
                          if ( combinedmap ) {
                              colortable = Random;
                              fprintf(stderr,"Combined map colortable set to random colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
    | COMBINED_MAP REDYELLOWGREEN
                      { 
                          if ( combinedmap ) {
                              colortable = RYG;
                              fprintf(stderr,"Combined map colortable set to red yellow green.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      }
    | COMBINED_MAP WAVE
                      { 
                          if ( combinedmap ) {
                              colortable = Wave;
                              fprintf(stderr,"Combined map colortable set to color wave.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      };

MainScript
    : Layer_Section Inferred_Section
    | Layer_Section Context_Section Inferred_Section
    | Layer_Section Context_Section Subjective_Section Inferred_Section ;


Layer_Section 
    : LAYER    ':' { 
                       expected_type = AttributeSymbol;
                       value_type = LayerAttribute; 
                   }
      Layer_Att_Declarations end 
                   { 
                       if ( verbose ) fprintf(stderr,"\nParsed layers section.\n"); 
                   };

Layer_Att_Declarations
    : Layer_Att_Declaration ep
    | Layer_Att_Declarations Layer_Att_Declaration ep ;

Layer_Att_Declaration
    : Name ':' { 
                   cur_sym = s_create($1,expected_type,value_type);
                   expected_type = ValueSymbol; 
               }
      Layer_Value_List 
      Question_Attachment 
               { 
                   expected_type = AttributeSymbol;
                   if ( $5 != (char *)0 )
                       cur_att->question = strsave($5); 
                   valno = 1;
               };

Layer_Value_List
    : /* no actual values */
    | '(' Layer_Value Layer_Value_Sublist rp ;

Layer_Value_Sublist
    : ',' Layer_Value
    | Layer_Value_Sublist ',' Layer_Value ;

Layer_Value
    : Name { 
               cur_sym = s_create($1,expected_type,value_type); 
           }
      Category_Range Question_Attachment 
           { 
               if ( $4 != (char *)0 )
                   cur_sym->question = strsave($4); 
           };


Category_Range
    : /* no actual range specified */
    | '[' Reclass_Rule rbr 
           { 
               cur_sym->element.val->desc.layer->cat_num = (CELL)valno;
            
               sprintf(cur_sym->reclass,"%s = %d %s",strsave($2),valno++,
                       cur_sym->name);
           };

Reclass_Rule
    : Input_Cat_List { $$ = strsave($1); }
    | Input_Cat_List Input_Cat_Range { sprintf(reclassbuf,"%s %s",$1,$2);
                                       $$ = strsave(reclassbuf); }
    | Input_Cat_Range { $$ = strsave($1); }
    | Input_Cat_Range Input_Cat_List { sprintf(reclassbuf,"%s %s",$1,$2);
                                       $$ = strsave(reclassbuf); };
    

Input_Cat_Range
    : Input_Cat THRU Input_Cat { sprintf(reclassbuf,"%s thru %s",$1,$3);
                                 $$ = strsave(reclassbuf); };
    
Input_Cat_List
    : Input_Cat_List Input_Cat { sprintf(reclassbuf,"%s %s",$1,$2);
                                 $$ = strsave(reclassbuf); }
    | Input_Cat { $$ = strsave($1); };

Input_Cat
    : Constant { $$ = strsave($1);};
  

Context_Section 
    : CONTEXT ':' { 
                      expected_type = AttributeSymbol;
                      value_type = ContextAttribute; 
                  }
      Context_Att_Declarations end 
           { 
               if ( verbose ) fprintf(stderr,"\nParsed context section.\n"); 
           };

Context_Att_Declarations
    : Context_Att_Declaration ep
    | Context_Att_Declarations Context_Att_Declaration ep ;

Context_Att_Declaration
    : Name ':' { 
                    cur_att = cur_sym = s_create($1,expected_type,value_type);
                    expected_type = ValueSymbol; 
               }
      Context_Value_List Question_Attachment 
               { 
                   expected_type = AttributeSymbol;
                   if ( $5 != (char *)0 )
                       cur_att->question = strsave($5); 
               };


Context_Value_List
    : '(' Context_Value Context_Value_Sublist rp ;

Context_Value_Sublist
    : ',' Context_Value
    | Context_Value_Sublist ',' Context_Value ;

Context_Value
    : Name { 
               cur_sym = s_create($1,expected_type,value_type); 
           }
      Question_Attachment 
           { 
               if ( $3 != (char *)0 )
                   cur_sym->question = strsave($3); 
           };


Subjective_Section
    : SUBJECTIVE ':' { 
                         expected_type = AttributeSymbol;
                         value_type = SubjectiveAttribute; 
                     }
      Context_Att_Declaration ep end 
                     { 
                         if ( verbose ) 
                             fprintf(stderr,"\nParsed subjective section.\n"); 
                     };

Inferred_Section 
    : INFERRED ':' { 
                        expected_type = AttributeSymbol;
                        value_type = InferredAttribute; 
                    }
      Inferred_Att_Declaration end 
                    { 
                        if ( verbose ) 
                            fprintf(stderr,"\nParsed inferred section.\n"); 
                    };

Inferred_Att_Declaration
    : Name ':' { 
                   cur_sym = s_create($1,expected_type,value_type);
                   expected_type = ValueSymbol; 
               }
      Determinant_List '(' Inferred_Value_List rp ep 
               { 
                   expected_type = AttributeSymbol; 
               };


Determinant_List
    : /* no determinant list given */
    | '{' Att_List rb ;

Att_List
    : Name { 
               if (!add_name($1)) yyerror("Name not stored"); 
           }
    | Att_List ',' Name 
           { 
               if (!add_name($3)) yyerror("Name not stored"); 
           };


Inferred_Value_List
    : Inferred_Value
    | Inferred_Value_List ',' Inferred_Value ;

Inferred_Value
    : Name { 
               cur_sym = s_create($1,expected_type,value_type); 
           }
      Optional_Color_Table Prior_Probability 
      '[' Probability_List Conditional_Probability_Table ';' rbr 
            {
                sprintf(probbuf,"%s%s;",$6,$7);
                if (!add_prob_list(probbuf)) yyerror("Problist not stored");
            };


Optional_Color_Table
    : /* default colortable (Ramp) */
    | ASPECT {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = AspectColors;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
    | GREY {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = GreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
    | HISTO {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = HistoGreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
    | RAINBOW {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Rainbow;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
    | RAMP {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Ramp;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
    | RANDOM {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Random;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
    | REDYELLOWGREEN {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = RYG;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           }
    | WAVE {
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Wave;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           };

Prior_Probability
    : '<' Constant '>' 
            { 
                cur_sym->element.val->desc.infr->prior_prob = atof($2); 
            };


Conditional_Probability_Table
    : ';' Probability_List 
            { 
                sprintf(probbuf,";%s",$2); 
                $$ = strsave(probbuf);
            }
    | Conditional_Probability_Table ';' Probability_List 
            { 
                sprintf(probbuf,"%s;%s",$1,$3);
                $$ = strsave(probbuf);
            };

Probability_List
    : Probability 
            { 
                $$ = strsave($1); 
            }
    | Probability_List ',' Probability 
            { 
                sprintf(probbuf,"%s,%s",$1,$3);
                $$ = strsave(probbuf);
            };

Probability
    : Constant { $$ = strsave($1); };

Question_Attachment
    : /* no attachments */ { $<y_sym>$ = (char *)0; }
    | '{' QUESTION String rb { $$ = strsave($3); };


Name    
    : Identifier ;

rp        : ')'    { yyerrok; };
ep        : '.'    { yyerrok; };
rb        : '}'    { yyerrok; };
rbr       : ']'    { yyerrok; };
end       : '%'    { yyerrok; };

%%


#include <stdio.h>
