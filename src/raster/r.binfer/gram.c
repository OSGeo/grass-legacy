# line 13 "binfer.y"

#include "symtab.h"
#include <math.h>
    struct symtab table;
    struct symtab *cur_sym;
    struct symtab *cur_att;
    struct names namelist;
    struct names problist;
    char *probbuf[256];
    char *reclassbuf[256];
    int valno = 1;
    int expected_type;
    int value_type;
    extern int verbose;
    extern int probabilitymaps;
    extern int combinedmap;
    extern int colortable;


# line 33 "binfer.y"
typedef union  {
    char *y_sym;
} YYSTYPE;
# define Identifier 257
# define String 258
# define Constant 259
# define LAYER 260
# define CONTEXT 261
# define SUBJECTIVE 262
# define INFERRED 263
# define QUESTION 264
# define THRU 265
# define NO_COMBINED_MAP 266
# define NO_PROBABILITY_MAPS 267
# define COMBINED_MAP 268
# define ASPECT 269
# define GREY 270
# define HISTO 271
# define RAINBOW 272
# define RAMP 273
# define RANDOM 274
# define REDYELLOWGREEN 275
# define WAVE 276
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 508 "binfer.y"



#include <stdio.h>
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 89
# define YYLAST 253
short yyact[]={

 120, 121, 122, 123, 124, 125, 126, 127,  16,  17,
  18,  19,  20,  21,  22,  23,  11, 135,  81,  27,
  14,  14,   8,   9,  10,  15, 130,  14, 144, 141,
 116,  94,  47,  38, 142, 129, 115,  89, 143,  90,
  72,  68, 113,  48, 114,  44,  79,  71,  88,  73,
 130,  86, 140, 100, 145, 147, 137, 149,  57,  45,
  53,  50,  32,  95,  29,  28,  24,  49,  97,  97,
 148, 106, 102, 147,  97,  92,  37,  96,  83,  41,
  75,  37,  43,  70,  65,  47, 136,  56,  36, 119,
  58, 103,  12,  76,  85,  60,  67,  61,  40,  62,
  52,  33,  54,  39,  93,  91,  25,  69,  63,  42,
  34,  74,  99,  84,  77,  82,  80,  78,  90,  31,
  66,  87,  51,  64,  46,  55,  59,  35,  30,  74,
  26,  13,   7,  98,   6, 104,   5,   4,  80, 107,
   3, 108,  74, 109,   2, 117, 110, 111,  87, 101,
 132, 134,  80, 128, 118, 105,   1, 133, 131, 146,
 112,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 138,   0, 139,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0, 151, 153, 152, 150,   0,   0,   0,
   0,   0, 144,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,  38 };
short yypact[]={

-1000,-1000,-1000,-1000,-244,-1000,-1000,-236,-1000,-1000,
-261,   8,-1000,-243,   7,   6,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-242,   4,-1000,-1000,
-224,-1000,-1000,-224,-224,  -5,  21,   3,-1000,-224,
  48,   2,  -5,  21,   0,-1000,  21,-1000,-1000,-1000,
-1000,  21,-1000,-1000,-1000,  21,-1000,-1000,-1000,  44,
  48, -82,-1000,  43, -83,-224,-1000,  40,-224, -83,
-224,-1000,-246,  34,-1000,-224,  -7,-1000,-1000,  31,
-1000,-227,  33,-224, -38,  28,-1000,-1000,-1000,-224,
-1000,  27,-224, -83, -86,-1000,-224,-1000,-1000, -83,
-229,  21,-224,-269,-1000,-1000,-224,-1000,-1000,-1000,
-1000,-1000, -43,-229,-229,-248,-1000,-1000,-1000,  -4,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-248,-229,-1000,-229, -39,-230,-1000,-1000,
-231,  -8,  11,-1000,-1000,-1000,  -2,-231,-231, -67,
-1000,  29,-1000,  29 };
short yypgo[]={

   0,  45,  36,  47,  42,  44, 160,  38,  34, 159,
 156, 144, 140, 137, 136, 134, 132,  92, 131, 130,
 128, 127,  59,  88,  43, 126, 123,  49, 115,  63,
 113, 112,  35, 110, 109,  82, 108, 107,  46, 105,
 104, 103, 101,  98,  97,  96,  94,  93,  48,  51,
  91,  89,  86 };
short yyr1[]={

   0,  11,  10,  12,  13,  13,  15,  15,  15,  15,
  15,  15,  15,  15,  15,  15,  14,  14,  14,  20,
  16,  21,  21,  25,  23,  26,  26,  28,  28,  30,
  27,  31,  31,   6,   6,   6,   6,   5,   4,   4,
   2,  33,  18,  34,  34,  36,  35,  37,  39,  39,
  40,  38,  41,  19,  42,  17,  44,  43,  45,  45,
  47,  47,  46,  46,  50,  49,  51,  51,  51,  51,
  51,  51,  51,  51,  51,  52,   9,   9,   8,   8,
   7,   3,   3,   1,  29,  24,  48,  32,  22 };
short yyr2[]={

   0,   0,   2,   2,   0,   2,   1,   1,   2,   2,
   2,   2,   2,   2,   2,   2,   2,   3,   4,   0,
   5,   2,   3,   0,   5,   0,   4,   2,   3,   0,
   4,   0,   3,   1,   2,   1,   2,   3,   2,   1,
   1,   0,   5,   2,   3,   0,   5,   4,   2,   3,
   0,   3,   0,   6,   0,   5,   0,   8,   0,   3,
   1,   3,   1,   3,   0,   9,   0,   1,   1,   1,
   1,   1,   1,   1,   1,   3,   2,   3,   1,   3,
   1,   0,   4,   1,   1,   1,   1,   1,   1 };
short yychk[]={

-1000, -10, -11, -12, -13, -14, -15, -16, 266, 267,
 268, 260, -17, -18, 263, 261, 269, 270, 271, 272,
 273, 274, 275, 276,  58, -17, -19, 262,  58,  58,
 -20, -17,  58, -42, -33, -21, -23,  -1, 257, -41,
 -43,  -1, -34, -35,  -1, -22, -23,  37, -24,  46,
  58, -35, -22,  58, -22, -35, -24,  58, -24, -25,
 -24, -44, -24, -36, -26,  40, -22, -45, 123, -37,
  40,  -3, 123, -27,  -1,  40, -47,  -1,  -3, -38,
  -1, 264, -28,  44, -30, -46, -49,  -1, -48,  44,
 125, -39,  44, -40, 258, -29,  44,  41, -27, -31,
  91, -29,  44, -50,  -1, -29,  44, -38,  -3, -48,
 -27,  -3,  -6,  -4,  -5,  -2, 259, -24, -49, -51,
 269, 270, 271, 272, 273, 274, 275, 276, -38, -32,
  93,  -5,  -2,  -4,  -2, 265, -52,  60,  -2,  -2,
  91, 259,  -8,  -7, 259,  62,  -9,  44,  59,  59,
  -7,  -8, -32,  -8 };
short yydef[]={

   1,  -2,   4,   2,   0,   3,   5,   0,   6,   7,
   0,   0,  16,   0,   0,   0,   8,   9,  10,  11,
  12,  13,  14,  15,  19,  17,   0,   0,  54,  41,
   0,  18,  52,   0,   0,   0,   0,   0,  83,   0,
   0,   0,   0,   0,   0,  20,   0,  88,  21,  85,
  23,   0,  55,  56,  42,   0,  43,  45,  22,  25,
   0,  58,  44,   0,  81,   0,  53,   0,   0,  81,
   0,  24,   0,   0,  29,   0,   0,  60,  46,   0,
  50,   0,   0,   0,  31,   0,  62,  64,  59,   0,
  86,   0,   0,  81,   0,  26,   0,  84,  27,  81,
   0,   0,   0,  66,  61,  47,   0,  48,  51,  82,
  28,  30,   0,  33,  35,  39,  40,  57,  63,   0,
  67,  68,  69,  70,  71,  72,  73,  74,  49,  32,
  87,  34,  38,  36,  39,   0,   0,   0,  38,  37,
   0,   0,   0,  78,  80,  75,   0,   0,   0,   0,
  79,  76,  65,  77 };
#ifndef lint
static	char yaccpar_sccsid[] = "@(#)yaccpar 1.6 88/02/08 SMI"; /* from UCB 4.1 83/02/11 */
#endif

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps>= &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 1:
# line 78 "binfer.y"
{ init(); } break;
case 2:
# line 79 "binfer.y"
{ 
                 check_table(&table,verbose);
                 if ( verbose ) fprintf(stderr,"\nAll input parsed.\n");
                 return((int)&table);
             } break;
case 6:
# line 93 "binfer.y"
{ 
                          combinedmap = 0;
                          fprintf(stderr,"NoCombinedMap option set.\n"); 
                      } break;
case 7:
# line 99 "binfer.y"
{ 
                          probabilitymaps = 0;
                          fprintf(stderr,"NoProbabiltyMaps option set.\n"); 
                      } break;
case 8:
# line 105 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = AspectColors;
                              fprintf(stderr,"Combined map colortable set to aspect colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 9:
# line 115 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = GreyScale;
                              fprintf(stderr,"Combined map colortable set to grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 10:
# line 125 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = HistoGreyScale;
                              fprintf(stderr,"Combined map colortable set to histogram stretched grey scale.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 11:
# line 135 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Rainbow;
                              fprintf(stderr,"Combined map colortable set to rainbow colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 12:
# line 145 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Ramp;
                              fprintf(stderr,"Combined map colortable set to color ramp.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 13:
# line 155 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Random;
                              fprintf(stderr,"Combined map colortable set to random colors.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 14:
# line 165 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = RYG;
                              fprintf(stderr,"Combined map colortable set to red yellow green.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 15:
# line 175 "binfer.y"
{ 
                          if ( combinedmap ) {
                              colortable = Wave;
                              fprintf(stderr,"Combined map colortable set to color wave.\n");
                          } else {
                              fprintf(stderr,"Warning: Combined map colortable not set.\n");
                              fprintf(stderr,"NoCombinedMap option is set.\n");
                          }
                      } break;
case 19:
# line 192 "binfer.y"
{ 
                       expected_type = AttributeSymbol;
                       value_type = LayerAttribute; 
                   } break;
case 20:
# line 197 "binfer.y"
{ 
                       if ( verbose ) fprintf(stderr,"\nParsed layers section.\n"); 
                   } break;
case 23:
# line 206 "binfer.y"
{ 
                   cur_sym = s_create(yypvt[-1].y_sym,expected_type,value_type);
                   expected_type = ValueSymbol; 
               } break;
case 24:
# line 212 "binfer.y"
{ 
                   expected_type = AttributeSymbol;
                   if ( yypvt[-0].y_sym != (char *)0 )
                       cur_att->question = strsave(yypvt[-0].y_sym); 
                   valno = 1;
               } break;
case 29:
# line 228 "binfer.y"
{ 
               cur_sym = s_create(yypvt[-0].y_sym,expected_type,value_type); 
           } break;
case 30:
# line 232 "binfer.y"
{ 
               if ( yypvt[-0].y_sym != (char *)0 )
                   cur_sym->question = strsave(yypvt[-0].y_sym); 
           } break;
case 32:
# line 241 "binfer.y"
{ 
               cur_sym->element.val->desc.layer->cat_num = (CELL)valno;
            
               sprintf(cur_sym->reclass,"%s = %d %s",strsave(yypvt[-1].y_sym),valno++,
                       cur_sym->name);
           } break;
case 33:
# line 249 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 34:
# line 250 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yypvt[-1].y_sym,yypvt[-0].y_sym);
                                       yyval.y_sym = strsave(reclassbuf); } break;
case 35:
# line 252 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 36:
# line 253 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yypvt[-1].y_sym,yypvt[-0].y_sym);
                                       yyval.y_sym = strsave(reclassbuf); } break;
case 37:
# line 258 "binfer.y"
{ sprintf(reclassbuf,"%s thru %s",yypvt[-2].y_sym,yypvt[-0].y_sym);
                                 yyval.y_sym = strsave(reclassbuf); } break;
case 38:
# line 262 "binfer.y"
{ sprintf(reclassbuf,"%s %s",yypvt[-1].y_sym,yypvt[-0].y_sym);
                                 yyval.y_sym = strsave(reclassbuf); } break;
case 39:
# line 264 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 40:
# line 267 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym);} break;
case 41:
# line 271 "binfer.y"
{ 
                      expected_type = AttributeSymbol;
                      value_type = ContextAttribute; 
                  } break;
case 42:
# line 276 "binfer.y"
{ 
               if ( verbose ) fprintf(stderr,"\nParsed context section.\n"); 
           } break;
case 45:
# line 285 "binfer.y"
{ 
                    cur_att = cur_sym = s_create(yypvt[-1].y_sym,expected_type,value_type);
                    expected_type = ValueSymbol; 
               } break;
case 46:
# line 290 "binfer.y"
{ 
                   expected_type = AttributeSymbol;
                   if ( yypvt[-0].y_sym != (char *)0 )
                       cur_att->question = strsave(yypvt[-0].y_sym); 
               } break;
case 50:
# line 305 "binfer.y"
{ 
               cur_sym = s_create(yypvt[-0].y_sym,expected_type,value_type); 
           } break;
case 51:
# line 309 "binfer.y"
{ 
               if ( yypvt[-0].y_sym != (char *)0 )
                   cur_sym->question = strsave(yypvt[-0].y_sym); 
           } break;
case 52:
# line 316 "binfer.y"
{ 
                         expected_type = AttributeSymbol;
                         value_type = SubjectiveAttribute; 
                     } break;
case 53:
# line 321 "binfer.y"
{ 
                         if ( verbose ) 
                             fprintf(stderr,"\nParsed subjective section.\n"); 
                     } break;
case 54:
# line 327 "binfer.y"
{ 
                        expected_type = AttributeSymbol;
                        value_type = InferredAttribute; 
                    } break;
case 55:
# line 332 "binfer.y"
{ 
                        if ( verbose ) 
                            fprintf(stderr,"\nParsed inferred section.\n"); 
                    } break;
case 56:
# line 338 "binfer.y"
{ 
                   cur_sym = s_create(yypvt[-1].y_sym,expected_type,value_type);
                   expected_type = ValueSymbol; 
               } break;
case 57:
# line 343 "binfer.y"
{ 
                   expected_type = AttributeSymbol; 
               } break;
case 60:
# line 353 "binfer.y"
{ 
               if (!add_name(yypvt[-0].y_sym)) yyerror("Name not stored"); 
           } break;
case 61:
# line 357 "binfer.y"
{ 
               if (!add_name(yypvt[-0].y_sym)) yyerror("Name not stored"); 
           } break;
case 64:
# line 367 "binfer.y"
{ 
               cur_sym = s_create(yypvt[-0].y_sym,expected_type,value_type); 
           } break;
case 65:
# line 372 "binfer.y"
{
                sprintf(probbuf,"%s%s;",yypvt[-3].y_sym,yypvt[-2].y_sym);
                if (!add_prob_list(probbuf)) yyerror("Problist not stored");
            } break;
case 67:
# line 380 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = AspectColors;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 68:
# line 390 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = GreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 69:
# line 400 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = HistoGreyScale;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 70:
# line 410 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Rainbow;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 71:
# line 420 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Ramp;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 72:
# line 430 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Random;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 73:
# line 440 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = RYG;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 74:
# line 450 "binfer.y"
{
                 if ( probabilitymaps ) {
                     cur_sym->colortable = Wave;
                 } else {
                     fprintf(stderr,
                             "Warning: %s probability map colortable not set.\n"
                             ,cur_sym->name);
                     fprintf(stderr,"NoProbabilityMaps option is set.\n");
                 }
           } break;
case 75:
# line 463 "binfer.y"
{ 
                cur_sym->element.val->desc.infr->prior_prob = atof(yypvt[-1].y_sym); 
            } break;
case 76:
# line 470 "binfer.y"
{ 
                sprintf(probbuf,";%s",yypvt[-0].y_sym); 
                yyval.y_sym = strsave(probbuf);
            } break;
case 77:
# line 475 "binfer.y"
{ 
                sprintf(probbuf,"%s;%s",yypvt[-2].y_sym,yypvt[-0].y_sym);
                yyval.y_sym = strsave(probbuf);
            } break;
case 78:
# line 482 "binfer.y"
{ 
                yyval.y_sym = strsave(yypvt[-0].y_sym); 
            } break;
case 79:
# line 486 "binfer.y"
{ 
                sprintf(probbuf,"%s,%s",yypvt[-2].y_sym,yypvt[-0].y_sym);
                yyval.y_sym = strsave(probbuf);
            } break;
case 80:
# line 492 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-0].y_sym); } break;
case 81:
# line 495 "binfer.y"
{ yyval.y_sym = (char *)0; } break;
case 82:
# line 496 "binfer.y"
{ yyval.y_sym = strsave(yypvt[-1].y_sym); } break;
case 84:
# line 502 "binfer.y"
{ yyerrok; } break;
case 85:
# line 503 "binfer.y"
{ yyerrok; } break;
case 86:
# line 504 "binfer.y"
{ yyerrok; } break;
case 87:
# line 505 "binfer.y"
{ yyerrok; } break;
case 88:
# line 506 "binfer.y"
{ yyerrok; } break;
		}
		goto yystack;  /* stack new state and value */

	}
