/* 2pm 7/31/86                                                  GENPROC.C */

/* GENERAL-PURPOSE "C" ROUTINES

   Ian Johnson and James A. Farley, Arkansas Archaeological Survey  */


static int termset=0; /* Terminal characteristics not yet read in */


char *gettime()
{
  long *seconds,time();
  char *todays,*ctime();

  *seconds=time(0);
  todays=ctime(seconds);
  return(todays);
} /* gettime */  

upcase(c)
   char c;
   {
   if (c >='a' && c <='z') c=c-('a'-'A');
   return(c);
   } /* upcase */

allcaps(s)
   char s[]; 
   {
   int i;
   for (i=0; s[i] != '\0'; i++) { s[i]=upcase(s[i]); printf("."); }
   } /* allcaps */

static char *tgoto();
static char *clears,*clrend,*movexy,*hlight,*nlight;
static char buf[512],*area;

putchr(c) 
   register char c; { putchar(c); } /* tputs can't handle putchar macro*/

gotoxy(x,y)
   int x,y;
   {
   if (!termset) set_term("");
   tputs(tgoto(movexy,x-1,y-1),1,putchr);
   } /*gotoxy*/

set_term (termtype)
   char termtype[];
   {
   char *term,entry[1024],s[20];
   char *getenv(), *tgetstr();
   if (strcmp(termtype,"")==0) term=getenv("TERM"); else term=termtype;
   while (tgetent(entry,term) != 1) 
      { printf("\n\nUnknown terminal '%s' : ",term);
        printf("Re-enter terminal type: ");
        gets(term);
      }
   area=buf;
   /* Get strings for functions */
   clears=tgetstr("cl",&area);
   movexy=tgetstr("cm",&area);
   hlight=tgetstr("so",&area);
   nlight=tgetstr("se",&area);
   clrend=tgetstr("ce",&area);
   termset=1;
   } /*set_term*/

clreol ()
   {
   tputs(clrend,1,putchr); 
   } /*clreol*/


clearend (line)
   /* Clears from line to end of screen, positions cursor on line*/
   int line;
   {
   int i;	
   if (!termset) set_term("");
   if (line<=1) 
         { 
         gotoxy(1,1);tputs(clears,1,putchr);
         } 
      else
   for (i=24; i>=line; i--) 
      { 
      gotoxy(1,i);clreol(); 
      }
   for (i=0; i<1000; i++) ; /*Delay*/
   } /*clearend*/



screen_head (heading)
   char heading[];
   {
   clearend(1);
   printf("GRELAS File Transfer ");
   printf("\n\n");
   if (heading != "") 
      {
      gotoxy(35,1);
      printf(" %s ",heading);printf("\n\n");
      }
   } 


highlight ()
   {
   tputs(hlight,1,putchr);
   }

normal ()
   {
   tputs(nlight,1,putchr);
   }


prompt (promptstring,valid)
char promptstring[], valid[];
   {
   char a;
   printf(promptstring);
   a=readlet(valid);
   return(a);
   } /*prompt*/


readlet (valid)
/* Reads in a character from keyboard, checks that it is in "valid".
   <Return> alone is always accepted and returns 0 */
char valid[];
   {
   char a;char s[10];
   int goodchar=0;
   int i;
   while (!goodchar)
      {
      gets(s);a=s[0];
      if ((a>='a') && (a<='z')) a=a-('a'-'A');
      for (i=0; valid[i] != '\0'; ++i) if (a==valid[i]) goodchar=1;
      if (a=='\0') goodchar=1;
      if (!goodchar) printf("?\7");
      }
   return(a);
   } /*readlet*/


/* waitspace()
   { char s[2];
     printf("\n\nPress <return> to continue: ");
     gets(s);
   }*/


printchar(ch,num)
   char ch; int num;
   {
   int backspace;
   if (num<0) 
        { backspace=num;
          putchar('[');
          while (num<0) { putchar(ch);num++; } 
          putchar(']');
          while (backspace<1) { putchar('\b');backspace++; }
        }
     else
        { while (num>0) { putchar(ch);num--; }
        }
   } /*printchar*/

menu (title,e)
   char title[],e[];
   /* Note: e must contain no more than 10 entries separated by | symbol 
      e.g.  "Leave program|Printer|VDU|Plotter"
      Entries on menu will be numbered from 0
      Use \ to continue line over several VDU lines */
   {
   char s[10];
   int i,entry,startline=6;
   screen_head(title);
   gotoxy(1,startline);printf("   0     ");
   for (entry=0,i=0; e[i] != '\0'; i++)
      { 
      if (e[i] != '|') /*Part of label*/ 
         { printf("%c",e[i]);
         }
       else
         { entry++;gotoxy(1,startline+(2*entry)); printf("   %d     ",entry);
           while (e[i+1]==' ') i++; /* Skip leading spaces on next entry*/
         } 
      }
   clearend(20);
   for (i=0; i<=entry; i++) s[i]=i+48;  s[i]='\0';/*terminator*/
   printf("Please enter choice: ");
   return(readlet(s));
   } /*menu*/




