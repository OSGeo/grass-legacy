
#include <stdio.h>
#include "config.h"
#include "helptext.h"

check_responses()
{
int helpcode;
char buf[512];

if (!strcmp(reset_loc,"help")||!strcmp(reset_loc,"HELP"))
   {
   helpcode=1;
   reset_loc[0] = 0;
   }
else if (!strcmp(config.north,"help")||!strcmp(config.north,"HELP"))
   {
   helpcode=2;
   config.north[0] = 0;
   }
else if (!strcmp(config.east,"help")||!strcmp(config.east,"HELP"))
   {
   helpcode=3;
   config.east[0] = 0;
   }
else if (!strcmp(config.ref,"help")||!strcmp(config.ref,"HELP"))
   {
   helpcode=4;
   config.ref[0] = 0;
   }
else if (!strcmp(config.xoffset,"help")||!strcmp(config.xoffset,"HELP"))
   {
   helpcode=5;
   config.xoffset[0] = 0;
   }
else if (!strcmp(config.yoffset,"help")||!strcmp(config.yoffset,"HELP"))
   {
   helpcode=6;
   config.yoffset[0] = 0;
   }
else if (!strcmp(config.text,"help")||!strcmp(config.text,"HELP"))
   {
   helpcode=7;
   }
else if (!strcmp(config.skip,"help")||!strcmp(config.skip,"HELP"))
   {
   helpcode=8;
   config.skip[0] = 0;
   }
else if (!strcmp(config.font,"help")||!strcmp(config.font,"HELP"))
   {
   helpcode=9;
   config.font[0] = 0;
   }
else if (!strcmp(config.size,"help")||!strcmp(config.size,"HELP"))
   {
   helpcode=10;
   config.size[0] = 0;
   }
else if (!strcmp(config.width,"help")||!strcmp(config.width,"HELP"))
   {
   helpcode=11;
   config.width[0] = 0;
   }
else if (!strcmp(config.hwidth,"help")||!strcmp(config.hwidth,"HELP"))
   {
   helpcode=12;
   config.hwidth[0] = 0;
   }
else if (!strcmp(config.color,"help")||!strcmp(config.color,"HELP"))
   {
   helpcode=13;
   config.color[0] = 0;
   }
else if (!strcmp(config.hcolor,"help")||!strcmp(config.hcolor,"HELP"))
   {
   helpcode=14;
   config.hcolor[0] = 0;
   }
else if(!strcmp(config.background,"help") || !strcmp(config.background,"HELP"))
   {
   helpcode=15;
   config.background[0] = 0;
   }
else if (!strcmp(config.border,"help")||!strcmp(config.border,"HELP"))
   {
   helpcode=16;
   config.border[0] = 0;
   }
else if (!strcmp(config.opaque,"help")||!strcmp(config.opaque,"HELP"))
   {
   helpcode=17;
   config.opaque[0] = 0;
   }
else
   helpcode=0;
 
if (helpcode)
   {
   printf("\n%s\n\nHit <RETURN> to continue:",helptext[helpcode-1]);
   gets(buf);
   return(0);
   }
else 
   {
   /* check color */
   if (!(!strcmp(config.color,"aqua")   || !strcmp(config.color,"black") ||
       !strcmp(config.color,"blue")   || !strcmp(config.color,"brown") ||
       !strcmp(config.color,"cyan")   || !strcmp(config.color,"gray") ||
       !strcmp(config.color,"green")  || !strcmp(config.color,"grey") ||
       !strcmp(config.color,"indigo") || !strcmp(config.color,"magenta") ||
       !strcmp(config.color,"orange") || !strcmp(config.color,"purple") ||
       !strcmp(config.color,"red")    || !strcmp(config.color,"violet") ||
       !strcmp(config.color,"white")  || !strcmp(config.color,"yellow") ||
       !strcmp(config.color,"none")))
       {
       printf("%s is an invalid color, type help in the",config.color);
       printf(" TEXT COLOR field\nto view a list of valid colors\n");
       printf("\nHit <RETURN> to continue:");
       gets(buf);
       return(0);
       }
   /* check highlight color */
   if (!(!strcmp(config.hcolor,"aqua")   || !strcmp(config.hcolor,"black") ||
       !strcmp(config.hcolor,"blue")   || !strcmp(config.hcolor,"brown") ||
       !strcmp(config.hcolor,"cyan")   || !strcmp(config.hcolor,"gray") ||
       !strcmp(config.hcolor,"green")  || !strcmp(config.hcolor,"grey") ||
       !strcmp(config.hcolor,"indigo") || !strcmp(config.hcolor,"magenta") ||
       !strcmp(config.hcolor,"orange") || !strcmp(config.hcolor,"purple") ||
       !strcmp(config.hcolor,"red")    || !strcmp(config.hcolor,"violet") ||
       !strcmp(config.hcolor,"white")  || !strcmp(config.hcolor,"yellow") ||
       !strcmp(config.hcolor,"none")))
       {
       printf("%s is an invalid highlight color, type help in",config.hcolor);
       printf(" the HIGHLIGHT COLOR field\nto view a list of valid colors\n");
       printf("\nHit <RETURN> to continue:");
       gets(buf);
       return(0);
       }
   /* check font */
   if (!(!strcmp(config.font,"standard") || !strcmp(config.font,"cyrilc") ||
       !strcmp(config.font,"gothgbt")  || !strcmp(config.font,"gothgrt") ||
       !strcmp(config.font,"gothitt")  || !strcmp(config.font,"greekc") ||
       !strcmp(config.font,"greekcs")  || !strcmp(config.font,"greekp") ||
       !strcmp(config.font,"greeks")   || !strcmp(config.font,"italicc") ||
       !strcmp(config.font,"italiccs") || !strcmp(config.font,"italict") ||
       !strcmp(config.font,"romanc")   || !strcmp(config.font,"romancs") ||
       !strcmp(config.font,"romand")   || !strcmp(config.font,"romans") ||
       !strcmp(config.font,"romant")   || !strcmp(config.font,"scriptc") ||
       !strcmp(config.font,"scripts")))
       {
       printf("%s is an invalid font, type help in the",config.font);
       printf(" FONT field\nto view a list of valid fonts\n");
       printf("\nHit <RETURN> to continue:");
       gets(buf);
       return(0);
       }
   }
return(1);
}
