/*
 * read_specs.c
 *
 * Subroutine of GRASS Gcell.colors.new program
 *
 * Dave Johnson, DBA Systems, Inc. - 1989
 *
 * Reads cell file color specifications from stdin, parses them,
 * and returns them in a linked list.  Parameters:
 *
 * specs	linked list of color specifications
 * min		min cell file category value AKA min color-table index
 * max		max cell file category value AKA max color-table index
 *
 * Valid formats for color specs from stdin:
 *
 * category number with RGB values    (eg. 5  255 0 255)
 * percentage number with RGB values  (eg. %40 0 255 0)
 * category number with color name    (eg. 5 orange)
 * percentage number with color name  (eg. %30 yellow)
 * unnumbered RGB values              (eg. 0 255 255) 
 * unnumbered color name              (eg. magenta) 
 *
 */

#include <stdio.h>
#include "gis.h"
#include "specs.h"

read_specs(specs,min,max)
struct SpecList *specs;
CELL min,max;
{
struct Spec  *spec  = NULL,
             *spec2 = NULL;
char         MsgBuf[1024],            /* buffer for error messages */
             InBuf[1024],             /* input buffer */
             color[1024],             /* temp. storage for color name */
             SpecType;                /* temp. storage for "%" */ 
int          R,G,B,                   /* temp. storage for RGB values */ 
             index,                   /* temp. storage for LUT index */
             GoodSpec,                /* true if last spec was valid */
             LineNum=0,               /* count input lines for error messages */
             i=0,j=0,k=0,l=0,         /* loop counters */
             NumColors = max - min,   /* number of cat. values (colors) */
             LastIndex = -1,          /* to make sure indices are in order */
             ColorNameOK,             /* true if color name is valid */
             NextWith,                /* next spec with an index */
             LastWith,                /* last spec with an index */
             NumWithout;              /* number of specs without indices */

             /* incriments for assigning LUT indices */
float        PercentIncr = (float)(NumColors/100.0),
             IndexIncr;

/* initialize the spec list */
specs->NumSpecs = 0;
specs->first = NULL;
specs->last = NULL;
specs->ptr = NULL;

while (1)   /* loop to read in color specs */
   {
   GoodSpec = 0;

   if (gets(InBuf) == NULL)
      break;

   spec = (struct Spec *)malloc(sizeof(struct Spec));
 
   LineNum++;

   /* category number with RGB values */
   if (sscanf(InBuf,"%d %d %d %d",&index,&R,&G,&B) == 4) 
      {
      spec->index = index;
      spec->R = R;
      spec->G = G;
      spec->B = B;
      if (spec_ok(spec) && spec->index > LastIndex)
         GoodSpec = 1;
      }

   /* percentage number with RGB values */
   else if (sscanf(InBuf,"%c%d %d %d %d",&SpecType,&index,&R,&G,&B) == 5)
      {
      if (SpecType == '%')
         {
         spec->index = min + (((float)index)*PercentIncr + 0.5); 
         spec->R = R;
         spec->G = G;
         spec->B = B;
         if (spec_ok(spec) && spec->index > LastIndex)
            GoodSpec = 1;
         }
      }
  
   /* unnumbered RGB values */
   else if (sscanf(InBuf,"%d %d %d",&R,&G,&B) == 3)
      {
      spec->index = -1; 
      spec->R = R;
      spec->G = G;
      spec->B = B;
      if (spec_ok(spec))
         GoodSpec = 1;
      }
   
   /* category number with color name */
   else if (sscanf(InBuf,"%d %s",&index,color) == 2) 
      {
      spec->index = index;
      ColorNameOK = assign_RGB_values(spec,color);
      if (ColorNameOK && spec_ok(spec) && spec->index > LastIndex)
         GoodSpec = 1;
      }

   /* percentage number with color name */
   else if (sscanf(InBuf,"%c%d %s",&SpecType,&index,color) == 3) 
      {
      if (SpecType == '%')
         {
         spec->index = min + (((float)index)*PercentIncr + 0.5); 
         ColorNameOK = assign_RGB_values(spec,color);
         if (ColorNameOK && spec_ok(spec) && spec->index > LastIndex)
            GoodSpec = 1;
         }
      }

   /* unnumbered color name */
   else if (sscanf(InBuf,"%s",color) == 1) 
      {
      spec->index = -1; 
      ColorNameOK = assign_RGB_values(spec,color);
      if (ColorNameOK && spec_ok(spec))
         GoodSpec = 1;
      }

   if (!GoodSpec)                     /* bad color spec, trash it */
      {
      free(spec);
      sprintf(MsgBuf,"BAD COLOR SPEC OR SPEC OUT OF ORDER ON LINE %d\n",LineNum);
      printf("%s",MsgBuf);
      }
   else                               /* good spec, put it in the list */
      {
      i++;
      LastIndex = spec->index;
      if (specs->first == NULL)       /* it's the first one */ 
         {
         spec->prev = NULL;          
         spec->next = NULL;
         specs->first = spec;
         specs->last  = spec;
         specs->ptr = spec;
         }
      else                            /* it's not the first one */ 
         {
         spec->next = NULL;
         spec->prev = specs->ptr;
         specs->ptr->next = spec;
         specs->ptr = spec;
         specs->last = spec;
         } 
      }
   }

if (i==1)
   return(-1);

specs->NumSpecs = i-1;

/*
 * make sure each spec has an index - assign indices to specs that
 * have no indices by evenly spacing them between those that do.
 *
 */

/* if first has no index - give it max cat value as index */
if (specs->first->index == -1) specs->first->index = min;

/* if last has no index - give it min cat value as index */
if (specs->last->index == -1) specs->last->index = max;

/* traverse specs, give indices to those without indices */ 
j = 0;
specs->ptr = specs->first;
while (specs->ptr != specs->last)
   {
   if (specs->ptr->index == -1 )          /* found one without */
      {
      LastWith = j-1;
      k = j;
      spec = specs->ptr;                  /* point to one without */ 
      specs->ptr = specs->ptr->prev;      /* point to last one with */
      while (spec->index == -1)           /* find next one with */ 
         {
         k++;
         spec = spec->next;
         }
      NextWith = k;
      
      /* calculate incriment to use for filling in indices */

      NumWithout = NextWith - LastWith;
      IndexIncr = (spec->index - specs->ptr->index)/NumWithout; 
     
      /* loop to assign indices between last one that had an index and
         the next one that has an index */ 

      spec2 = specs->ptr->next;
      for (l=1; l<NumWithout; l++)
         {
         spec2->index = specs->ptr->index + l*IndexIncr;
         spec2 = spec2->next;
         }
      j = LastWith = NextWith;
      specs->ptr = spec2;
      }
   else
      {
      specs->ptr = specs->ptr->next; 
      LastWith = j++;
      }
   }
return(0);
}


/*
 * spec_ok()
 * 
 * Check a color-spec's RGB values to make sure they're A-OK,
 * truly, truly.  Returns 1 if they are OK.  Returns 0 if
 * they are not OK
 *
 */
int spec_ok(aspec)
struct Spec *aspec;
{
if (aspec->R >= 0 && aspec->G >= 0 && aspec->B >= 0 &&
    aspec->R <= 255 && aspec->G <= 255 && aspec->B <= 255)
   return(1);
else 
   return(0);
}


/*
 * assign_RGB_values()
 *
 * Assigns RGB values to a color-spec.  There are 15 or so colors
 * defined by name in GRASS and each has its very own RGB values
 * defined.  This routine accepts a color-spec and a color-name
 * and assigns the RGB values associated with the color-name to
 * the color spec. 
 */
int assign_RGB_values(specs,color_str)
struct Spec *specs;
char *color_str;
{

/* determine the color's R,G and B numbers */
if (strcmp(color_str,"white") == 0)
   {
   specs->R = 255;
   specs->G = 255;
   specs->B = 255;
   }
else if (strcmp(color_str,"black") == 0)
   {
   specs->R = 0;
   specs->G = 0;
   specs->B = 0;
   }
else if (strcmp(color_str,"red") == 0)
   {
   specs->R = 255;
   specs->G = 0;
   specs->B = 0;
   }
else if (strcmp(color_str,"green") == 0)
   {
   specs->R = 0;
   specs->G = 255;
   specs->B = 0;
   }
else if (strcmp(color_str,"blue") == 0)
   {
   specs->R = 0;
   specs->G = 0;
   specs->B = 255;
   }
else if (strcmp(color_str,"yellow") == 0)
   {
   specs->R = 255;
   specs->G = 255;
   specs->B = 0;
   }
else if (strcmp(color_str,"magenta") == 0)
   {
   specs->R = 255;
   specs->G = 0;
   specs->B = 255;
   }
else if (strcmp(color_str,"cyan") == 0)
   {
   specs->R = 0;
   specs->G = 255;
   specs->B = 255;
   }
else if (strcmp(color_str,"aqua") == 0)
   {
   specs->R = 0;
   specs->G = 191;
   specs->B = 191;
   }
else if (strcmp(color_str,"gray") == 0)
   {
   specs->R = 191;
   specs->G = 191;
   specs->B = 191;
   }
else if (strcmp(color_str,"grey") == 0)
   {
   specs->R = 191;
   specs->G = 191;
   specs->B = 191;
   }
else if (strcmp(color_str,"orange") == 0)
   {
   specs->R = 255;
   specs->G = 127;
   specs->B = 0;
   }
else if (strcmp(color_str,"brown") == 0)
   {
   specs->R = 191;
   specs->G = 127;
   specs->B = 64;
   }
else if (strcmp(color_str,"purple") == 0)
   {
   specs->R = 127;
   specs->G = 0;
   specs->B = 255;
   }
else if (strcmp(color_str,"violet") == 0)
   {
   specs->R = 127;
   specs->G = 0;
   specs->B = 255;
   }
else if (strcmp(color_str,"indigo") == 0)
   {
   specs->R = 0;
   specs->G = 127;
   specs->B = 255;
   }
else 
   return(0); /* invalid color string */

return(1); /* normal exit - no errors */
}
