/* get_deg.c    1.0   6/182/91
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: function
*			Provide a means of collecting user lat/long
*			data, in different formats; convert to
*                       decimal degrees
*    Input arguements : lat or long string   and
*                       a 1 for latitude or 0 for longitude
*    Output arguements: decimal degrees in string
*
*		Note: All functions are callable directly see
*			g.get_stp	g.get_fips	g.stp_proj  geo
*
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>


int get_deg(strng,ll_swt)
char *strng;
int ll_swt;
{
	 int n=1, cnt=1, sign=0, markd=0;
	 double degrees, min, sec;
         char *a, *b, *c, *d, string[20], dummy[1];

	 degrees = min = sec = 0.0;
	 a = d = strng;
	 dummy[0] = '\0';
	 b = c = dummy;
	 sprintf(string,"%s",strng);
	 while (*d != '\0')
	   {
	   switch(*d)
	     {
	     case '0': case '1': case '2':
	     case '3': case '4': case '5':
	     case '6': case '7': case '8':
	     case '9': case '.':
		       cnt++;
		       break;

             case 'N': case 'n':
		       if (!ll_swt)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for longitude **\n", strng);
                         sleep(2);
	                 return(0);
	                 }
		       markd = 1;
                       if (sign == -1)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for latitude north **\n",
					       strng);
                         sleep(2);
	                 return(0);
			 }
		       sign = 1;
		       *d = 0;
		       break;

             case 'E': case 'e':
		       if (ll_swt)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for latitude **\n", strng);
                         sleep(2);
	                 return(0);
	                 }
		       markd = 1;
                       if (sign == -1)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for longitude east **\n",
					       strng);
                         sleep(2);
	                 return(0);
			 }
                       sign = 1;
		       *d = 0;
		       break;

             case 'W': case 'w':
		       if (ll_swt)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for latitude **\n", strng);
                         sleep(2);
	                 return(0);
	                 }
		       markd = 1;
                       if (sign == 1)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for longitude west **\n",
					       strng);
                         sleep(2);
	                 return(0);
			 }
		       sign = -1;
		       *d = 0;
		       break;

             case 'S': case 's':
		       if (!ll_swt)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for longitude **\n", strng);
                         sleep(2);
	                 return(0);
	                 }
		       markd = 1;
                       if (sign == 1)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** %s is invalid for latitude south **\n",
					       strng);
                         sleep(2);
	                 return(0);
			 }
                       sign = -1;
		       *d = 0;
		       break;

             case 'D': case 'd':
             case '"': case '\'':
		       *d = 0;
		       break;
             case ' ':            /* look for leading spaces */
		       if (cnt != 1)
			  {
			  n++;
	                  if (n == 2) b = d;
	                  if (n == 3) c = d;
			  }
		       break;
             case '-':
                       if (cnt > 1)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** - sign must be at the beginning of %s **\n",
				       strng);
                         sleep(2);
	                 return(0);
			 }
		       sign = -1;
		       *d = '\040';
		       break;
             case '+':
                       if (cnt > 1)
			 {
		         sprintf(strng,"%s",string);
	                 fprintf(stderr,
			  "\n\t** + sign must be at the beginning of %s **\n",
				       strng);
                         sleep(2);
	                 return(0);
			 }
		       sign = 1;
		       *d = '\040';
		       break;
             default:  
		       sprintf(strng,"%s",string);
		       fprintf(stderr,
			  "\n\t** character %d of <%s> is Invalid **\n",
					cnt, strng);
		       sleep(2);
	               return(0);
		       break;
	     }  /* end of switch */
           d++;
	   }  /* end while looking for end of strng */

         if (!markd && !sign)
	    {
	    sprintf(strng,"%s",string);
            if (!ll_swt) 
               fprintf(stderr,"\n\t** %se, %sw, or -%s is valid **\n",
                                strng, strng, strng);
            else
               fprintf(stderr,"\n\t** %sn, %ss, or -%s is valid **\n",
                                strng, strng, strng);
            sleep(2);
	    return(0);
	    }

         sprintf(string,"%s %s %s",a,b,c);

	 if (n == 1) sscanf(string,"%lf", &degrees);
	 if (n == 2) 
	       {
	       sscanf(string,"%lf %lf", &degrees, &min);
	       degrees = degrees + (min / 60);
	       }
	 if (n == 3) 
	       {
	       sscanf(string,"%lf %lf %lf", &degrees,&min,&sec);
	       degrees = degrees + min / 60. + sec / 3600.;
	       }
         sprintf(strng,"%lf",degrees * sign);
         return(1);
}
