#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "gis.h"

static int color_parse_hex (const char *,
                            unsigned char *,
                            unsigned char *,
                            unsigned char *);
static int color_parse_rgb (const char *,
                            unsigned char *,
                            unsigned char *,
                            unsigned char *);
static int color_parse_named (const char *,
                              unsigned char *,
                              unsigned char *,
                              unsigned char *);

/* ================================================================= *
 * color_parse(): Takes a string specifying a color and tries to
 *                convert it to appropriate RGB values.  Returns 0
 *                on success and -1 on normal failure
 * ================================================================= */
int color_parse (const char *name,  /* input:  color string to parse */
                  unsigned char *R, /* output: red                   */
                  unsigned char *G, /* output: green                 */
                  unsigned char *B) /* output: blue                  */
{
   int status;

   /* try hexadecimal, then rgb() then, color name lookup */
   status = color_parse_hex (name, R, G, B);
   if (status != 0)
   {
      status = color_parse_rgb (name, R, G, B);
      if (status != 0)
      {
         status = color_parse_named (name, R, G, B);
      }
   }

   return (status) ? -1 : 0;
}


static int color_parse_hex (const char *name,
                            unsigned char *R,
                            unsigned char *G,
                            unsigned char *B)
{
   int status = -1, len = 0, i, j;
   unsigned char array[6];
   const char *hexed = "0123456789ABCDEF";
            
   len = strlen(name);

   if (len != 6)
      return status;

   for (i = 0; i < len; i++)
   {
      for (j = 0; j < 16; j++)
      {
         if (toupper(name[i]) == hexed[j])
            break;
      }
      if (j > 15)
         return status;
      array[i] = (unsigned char) j;
   }

   *R = array[0] * 16 + array[1];
   *G = array[2] * 16 + array[3];
   *B = array[4] * 16 + array[5];

   return 0;
}

static int color_parse_rgb (const char *name,
                            unsigned char *R,
                            unsigned char *G,
                            unsigned char *B)
{
   int status, red, green, blue;

   status = sscanf (name, "rgb( %d %d %d )", &red, &green, &blue);

   if (status != 3)
      return -1;

   if (red >= 0 && red <= 255)
      *R = (unsigned char) red;
   else
      return -1;

   if (green >= 0 && green <= 255)
      *G = (unsigned char) green;
   else
      return -1;

   if (blue >= 0 && blue <= 255)
      *B = (unsigned char) blue;
   else
      return -1;

   return 0;
}


static int color_parse_named (const char *name,
                              unsigned char *R,
                              unsigned char *G,
                              unsigned char *B)
{
   char *path, *base, *beg, *end;
   int len;
   int status = -1, red, green, blue;
   char buffer[80];
   FILE *rgb_file;
   
   base = G_gisbase();
   len = strlen(base) + 13;
   path = G_malloc(len);
   sprintf (path, "%s/%s", base, "etc/rgb.txt");
   
   rgb_file = fopen (path, "r");

   if (rgb_file == NULL)
   {
      G_free (path);
      return status;
   }
   
   status = 0;
   buffer[0] = '\0';
   while (!status && NULL != (fgets (buffer, 80, rgb_file)))
   {
       if (buffer[0] == '\0' || buffer[0] == '!')
          continue;
       beg = &buffer[0];
       while (*beg != '\0' && !isalpha(*beg))
          beg++;
       end = strrchr (beg, '\0');
       end--;
       while (end > beg && isspace(*end))
          end--;
       end[1] = '\0';
       if (!strcmp (beg, name))
       {
          status = sscanf (buffer, " %d %d %d ", &red, &green, &blue);
          if (status != 3)
            G_fatal_error ("Reading rgb.txt");
          *R = (unsigned char) red;
          *G = (unsigned char) green;
          *B = (unsigned char) blue;
       }
   }

   fclose (rgb_file);
   G_free (path);
   return (status) ? 0 : -1;
}

/* vim: set softtabstop=3 shiftwidth=3 expandtab: */
