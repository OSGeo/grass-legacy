/* main.c */
int debugf(void);
/* plot.c */
int plot1(char *, char *, struct line_pnts *, int, int);
/* color_parse.c */
int color_parse (const char *,     /* input:  color string to parse */
                  unsigned char *, /* output: red                   */
                  unsigned char *, /* output: green                 */
                  unsigned char *); /* output: blue                  */
