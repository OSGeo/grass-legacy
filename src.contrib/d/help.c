static char *HELP = "\
\n\
c(ell)      layer\n\
o(overlay)  layer\n\
v(ect)      vector color\n\
l(abels)    labels\n\
s(ites)     sitelist color icon(+xbd) size\n\
\n\
w(indow)    [name [bottom(0) top(100) left(0) right(100)]]\n\
z(oom)      [name [bottom(0) top(100) left(0) right(100)]]\n\
m(mode)     [fixed|float]\n\
\n\
u(ndo)      [a(all)c(cell)o(verlay)v(vector)l(abels)s(ites)]\n\
r(epeat)    [background_color]\n\
<           scriptfile\n\
>[!]        scriptfile\n\
\n\
q        quit\n\
";
help()
{
    printf ("%s\n", HELP);
}
