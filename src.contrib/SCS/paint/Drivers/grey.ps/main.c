static char rcsid[]="$Header: /gis3/4.0/src/paint/Drivers/ppm/RCS/main.c,v 1.1 1991/09/28 22:03:56 paul Exp $";
main(argc,argv) char *argv[];
{
    Pset_color_levels(6);
    paint_interface(argc, argv);
}
