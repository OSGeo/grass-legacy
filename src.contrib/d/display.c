display(argc, argv)
    char *argv[];
{
    switch (*argv[0])
    {
    case '>': return save_script (argc, argv); 
    case '<': return restore_script (argc, argv); 

    case 'c': return cell (argc-1, argv+1); 
    case 'u': return unset (argc-1, argv+1); 
    case 'l': return labels (argc-1, argv+1); 
    case 'm': return colormode (argc-1, argv+1); 
    case 'o': return overlay (argc-1, argv+1); 
    case 'r': repeat(argc-1, argv+1); return 1;
    case 's': return sites (argc-1, argv+1); 
    case 'v': return vect (argc-1, argv+1); 
    case 'w': return window (argc-1, argv+1); 
    case 'z': return zoom (argc-1, argv+1);


    case 0:
    case '?': help(); return 1;
    default:  printf ("??\n"); help(); return 0;
    }
    return 1;
}
