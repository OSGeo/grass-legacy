/* %W% %G% */

#include <stdio.h>
read_cbuf(fileptr, buf, size) FILE *fileptr ; unsigned char *buf ;
{
        int num_read ;

        num_read = fread(buf, sizeof(unsigned char), size, fileptr) ;

        if(num_read != size)
        {
                fprintf(stderr, "read_cbuf: could not read.\n") ; 
                return(0) ;
        }
        return(1) ;
}
writ_cbuf(fileptr, buf, size) FILE *fileptr ; unsigned char *buf ;
{
        int num_writ ;

        num_writ = fwrite(buf, sizeof(unsigned char), size, fileptr) ;

        if(num_writ != size)
        {
                fprintf(stderr, "writ_cbuf: could not write.\n") ; 
                return(0) ;
        }
        return(1) ;
}

FILE *open_file(filename, purpose) char *filename, *purpose ;
{
        FILE *fileptr ;

        fileptr = fopen(filename, purpose) ;

        if(fileptr == NULL)
        {
                fprintf(stderr, "open_file: could not open %s\n", filename) ; 
                return(0) ;
        }
        return(fileptr) ;
}
