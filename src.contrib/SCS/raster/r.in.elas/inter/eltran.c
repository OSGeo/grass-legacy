#include <stdio.h>
#include <fcntl.h>

/* int tptran(); */

/*
**      makestring is a routine called by eltran to convert a name
**      given in an integer array into a c string.
*/

makestring ( src, dst )
char *src, *dst;
{
    char *s, *d;

    s = src;
    d = dst;
    for ( ; *s != ' '; *d++ = *s++ );

    *d = '\0';
}

char *TAPE = "/dev/nrmt0";
char *TAPE2 = "/dev/nrmt1";
char *NEWFIL =" New File :                              ";


int tptst ( src )
char *src;
{
    char name[39];

    makestring ( src, name );
    return ( (strcmp(name,TAPE)==0) || (strcmp(name,TAPE2)==0) );
}

char *fdcf = "00";


/*
**      ELTRAN is a routine called from Fortran in ELAS.
**
**      CALL ELTRAN ( LU, FUN, IRN, NBTM, IA, L )
**
**      LU    i = Logical unit number (fake numbers)
**      FUN   i = Function to perform
**      IRN   i = Record number usually
**      NBTM  i = Number of bytes to transfer usually
**      IA   ia = Data array
**      L     i = Status  ( L < 0 means error )
*/

static short td[20]={-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
        fd_list[20]={-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

eltran_ ( lu, fun, irn, nbtm, ia, l )
long *lu, *fun, *irn, *nbtm, *ia, *l;
{
        int j;
    short i, fd, nbps;
        long lnbtm;
    char name[39];
        lnbtm=(*nbtm);

/*    if ( (*fun<13) & (( td[*lu] == 0 ) | ((*fun==5) && tptst(ia)))) {
        tptran ( td, fd_list, lu, fun, irn, nbtm, ia, l );
        return;
    } */

    if ( (*lu<1) || (*lu>19) ) {
        printf("Illegal logical unit %10ld\n",*lu);
        *l = -1;
        return;
    }

    if ( (*fun<1) || (*fun>14)) {
        printf("Illegal function %10ld\n",*fun);
        *l = -1;
        return;
    }

/*
**      Now it's time to do something to a disk file.
**      The first thing to do is retrieve the file descriptor.
**      Then switch to the correct code for the request.
*/
        /* rdwr and dfaloc (dfazer) send -nbtm */

        if(lnbtm < 0) {
        nbps = 512 ;
        lnbtm=(-lnbtm);
        }
        else nbps=lnbtm;

    fd = fd_list[*lu];

    switch ( *fun )
    {

    case 1:     /*      Sequential read         */

/*      *l = read ( fd, ia, lnbtm );  */
        *l = sqrd(fd,ia,lnbtm);
        if ( *l == 0 ) *l = -3;
        break;

    case 2:     /*      Sequential write        */

/*      *l = write ( fd, ia, lnbtm );  */
        *l = sqwr( fd,ia,lnbtm);
        break;

    case 3:     /*      Random read             */

        lseek ( fd, *irn * nbps, 0 );
        *l = read ( fd, ia, lnbtm );
        if ( *l == 0 ) *l = -3;
        break;

    case 4:     /*      Random write            */

        lseek ( fd, *irn * nbps, 0 );
        *l = write ( fd, ia, lnbtm );
        break;

    case 5:     /*      Open a file             */

        makestring ( ia, name );
            fd = open ( name, 2 );
        if ( fd == -1 ) {
            *l = -1;
        }
        else {
        *l = (*lu<5) ? 240 : 512 ;
            fd_list[*lu] = fd;
            td[*lu] = 1;
        }
        break;

    case 6:     /*      Close a file            */

        close ( fd );
        fd_list[*lu] = -1;
        td[*lu] = -1;
        break;

    case 7:     /*      Skip records            */
                break;
    case 8:     /*      Skip files              */
        lseek(fd,0L,2)
        ; break;
    case 9:     /*      Write EOF               */

        break;

    case 10:    /*      Rewind                  */

        lseek ( fd, 0L, 0 );
        break;

    case 11:    /*      Create a file           */

        makestring ( ia, name );
  /* fprintf(2," File : %s\n", name );  */
        for(i = 0 ; ( i < 20 ) && ( name[i] != '\0' ) ; i++)
        {       j=i+12 ;
                NEWFIL[j] = name[i]; }
                NEWFIL[++j]= '\n' ;
                ++j ;
                if (NEWFIL[12] == '.' && NEWFIL[13] == 'i' )
                        ;
                else
                write(2,NEWFIL,j) ;

        if ( *irn == 1 ) {              /* "Indexed" file */
            fd = open ( name, 2);
            if ( fd == -1 ) fd = creat ( name, 0664 );
            if ( fd == -1 ) {
                *l = -1;
            }
            else {
                        close( fd );
                        fd = open( name,2);
                *l = lnbtm;
                fd_list[*lu] = fd;
                td[*lu] = 1;
            }
        }
        else {                          /* Contiguous file */
            fd = open ( name, 2 );
/*          if ( fd == -1 ) fd = open ( name, O_RDWR | O_CREAT |
                        O_TRUNC | O_CTG, 0664, lnbtm * 512 );

  The above option O_CTG is not available for ATT System 5  
				 RLG, SCS, 9/21/89               */
            if ( fd == -1 ) fd = open ( name, O_RDWR | O_CREAT |
                        O_TRUNC , 0664, lnbtm * 512 );
            if ( fd == -1 ) fd = creat ( name, 0664 );
            if ( fd == -1 ) {
                *l = -1;
            }
            else {
        *l = nbps;
                fd_list[*lu] = fd;
                td[*lu] = 1;
            }
        }
        break;

    case 12:    /*      Attribute request               */

        *l = (*lu<5) ? 240 : 512 ;
        break;

    case 13:    /*      Retrieve td & fd_list           */

        for ( i=0; i<20; i++ ) {
            ia[i] = td[i];
            ia[i+20] = fd_list[i];
        }
        *fdcf = (char) fd_list[4];
        break;

    case 14:    /*      Set up td & fd_list             */

        for ( i=0; i<20; i++ ) {
            td[i] = ia[i];
            fd_list[i] = ia[i+20];
        }
        break;

    }           /*      End of switch statement         */

}

/*
**      elexec is a FORTRAN callable routine to load a new
**      ELAS program from /usr/elas/bin.
*/

elexec_ ( name )
char name[];
{
    static char path[] = "/usr/elas/bin/fmgr     ";
    int i;

    for ( i=0; (i<8) && ( name[i] != ' '); i++ ) {
        if ( (name[i] >= 'A') & (name[i] <= 'Z') ) {
            path[i+14] = name[i] + 32;
        }
        else {
            path[i+14] = name[i];
        }
    }

    path[i+14] = '\0';

    execlp ( path, path, fdcf, (char *) 0 );

    exit(1);
}
sqrd (fd,buf,nbts)
char buf[];
short fd;
long nbts;
{

long pos;
long ll;
short i;
int nbr;
        pos=lseek(fd,0l,1);
        nbr=read(fd, buf, nbts);
        if(nbr == 0 ) return(-3);
        if(nbr < 1) return(-2);
        for(i=0; (i < nbr) && (buf[i] != '\n');i++)
                pos++;
        pos++;
        for( ; i < nbts ; i++)
                buf[i]=' ';
        lseek(fd,pos,0);
        return(nbts);
}

sqwr (fd,buf,nbts)
char buf[];
short fd;
long nbts;
{

char i10='\n',ibl=' ';
int i;

        for (i = nbts-1 ; (i > -1) && (buf[i] == ibl) ; i--)
                ;
        if(i != (nbts-1))
            {   buf[++i]=i10;
                i++;
                write(fd,buf,i);
                        buf[--i]=ibl;   }
                else {
        write(fd,buf,nbts);
        write(fd,i10,1);    }
        return(nbts);
}


