extern int errno;
extern char *sys_errlist[];

int main(int argc, char *argv[]) {
    FILE *stream;
    
        if (argc != 2) {
                fprintf(stderr, "Usage: err file\n");
                        return 1;
                            }
                            
         stream = fopen(argv[1], "r");
            if (stream == NULL) {
                  fprintf(stderr, "Can't open %s (%s)\n", argv[1],
                        sys_errlist[errno]);
                fprintf(stderr, "Can't open %s (%s)\n", argv[1],
                       strerror(errno));
               return 1;
                               }
                          
       return 0;
    }
                                                                    