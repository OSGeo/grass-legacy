#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <setjmp.h>
#include <signal.h>

static jmp_buf my_environment;

main()
{
static void timeout();
int in_file, out_file;
int time=5;

if (setjmp(my_environment)) 
   {
   printf("my longjmp was successful so i'm outta here!\n");
   signal(SIGALRM,SIG_DFL); 
   exit(0);
   }

signal(SIGALRM, timeout);
alarm(time);                  
printf("alarm set, now i'm gonna open those fifos\n");

in_file = open("/grass/grass3/dev/fifo.1a", O_RDONLY);   
out_file = open("/grass/grass3/dev/fifo.1b", O_WRONLY);  

alarm(0);
signal(SIGALRM,SIG_DFL);  

printf("hummm... must have had an error on opening the fifos, bye\n");

close(out_file);
close(in_file);

exit(0);
}

static void timeout(sig,code,scp,addr)
int sig, code;
struct sigcontext *scp;
char *addr;
{
printf("alarm went off so i'll do a longjmp\n");
longjmp(my_environment,1);
}

