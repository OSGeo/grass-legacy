#include <sgtty.h>
main()
{
#ifndef TIOCFLUSH
  exit(1);
#endif
#ifndef TIOCGETC
  exit(1);
#endif
#ifndef TIOCGETD
  exit(1);
#endif
#ifndef TIOCGETP
  exit(1);
#endif
#ifndef TIOCGLTC
  exit(1);
#endif
#ifndef TIOCLSET
  exit(1);
#endif
#ifndef TIOCSETC
  exit(1);
#endif
#ifndef TIOCSETD
  exit(1);
#endif
#ifndef TIOCSETP
  exit(1);
#endif
#ifndef TIOCSLTC
  exit(1);
#endif
#ifndef LLITOUT
  exit(1);
#endif
#ifndef FIONREAD
  exit(1);
#endif
}
