#ifndef __CHECKS_H
#define __CHECKS_H

#ifdef __cplusplus
extern "C" {
#endif
  /* @FPUB */
  int message_box(const char* fmt, ...);
  int warning_box(const char* fmt, ...);
  int sorry_box(const char* fmt, ...);
  int error_box(const char* fmt, ...);
  int fatal_box(const char* fmt, ...);
  int yesno_box(const char* fmt, ...);
  int yesnocancel_box(const char* fmt, ...);
  int yesnofatal_box(const char* fmt, ...);

  /* @END  */
#ifdef __cplusplus
}
#endif


/* @M 
   Utilizzate in fase di debug (definire il simbolo DBG in compilazione)
   */
#ifdef DBG
#define CHECK(p, m) ( (p) ? (void)0 : (void) fatal_box(          \
						       "Check failed in %s, line %d:\n\r%s",      \
						       __FILE__, __LINE__, m) )

#define CHECKS(p, m, s0) ( (p) ? (void)0 : (void) fatal_box(      \
							    "Check failed in %s, line %d:\n\r%s%s", \
							    __FILE__, __LINE__, m, s0) )

#define CHECKD(p, m, d0) ( (p) ? (void)0 : (void) fatal_box(      \
							    "Check failed in %s, line %d:\n\r%s%d", \
							    __FILE__, __LINE__, m, d0) )
#else

#define CHECK(p, m)
#define CHECKS(p, m, s)
#define CHECKD(p, m, d)

#endif
/* @END */


/* @M
   Utilizzata in fase di debug (definire il simbolo TRC in compilazione)
   */
#ifdef TRC
#define TRACE(m) warning_box(m)
#else
#define TRACE(m) ((void) 0)
#endif
/* @END */

#endif // __CHECKS_H
