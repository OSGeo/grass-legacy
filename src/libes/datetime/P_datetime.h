/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
#ifdef __STDC__
# define	P(s) s
#else
# define P(s) ()
#endif


/* between.c */
int datetime_is_between P((int x , int a , int b ));

/* change.c */
int datetime_change_from_to P((DateTime *dt , int from , int to , int round ));

/* copy.c */
void datetime_copy P((DateTime *src , DateTime *dst ));

/* diff.c */
int datetime_difference P((DateTime *a , DateTime *b , DateTime *result ));

/* error.c */
int datetime_error P((int code , char *msg ));
int datetime_error_code P((void ));
char *datetime_error_msg P((void ));
void datetime_clear_error P((void ));

/* format.c */
int datetime_format P((DateTime *dt , char *buf ));

/* incr1.c */
int datetime_increment P((DateTime *src , DateTime *incr ));

/* incr2.c */
int datetime_is_valid_increment P((DateTime *src , DateTime *incr ));
int datetime_check_increment P((DateTime *src , DateTime *incr ));

/* incr3.c */
int datetime_get_increment_type P((DateTime *dt , int *mode , int *from , int *to , int *fracsec ));
int datetime_set_increment_type P((DateTime *src , DateTime *incr ));

/* local.c */
int datetime_get_local_timezone P((int *minutes ));
void datetime_get_local_time P((DateTime *dt ));

/* misc.c */
int datetime_days_in_month P((int year , int month , int ad ));
int datetime_is_leap_year P((int year , int ad ));
int datetime_days_in_year P((int year , int ad ));

/* scan.c */
int datetime_scan P((DateTime *dt , char *buf ));

/* sign.c */
int datetime_is_positive P((DateTime *dt ));
int datetime_is_negative P((DateTime *dt ));
void datetime_set_positive P((DateTime *dt ));
void datetime_set_negative P((DateTime *dt ));
void datetime_invert_sign P((DateTime *dt ));

/* type.c */
int datetime_set_type P((DateTime *dt , int mode , int from , int to , int fracsec ));
int datetime_get_type P((DateTime *dt , int *mode , int *from , int *to , int *fracsec ));
int datetime_is_valid_type P((DateTime *dt ));
int datetime_check_type P((DateTime *dt ));
int datetime_in_interval_year_month P((int x ));
int datetime_in_interval_day_second P((int x ));
int datetime_is_absolute P((DateTime *dt ));
int datetime_is_relative P((DateTime *dt ));

/* tz1.c */
int datetime_check_timezone P((DateTime *dt , int minutes ));
int datetime_get_timezone P((DateTime *dt , int *minutes ));
int datetime_set_timezone P((DateTime *dt , int minutes ));
int datetime_unset_timezone P((DateTime *dt ));
int datetime_is_valid_timezone P((int minutes ));

/* tz2.c */
int datetime_change_timezone P((DateTime *dt , int minutes ));
int datetime_change_to_utc P((DateTime *dt ));
void datetime_decompose_timezone P((int tz , int *hours , int *minutes ));

/* values.c */
int datetime_check_year P((DateTime *dt , int year ));
int datetime_check_month P((DateTime *dt , int month ));
int datetime_check_day P((DateTime *dt , int day ));
int datetime_check_hour P((DateTime *dt , int hour ));
int datetime_check_minute P((DateTime *dt , int minute ));
int datetime_check_second P((DateTime *dt , double second ));
int datetime_check_fracsec P((DateTime *dt , int fracsec ));
int datetime_get_year P((DateTime *dt , int *year ));
int datetime_set_year P((DateTime *dt , int year ));
int datetime_get_month P((DateTime *dt , int *month ));
int datetime_set_month P((DateTime *dt , int month ));
int datetime_get_day P((DateTime *dt , int *day ));
int datetime_set_day P((DateTime *dt , int day ));
int datetime_get_hour P((DateTime *dt , int *hour ));
int datetime_set_hour P((DateTime *dt , int hour ));
int datetime_get_minute P((DateTime *dt , int *minute ));
int datetime_set_minute P((DateTime *dt , int minute ));
int datetime_get_second P((DateTime *dt , double *second ));
int datetime_set_second P((DateTime *dt , double second ));
int datetime_get_fracsec P((DateTime *dt , int *fracsec ));
int datetime_set_fracsec P((DateTime *dt , int fracsec ));

#undef P
