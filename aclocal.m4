
AC_DEFUN(LOC_CHECK_USE,[
AC_MSG_CHECKING(whether to use $2)
AC_MSG_RESULT("$with_$1")
case "$with_$1" in
	"no")	$3=	;;
	"yes")	$3="1"	;;
	*)	AC_MSG_ERROR([*** You must answer yes or no.])	;;
esac

])

AC_DEFUN(LOC_CHECK_INC_PATH,[
AC_MSG_CHECKING(for location of $2 includes)
case "$with_$1_includes" in
y | ye | yes | n | no)
	AC_MSG_ERROR([*** You must supply a directory to --with-$1-includes.])
	;;
esac
AC_MSG_RESULT($with_$1_includes)

if test -n "$with_$1_includes" ; then
    for dir in $with_$1_includes; do
        if test -d "$dir"; then
            $3="$$3 -I$dir"
        else
            AC_MSG_ERROR([*** $2 includes directory $dir does not exist.])
        fi
    done
fi
])

AC_DEFUN(LOC_CHECK_LIB_PATH,[
AC_MSG_CHECKING(for location of $2 library)
case "$with_$1_libs" in
y | ye | yes | n | no)
	AC_MSG_ERROR([*** You must supply a directory to --with-$1-libs.])
	;;
esac
AC_MSG_RESULT($with_$1_libs)

if test -n "$with_$1_libs"; then
    for dir in $with_$1_libs; do
        if test -d "$dir"; then
            $3="$$3 -L$dir"
        else
            AC_MSG_ERROR([*** $2 library directory $dir does not exist.])
        fi
    done
fi
])

AC_DEFUN(LOC_CHECK_INCLUDES,[
ac_save_cppflags="$CPPFLAGS"
CPPFLAGS="$3 $CPPFLAGS"
AC_CHECK_HEADERS($1, [], ifelse($4,[],[
    AC_MSG_ERROR([*** Unable to locate $2 includes.])
], $4))
CPPFLAGS=$ac_save_cppflags
])

dnl autoconf undefines "shift", so use "builtin([shift], ...)"

define(LOC_SHIFT1,[builtin([shift],$*)])
define(LOC_SHIFT2,[LOC_SHIFT1(LOC_SHIFT1($*))])
define(LOC_SHIFT4,[LOC_SHIFT2(LOC_SHIFT2($*))])
define(LOC_SHIFT8,[LOC_SHIFT4(LOC_SHIFT4($*))])
define(LOC_SHIFT9,[LOC_SHIFT1(LOC_SHIFT8($*))])

dnl $1  = library
dnl $2  = function
dnl $3  = descriptive name
dnl $4  = LDFLAGS initialiser
dnl $5  = result variable
dnl $6  = mandatory dependencies (not added to $5)
dnl $7  = mandatory dependencies (added to $5)
dnl $8  = ACTION-IF-NOT-FOUND
dnl $9+ = optional dependencies

define(LOC_CHECK_LIBS_0,[
AC_CHECK_LIB($1, $2, $5="$$5 -l$1 $7",[
[$8]
],$6 $7)
])

define(LOC_CHECK_LIBS_1,[
ifelse($9,[],
LOC_CHECK_LIBS_0($1,$2,,,$5,$6,$7,$8),
[
LOC_CHECK_LIBS_1($1,$2,,,$5,$6,$7,
LOC_CHECK_LIBS_1($1,$2,,,$5,$6,$7 $9,$8,LOC_SHIFT9($*)),
LOC_SHIFT9($*))
]
)
])

define(LOC_CHECK_LIBS,[
ac_save_ldflags="$LDFLAGS"
LDFLAGS="$4 $LDFLAGS"
LOC_CHECK_LIBS_1($1,$2,,,$5,$6,$7,
LDFLAGS=${ac_save_ldflags}
ifelse($8,[],[
    AC_MSG_ERROR([*** Unable to locate $3 library.])
],$8),LOC_SHIFT8($*))
LDFLAGS=${ac_save_ldflags}
])

AC_DEFUN(LOC_CHECK_VERSION,[
AC_MSG_CHECKING($3 version)
ac_save_cppflags="$CPPFLAGS"
CPPFLAGS="$5 $CPPFLAGS"
AC_TRY_RUN([
#include <$1>
int main(void) {
 FILE *fp = fopen("conftestdata","w");
 fputs($2, fp);
 return 0;
}
],
[   $4=`cat conftestdata`
    AC_MSG_RESULT($$4)],
[   AC_MSG_ERROR([*** Could not determine $3 version.]) ],
[   $4=$5
    AC_MSG_RESULT([unknown (cross-compiling)]) ])
CPPFLAGS=$ac_save_cppflags
])

dnl autoconf undefines "eval", so use "builtin([eval], ...)"

AC_DEFUN(LOC_PAD,[$1[]ifelse(builtin([eval],len($1) > 23),1,[
                          ],substr([                        ],len($1)))])

AC_DEFUN(LOC_ARG_WITH,[
AC_ARG_WITH($1,
LOC_PAD([  --with-$1])[support $2 functionality (default: ]ifelse([$3],,yes,[$3])[)],,
[with_$1=]ifelse([$3],,yes,[$3]))
])

AC_DEFUN(LOC_ARG_WITH_INC,[
AC_ARG_WITH($1-includes,
LOC_PAD([  --with-$1-includes=DIRS])[$2 include files are in DIRS])
])

AC_DEFUN(LOC_ARG_WITH_LIB,[
AC_ARG_WITH($1-libs,
LOC_PAD([  --with-$1-libs=DIRS])[$2 library files are in DIRS])
])

AC_DEFUN(LOC_OPTIONAL,[
AC_MSG_CHECKING(whether to build $1)
if test -n "$USE_$2" ; then
	AC_MSG_RESULT(yes)
	BUILD_$3="$4"
else
	AC_MSG_RESULT(no)
	BUILD_$3=
fi
AC_SUBST(BUILD_$3)
])

dnl checks for complete floating-point support (infinity, NaN)

define(LOC_FP_TEST,[
#include <float.h>
int main(void) {
 double one = 1.0;
 double zero = 0.0;
 if (one/zero > DBL_MAX)        /* infinity */
   if (zero/zero != zero/zero)  /* NaN */
     return 0;
 return 1;
}
])

AC_DEFUN(LOC_CHECK_FP_INF_NAN,[
AC_MSG_CHECKING([for full floating-point support]$1)
AC_TRY_RUN(LOC_FP_TEST,
[   AC_MSG_RESULT(yes)
    $2],
[   AC_MSG_RESULT(no)
    $3],
[   AC_MSG_RESULT([unknown (cross-compiling)])
    $4]
)
])

dnl check whether the compiler supports the -mieee switch

AC_DEFUN(LOC_CHECK_CC_MIEEE,[
AC_MSG_CHECKING(whether "cc -mieee" works)
ac_save_cflags=${CFLAGS}
CFLAGS="$CFLAGS -mieee"
AC_TRY_COMPILE(,,
    [   AC_MSG_RESULT(yes)
        IEEEFLAG="-mieee"],
    [   AC_MSG_RESULT(no)])
CFLAGS=${ac_save_cflags}
])

AC_DEFUN(LOC_MSG,[
echo "$1"
])

AC_DEFUN(LOC_PAD_26,[substr([                           ],len($1))])

AC_DEFUN(LOC_YES_NO,[if test -n "${$1}" ; then echo yes ; else echo no ; fi])

AC_DEFUN(LOC_MSG_USE,[
[echo "  $1:]LOC_PAD_26($1)`LOC_YES_NO($2)`"])
