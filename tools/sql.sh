#!/bin/bash

tmpdir=/tmp/sql-grass
#builddir=/usr/src/grass
builddir=$HOME/grass/build
dbname=grass

rm -rf "$tmpdir"
mkdir -m 711 "$tmpdir" || exit 1

cd $builddir

( cd dist.*

find . -type f -perm +111 \! -name '*.so.*' \
	| while read file ; do ldd $file | sed 's!^!'$file'!' ; done 2>/dev/null \
	| sed -e 's/^\.\///' -e 's/ => \(.*\) (0x.*)$/	\1/' -e 's/ => .*$//' \
	| fgrep -v 'not a dynamic executable' \
	> "$tmpdir/ldd.lst"

find . -type f -perm +111 \! -name '*.so.*' \
	| xargs nm -AD 2>/dev/null \
	| egrep ': {8} U ' \
	| sed -e 's/:/ /g' -e 's/\.\///' \
	| awk -vOFS='\t' '{print $1,$3}' \
	> "$tmpdir/programs.lst"

)

find src* -name 'lib?*.a' \
	| xargs nm -A \
	| egrep ':[0-9a-f]{8} [BCDGRSTW] ' \
	| sed 's/:/ /g' \
	| awk -vOFS='\t' '{print gensub("^[^ ]*/","",1,$1),$2,$5}' \
	> "$tmpdir/exports.lst"

find src* -name 'lib?*.so' \
	| xargs nm -AD \
	| egrep ':[0-9a-f]{8} [BCDGRSTW] ' \
	| sed 's/:/ /g' \
	| awk -vOFS='\t' '{print gensub("^[^ ]*/","",1,$1),$4}' \
	> "$tmpdir/exports2.lst"

find src* -name 'lib?*.a' \
	| xargs nm -A \
	| egrep ': {8} U ' \
	| sed 's/:/ /g' \
	| awk -vOFS='\t' '{print gensub("^[^ ]*/","",1,$1),$2,$4}' \
	> "$tmpdir/imports.lst"

find src* -name 'lib?*.so' \
	| xargs nm -AD \
	| egrep ': {8} U ' \
	| sed 's/:/ /g' \
	| awk -vOFS='\t' '{print gensub("^[^ ]*/","",1,$1),$3}' \
	> "$tmpdir/imports2.lst"

libs=`awk '{print $3}' "$tmpdir/ldd.lst" | uniq | sort | uniq`

nm -AD $libs \
	| egrep ':[0-9a-f]{8} [TWDRC] ' \
	| sed 's/:/ /g' \
	| awk -vOFS='\t' '{print gensub("^[^ ]*/","",1,$1),$4}' \
	> "$tmpdir/libs.lst"

cat > "$tmpdir/ansi.lst" <<EOF
abort
asctime
atexit
atof
atoi
atol
bsearch
calloc
clearerr
ctime
exit
fclose
feof
ferror
fflush
fgetc
fgetpos
fgets
fopen
fprintf
fputc
fputs
fread
free
freopen
fscanf
fseek
fsetpos
ftell
fwrite
getc
getchar
getenv
gets
isalnum
isalpha
isdigit
islower
isspace
ldiv
localtime
longjmp
malloc
memcpy
memmove
memset
perror
printf
putc
putchar
puts
qsort
rand
realloc
remove
rename
rewind
scanf
setbuf
setjmp
signal
sprintf
srand
sscanf
stderr
stdin
stdout
strcat
strchr
strcmp
strcpy
strcspn
strerror
strftime
strlen
strncat
strncmp
strncpy
strpbrk
strrchr
strspn
strstr
strtod
strtok
strtol
time
tmpfile
tmpnam
tolower
toupper
ungetc
vfprintf
vsprintf
EOF

destroydb "$dbname"
createdb "$dbname"

psql -n -q -d "$dbname" << EOF

-- ----------------------------------------------------------------------

CREATE TABLE exports_0 (
	library VARCHAR(40) NOT NULL,
	object VARCHAR(40) NOT NULL,
	symbol VARCHAR(80) NOT NULL
	) ;

COPY exports_0 FROM '$tmpdir/exports.lst' ;

CREATE TABLE exports_1 (
	library VARCHAR(40) NOT NULL,
	symbol VARCHAR(80) NOT NULL
	) ;

COPY exports_1 FROM '$tmpdir/exports2.lst' ;

CREATE TABLE imports_0 (
	library VARCHAR(40) NOT NULL,
	object VARCHAR(40) NOT NULL,
	symbol VARCHAR(80) NOT NULL
	) ;

COPY imports_0 FROM '$tmpdir/imports.lst' ;

CREATE TABLE imports_1 (
	library VARCHAR(40) NOT NULL,
	symbol VARCHAR(80) NOT NULL
	) ;

COPY imports_1 FROM '$tmpdir/imports2.lst' ;

CREATE TABLE programs (
	program VARCHAR(80) NOT NULL,
	symbol VARCHAR(80) NOT NULL
	) ;

COPY programs FROM '$tmpdir/programs.lst' ;

CREATE TABLE libs (
	library VARCHAR(20) NOT NULL,
	symbol VARCHAR(60) NOT NULL
	) ;

COPY libs FROM '$tmpdir/libs.lst' ;

CREATE TABLE ldd (
	program VARCHAR(80) NOT NULL,
	library VARCHAR(40) NOT NULL,
	path VARCHAR(80)
	) ;

COPY ldd FROM '$tmpdir/ldd.lst' ;

CREATE TABLE ansi (
	symbol VARCHAR(40) NOT NULL
	) ;

COPY ansi FROM '$tmpdir/ansi.lst' ;

-- ----------------------------------------------------------------------

SELECT DISTINCT library, symbol
	INTO TABLE exports
	FROM exports_0
UNION
SELECT DISTINCT library, symbol
	FROM exports_1 ;

CREATE TABLE duplicates AS
	SELECT DISTINCT symbol
	FROM exports
	GROUP BY symbol
	HAVING COUNT(*) > 1 ;

CREATE TABLE duplicates2 AS
	SELECT *
	FROM exports
	WHERE symbol IN (
		SELECT symbol
		FROM duplicates
	) ;

SELECT DISTINCT library, symbol
	INTO TABLE imports_2
	FROM imports_0
UNION
SELECT DISTINCT library, symbol
	FROM imports_1 ;

CREATE TABLE imports AS
	SELECT a.library, a.symbol
	FROM imports_2 a
	WHERE NOT EXISTS (
		SELECT b.library, b.symbol
		FROM exports b
		WHERE b.symbol = a.symbol
		AND b.library = a.library
	) ;

CREATE TABLE defined AS
	SELECT DISTINCT symbol
	FROM exports ;

CREATE TABLE used AS
	SELECT DISTINCT symbol
	FROM imports ;

CREATE TABLE undefined AS
	SELECT symbol
	FROM used u
	WHERE NOT EXISTS (
		SELECT *
		FROM defined d
		WHERE d.symbol = u.symbol
	) ;

SELECT symbol
INTO TABLE undefined_1
	FROM undefined
EXCEPT
SELECT b.symbol
	FROM undefined a, libs b
	WHERE a.symbol = b.symbol ;

CREATE TABLE undefined_2 AS
	SELECT i.symbol, i.object, i.library
	FROM imports_0 i, undefined_1 u
	WHERE i.symbol = u.symbol ;

CREATE TABLE depends AS
	SELECT	i.library AS im_lib,
		i.symbol AS symbol,
		e.library AS ex_lib
	FROM imports i, exports e
	WHERE i.symbol = e.symbol ;

CREATE TABLE lib_deps AS
	SELECT DISTINCT im_lib, ex_lib
	FROM depends
	WHERE im_lib <> ex_lib ;

CREATE TABLE lib_deps_1 AS
	SELECT	a.im_lib,
		a.ex_lib AS in_lib,
		b.ex_lib
	FROM lib_deps a, lib_deps b
	WHERE a.ex_lib = b.im_lib ;

CREATE TABLE lib_deps_2 AS
	SELECT	a.im_lib,
		a.in_lib AS in1_lib,
		a.ex_lib AS in2_lib,
		b.ex_lib
	FROM lib_deps_1 a, lib_deps b
	WHERE a.ex_lib = b.im_lib
	AND a.im_lib <> a.ex_lib ;

SELECT im_lib, ex_lib
INTO TABLE lib_deps_trans
	FROM lib_deps
UNION
SELECT im_lib, ex_lib
	FROM lib_deps_1
UNION
SELECT im_lib, ex_lib
	FROM lib_deps_2 ;

CREATE TABLE prog_libs AS
SELECT DISTINCT a.program, b.library
FROM programs a, exports b
WHERE a.symbol = b.symbol ;

SELECT DISTINCT a.symbol
INTO TABLE libc
	FROM programs a, libs b
	WHERE a.symbol = b.symbol
	AND b.library = 'libc.so.6' 
UNION
	SELECT DISTINCT a.symbol
	FROM imports a, libs b
	WHERE a.symbol = b.symbol
	AND b.library = 'libc.so.6' ;

SELECT symbol
INTO nonansi
	FROM libc
	WHERE symbol !~ '_.*'
EXCEPT
SELECT symbol
	FROM ansi ;

CREATE TABLE nonansi_progs AS
	SELECT a.symbol, COUNT(*)
	FROM programs a, nonansi b
	WHERE a.symbol = b.symbol
	AND a.program NOT LIKE 'bin/%'
	GROUP BY a.symbol ;

CREATE TABLE nonansi_libs AS
	SELECT a.symbol, COUNT(*)
	FROM imports a, nonansi b
	WHERE a.symbol = b.symbol
	GROUP BY a.symbol ;

SELECT symbol
INTO TABLE nonansi_counts
	FROM nonansi_progs
UNION
SELECT symbol
	FROM nonansi_libs ;

ALTER TABLE nonansi_counts
	ADD COLUMN progs INTEGER ;

ALTER TABLE nonansi_counts
	ADD COLUMN libs INTEGER ;

UPDATE nonansi_counts
	SET progs = 0, libs = 0 ;

UPDATE nonansi_counts
	SET progs = b.count
	FROM nonansi_progs b
	WHERE nonansi_counts.symbol = b.symbol ;

UPDATE nonansi_counts
	SET libs = c.count
	FROM nonansi_libs c
	WHERE nonansi_counts.symbol = c.symbol;

-- SELECT a.symbol, a.program
-- 	FROM programs a, nonansi_progs b
-- 	WHERE a.symbol = b.symbol
-- 	AND a.program NOT LIKE 'bin/%'
-- 	ORDER BY b.count DESC, b.symbol ;

-- SELECT symbol, library
-- 	FROM duplicates2
-- 	ORDER BY symbol ;

-- SELECT a.im_lib, a.ex_lib
-- 	FROM lib_deps a, lib_deps b
-- 	WHERE	a.ex_lib = b.im_lib
-- 	AND	b.ex_lib = a.im_lib ;

-- SELECT * FROM lib_deps_2
-- 	WHERE im_lib = ex_lib ;

-- SELECT * FROM lib_deps_1
-- 	WHERE im_lib = ex_lib ;

-- SELECT im_lib FROM lib_deps_trans
-- 	WHERE im_lib = ex_lib ;

-- SELECT a.program, a.library
-- FROM ldd a
-- WHERE a.library LIKE 'libgrass_%.so'
-- AND a.library NOT IN (
-- SELECT b.library
-- FROM prog_libs b
-- WHERE b.program = a.program
-- ) ;

-- ----------------------------------------------------------------------

EOF

