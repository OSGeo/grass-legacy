# settings for autocompile.sh script
# $Id$
# Name of tarball file for sources
TARBALL="grass5src_cvs_snapshot_relbranchJan_11_2002"
# TARBALL="grass5src_CVS"
# Extension of source tarball
TARBALLEXT=".tar.gz"
# Host where to download the source tarball from
SRCHOST="http://grass.itc.it/"
# SRCHOST="http://192.168.1.1/"
# directory on this host
SRCDIR="grass5/source/snapshot/"
# where to put final binary file
# DSTHOST="hgeo2.uni-hannover.de"
DSTHOST="192.168.1.1"
# path on this host
DSTDIR="/pub/incoming/"
# root of build directories
BUILDROOT="/tmp"
# directory to build all
BUILDDIR="${BUILDROOT}/build/src/"
# staging directory to install under
TARGETDIR="${BUILDROOT}/build/grass5/"
# name of subdirectory created with tar xzf tarball.tar.gz
SUBDIR="grass/"
# mail error messages to maintainer
MAILTO="put your mail address here"
# options to configure
CONFOPTS="--with-postgres-includes=/usr/include/pgsql --with-dbm-includes=/usr/include/gdbm"
# where to store output of make commands (/dev/null)
MAKEOUTPUT="/dev/null"
# where wget is found
WGET="/usr/bin/wget"
# where tar is found (needs GNU tar for xzf !)
TAR="/bin/tar"
# where ncftpput is found
NCFTPPUT="/usr/bin/ncftpput"
# where mail is found
MAIL="/bin/mail"
# end.