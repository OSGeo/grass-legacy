#!/bin/sh
# export PATH=$PATH:/opt/sfw/bin:/usr/xpg4/bin
./configure --prefix=/opt/GNUgrass \
-with-libs=/opt/sfw/lib --with-libs=/usr/local/lib \
--with-includes=/opt/sfw/include --with-includes=/usr/local/include \
--with-postgres=no --with-odbc=no \
--with-blas=no --with-motif=yes

