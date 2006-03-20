# GRASS 6.1 RPM spec file for ITC
# This file is Free Software under GNU GPL v>=2.
# Derived from grass_FC4.spec

%define PACKAGE_NAME grass
%define PACKAGE_VERSION 6.1.cvs
%define PACKAGE_URL http://grass.itc.it/index.php
%define _prefix /usr
%define _bindir /usr/bin
%define shortver 61

%define with_blas        %{?_with_blas:1}%{!?_with_blas:0}
%define with_ffmpeg      %{?_with_ffmpeg:1}%{!?_with_ffmpeg:0}
%define with_odbc        %{?_with_odbc:1}%{!?_with_odbc:0}
%define with_mysql       %{?_with_mysql:1}%{!?_with_mysql:0}
%define with_postgres    %{?_without_postgres:0}%{!?_without_postgres:1}
%define with_largefiles  %{?_without_largefiles:0}%{!?_without_largefiles:1}
%define with_motif       %{?_without_motif:0}%{!?_without_motif:1}
%define with_fftw        %{?_without_fftw:0}%{!?_without_fftw:1}
%define with_freetype    %{?_without_freetype:0}%{!?_without_freetype:1}
%define with_glw         %{?_without_glw:0}%{!?_without_glw:1}
%define with_sqlite      %{?_without_sqlite:0}%{!?_without_sqlite:1}
%define cvssnapshot	 %{?cvs:_src_snapshot_%cvs}%{!?cvs:_src_snapshot_2006_02_25}

# Turn off automatic generation of dependencies to
# avoid a problem with libgrass* dependency issues.
# Other dependencies listed below.
%define _use_internal_dependency_generator 0
# Filter out the library number on provides
%define __find_provides %{_tmppath}/find_provides.sh
# Disable the _find_requires macro.
%define __find_requires %{nil}



#Query the RPM database to find which redhat/fedora release we are running.
%if %(rpmquery fedora-release | grep -cv 'not installed$')
    %define FCL 1
    %define VER1 %(rpmquery --qf '%{VERSION}' fedora-release)
%endif
%if %(rpmquery redhat-release | grep -v 'not installed$' | grep -c -e '-[0-9][AEW]S')
    %define ENT 1
    %define VER1 %(rpmquery --qf '%{VERSION}' redhat-release|cut -c1)	
%endif

Summary:	GRASS - Geographic Resources Analysis Support System
Name:		%PACKAGE_NAME
Version:	%PACKAGE_VERSION
Epoch: 0
%{?FCL:Release: 0.fdr.%{REL}.fc%{VER1}}
%{?ENT:Release: 0.E%{VER1}}
Source:	        ftp://grass.itc.it/pub/grass/grass%{shortver}/source/snapshot/grass-%{version}%{cvssnapshot}.tar.gz
License:	GPL, Copyright by the GRASS Development Team
Group:		Sciences/Geosciences
Packager:       Markus Neteler <neteler@itc.it>
URL:            %PACKAGE_URL
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)/%{name}-%{version}
Prefix:         %{_prefix}

Requires:       gdal >= 1.3
Requires:	tcl >= 8.3
Requires:	tk >= 8.3
Requires:	proj >= 4.4.9
Requires:       geos >= 2.1.1
%if "%{with_freetype}" == "1"
Requires:       freetype >= 2.0.0
BuildRequires:	freetype-devel >= 2.0.0
%endif
Requires:       bash >= 3.0
%if "%{with_glw}" == "1"
Requires:       xorg-x11-Mesa-libGL >= 6.8
BuildRequires:  xorg-x11-Mesa-libGL >= 6.8
%endif
Requires:       xorg-x11-libs >= 6.8
BuildRequires:  xorg-x11-libs >= 6.8
%if "%{with_motif}" == "1"
Requires:       openmotif >= 2.2.3
Requires:       xorg-x11-deprecated-libs
BuildRequires:  xorg-x11-deprecated-libs-devel
BuildRequires:  openmotif-devel >= 2.2.3
%endif
%if "%{with_fftw}" == "1"
Requires:       fftw
BuildRequires:  fftw-devel
%endif
Requires:       glibc >= 2.0
Requires:       libgcc >= 3.4.2
Requires:       ncurses >= 5.4
BuildRequires:	ncurses-devel >= 5.2
Requires:       libpng >= 1.2.8
BuildRequires:	libpng-devel >= 1.2.2
Requires:       libstdc++ >= 3.4
Requires:       libtiff >= 3.6
Requires:       zlib >= 1.2
BuildRequires:	zlib-devel >= 1.2
Requires:	libjpeg
BuildRequires:	libjpeg-devel
%if "%{with_blas}" == "1"
Requires:       blas >= 3.0
Requires:       lapack >= 3.0
%endif
%if "%{with_ffmpeg}" == "1"
Requires:       ffmpeg
%endif
%if "%{with_odbc}" == "1"
Requires:	unixODBC
BuildRequires:	unixODBC-devel
%endif
%if "%{with_mysql}" == "1"
Requires:	mysql
%endif
%if "%{with_postgres}" == "1"
Requires:	postgresql-libs >= 7.3
BuildRequires:	postgresql-devel
%endif
BuildRequires:	bison
BuildRequires:	flex
BuildRequires:	man
%if "%{with_mysql}" == "1"
BuildRequires:	mysql-devel
%endif

Vendor: GRASS
#
# clean up of provides for other packages: gdal-grass, qgis etc.
#

%description
GRASS (Geographic Resources Analysis Support System) is a Geographic
Information System (GIS) used for geospatial data management and
analysis, image processing, graphics/maps production, spatial
modeling, and visualization. GRASS is currently used in academic and
commercial settings around the world, as well as by many governmental
agencies and environmental consulting companies.
Available rpmbuild options :

--without postgres fftw motif largefiles glw freetype sqlite

to disable default features
 
--with blas ffmpeg odbc mysql 

to enable additional features.

%prep
#%setup -q   ## run quietly with minimal output.
%setup  -n %{name}-%{version}%{cvssnapshot}  ## name the directory
#
# Filter out library number
#
cat > %{_tmppath}/find_provides.sh <<EOF
#!/bin/sh
/usr/lib/rpm/redhat/find-provides | sed -e 's/%{version}\.//g' | sort -u
exit 0
EOF
chmod ugo+x %{_tmppath}/find_provides.sh

%build
#
#configure with shared libs:
#
CFLAGS="-O2 -g -Wall -Werror-implicit-function-declaration -fno-common"
CXXFLAGS="-g -Wall"
#LDFLAGS="-s"

( %configure  \
   --prefix=%{buildroot}/%{_prefix} \
   --bindir=%{buildroot}/%{_bindir} \
   --enable-shared \
%ifarch x86_64
   --enable-64bit \
%endif
%if "%{with_largefiles}" == "1"
   --enable-largefile \
%endif
   --with-fftw \
   --with-includes=/usr/include \
%ifarch x86_64
   --with-libs=/usr/lib64 \
%else
   --with-libs=/usr/lib \
%endif
%if "%{with_motif}" == "1"
   --with-motif \
   --with-motif-includes=/usr/X11R6/include/Xm \
%else
   --without-motif  \
%endif
%if "%{with_freetype}" == "1"
   --with-freetype=yes \
   --with-freetype-includes=/usr/include/freetype2 \
%else
   --without-freetype  \
%endif
   --with-nls \
   --with-gdal=/usr/bin/gdal-config \
   --with-proj \
   --with-proj-includes=/usr/include \
%ifarch x86_64
   --with-proj-libs=/usr/lib64 \
%else
   --with-proj-libs=/usr/lib \
%endif
%if "%{with_glw}" == "1"
   --with-glw \
%else
   --without-glw  \
%endif
%if "%{with_sqlite}" == "1"
   --with-sqlite \
%else
   --without-sqlite  \
%endif
%if "%{with_mysql}" == "1"
   --with-mysql \
   --with-mysql-includes=/usr/include/mysql \
%ifarch x86_64
   --with-mysql-libs=/usr/lib64/mysql \
%else
   --with-mysql-libs=/usr/lib/mysql \
%endif
%else
   --without-mysql \
%endif
%if "%{with_odbc}" == "1"
   --with-odbc  \
%ifarch x86_64
   --with-odbc-libs=/usr/lib64 \
%else
   --with-odbc-libs=/usr/lib \
%endif
   --with-odbc-includes=/usr/include \
%else
   --without-odbc \
%endif
%if "%{with_postgres}" == "1"
   --with-postgres  \
   --with-postgres-includes=/usr/include/pgsql \
%ifarch x86_64
   --with-postgres-libs=/usr/lib64  \
%else
   --with-postgres-libs=/usr/lib  \
%endif
%else
   --without-postgres  \
%endif
%if "%{with_blas}" == "1"
   --with-blas  \
   --with-lapack  \
%endif
%if "%{with_ffmpeg}" == "1"
   --with-ffmpeg \
%endif
   --with-cxx
)

#configure with shared libs:

make prefix=%{buildroot}%{_prefix} BINDIR=%{buildroot}%{_bindir}  \
     PREFIX=%{buildroot}%{_prefix}

%install

rm -rf %{buildroot}

make prefix=%{buildroot}%{_prefix} BINDIR=%{buildroot}%{_bindir} \
   PREFIX=%{buildroot}%{_prefix} install

# changing GISBASE in startup script (deleting %{buildroot} from path)
mv %{buildroot}%{_bindir}/grass%{shortver} %{buildroot}%{_bindir}/grass%{shortver}.tmp

cat  %{buildroot}%{_bindir}/grass%{shortver}.tmp | \
    sed -e "1,\$s&^GISBASE.*&GISBASE=%{_prefix}/grass-%{version}&" | \
    cat - > %{buildroot}%{_bindir}/grass%{shortver}

rm %{buildroot}%{_bindir}/grass%{shortver}.tmp
chmod +x %{buildroot}%{_bindir}/grass%{shortver}

# Make grass libraries available on the system
install -d %{buildroot}/etc/ld.so.conf.d
echo %{_prefix}/grass-%{version}/lib >> %{buildroot}/etc/ld.so.conf.d/grass-%{version}.conf

%clean
rm -rf %{buildroot}

#cd ..
#rm -rf grass-%{version}

%files
%defattr(-,root,root)

%doc AUTHORS COPYING GPL.TXT README REQUIREMENTS.html

%attr(0755,root,root)

%{_bindir}/grass%{shortver}
%{_prefix}/grass-%{version}
/etc/ld.so.conf.d/grass-%{version}.conf

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%Changelog
* Tue Mar 17 2006 Roberto Flor <flor@itc.it>
  - Added with/without option
  - Added cvs snapshot option
  - Fixed a lot of inconsistency on x86_64 lib
* Tue Feb 28 2006 Roberto Flor <flor@itc.it>
  - Small changes and cleanup. Requires FC4 or RH Enterprise 4.
  - Dirty fix for provides error
* Thu Oct 12 2005 Markus Neteler <neteler@itc.it>
  - First build of RPM for Fedora Core 4.
* Thu Mar 30 2005 Craig Aumann <caumann@ualberta.ca>
  - First build of RPM for Fedora Core 3.
* Wed Sep 01 2004 Bernhard Reiter <bernhard@intevation.de>
  - made ready to be checked into GRASS CVS: added header, disabled Patch1
* Tue Aug 10 2004 Silke Reimer <silke.reimer@intevation.net>
  - small changes to fit to Fedora naming conventions
* Thu Jul 01 2004 Silke Reimer <silke.reimer@intevation.net>
  - Initial build
