# Modifications: From: Markus Neteler (based on 5.3 spec file)

%define PACKAGE_NAME grass
%define PACKAGE_VERSION 5.7.0
%define PACKAGE_URL http://grass.itc.it/index.html
%define _prefix /opt
%define shortver 57

Summary:	GRASS - Geographic Resources Analysis Support System
Name:		%PACKAGE_NAME
Version:	%PACKAGE_VERSION
Release:	1
Source:		grass-5.7.0.tar.gz
Copyright:	GPL; Copyright by the GRASS Development Team
Group:		Sciences/Geosciences
Requires:	gdal >= 1.1.9
Requires:	tcl >= 8
Requires:	tk >= 8
Requires:	postgresql >= 7.3
Requires:	proj >= 4.4.7
Requires:	lesstif
BuildRequires:	bison
BuildRequires:	fftw2-devel
BuildRequires:	flex
BuildRequires:	freetype-devel >= 2.0.0
BuildRequires:	gcc-g77
BuildRequires:	libjpeg-devel
BuildRequires:	libpng-devel >= 1.2.2
BuildRequires:	man
BuildRequires:	lesstif-devel
BuildRequires:	ncurses-devel >= 5.2
BuildRequires:	postgresql-devel
BuildRequires:	unixODBC-devel
BuildRequires:	zlib-devel
Vendor: GRASS Development Team <http://grass.itc.it>

BuildRoot: %{_builddir}/%{name}-root
Prefix: %{_prefix}

%description
GRASS (Geographic Resources Analysis Support System)
is a complete GIS with integrated database management
system, image processing system, graphics production 
system, and spatial modeling system. A graphical user
interface for X-Window is provided.

%prep
%setup -n grass-%{version}
#cd src/CMD/generic/
#%patch0 -p0 
#cd ../../..

GRASS53="/usr/src/redhat/BUILD/grass-5.3.0"

#configure with shared libs:
CFLAGS="-O2" LDFLAGS="-s" ./configure \
	--prefix=%%{buildroot}/{_prefix} --bindir=%{buildroot}/%{_bindir} \
	--with-grass50=$GRASS53 \
	--enable-shared \
	--with-cxx \
	--with-gdal=/usr/local/bin/gdal-config \
	--with-postgres-includes=/usr/include/pgsql --with-postgres-libs=/usr/lib \
	--with-fftw \
	--with-freetype --with-freetype-includes=/usr/include/freetype2

%build
make mix
make prefix=%{buildroot}/%{_prefix} BINDIR=%{buildroot}/%{_bindir} \
PREFIX=%{buildroot}%{_prefix}

%install
make prefix=%{buildroot}/%{_prefix} BINDIR=%{buildroot}/%{_bindir} \
PREFIX=%{buildroot}%{_prefix} install

# changing GISBASE (deleting %{buildroot} from path)
mv %{buildroot}%{_prefix}/bin/grass%{shortver} %{buildroot}%{_prefix}/bin/grass%{shortver}.tmp
cat  %{buildroot}%{_prefix}/bin/grass%{shortver}.tmp |
	sed -e "1,\$s&^GISBASE.*&GISBASE=%{_prefix}/grass%{shortver}&" |
    cat - > %{buildroot}%{_prefix}/bin/grass%{shortver}
rm %{buildroot}%{_prefix}/bin/grass%{shortver}.tmp
chmod +x %{buildroot}%{_prefix}/bin/grass%{shortver}

install -d %{buildroot}/usr/bin
mv %{buildroot}%{_prefix}/bin/* %{buildroot}/usr/bin

#Get rid of lock dir for hostname:
#??rmdir %{buildroot}%{_prefix}/grass%{shortver}/locks/`hostname`

%clean
rm -rf %{buildroot}
cd ..
rm -rf grass-%{version}

%files
%defattr(-,root,root)
%doc COPYING GPL.TXT README REQUIREMENTS.html

%attr(0755,root,root) /usr/bin/grass%{shortver}
# %attr(1777,root,root) %{_prefix}/grass%{shortver}/locks
/usr/bin/grass%{shortver}
%{_prefix}/grass%{shortver}/

%changelog
* Tue May 24 2004 Markus Neteler <neteler itc it> 5.7.0-1
- rewritten from 5.3 specs

