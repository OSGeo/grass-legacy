;----------------------------------------------------------------------------------------------------------------------------

;GRASS Installer for Windows
;Written by Marco Pasetti
;Last Update: 17 May 2008
;Mail to: marco.pasetti@alice.it 

;----------------------------------------------------------------------------------------------------------------------------

;Script and frequently changed version variables

!define DEMOLOCATION_PATH "c:\msys\local\src\grass-6.3.0\demolocation"

;Select if you are building a "Development Version" or a "Release Version" installer of GRASS
;Change INSTALLER_TYPE variable to Release, Dev6 or Dev7

!define INSTALLER_TYPE "Release"

;----------------------------------------------------------------------------------------------------------------------------

;Version variables that may need to be modified

!define RELEASE_VERSION_NUMBER "6.3.0"
!define RELEASE_SVN_REVISION "31095"
!define RELEASE_BINARY_REVISION "3"
!define RELEASE_GRASS_COMMAND "grass63"
!define RELEASE_GRASS_BASE "GRASS"

!define DEV6_VERSION_NUMBER "6-SVN"
!define DEV6_SVN_REVISION ""
!define DEV6_BINARY_REVISION "1"
!define DEV6_GRASS_COMMAND "grass64"
!define DEV6_GRASS_BASE "GRASS-6-SVN"

!define DEV7_VERSION_NUMBER "7-SVN"
!define DEV7_SVN_REVISION ""
!define DEV7_BINARY_REVISION "1"
!define DEV7_GRASS_COMMAND "grass7"
!define DEV7_GRASS_BASE"GRASS-7-SVN"

;----------------------------------------------------------------------------------------------------------------------------

;Don't modify the following lines

;----------------------------------------------------------------------------------------------------------------------------

;NSIS Includes

!include "MUI2.nsh"
!include "LogicLib.nsh"

;----------------------------------------------------------------------------------------------------------------------------

!if ${INSTALLER_TYPE} == "Release"
	!define VERSION_NUMBER "${RELEASE_VERSION_NUMBER}"
	!define SVN_REVISION "${RELEASE_SVN_REVISION}"
	!define BINARY_REVISION "${RELEASE_BINARY_REVISION}"
	!define GRASS_COMMAND "${RELEASE_GRASS_COMMAND}"
	!define GRASS_BASE "${RELEASE_GRASS_BASE}"
	!define INSTALLER_NAME "WinGRASS-${VERSION_NUMBER}-${BINARY_REVISION}-Setup.exe"
	!define DISPLAYED_NAME "GRASS ${VERSION_NUMBER}-${BINARY_REVISION}"
	!define CHECK_INSTALL_NAME "GRASS"
	!define PACKAGE_FOLDER ".\GRASS-Release-Package"
!else if ${INSTALLER_TYPE} == "Dev6"
	!define VERSION_NUMBER "${DEV6_VERSION_NUMBER}"
	!define SVN_REVISION "${DEV6_SVN_REVISION}"
	!define BINARY_REVISION "${DEV6_BINARY_REVISION}"
	!define GRASS_COMMAND "${DEV6_GRASS_COMMAND}"
	!define GRASS_BASE "${DEV6_GRASS_BASE}"
	!define INSTALLER_NAME "WinGRASS-${VERSION_NUMBER}-r${SVN_REVISION}-${BINARY_REVISION}-Setup.exe"
	!define DISPLAYED_NAME "GRASS ${VERSION_NUMBER}-r${SVN_REVISION}-${BINARY_REVISION}"
	!define CHECK_INSTALL_NAME "GRASS 6 SVN"
	!define PACKAGE_FOLDER ".\GRASS-6-Dev-Package"
!else if ${INSTALLER_TYPE} == "Dev7"
	!define VERSION_NUMBER "${DV7_VERSION_NUMBER}"
	!define SVN_REVISION "${DV7_SVN_REVISION}"
	!define BINARY_REVISION "${DV7_BINARY_REVISION}"
	!define GRASS_COMMAND "${DEV7_GRASS_COMMAND}"
	!define GRASS_BASE "${DEV7_GRASS_BASE}"
	!define INSTALLER_NAME "WinGRASS-${VERSION_NUMBER}-r${SVN_REVISION}-${BINARY_REVISION}-Setup.exe"
	!define DISPLAYED_NAME "GRASS ${VERSION_NUMBER}-r${SVN_REVISION}-${BINARY_REVISION}"
	!define CHECK_INSTALL_NAME "GRASS 7 SVN"
	!define PACKAGE_FOLDER ".\GRASS-7-Dev-Package"
!endif

;----------------------------------------------------------------------------------------------------------------------------

Function .onInit

	Var /GLOBAL INSTALLED_VERSION_NUMBER
	Var /GLOBAL INSTALLED_SVN_REVISION
	Var /GLOBAL INSTALLED_BINARY_REVISION
	
	Var /GLOBAL INSTALLED_VERSION
	
	Var /GLOBAL MESSAGE_0_
	Var /GLOBAL MESSAGE_1_
	Var /GLOBAL MESSAGE_2_	
	
	ReadRegStr $INSTALLED_VERSION_NUMBER HKLM "Software\${GRASS_BASE}" "VersionNumber"
	ReadRegStr $INSTALLED_SVN_REVISION HKLM "Software\${GRASS_BASE}" "SvnRevision"
	
	${If} $INSTALLED_SVN_REVISION == ""
		ReadRegStr $INSTALLED_SVN_REVISION HKLM "Software\${GRASS_BASE}" "Revision"
	${EndIf}	
	
	ReadRegStr $INSTALLED_BINARY_REVISION HKLM "Software\${GRASS_BASE}" "BinaryRevision"
	
	StrCpy $MESSAGE_0_ "${CHECK_INSTALL_NAME} is already installed on your system.$\r$\n"
	StrCpy $MESSAGE_0_ "$MESSAGE_0_$\r$\n"
	
	!if ${INSTALLER_TYPE} == "Release"
		StrCpy $MESSAGE_0_ "$MESSAGE_0_The installed version is $INSTALLED_VERSION_NUMBER"
		${If} $INSTALLED_BINARY_REVISION == ""
		${Else}
			StrCpy $MESSAGE_0_ "$MESSAGE_0_-$INSTALLED_BINARY_REVISION"
		${EndIf}
		StrCpy $MESSAGE_0_ "$MESSAGE_0_$\r$\n"	
	!else
		StrCpy $MESSAGE_0_ "$MESSAGE_0_The installed version is $INSTALLED_VERSION_NUMBER"
		StrCpy $MESSAGE_0_ "$MESSAGE_0_-$INSTALLED_SVN_REVISION-$INSTALLED_BINARY_REVISION$\r$\n"
	!endif
	
	StrCpy $MESSAGE_1_ "$MESSAGE_0_$\r$\n"
	StrCpy $MESSAGE_1_ "$MESSAGE_1_Please uninstall it before to install the new version."
	
	StrCpy $MESSAGE_2_ "$MESSAGE_0_$\r$\n"
	StrCpy $MESSAGE_2_ "$MESSAGE_2_This is the latest version available."
	
	IntOp $INSTALLED_SVN_REVISION $INSTALLED_SVN_REVISION * 1
	IntOp $INSTALLED_BINARY_REVISION $INSTALLED_BINARY_REVISION * 1
	IntOp $INSTALLED_VERSION $INSTALLED_SVN_REVISION + $INSTALLED_BINARY_REVISION
	
	!define /math VERSION ${SVN_REVISION} + ${BINARY_REVISION}
	
	${If} $INSTALLED_VERSION_NUMBER == ""
	${Else}
		${If} $INSTALLED_VERSION < ${VERSION}
			MessageBox MB_OK "$MESSAGE_1_"
			Abort
		${Else}
			MessageBox MB_OK "$MESSAGE_2_"
			Abort
		${EndIf}	
	${EndIf}	

FunctionEnd

;----------------------------------------------------------------------------------------------------------------------------

;Version variables

!define PUBLISHER "GRASS Development Team"
!define WEB_SITE "http://grass.osgeo.org/"
!define WIKI_PAGE "http://grass.osgeo.org/wiki/Main_Page"

;----------------------------------------------------------------------------------------------------------------------------

;General Definitions

;Name of the application shown during install
Name "${DISPLAYED_NAME}"

;Name of the output file (installer executable)
OutFile "${INSTALLER_NAME}"

;Default installation folder
InstallDir "C:\${GRASS_BASE}"

;Request application privileges for Windows Vista
RequestExecutionLevel user

;Tell the installer to show Install and Uninstall details as default
ShowInstDetails show
ShowUnInstDetails show

;----------------------------------------------------------------------------------------------------------------------------

;StrReplace
;Replaces all ocurrences of a given needle within a haystack with another string
;Written by dandaman32
 
Var STR_REPLACE_VAR_0
Var STR_REPLACE_VAR_1
Var STR_REPLACE_VAR_2
Var STR_REPLACE_VAR_3
Var STR_REPLACE_VAR_4
Var STR_REPLACE_VAR_5
Var STR_REPLACE_VAR_6
Var STR_REPLACE_VAR_7
Var STR_REPLACE_VAR_8
 
Function StrReplace
	Exch $STR_REPLACE_VAR_2
	Exch 1
	Exch $STR_REPLACE_VAR_1
	Exch 2
	Exch $STR_REPLACE_VAR_0
		StrCpy $STR_REPLACE_VAR_3 -1
		StrLen $STR_REPLACE_VAR_4 $STR_REPLACE_VAR_1
		StrLen $STR_REPLACE_VAR_6 $STR_REPLACE_VAR_0
		loop:
			IntOp $STR_REPLACE_VAR_3 $STR_REPLACE_VAR_3 + 1
			StrCpy $STR_REPLACE_VAR_5 $STR_REPLACE_VAR_0 $STR_REPLACE_VAR_4 $STR_REPLACE_VAR_3
			StrCmp $STR_REPLACE_VAR_5 $STR_REPLACE_VAR_1 found
			StrCmp $STR_REPLACE_VAR_3 $STR_REPLACE_VAR_6 done
			Goto loop
		found:
			StrCpy $STR_REPLACE_VAR_5 $STR_REPLACE_VAR_0 $STR_REPLACE_VAR_3
			IntOp $STR_REPLACE_VAR_8 $STR_REPLACE_VAR_3 + $STR_REPLACE_VAR_4
			StrCpy $STR_REPLACE_VAR_7 $STR_REPLACE_VAR_0 "" $STR_REPLACE_VAR_8
			StrCpy $STR_REPLACE_VAR_0 $STR_REPLACE_VAR_5$STR_REPLACE_VAR_2$STR_REPLACE_VAR_7
			StrLen $STR_REPLACE_VAR_6 $STR_REPLACE_VAR_0
			Goto loop
		done:
	Pop $STR_REPLACE_VAR_1 ; Prevent "invalid opcode" errors and keep the
	Pop $STR_REPLACE_VAR_1 ; stack as it was before the function was called
	Exch $STR_REPLACE_VAR_0
FunctionEnd
 
!macro _strReplaceConstructor OUT NEEDLE NEEDLE2 HAYSTACK
	Push "${HAYSTACK}"
	Push "${NEEDLE}"
	Push "${NEEDLE2}"
	Call StrReplace
	Pop "${OUT}"
!macroend
 
!define StrReplace '!insertmacro "_strReplaceConstructor"'

;----------------------------------------------------------------------------------------------------------------------------

Function CheckInstDir

	Var /GLOBAL INSTDIR_TEST
	Var /GLOBAL INSTDIR_LENGHT	
	Var /GLOBAL INSTDIR_TEST_LENGHT
	Var /GLOBAL MESSAGE_3_
	
	StrCpy $MESSAGE_3_ "WARNING: you are about to install GRASS into a directory that has spaces$\r$\n"
	StrCpy $MESSAGE_3_ "$MESSAGE_3_in either its name or the path of directories leading up to it.$\r$\n"
	StrCpy $MESSAGE_3_ "$MESSAGE_3_Some functionalities of GRASS might be hampered by this. We would highly$\r$\n"
	StrCpy $MESSAGE_3_ "$MESSAGE_3_appreciate if you tried and reported any problems, so that we can fix them.$\r$\n"
	StrCpy $MESSAGE_3_ "$MESSAGE_3_However, if you want to avoid any such issues, we recommend that you$\r$\n"
	StrCpy $MESSAGE_3_ "$MESSAGE_3_choose a simple installation path without spaces, such as: C:\${GRASS_BASE}.$\r$\n"
	
	${StrReplace} "$INSTDIR_TEST" " " "" "$INSTDIR"
	
	StrLen $INSTDIR_LENGHT "$INSTDIR"
	StrLen $INSTDIR_TEST_LENGHT "$INSTDIR_TEST"
	
	${If} $INSTDIR_TEST_LENGHT < $INSTDIR_LENGHT	
		MessageBox MB_OK|MB_ICONEXCLAMATION "$MESSAGE_3_"
	${EndIf}
	
FunctionEnd

;----------------------------------------------------------------------------------------------------------------------------

;Interface Settings

!define MUI_ABORTWARNING
!define MUI_ICON ".\Extras\install_grass.ico"
!define MUI_UNICON ".\Extras\uninstall_grass.ico"
!define MUI_HEADERIMAGE_BITMAP_NOSTETCH ".\Extras\InstallHeaderImage.bmp"
!define MUI_HEADERIMAGE_UNBITMAP_NOSTRETCH ".\Extras\UnInstallHeaderImage.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP ".\Extras\WelcomeFinishPage.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP ".\Extras\UnWelcomeFinishPage.bmp"

;----------------------------------------------------------------------------------------------------------------------------

;Installer Pages

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${PACKAGE_FOLDER}\GPL.TXT"
!insertmacro MUI_PAGE_DIRECTORY

Page custom CheckInstDir

!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

;----------------------------------------------------------------------------------------------------------------------------

;Language files

!insertmacro MUI_LANGUAGE "English"

;----------------------------------------------------------------------------------------------------------------------------
	
;Installer Sections

Section "GRASS" SecGRASS

	SectionIn RO
	
	;Set to try to overwrite existing files
	SetOverwrite try

	;Set the install path
	SetOutPath "$INSTDIR"
	
	;add GRASS files
	File /r ${PACKAGE_FOLDER}\*.*	
	File .\Extras\grass.ico
	File .\Extras\grass_web.ico
	File .\Extras\GRASS-WebSite.url
	File .\Extras\README.html	
	
	;Set GIS_DATABASE directory
	SetShellVarContext current
	Var /GLOBAL GIS_DATABASE	
	StrCpy $GIS_DATABASE "$DOCUMENTS\GIS DataBase"
	
	;Create GIS_DATABASE directory
	CreateDirectory "$GIS_DATABASE"
	
	;Install demolocation into GIS_DATABASE directory
	SetOutPath "$GIS_DATABASE\demolocation"
	File /r ${DEMOLOCATION_PATH}\*.*
	
	;Create uninstaller
	WriteUninstaller "$INSTDIR\Uninstall.exe"
	
	;Registry Key Entries
	
	;HKEY_LOCAL_MACHINE Install entries
	;Set the Name, Version and Revision of GRASS + PublisherInfo + InstallPath	
	WriteRegStr HKLM "Software\${GRASS_BASE}" "Name" "GRASS"
	WriteRegStr HKLM "Software\${GRASS_BASE}" "VersionNumber" "${VERSION_NUMBER}"
	WriteRegStr HKLM "Software\${GRASS_BASE}" "SvnRevision" "${SVN_REVISION}"
	WriteRegStr HKLM "Software\${GRASS_BASE}" "BinaryRevision" "${BINARY_REVISION}"
	WriteRegStr HKLM "Software\${GRASS_BASE}" "Publisher" "${PUBLISHER}"
	WriteRegStr HKLM "Software\${GRASS_BASE}" "WebSite" "${WEB_SITE}"
	WriteRegStr HKLM "Software\${GRASS_BASE}" "InstallPath" "$INSTDIR"
	
	;HKEY_LOCAL_MACHINE Uninstall entries
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}" "DisplayName" "GRASS"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}" "UninstallString" "$INSTDIR\Uninstall.exe"
	
	!if ${INSTALLER_TYPE} == "Release"
		WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}"\
		"DisplayVersion" "${VERSION_NUMBER}-${BINARY_REVISION}"
	!else
		WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}"\
		"DisplayVersion" "${VERSION_NUMBER}-r${SVN_REVISION}-${BINARY_REVISION}"
	!endif
	
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}" "DisplayIcon" "$INSTDIR\grass.ico"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}" "EstimatedSize" 1
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}" "HelpLink" "${WIKI_PAGE}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}" "URLInfoAbout" "${WEB_SITE}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}" "Publisher" "${PUBLISHER}"
  
	;create Desktop Shortcut
	SetShellVarContext all
	
	CreateShortCut "$DESKTOP\GRASS ${VERSION_NUMBER}.lnk" "$INSTDIR\${GRASS_COMMAND}.bat" "" "$INSTDIR\grass.ico" ""\
	SW_SHOWNORMAL "" "Launch GRASS ${VERSION_NUMBER}"
 
	;create Windows Start Menu Shortcuts
	SetShellVarContext all
	
	CreateDirectory "$SMPROGRAMS\${GRASS_BASE}"
	
	CreateShortCut "$SMPROGRAMS\${GRASS_BASE}\GRASS ${VERSION_NUMBER}.lnk" "$INSTDIR\${GRASS_COMMAND}.bat"\
	"" "$INSTDIR\grass.ico" "" SW_SHOWNORMAL "" "Launch GRASS ${VERSION_NUMBER}"
	
	CreateShortCut "$SMPROGRAMS\${GRASS_BASE}\MSYS.lnk" "$INSTDIR\msys\msys.bat" "" "$INSTDIR\msys\m.ico" ""\
	SW_SHOWNORMAL "" "Open MSYS Console"
	
	CreateShortCut "$SMPROGRAMS\${GRASS_BASE}\Web Site.lnk" "$INSTDIR\GRASS-WebSite.url" "" "$INSTDIR\grass_web.ico" ""\
	SW_SHOWNORMAL "" "Visit GRASS Web Site"
	
	!if ${INSTALLER_TYPE} == "Release"
		CreateShortCut "$SMPROGRAMS\${GRASS_BASE}\ReadMe.lnk" "$INSTDIR\README.html" "" "" ""\
		SW_SHOWNORMAL "" "View GRASS ${VERSION_NUMBER} README File"
	!endif
	
	CreateShortCut "$SMPROGRAMS\${GRASS_BASE}\Uninstall.lnk" "$INSTDIR\Uninstall.exe" "" "" ""\
	SW_SHOWNORMAL "" "Uninstall GRASS ${VERSION_NUMBER}"
	
	;create grass_command.bat
	ClearErrors
	FileOpen $0 $INSTDIR\${GRASS_COMMAND}.bat w
	IfErrors done_create_grass_command.bat
	FileWrite $0 '@echo off$\r$\n'
	FileWrite $0 'rem #########################################################################$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem # File dynamically created by NSIS installer script;$\r$\n'
	FileWrite $0 'rem # Written by Marco Pasetti;$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem #########################################################################$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem # GRASS Initialization$\r$\n'
	FileWrite $0 'rem #$\r$\n'
	FileWrite $0 'rem #########################################################################$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem *******Environment variables***********$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Set GRASS Installation Directory Variable$\r$\n'
	FileWrite $0 'set GRASSDIR=$INSTDIR$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Directory where your .grassrc6 file will be stored$\r$\n'
	FileWrite $0 'set HOME=%USERPROFILE%$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Name of the wish (Tk) executable$\r$\n'	
	FileWrite $0 'set GRASS_WISH=wish.exe$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Path to the shell command$\r$\n'	 
	FileWrite $0 'set GRASS_SH=%GRASSDIR%\msys\bin\sh.exe$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Set Path to utilities (libraries and bynaries) used by GRASS$\r$\n'
	FileWrite $0 'set PATH=%GRASSDIR%\msys\bin;%PATH%$\r$\n'
	FileWrite $0 'set PATH=%GRASSDIR%\extrabin;%GRASSDIR%\extralib;%PATH%$\r$\n'
	FileWrite $0 'set PATH=%GRASSDIR%\tcl-tk\bin;%GRASSDIR%\sqlite\bin;%PATH%$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Set Path to MSIE web browser$\r$\n'	
	FileWrite $0 'set GRASS_HTML_BROWSER=%PROGRAMFILES%/Internet Explorer/iexplore.exe$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'rem Path to the proj files (notably the epsg projection list)$\r$\n'	
	FileWrite $0 'set GRASS_PROJSHARE=%GRASSDIR%\proj$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'set WINGISBASE=%GRASSDIR%$\r$\n'
	FileWrite $0 '"%WINGISBASE%\etc\init.bat" %*'
	FileClose $0
	done_create_grass_command.bat:
	
	;Set UNIX_LIKE GRASS Path
	Var /GLOBAL UNIX_LIKE_DRIVE
	Var /GLOBAL UNIX_LIKE_PATH
  
	StrCpy $UNIX_LIKE_DRIVE "$INSTDIR" 3
	StrCpy $UNIX_LIKE_PATH "$INSTDIR" "" 3
  
	;replace "\" with "/" in $UNIX_LIKE_DRIVE
	${StrReplace} "$UNIX_LIKE_DRIVE" "\" "/" "$UNIX_LIKE_DRIVE"
  
	;replace ":" with "" in $UNIX_LIKE_DRIVE
	${StrReplace} "$UNIX_LIKE_DRIVE" ":" "" "$UNIX_LIKE_DRIVE"
	
	;replace "\" with "/" in $UNIX_LIKE_PATH
	${StrReplace} "$UNIX_LIKE_PATH" "\" "/" "$UNIX_LIKE_PATH"

	;Set USERNAME
	Var /GLOBAL USERNAME

	ReadRegStr $USERNAME HKCU "Software\Microsoft\Windows\CurrentVersion\Explorer" "Logon User Name"
  
	;Create $INSTDIR\msys\home and $INSTDIR\msys\home\$USERNAME directories
	CreateDirectory $INSTDIR\msys\home
	CreateDirectory $INSTDIR\msys\home\$USERNAME
  
	;create $INSTDIR\msys\home\$USERNAME\grass_command
	ClearErrors
	FileOpen $0 $INSTDIR\msys\home\$USERNAME\${GRASS_COMMAND} w
	IfErrors done_create_grass_command
	FileWrite $0 '#! /bin/sh$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '# File dynamically created by NSIS installer script;$\r$\n'
	FileWrite $0 '# Written by Marco Pasetti;$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '# MODULE:   	GRASS Initialization$\r$\n'
	FileWrite $0 '# AUTHOR(S):	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th$\r$\n'
	FileWrite $0 '# PURPOSE:  	The source file for this shell script is in$\r$\n'
	FileWrite $0 '#   	    	lib/init/grass.src and is the grass startup script. It$\r$\n'
	FileWrite $0 '#   	    	requires a source file because the definition of GISBASE$\r$\n'
	FileWrite $0 '#   	    	is not known until compile time and is substituted from the$\r$\n'
	FileWrite $0 '#   	    	Makefile. Any command line options are passed to Init.sh.$\r$\n'
	FileWrite $0 '# COPYRIGHT:    (C) 2000-2005 by the GRASS Development Team$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#               This program is free software under the GNU General Public$\r$\n'
	FileWrite $0 '#   	    	License (>=v2). Read the file COPYING that comes with GRASS$\r$\n'
	FileWrite $0 '#   	    	for details.$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '# Modified by Marco Pasetti$\r$\n'
	FileWrite $0 '# added export PATH instructions to let GRASS work from$\r$\n'
	FileWrite $0 '# MSYS environment in dynamic NSIS installation$\r$\n'
	FileWrite $0 '#$\r$\n'
	FileWrite $0 '#########################################################################$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'trap "echo '
	FileWrite $0 "'User break!' ; "
	FileWrite $0 'exit" 2 3 9 15$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 '# Set the GISBASE variable$\r$\n'
	FileWrite $0 'GISBASE=/$UNIX_LIKE_DRIVE$UNIX_LIKE_PATH$\r$\n'
	FileWrite $0 'export GISBASE$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 '# Set the PATH variable$\r$\n'
	FileWrite $0 'PATH="$$GISBASE/extrabin:$$GISBASE/extralib:$$PATH"$\r$\n'
	FileWrite $0 'PATH="$$GISBASE/tcl-tk/bin:$$GISBASE/sqlite/bin:$$PATH"$\r$\n'
	FileWrite $0 'export PATH$\r$\n'
	FileWrite $0 '$\r$\n'
	FileWrite $0 'exec "$$GISBASE/etc/Init.sh" "$$@"'
	FileClose $0
	done_create_grass_command:
	
	;Set GIS_DATABASE Path
	Var /GLOBAL GIS_DATABASE_PATH
  
	;replace \ with / in $GIS_DATABASE
	${StrReplace} "$GIS_DATABASE_PATH" "\" "/" "$GIS_DATABASE"
  
	;create $PROFILE\.grassrc6
	SetShellVarContext current
	ClearErrors
	FileOpen $0 $PROFILE\.grassrc6 w
	IfErrors done_create_.grassrc6
	FileWrite $0 'GISDBASE: $GIS_DATABASE_PATH$\r$\n'
	FileWrite $0 'LOCATION_NAME: demolocation$\r$\n'
	FileWrite $0 'MAPSET: PERMANENT$\r$\n'
	FileClose $0	
	done_create_.grassrc6:
	
	CopyFiles $PROFILE\.grassrc6 $INSTDIR\msys\home\$USERNAME
                 
SectionEnd

Section /O "North Carolina Data Set" SecNorthCarolina 
  
  	SetOutPath "$GIS_DATABASE"
	AddSize 293314
	
	Var /GLOBAL NORTH_CAROLINA_DOWNLOAD_PATH
	
	StrCpy $NORTH_CAROLINA_DOWNLOAD_PATH "http://grass.osgeo.org/sampledata/nc_spm_07_2007_dec20.tar.gz"

	InitPluginsDir
	NSISdl::download $NORTH_CAROLINA_DOWNLOAD_PATH "$GIS_DATABASE\nc_spm_07_2007_dec20.tar.gz"
	Pop $R0 ;Get the return value
	StrCmp $R0 "success" +3
		MessageBox MB_OK "Download $R0. GRASS will be installed without North Carolina GRASS Sample Database"

	InitPluginsDir
	untgz::extract "$GIS_DATABASE\nc_spm_07_2007_dec20.tar.gz" "$GIS_DATABASE"
	Pop $0
	StrCmp $0 "success" ok
  		DetailPrint "$0" ;print error message to log
	ok:
	
	Rename "$GIS_DATABASE\nc_spm_07" "$GIS_DATABASE\North-Carolina"
	Delete "$GIS_DATABASE\nc_spm_07_2007_dec20.tar.gz"

SectionEnd

Section /O "South Dakota Data Set" SecSouthDakota  
  
  	SetOutPath "$GIS_DATABASE"
	AddSize 42171
	
	Var /GLOBAL SOUTH_DAKOTA_DOWNLOAD_PATH
	
	StrCpy $SOUTH_DAKOTA_DOWNLOAD_PATH "http://grass.osgeo.org/sampledata/spearfish_grass60data-0.3.tar.gz"

	InitPluginsDir
	NSISdl::download $SOUTH_DAKOTA_DOWNLOAD_PATH "$GIS_DATABASE\spearfish_grass60data-0.3.tar.gz"
	Pop $R0 ;Get the return value
	StrCmp $R0 "success" +3
		MessageBox MB_OK "Download $R0. GRASS will be installed without South Dakota GRASS Sample Database"

	InitPluginsDir
	untgz::extract "$GIS_DATABASE\spearfish_grass60data-0.3.tar.gz" "$GIS_DATABASE"
	Pop $0
	StrCmp $0 "success" ok
  		DetailPrint "$0" ;print error message to log
	ok:
	
	Rename "$GIS_DATABASE\spearfish60" "$GIS_DATABASE\South-Dakota"
	Delete "$GIS_DATABASE\spearfish_grass60data-0.3.tar.gz"

SectionEnd

;----------------------------------------------------------------------------------------------------------------------------

;Descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
	!insertmacro MUI_DESCRIPTION_TEXT ${SecGRASS} "Install GRASS ${VERSION_NUMBER}"
	!insertmacro MUI_DESCRIPTION_TEXT ${SecNorthCarolina} "Download and install North Carolina GRASS sample database"
	!insertmacro MUI_DESCRIPTION_TEXT ${SecSouthDakota} "Download and install South Dakota GRASS sample database"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;----------------------------------------------------------------------------------------------------------------------------

;Uninstaller Section

Section "Uninstall"

	;remove files
	Delete "$INSTDIR\Uninstall.exe"
	Delete "$INSTDIR\grass.ico"
	Delete "$INSTDIR\grass_web.ico"
	Delete "$INSTDIR\GPL.TXT"
	Delete "$INSTDIR\AUTHORS"
	Delete "$INSTDIR\CHANGES"
	Delete "$INSTDIR\COPYING"	
	Delete "$INSTDIR\${GRASS_COMMAND}.bat"	
	Delete "$INSTDIR\GRASS-WebSite.url"	
	Delete "$INSTDIR\README.html"
	Delete "$INSTDIR\REQUIREMENTS.html"
	
	;remove folders
	RMDir /r "$INSTDIR\bin"
	RMDir /r "$INSTDIR\bwidget"
	RMDir /r "$INSTDIR\docs"
	RMDir /r "$INSTDIR\driver"
	RMDir /r "$INSTDIR\etc"
	RMDir /r "$INSTDIR\extrabin"
	RMDir /r "$INSTDIR\extralib"
	RMDir /r "$INSTDIR\fonts"
	RMDir /r "$INSTDIR\include"
	RMDir /r "$INSTDIR\lib"
	RMDir /r "$INSTDIR\msys"
	RMDir /r "$INSTDIR\proj"
	RMDir /r "$INSTDIR\scripts"
	RMDir /r "$INSTDIR\sqlite"
	RMDir /r "$INSTDIR\tcl-tk"
	
	;if empty, remove install folder
	RMDir "$INSTDIR"
	
	;remove Desktop ShortCut
	SetShellVarContext all
	Delete "$DESKTOP\GRASS ${VERSION_NUMBER}.lnk"
	
	;remove Programs Start ShortCut
	SetShellVarContext all
	RMDir /r "$SMPROGRAMS\${GRASS_BASE}"
	
	;remove .grassrc6 file
	SetShellVarContext current
	Delete "$PROFILE\.grassrc6"	

	;remove Registry Entries
	DeleteRegKey HKLM "Software\${GRASS_BASE}"
	DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${GRASS_BASE}"

SectionEnd
