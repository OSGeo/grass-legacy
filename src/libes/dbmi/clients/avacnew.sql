grant dba to root;
grant dba to public;
--
--  Author: martin
--  Date: 1994/04/09
--  Id: avacnew.sql, 1994/07/20 martin
--
--
--  
--  #22 vacres table -
--  Contains info needed to link gis vacant spatial data with jappel dbms data.
--  Now, contains all CD data for the 5 community areas of relevance.
create table vacres (
recno_nw	integer,	--record number new CD (mar/94)
pin_nw	float,	--14-digit PIN number associated with recno_nw
comarea	char(3),	--community area designation for recno, recno_nw
acres	char(8)	--acres of recno, recno_nw
);
