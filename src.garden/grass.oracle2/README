Date: Mon, 05 Jun 2000 15:30:26 +0200 
From: Christian Saldarini <cristian@torno.como.polimi.it> 


The prototype I send is not an engineered version, and it needs oracle 
installed on your systems, preferrable with oracle spatial cartridge 
also. 
To make it work you have to set up some(!) includes in "main.c" and in 
"Gmakefile" (that is the worst thing i ever tried to compile: take a 
look and believe). 

"Main.c" is made of: 
- two logon-logoff routines (nothing more to say) 
- a setup routine, that is the most important one: it binds colums in a 
  query with variables 
 in your code, for example in my code, column n°1 is linked with 
 variable y, and n°2 with x. 
 (Why y,x and not x,y?This is due to a mistake in filling our tables 
 see later) 
 Remember that you can add more columns, for example some others 
 attributes, and bind them with other variables, in this routine. 
- getdata routine with oexec and ofetch procedure, you execute a query 
 and retrive one tuple a-time. 
- error messaging function. 
- do exet function to close everything. 
- a standard main grass routine including a selection query 
  note that the sdo_x1 column is binded with y and sdo_y1 is binded 
 with x. this is due we 
 filled example tables in the wrong way (x and y twisted). So don't 
  worry and change according to your tables. 
 
The d.ora.rast does the same thing, but is less useful; it takes raster 
data as they were sites point on a regular grid. Its existance is only 
for completeness on my thesis work. 
