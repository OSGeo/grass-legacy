struct window *output_map(ptr)   
	
	struct window *ptr;
{
	extern char buf[];
	extern int txt_a,txt_b; 
	extern char out_layer[];   

	while(1){
        erase_in_poly(ptr);

	sprintf(buf,"Enter the name of the output overlay : "); 
	throw_text(buf,txt_a,txt_b);
	input();
	sprintf(out_layer,buf);

	if(!G_find_file("cell",out_layer,G_mapset())) 
        break;

	else
        {
        erase_in_poly(ptr);
        sprintf(buf,"Landpattern already exists. Provide unique name.");

        throw_text(buf,txt_a,txt_b);
        sleep(2);
        }

        } 	/* end of while	*/

	/* printf("\n outmap = %s\n",out_layer);  */
	return(ptr);
	
}





