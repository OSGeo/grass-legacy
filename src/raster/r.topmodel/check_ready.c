#include "local_proto.h"


int
check_ready(void)
{
	int	retval;


	flg.overwrlist = 0;

	retval = 0;

	if(check_required() ||
	   check_names()    ||
	   check_io())
		retval = 1;

	if(!retval){
		if(flg.overwrlist & FILL)
			G_remove("cell", map.fill);

		if(flg.overwrlist & DIR)
			G_remove("cell", map.dir);

		if(flg.overwrlist & BELEV)
			G_remove("cell", map.belev);

		if(flg.overwrlist & TOPIDX)
			G_remove("cell", map.topidx);

		if(flg.overwrlist & IDXSTATS)
			unlink(file.idxstats);

		if(flg.overwrlist & OUTPUT)
			unlink(file.output);
	}


	return(retval);
}


int
check_required(void)
{
	int	retval;


	retval = 0;

	if(!flg.input){
		if(!map.elev){
			fprintf(stderr, 
				"\n** elevation required **\n");
			retval = 1;
		}

		if(!map.basin){
			fprintf(stderr, 
				"\n** basin required **\n");
			retval = 1;
		}

		if(!map.belev){
			fprintf(stderr, 
				"\n** belevation required **\n");
			retval = 1;
		}

		if(!map.topidx){
			fprintf(stderr, 
				"\n** topidx required **\n");
			retval = 1;
		}

		if(map.fill && !map.dir){
			fprintf(stderr, 
				"\n** direction required "
				"if depressionless is given **\n");
			retval = 1;
		}

		if(map.dir && !map.fill){
			fprintf(stderr, 
				"\n** depressionless required "
				"if direction is given **\n");
			retval = 1;
		}
	}else{
		if(map.belev && !map.topidx){
			fprintf(stderr, 
				"\n** topidx required "
				"if belevation is given **\n");
			retval = 1;
		}
	}


	return(retval);
}


int
check_names(void)
{
	int	retval;


	retval = 0;

	if(!flg.input){
		if(map.elev){
			if(map.basin && !strcmp(map.elev, map.basin)){
				fprintf(stderr, 
					"\n** elevation == basin **\n");
				retval = 1;
			}
	
			if(map.fill && !strcmp(map.elev, map.fill)){
				fprintf(stderr, 
					"\n** elevation == "
							"depressionless **\n");
				retval = 1;
			}
	
			if(map.dir && !strcmp(map.elev, map.dir)){
				fprintf(stderr, 
					"\n** elevation == direction **\n");
				retval = 1;
			}
	
			if(map.belev && !strcmp(map.elev, map.belev)){
				fprintf(stderr, 
					"\n** elevation == belevation **\n");
				retval = 1;
			}
	
			if(map.topidx && !strcmp(map.elev, map.topidx)){
				fprintf(stderr, 
					"\n** elevation == topidx **\n");
				retval = 1;
			}
		}
	
		if(map.basin){
			if(map.fill && !strcmp(map.basin, map.fill)){
				fprintf(stderr, 
					"\n** basin == depressionless **\n");
				retval = 1;
			}
	
			if(map.dir && !strcmp(map.basin, map.dir)){
				fprintf(stderr, 
					"\n** basin == direction **\n");
				retval = 1;
			}
	
			if(map.belev && !strcmp(map.basin, map.belev)){
				fprintf(stderr, 
					"\n** basin == belevation **\n");
				retval = 1;
			}
	
			if(map.topidx && !strcmp(map.basin, map.topidx)){
				fprintf(stderr, 
					"\n** basin == topidx **\n");
				retval = 1;
			}
		}
	
		if(map.fill){
			if(map.dir && !strcmp(map.fill, map.dir)){
				fprintf(stderr, 
					"\n** depressionless == "
							"direction **\n");
				retval = 1;
			}
	
			if(map.belev && !strcmp(map.fill, map.belev)){
				fprintf(stderr, 
					"\n** depressionless == "
							"belevation **\n");
				retval = 1;
			}
	
			if(map.topidx && !strcmp(map.fill, map.topidx)){
				fprintf(stderr, 
					"\n** depressionless == topidx **\n");
				retval = 1;
			}
		}
	
		if(map.dir){
			if(map.belev && !strcmp(map.dir, map.belev)){
				fprintf(stderr, 
					"\n** direction == belevation **\n");
				retval = 1;
			}
	
			if(map.topidx && !strcmp(map.dir, map.topidx)){
				fprintf(stderr, 
					"\n** direction == topidx **\n");
				retval = 1;
			}
		}
	}

	if(map.belev){
		if(map.topidx && !strcmp(map.belev, map.topidx)){
			fprintf(stderr, 
				"\n** belevation == topidx **\n");
			retval = 1;
		}
	}

	if(!strcmp(file.idxstats, file.params)){
		fprintf(stderr, 
			"\n** idxstats == parameters **\n");
		retval = 1;
	}

	if(!strcmp(file.idxstats, file.input)){
		fprintf(stderr, 
			"\n** idxstats == input **\n");
		retval = 1;
	}

	if(!strcmp(file.idxstats, file.output)){
		fprintf(stderr, 
			"\n** idxstats == output **\n");
		retval = 1;
	}

	if(file.Qobs && !strcmp(file.idxstats, file.Qobs)){
		fprintf(stderr, 
			"\n** idxstats == Qobs **\n");
		retval = 1;
	}

	if(!strcmp(file.params, file.input)){
		fprintf(stderr, 
			"\n** parameters == input **\n");
		retval = 1;
	}

	if(!strcmp(file.params, file.output)){
		fprintf(stderr, 
			"\n** parameters == output **\n");
		retval = 1;
	}

	if(file.Qobs && !strcmp(file.params, file.Qobs)){
		fprintf(stderr, 
			"\n** parameters == Qobs **\n");
		retval = 1;
	}

	if(!strcmp(file.input, file.output)){
		fprintf(stderr, 
			"\n** input == output **\n");
		retval = 1;
	}

	if(file.Qobs && !strcmp(file.input, file.Qobs)){
		fprintf(stderr, 
			"\n** input == Qobs **\n");
		retval = 1;
	}

	if(file.Qobs && !strcmp(file.output, file.Qobs)){
		fprintf(stderr, 
			"\n** output == Qobs **\n");
		retval = 1;
	}


	return(retval);
}


int
check_io(void)
{
	int	retval;
	FILE	*fp;


	retval = 0;

	if(!flg.input){
		if(map.elev && !G_find_file("cell", map.elev, mapset)){
			fprintf(stderr,
				"\n** %s - not exists **\n",
				map.elev);
			retval = 1;
		}

		if(map.basin && !G_find_file("cell", map.basin, mapset)){
			fprintf(stderr, 
				"\n** %s - not exists **\n", 
				map.basin);
			retval = 1;
		}

		if(map.fill && G_find_file("cell", map.fill, mapset)){
			if(flg.overwr){
				flg.overwrlist |= FILL;
			}else{
				fprintf(stderr, 
					"\n** %s - already exists **\n", 
					map.fill);
				retval = 1;
			}
		}

		if(map.dir && G_find_file("cell", map.dir, mapset)){
			if(flg.overwr){
				flg.overwrlist |= DIR;
			}else{
				fprintf(stderr, 
					"\n** %s - already exists **\n", 
					map.dir);
				retval = 1;
			}
		}

		if(map.belev && G_find_file("cell", map.belev, mapset)){
			if(flg.overwr){
				flg.overwrlist |= BELEV;
			}else{
				fprintf(stderr, 
					"\n** %s - already exists **\n", 
					map.belev);
				retval = 1;
			}
		}

		if(map.topidx && G_find_file("cell", map.topidx, mapset)){
			if(flg.overwr){
				flg.overwrlist |= TOPIDX;
			}else{
				fprintf(stderr, 
					"\n** %s - already exists **\n", 
					map.topidx);
				retval = 1;
			}
		}

		if(file.idxstats && (fp=fopen(file.idxstats, "r"))){
			fclose(fp);
			if(flg.overwr){
				flg.overwrlist |= IDXSTATS;
			}else{
				fprintf(stderr, 
					"\n** %s - file already exists **\n", 
					file.idxstats);
				retval = 1;
			}
		}
	}else{
		if(map.belev){
			if(!G_find_file("cell", map.belev, mapset)){
				fprintf(stderr, 
					"\n** %s - not exists **\n", 
					map.belev);
				retval = 1;
			}else{
				if(map.topidx &&
				   G_find_file("cell", map.topidx, mapset)){
					if(flg.overwr){
						flg.overwrlist |= TOPIDX;
					}else{
						fprintf(stderr, 
							"\n** %s - "
							"already exists **\n",
							map.topidx);
						retval = 1;
					}
				}

				if(file.idxstats && 
				   (fp=fopen(file.idxstats, "r"))){
					fclose(fp);
					if(flg.overwr){
						flg.overwrlist |= IDXSTATS;
					}else{
						fprintf(stderr, 
							"\n** %s - file "
							"already exists **\n", 
							file.idxstats);
						retval = 1;
					}
				}
			}
		}else if(map.topidx){
			if(!G_find_file("cell", map.topidx, mapset)){
				fprintf(stderr,
					"\n** %s - not exists **\n", 
					map.topidx);
				retval = 1;
			}else{
				if(file.idxstats && 
				   (fp=fopen(file.idxstats, "r"))){
					fclose(fp);
					if(flg.overwr){
						flg.overwrlist |= IDXSTATS;
					}else{
						fprintf(stderr, 
							"\n** %s - file "
							"already exists **\n", 
							file.idxstats);
						retval = 1;
					}
				}
			}
		}else if(file.idxstats){
			if(!(fp=fopen(file.idxstats, "r"))){
				fprintf(stderr, 
					"\n** %s - file not exists **\n", 
					file.idxstats);
				retval = 1;
			}else{
				fclose(fp);
			}
		}
	}

	if(file.params){
		if(!(fp=fopen(file.params, "r"))){
			fprintf(stderr, 
				"\n** %s - file not exists **\n", 
				file.params);
			retval = 1;
		}else{
			fclose(fp);
		}
	}

	if(file.input){
		if(!(fp=fopen(file.input, "r"))){
			fprintf(stderr, 
				"\n** %s - file not exists **\n", 
				file.input);
			retval = 1;
		}else{
			fclose(fp);
		}
	}

	if(file.output && (fp=fopen(file.output, "r"))){
		fclose(fp);
		if(flg.overwr){
			flg.overwrlist |= OUTPUT;
		}else{
			fprintf(stderr,
				"\n** %s - file already exists **\n", 
				file.output);
			retval = 1;
		}
	}

	if(file.Qobs){
		if(!(fp=fopen(file.Qobs, "r"))){
			fprintf(stderr, 
				"\n** %s - file not exists **\n", 
				file.Qobs);
			retval = 1;
		}else{
			fclose(fp);
		}
	}


	return(retval);
}

