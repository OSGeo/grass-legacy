BEGIN {
	FS = ":"
	counter = 1 
	rule_number = 0
	end_rules = 1
	exit_status = 0
#
# determine the total number of lines in the rules file
#
total = system (" gawk '{ } END {exit NR}' /tmp/db.rules ") 
while (line <= total ) {

#
# Load up the database query rules into a set of matrices (att cat and rule) 
# with index rule_number.  
#

	getline < "/tmp/db.rules"

	if ($1 != "") {
		if (counter == 1) {
			rule_number = rule_number + 1
			att[rule_number] = $0
			counter = counter + 1
			line = line + 1
			if (att[rule_number] ~ /[A-Za-z]+/) {
				printf ("Rule number %d has tried to make a non-integer attribute assignment\n", rule_number) > "/dev/tty"
				exit_status = 1
				exit 1
				}
			}
		else 	{
			if (counter == 2){
				cat[rule_number] = $0
				counter = counter + 1
				line = line + 1
				}
			else 	{
				if (counter == 3) {
					rule[rule_number] = $0
					counter = 1
					line = line + 1
					}
				}
			}
		}
#
# Usually ignore blank lines, unless they are an unlabeled category
# 
	else 	{ 
		line = line + 1
		if (counter == 2) {
			cat[rule_number] = ""
			counter = counter + 1
			}
		}
	}

#
# error checking if the att or the rule was not set, exit out
#

for (i=1; i<= rule_number; i++) {
	if ( !att[i] || !rule[i] ) {
		printf ("\n", i) > "/dev/tty"
		printf ("Rule number %d is invalid:\n", i) > "/dev/tty"
		printf ("Query rule is:  %s\n", rule[i]) > "/dev/tty"
		printf ("New category is: %d %s\n", att[i], cat[i]) > "/dev/tty"
		exit_status =  1
		exit 1
		}
	}


#
# Load field names from database file into matrix field_number using the
# field name as the index
#
getline
getline
for (i=1; i<=NF; i++) {
     	field_number[$i]=i
  	}
#
# create the new gawk query string
#

## commented 8/98 M. Neteler
#newrule=""

#
# correct syntax, using spaces and operands
#
for (j=1;  j<= rule_number; j++) {
	gsub ("\\(", " \( ", rule[j])
	gsub ("\\)", " \) ", rule[j])
	gsub (" and ", " \\&\\& ", rule[j])
	gsub (" or " , " || ", rule[j])
## added ~ to ! command: !~ instead of ! 9/98 M. Neteler
	gsub (" not " , " !~ ", rule[j])
	gsub ("^not " , " !~ ", rule[j])
	gsub ("==" , " = ", rule[j])
	gsub ("=" , " == ", rule[j])
	gsub (">" , " > ", rule[j])
	gsub ("<" , " < ", rule[j])

#
# create array command[].  Rebuild the query substituting in the field
# number for the field name.
# 

# first parse out spaces within quotes. Even numbered quoting[i]
# should be quoted, odd not.
#
	quoting_no = split(rule[j], quoting, "\"")
	rule[j] = ""
	for (i=1; i<=quoting_no; i++){
		if (i%2 == 0){
			gsub(" ", ":", quoting[i])
			rule[j] = rule[j] "\"" quoting[i] "\""
			}
		else {rule[j] = rule[j] quoting[i]}
		}

#
# now parse the command using spaces, later put the quoted spaces back in
#
	arg_number = split(rule[j], command, " ")
	for (i=1;i<=arg_number; i++) {
		if ( (command[i] ~ "[A-Za-z][A-Za-z]*") && ! (command[i] ~ "^\".*\"$") ) {
			if ( ! field_number[command[i]] ) {
				printf("Unknown field %s in rule %d, please try again\n", command[i], j) > "/dev/tty"
				exit_status = 1
				exit 1
			}
			else 	{
				command[i] = sprintf ("$%s",field_number[command[i]])
				}
			}
		else { if (command[i] ~ "^\".*\"$") {
				gsub(":", " ", command[i])
				gsub("\"", "", command[i])
				gsub("\\.", "[.]", command[i])
				gsub("*", ".*", command[i])

## changed / to \" next line due to errors in < and > comparisons 9/98 M. Neteler
				command[i] = sprintf ("\"%s\"", command[i])
				}
			else { if (command[i] == "==" && command[i+1] ~ "^\".*\"$") {
					command[i] = "~"
					}
				}
			}
		newrule[j] = newrule[j] command[i] " "
		}
	}

}

{ 
if (exit_status == 0) {
	print $0 > "/tmp/db.data" 
	}
}

END {


#
# print reclass rules
#

if (exit_status == 1) { exit 1 }
print "0 = 0 no data"
system ("/bin/cp /dev/null /tmp/db.prog")
for (i=1; i<=rule_number; i++) {
 	printf ("gawk -F: '{if ( %s ) { print $1, \" =  %d %s \" }}' /tmp/db.data\n", newrule[i], att[i], cat[i] ) >> "/tmp/db.prog" 
   	}
system("sh  /tmp/db.prog")
}
