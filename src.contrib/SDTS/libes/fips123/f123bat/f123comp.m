for f in `ls ../f123src/*.c`
    do
        cd ../f123obj; gcc -c -Wall -ansi -pedantic -DFIPS_BSD -I../f123inc $f
    done
