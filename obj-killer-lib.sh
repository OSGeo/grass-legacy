echo"Cleanup after sucessfully building the code..."
echo""
echo "remove all the object-file... "
find . -name 'OBJ*' -exec /bin/rm -rf {} \;
find . -name '*.o' -exec /bin/rm -rf {} \;
echo "ready."
echo "delete LIB.linux files:"
find . -name 'LIB*' -exec /bin/rm -rf {} \;
 