:
#
# floppy backup program
#
echo " "
echo "Creating a tar file"
sleep 1
rm -f tarfile
tar cFFf tarfile *

echo " "
echo "Checking the tar file"
tar tvf tarfile

echo " "
echo "Compressing the tar file, original size:"
ls -al tarfile
compress tarfile
echo " "
echo "Compression complete, compressed size:"
ls -al tarfile.Z

echo " "
echo "Tar-ing the tar file to floppy" 
tar cvf /dev/fd0 tarfile.Z

echo " "
echo "Checking the floppy" 
tar tvf /dev/fd0

echo " "
echo "Removing the tar file"
rm tarfile.Z

echo " "
echo "All done"
echo " "

