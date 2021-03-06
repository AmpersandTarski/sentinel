# To be run from a Windows bash

set -u # fail on uninitialized variables

if [ $# -eq 0 ]; then
echo Please specify an executable.
exit 1
fi

echo Executables are released via travis. Please head to https://github.com/AmpersandTarski/Ampersand/releases/latest
exit 1




echo "Uploading $1.exe and its version info (in $1-version.txt from)"
echo cabal bin to sentinel.tarski.nl/windowsExecutables/beta
echo

# scp doesn't like colons in $APPDATA, so first copy everything to a temp directory
tempDirName=/tmp/sentinel-publish
mkdir -p $tempDirName

# clear, in case it already existed
rm $tempDirName/* 2> /dev/null

cp $APPDATA/cabal/bin/$1.exe $tempDirName/
$1 --version > $tempDirName/$1-version.txt

cat $tempDirName/$1-version.txt
scp $tempDirName/* sentinel@sentinel.tarski.nl:git/sentinel/www/windowsExecutables/beta/

# Let's not rm -rf on variables
rm $tempDirName/*
rmdir $tempDirName
