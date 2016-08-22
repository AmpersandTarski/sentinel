# To run this from Parallells, create a shortcut to bash.exe and change Target property to:
# "C:\Program Files (x86)\Git\bin\bash.exe" --login C:\Users\<user name>\git\sentinel\scripts\win-update-publish-exe.sh


echo Executables are released via travis. Please head to https://github.com/AmpersandTarski/Ampersand/releases/latest
exit 1




cd ~/git/ampersand-publish
echo Fetch/reset to latest ampersand master
git fetch --prune
git reset origin/master --hard

# cabal clean gives an error, so use rm
rm -rf dist

echo Stack installing ampersand
stack install

~/git/sentinel/scripts/win-publish-exe.sh ampersand
echo
read -p "Script execution finished, press any key to exit."
