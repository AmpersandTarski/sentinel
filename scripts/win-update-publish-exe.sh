cd ~/git/ampersand-publish
echo Fetch/reset to latest ampersand master
git fetch --prune
git reset origin/master --hard
echo Cabal installing ampersand
#cabal clean
#cabal install

~/git/sentinel/scripts/win-publish-exe.sh ampersand
