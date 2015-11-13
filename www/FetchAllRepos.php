<?php
exec('sudo -u sentinel /home/sentinel/git/sentinel/scripts/phpFetchAllRepos',$output,$exitCode);
if ($exitCode) {
	echo "Error while running scripts/phpFetchAllRepos:\n\n";
	foreach ($output as $line)
		echo $line."\n";
}
?>
