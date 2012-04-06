<?php
exec('sudo /home/martijn/svn/Sentinel/scripts/phpRootRunSentinel',$output,$exitCode);
echo implode("<br/>", $output);
echo '<br/><a href="ampersand/SentinelOutput.txt" >View test output</a>';
?>