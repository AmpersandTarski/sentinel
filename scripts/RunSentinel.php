<?php
system('sudo /home/martijn/svn/Sentinel/scripts/phpRootRunSentinel',$exitCode);
echo '<br/>Exit code'.$exitCode;
echo '<br/><a href="SentinelOutput.txt" >View test output</a>';
?>