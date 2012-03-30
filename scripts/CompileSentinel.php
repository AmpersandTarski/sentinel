<?php
system('sudo /home/martijn/svn/Sentinel/scripts/phpRootCompileSentinel',$exitCode);
echo '<br><br/>Compilation '.$exitCode ? 'successful' : "failed with exit code $exitCode";
?>