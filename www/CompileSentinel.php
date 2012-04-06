<?php
exec('sudo /home/martijn/svn/Sentinel/scripts/phpRootCompileSentinel',$output,$exitCode);
echo implode("<br/>", $output);
?>
<p><button onclick="history.go(-1)">Back</button>