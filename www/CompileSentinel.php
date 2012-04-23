<?php
exec('sudo /home/sentinel/svn/Sentinel/scripts/phpRootCompileSentinel 2>&1',$output,$exitCode);
echo implode("<br/>", $output);
?>
<p><button onclick="history.go(-1)">Back</button>