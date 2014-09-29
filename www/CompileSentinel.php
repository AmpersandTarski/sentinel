<?php
exec('sudo /home/sentinel/git/sentinel/scripts/phpRootCompileSentinel 2>&1',$output,$exitCode);
echo implode("<br/>", $output);
?>
<p><button onclick="history.go(-1)">Back</button>