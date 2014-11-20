<?php
exec('sudo -u sentinel /home/sentinel/git/sentinel/scripts/phpCompileSentinel 2>&1',$output,$exitCode);
echo implode("<br/>", $output);
?>
<p><button onclick="history.go(-1)">Back</button>