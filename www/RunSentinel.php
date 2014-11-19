<?php
exec('sudo /home/sentinel/git/sentinel/scripts/phpRootRunSentinel '. $_REQUEST['args'] .' > /dev/null &');
?>
