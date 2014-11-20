<?php
exec('sudo -u sentinel /home/sentinel/git/sentinel/scripts/phpKillAndRunSentinel '. $_REQUEST['args'] .' > /dev/null &');
?>
