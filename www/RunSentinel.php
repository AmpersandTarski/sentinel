<?php
exec('sudo -u sentinel /home/sentinel/git/sentinel/scripts/phpRootRunSentinel '. $_REQUEST['args'] .' > /dev/null &');
?>
