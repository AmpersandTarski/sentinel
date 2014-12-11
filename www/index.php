<?php
error_reporting(E_ALL);
ini_set("display_errors", 1);

$refreshInterval = 5;
$sentinelOutput = implode("<br/>\n", file('logs/SentinelOutput.txt'));
$sentinelIsRunning = !preg_match("/######## Sentinel exited/", $sentinelOutput);

?>
<html>
  <head>
  <title>Ampersand Sentinel</title>
  
  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"></script>
  <script type="text/javascript">

var sentinelIsRunning = <?php echo $sentinelIsRunning ? 'true' : 'false' ?>; // value is set by PHP

var refreshTimer;

$(document).ready(function(){
  if (sentinelIsRunning)
    refreshPageIn(<?php echo $refreshInterval ?> * 1000);
});

function refreshPageIn (ms) {
  if (refreshTimer)
    clearTimeout(refreshTimer);

  refreshTimer = setTimeout( function () { window.location.reload()}, ms);
  console.log('reload');
  // extra function() is necessary to make reload lazy
}

function compileSentinel() {
  clearTimeout(refreshTimer); // need to disable refresh timer, or the page with compiler output will not be shown.
  location.href='CompileSentinel.php';
}

function runSentinel(deleteSandbox) {
  $.ajax({ url: 'RunSentinel.php',
           data: {args: '-a ' + $('#branch-selector-'+'ampersand').val() +
                       ' -m ' + $('#branch-selector-'+'ampersand-models').val() +
                        (deleteSandbox ? ' --deleteSandbox' : '')},
           cache: false,
           success: function(data){ refreshPageIn(250); }
           // delay reload with 250ms to ensure sentinel process has started
  });
}

function fetchAllRepos() {
  $.ajax({ url: 'FetchAllRepos.php',
    cache: false,
    success: function(data){ 
      if (data) 
        alert(data);
      else {
        alert('Branch selectors have been updated by fetching all repos.');
        refreshPageIn(0);
      }
    }
});
}
  </script>
  
  </head>
<body>
  <?php 
  mkBranchSelector('ampersand');
  mkBranchSelector('ampersand-models');
  ?>
  <button onclick="fetchAllRepos()">Refresh branches (fetch)</button>
  <p>
  <button onclick="compileSentinel()">Update & recompile Sentinel</button>
  (Only necessary if the Sentinel source has been modified. Note that no output will be shown until compilation has finished.)
  <p>
  <button onclick="runSentinel(true)">Run Sentinel, clean install (slow)</button> <button onclick="runSentinel(false)">Run Sentinel normal</button> <a href="/logs/runSentinel.log.txt">View output/errors of runSentinel script</a>, <a href="/ampersand/">View generated prototypes</a>
  <p>
  <a href="logs/?C=M;O=D">View Sentinel logs</a>
  <a style="float: right; margin-right: 3em" href="windowsExecutables/beta">Executables for Windows (not necessarily recent)</a>
<hr/>
<?php

function mkBranchSelector($repo) {
  exec("cd ../../$repo; git for-each-ref --format='%(refname:short)' refs/remotes/origin", $branches);

  echo $repo.': <select id="branch-selector-'.$repo.'">';
  foreach ($branches as $branchStr) {
    $branch= substr($branchStr, 7); # remove leading "origin/"
    if ($branch!='HEAD') {
      echo '<option'. ($branch=='master' ? ' selected' : '') .'>'.$branch.'</option>';
    }
  }
  echo "</select>&nbsp;&nbsp;\n";
}

function isNoComment($author)
{
  return !preg_match("/^--/", $author);
}

$authors = file('Authors.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$authors = array_filter($authors, "isNoComment");
echo "Authors that will be notified on nightly tests: ".implode(", ", $authors);

echo '<hr/></br><div style="font-size: 120%">';

$sentinelOutput = implode("<br/>\n", file('logs/SentinelOutput.txt'));
if ($sentinelIsRunning)
  echo "<span style=\"color: darkblue\">Tests are still running (this page is refreshed every $refreshInterval seconds to show the results).</span>";
else {
  echo 'Results of the last test run: ';
  $matches = array();
  $m1 = preg_match("/Total number of tests: ([0-9]+)/", $sentinelOutput, $matches1);
  $m2 = preg_match("/Number of failed tests: ([0-9]+)/", $sentinelOutput, $matches2);
  if (!$m1 || !$m2)
    echo "<span style=\"color: red\">Error, Sentinel terminated unexpectedly.</span>"; 
  else {
    $total = $matches1[1];
    $notPassed = $matches2[1];
    if ($notPassed == 0)
      echo "<span style=\"color: green\">All $total tests passed.</span>";
    else
      echo "<span style=\"color: red\">$notPassed out of $total tests did not pass.</span>";
  }
}
echo '<br/><br/></div>Sentinel output:<br/></br>'.$sentinelOutput;
?>
</body>
</html>
