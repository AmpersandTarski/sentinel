<?php
error_reporting(E_ALL);
ini_set("display_errors", 1);

$sentinelOutput = implode("<br/>\n", file('ampersand/SentinelOutput.txt'));
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
    refreshPageIn(5000);
});

function refreshPageIn (ms) {
  if (refreshTimer)
    clearTimeout(refreshTimer);

  refreshTimer = setTimeout( function () { window.location.reload()}, ms);
  // extra function() is necessary to make reload lazy
}

function compileSentinel() {
  clearTimeout(refreshTimer); // need to disable refresh timer, or the page with compiler output will not be shown.
  location.href='CompileSentinel.php';
}

function runSentinel() {
  $.ajax({ url: 'RunSentinel.php',
           cache: false,
           success: function(data){ refreshPageIn(250);
           // delay reload with 250ms to ensure sentinel process has started
         }
  });
}
  </script>
  
  </head>
<body>
  
  <p>
  <button onclick="compileSentinel()">Update & recompile Sentinel</button>
  (Only necessary if the Sentinel source has been modified. Note that no output will be shown until compilation has finished.)
  <p>
  <button onclick="runSentinel()">Run Sentinel</button> (<a href="/ampersand/">view generated prototypes</a>)
  <p>
  <p>testing...</p>
  <a href="logs/?C=M;O=A">View Sentinel logs</a>
<hr/>
<?php
function isNoComment($author)
{
  return !preg_match("/^--/", $author);
}

$authors = file('Authors.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$authors = array_filter($authors, "isNoComment");
echo "Authors that will be notified on nightly tests: ".implode(", ", $authors);

echo '<hr/></br><div style="font-size: 120%">';

$sentinelOutput = implode("<br/>\n", file('ampersand/SentinelOutput.txt'));
if ($sentinelIsRunning)
  echo '<span style="color: darkblue">Tests are still running (this page is refreshed every 5 seconds to show the results).</span>';
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
