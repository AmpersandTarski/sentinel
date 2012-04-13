<?php
error_reporting(E_ALL);
ini_set("display_errors", 1);
?>
<html>
  <head>
  <title>Ampersand Sentinel</title>
  
  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"></script>
  <script type="text/javascript">
function runSentinel() {
  $.ajax({ url: 'RunSentinel.php',
           cache: false,
           success: function(data){ window.location.reload(); }
  });
}
  </script>
  
  </head>
<body>
  
  <p>
  <button onclick="location.href='CompileSentinel.php'">Recompile Sentinel</button>
  (Only necessary if the Sentinel source has been modified. Note that no output will be shown until compilation has finished.)
  <p>
  <button onclick="runSentinel()">Run Sentinel</button> (<a href="/www/ampersand/">view generated prototypes</a>)
  <p>
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
if (!preg_match("/######## Sentinel exited/", $sentinelOutput))
  echo '<span style="color: darkblue">Tests are still running, refresh this page to update the results.</span>';
else {
  echo 'Results of the last test run: ';
  $matches = array();
  preg_match("/Total number of tests: ([0-9]+)/", $sentinelOutput, $matches);
  $total = $matches[1];
  preg_match("/Number of failed tests: ([0-9]+)/", $sentinelOutput, $matches);
  $notPassed = $matches[1];
  if ($notPassed == 0)
    echo "<span style=\"color: green\">All $total tests passed.</span>";
  else
    echo "<span style=\"color: red\">$notPassed out of $total tests did not pass.</span>";
}
echo '<br/><br/></div>Sentinel output:<br/></br>'.$sentinelOutput;
?>
</body>
</html>