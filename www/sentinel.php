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
echo "Authors that will be notified: ".implode(", ", $authors);
?>
<hr/>
<?php
$sentinelOutput = implode("<br/>\n", file('ampersand/SentinelOutput.txt'));
if (!preg_match("/######## Sentinel exited/", $sentinelOutput))
  echo '<span style="color: red">Tests are still running, refresh this page to update the results.</span><br/><br/>';
else
  echo 'Results of the latest test run.';
?>
<p style="color: blue">
<?php
echo $sentinelOutput;
?>
</p>
</body>
</html>