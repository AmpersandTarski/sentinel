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
  <button onclick="location.href='CompileSentinel.php'">Recompile sentinel</button>
  Only necessary if the sentinel source has been modified. Note that no output is shown until compilation has finished.
  <p>
  <button onclick="runSentinel()">Run tests</button>
  Takes a couple of minutes before any output is shown. Follow the link on the next page and press refresh after a while.
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
<p style="color: blue">
<?php
$sentinelOutput = file('ampersand/SentinelOutput.txt');
if (!preg_match("/######## Sentinel exited/", $sentinelOutput))
  echo '<span style="color: red">Tests are currently running. Refresh this page to update the results.</span><br/><br/>';

echo implode("<br/>\n", $sentinelOutput);
?>
</p>
</body>
</html>