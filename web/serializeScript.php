<?php

if ($_SERVER['SERVER_NAME'] === 'www.cs.stir.ac.uk')
{
  header("Location: https://vm100.cs.stir.ac.uk/~rkl/serializeScript.php?" . $_SERVER['QUERY_STRING']);
  die();
}

$request = "139.153.253.238:3000/serialize?script="
         . urlencode($_GET["script"])
         . "&ip=" . urlencode($_SERVER["REMOTE_ADDR"]);
#echo $request;

$ch = curl_init($request);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch,CURLOPT_FAILONERROR,true);
$output=curl_exec($ch);
if (curl_error($ch))
{
    $error_msg = curl_error($ch);
}
curl_close($ch);

if (isset($error_msg))
{
  echo "Sorry, SCRIPT Analyser is temporarily disabled";
}
else
{
  echo $output;

  echo $explanation;
}
