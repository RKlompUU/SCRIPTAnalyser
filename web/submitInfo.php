<?php

$request = "139.153.253.238:3000/submitInfo?"
         . "name=" . urlencode($_GET["name"])
         . "&email=" . urlencode($_GET["email"])
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
  echo "Unfortunately, our backend is currently down. We would however very much appreciate it if you could retry entering your personal information again later.";
}
else
{
  echo "Thank you!";
}
