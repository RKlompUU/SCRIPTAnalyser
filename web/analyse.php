<?php

$request = "localhost:3000/analyse?output_script="
         . urlencode($_GET["output_script"])
         . "&name=" . urlencode($_GET["name"])
         . "&email=" . urlencode($_GET["email"])
         . "&verbosity=" . urlencode($_GET["verbosity"]);
echo $request;

$ch = curl_init($request);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$output=curl_exec($ch);
curl_close($ch);

echo $output;
