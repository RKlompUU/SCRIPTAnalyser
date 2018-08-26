<?php

$explanation = <<<XYZ
<br>
<h2>Explanation of output</h2>
<p>(Note: this only describes the default verbosity output mode)</p>
<p>For every unique branch of the supplied output SCRIPT, the tool will print a verdict under a distinct “— Symbolic evaluation report of execution branch <em>i</em>” section</p>
<p>In this section, the following information is printed:</p>
<ul>
<li>The respective branch’s decision points are shown (i.e. for every encountered IF operation, is its True branch traversed or is its False branch traversed?)</li>
<li>The symbolic stack that the input script must supply</li>
<li>The inferred constraints (on these supplied variables)</li>
<li>The final resulting symbolic stack (note, at this point we have already performed an additional OP_VERIFY, thus the constraint that the resulting full execution of this branch must end with a true value on the stack is already in the inferred constraints set)</li>
</ul>
XYZ;

$request = "localhost:3000/analyse?output_script="
         . urlencode($_GET["output_script"])
         . "&name=" . urlencode($_GET["name"])
         . "&email=" . urlencode($_GET["email"])
         . "&verbosity=" . urlencode($_GET["verbosity"]);
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
