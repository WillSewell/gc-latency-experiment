<?php

define('msgCount', 200000);
define('windowSize', 1000000);

$channel = [];
$worst = 0;

function mkMessage($highID)
{
    $m = [];
    for ($i = 0 ; $i < 1024 ; $i++) {
        $m[$i] = $highID;
    }

    return $m;
}

function pushMsg(&$channel, $highID) {
    global $worst;
    $start = microtime(true);
    $m = mkMessage($highID);
    $channel[$highID%windowSize] = $m;
    $elapsed = microtime(true) - $start;
    if ($elapsed > $worst) {
        $worst = $elapsed;
    }
}

for ($i = 0 ; $i < msgCount ; $i ++) {
    pushMsg($channel, $i);
}
echo ($worst * 1000) . "\n";
