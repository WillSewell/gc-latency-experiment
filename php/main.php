<?php

define('msgCount', 200000);
define('windowSize', 1000000);

$channel = [];
$worst = 0;

function mkMessage($highID)
{
    return str_repeat(pack("C", $highID), 1024);
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
