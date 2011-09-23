<?php

$host = $_SERVER['SERVER_NAME'] . ':' . $_SERVER['SERVER_PORT'];
header('Location: http://' . $host . '/ping');
echo 'Ping!', "\n";