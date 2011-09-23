<?php

if (isset($_GET['name'])) {
	$param = 'HTTP_' . str_replace('-', '_', strtoupper($_GET['name']));
	if (isset($_SERVER[$param])) {
		echo $_SERVER[$param];
		exit;
	}
}

header('Status: 400 Bad Request');
