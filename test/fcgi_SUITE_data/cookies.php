<?php

if (isset($_GET['names'])) {
	foreach (explode(',', $_GET['names']) as $name) {
		if (isset($_COOKIE[$name])) {
			echo $_COOKIE[$name];
		}
		echo "\n";
	}
}
