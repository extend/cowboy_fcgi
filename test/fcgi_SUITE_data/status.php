<?php

if ($_SERVER['REQUEST_METHOD'] == 'POST') {
	if (isset($_POST['god-location'], $_POST['world-state'])) {
		if ($_POST['god-location'] == 'heaven' && $_POST['world-state'] == 'ok')
			echo 'God\'s in his Heaven-- All\'s right with the world!';
			exit;
	}
}

header('Status: 500 Hell is coming on you');
