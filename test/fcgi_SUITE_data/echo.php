<?php

if ($_SERVER['REQUEST_METHOD'] == 'POST') {
	header('Content-Type: text/x-plain-and-boring;charset=UTF-8');
	readfile('php://input');
} else {
	header('Status: 405 Method Not Allowed');
}
