<?php

if (isset($_SERVER['PATH_INFO'])) {
  header('X-Path-Info: ' . $_SERVER['PATH_INFO']);
}

if (isset($_SERVER['PATH_TRANSLATED'])) {
  header('X-Path-Translated: ' . $_SERVER['PATH_TRANSLATED']);
}

?>
ok
