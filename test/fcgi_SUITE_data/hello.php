<?php

header('Content-Type: text/plain;charset=UTF-8');

?>
<?php if ($_GET['name'] == 'Shinji'):?>
Shut up!
<?php else:?>
Hello <?php echo $_GET['name']?>!
<?php endif?>
