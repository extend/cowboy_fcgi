<?php

header('Content-Type: text/plain');

?>
<?php if ($_GET['name'] == 'Shinji'):?>
Shut up!
<?php else:?>
Hello <?php echo $_GET['name']?>!
<?php endif?>
