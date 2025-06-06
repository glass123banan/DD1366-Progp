<?php
session_start();
session_destroy(); // blir av med kaka nom nom
header('Location: login.php');
exit;