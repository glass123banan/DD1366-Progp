<?php
session_start(); // cookie typ 🍪

// Check if user is logged in in this session
if (isset($_SESSION['user_id'])) {
    // User is logged in, redirect to shopping list
    header('Location: src/shopping_list.php'); // byt url
    exit;
} else {
    // User is not logged in, redirect to login
    header('Location: src/login.php');
    exit;
}
?>