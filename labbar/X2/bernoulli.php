<?php
    // RUN FILE: 
    //          php bernoulli.php

    function B($n): float {
        $B = array();
        $B[0] = 1;

        for ($m = 1; $m <= $n; $m++) {
            $B[$m] = 0;
            for ($k = 0; $k <= $m-1; $k++) {
                $B[$m] = $B[$m] - binom($m+1, $k) * $B[$k];
            }
            $B[$m] = $B[$m]/($m+1);
        }        
        return $B[$n];
    }

    function binom($n, $k) {
        $r = 1;
        for ($i = 1; $i <= $k; $i++) {
            $r = $r * ($n - $i + 1)/$i;
        }
        return $r;
    }
    // Test binom: should output 10
    // echo binom(5, 2);
    // echo "\n";

    $res = B(4);
    echo "B(4) = ";
    echo $res;
    echo "\n";
?>