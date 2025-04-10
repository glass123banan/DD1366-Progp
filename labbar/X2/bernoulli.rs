// COMPILE & RUN FILE: 
//          rustc bernoulli.rs
//          ./bernoulli

// Function for binomial
// Parameters: 
//              n is of type 16 bit unsigned
//              k -||-
fn binom(n: u16, k: u16) -> f64{
    let mut r = 1.0; // set r as muteable 

    // For loop from 1 to k
    for i in 1..= k {
        // use "as f64" so täljare and nämnare is of same datatype (float 64bit)
        r = r * (n-i+1) as f64/i as f64;        //  r · (n − i + 1)/i
    }
    // return the value of r after loop
    return r;
}

// Function for calculating nth bernoulli number
// Parameter: n is of type unsigned integer of 16bit
// Return-type: float 64bit     
fn B(n: u16) -> f64 {
    // mut -> gör att variabeln är muteable och kan ändras, innehåller f64 elems
    // vektorn initieras med (n+1) st 0.0 elems
    let mut b: Vec<f64> = vec![0.0; (n + 1) as usize]; // Create a vector to store bernoulli values
    
    b[0] = 1.0; // sätt B0 = 1
    
    // for loop från 1 till n
    for m in 1..=n {
        b[m as usize] = 0.0; // "as usize" ser till att typen är rätt
        // for loop från 0 till (m-1)
        for k in 0..=m-1 {
            b[m as usize] -= binom(m + 1, k) * b[k as usize]; // B[m] = B[m] - binom(m+1, k) * B[k]
        }
        b[m as usize] /= (m + 1) as f64; // B[m] = B[m] / (m+1)
    }
    
    return b[n as usize] // Return B[n]
}

fn main() {
    // let test = binom(5, 2);
    // println!("{}", test);

    // let bernoulli = B(1); 
    // println!("B(4) = {}", bernoulli);

    // print first 10 bernoulli numbers
    for n in 0..10 {
        let nr = B(n); // current bernoulli number
        println!("B({}) = {:.6}", n, nr); // print B(n) = nr with 6 decimals
    }
}