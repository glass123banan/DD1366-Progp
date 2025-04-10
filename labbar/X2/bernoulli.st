"RUN FILE: 
         gst bernoulli.st"

" bernoulli-func that returns nth bernoulli number"
b := [:n |
    | b | "lokala variabler finns här, i detta fall listan b"
    b := OrderedCollection with: 1.  "dynamnic array -> B[0] = 1, DOCK 1-indexerat"

    " from 1-n, each m should do... "
    1 to: n do: [:m |
        b add: 0.  "add B[m]=0 to dynamic array"

        " from 0-(m-1), each k should do... "
        0 to: m - 1 do: [:k |
            " b at: m+1 betyder att man kommer åt index m+1 i arrayen b "
            " put: lägger det uträknade värdet i b(m+1)"
            " NOTERA att m+1 eftersom array 1-indexerat! "
            b at: m + 1 put: (b at: m + 1) - ((binom value: m + 1 value: k) * (b at: k + 1)). "calculate B[m]"
        ].
        b at: m + 1 put: (b at: m + 1) / (m + 1). "divide with (m+1) and store at B[m]"
    ].
    b at: n + 1 "return index n+1 due to 1-indexing"
].

" binom func, return binomial of two numbers "
binom := [:n :k |
    | r | "local variable r"
    r := 1.

    "for loop from 1 to k (input parameter)"
    1 to: k do: [:i |
        r := r * (n-i+1)/i "calculate new r"
    ].
    " return r "
    r
].

" Print function for first 10 Bernoulli numbers "
printB := [
    0 to: 9 do: [:i |
        " Print: B(i)=b(i) "
        Transcript 
            show: 'B(';
            show: i asString; "asString so its printable"
            show: ') = ';
            show: (b value: i) asString; 
            cr.
    ].
].

" Main execution -> call printB to print out 10 bernoulli numbers"
printB value.



" result := binom value:5 value:2.
Transcript show: 'binom(5,2) = ', result asString; cr. "

" result := b value:4.
Transcript show: result asString; cr. "