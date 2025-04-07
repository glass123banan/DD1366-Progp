       IDENTIFICATION DIVISION.
       PROGRAM-ID. bernoulli.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 k    PIC 9(2). 
       01 n    PIC 9(2) VALUE 9.
       01 m    PIC 9(2). 
       01 i    PIC 9(2).
       
       01 r    PIC S9(5)V9(5) VALUE 1.
       01 temp PIC S9(5)V9(5).
       
       01 b.
           05 B-item PIC S9(5)V9(5) OCCURS 10 TIMES INDEXED BY idx. 
       
       PROCEDURE DIVISION.
           PERFORM bernoulli
           STOP RUN.
       
       bernoulli.
           MOVE 1 TO B-item(1)  *> B_0 = 1
           DISPLAY "B(0) = " B-item(1)
           PERFORM VARYING m FROM 1 BY 1 UNTIL m > n
               MOVE 0 TO temp  *> Använd en temporär variabel för summering
               PERFORM VARYING k FROM 0 BY 1 UNTIL k = m
                   PERFORM binom
                   COMPUTE temp = temp + r * B-item(k + 1)
               END-PERFORM
               COMPUTE B-item(m + 1) = -temp / (m + 1)
               DISPLAY "B(" m ") = " B-item(m + 1)
           END-PERFORM.
       
       binom.
           MOVE 1 TO r
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > k
               COMPUTE r = r * (m + 1 - i + 1) / i *> 1-indexerat så m+1
           END-PERFORM
           EXIT.
