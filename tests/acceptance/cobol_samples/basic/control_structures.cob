       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROL-STRUCTURES.
       AUTHOR. COBGO-ACCEPTANCE-TESTS.
       DATE-WRITTEN. 2024.
       
       * Control structures: IF-THEN-ELSE and PERFORM
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-AGE        PIC 9(3) VALUE 25.
       01 USER-STATUS     PIC X(10).
       01 LOOP-COUNTER    PIC 9(3) VALUE 0.
       01 MAX-LOOPS       PIC 9(3) VALUE 5.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM AGE-CLASSIFICATION
           PERFORM LOOP-EXAMPLE
           STOP RUN.
       
       AGE-CLASSIFICATION.
           IF USER-AGE < 18
               MOVE 'MINOR' TO USER-STATUS
           ELSE IF USER-AGE < 65
               MOVE 'ADULT' TO USER-STATUS
           ELSE
               MOVE 'SENIOR' TO USER-STATUS
           END-IF
           
           DISPLAY 'Age: ' USER-AGE ' Status: ' USER-STATUS.
       
       LOOP-EXAMPLE.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
               UNTIL LOOP-COUNTER > MAX-LOOPS
               DISPLAY 'Loop iteration: ' LOOP-COUNTER
           END-PERFORM.
