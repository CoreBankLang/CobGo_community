       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       AUTHOR. COBGO-EXAMPLES.
       DATE-WRITTEN. 2024-01-01.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5) VALUE 0.
       01 WS-NUM2 PIC 9(5) VALUE 0.
       01 WS-RESULT PIC 9(10) VALUE 0.
       01 WS-OPERATION PIC X(1) VALUE ' '.
       01 WS-CONTINUE PIC X(1) VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM UNTIL WS-CONTINUE = 'N'
               DISPLAY 'Enter first number: '
               ACCEPT WS-NUM1
               DISPLAY 'Enter second number: '
               ACCEPT WS-NUM2
               DISPLAY 'Enter operation (+, -, *, /): '
               ACCEPT WS-OPERATION
               
               EVALUATE WS-OPERATION
                   WHEN '+'
                       ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT
                   WHEN '-'
                       SUBTRACT WS-NUM2 FROM WS-NUM1 GIVING WS-RESULT
                   WHEN '*'
                       MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
                   WHEN '/'
                       DIVIDE WS-NUM1 BY WS-NUM2 GIVING WS-RESULT
                   WHEN OTHER
                       DISPLAY 'Invalid operation'
                       MOVE 0 TO WS-RESULT
               END-EVALUATE
               
               DISPLAY 'Result: ' WS-RESULT
               DISPLAY 'Continue? (Y/N): '
               ACCEPT WS-CONTINUE
           END-PERFORM
           
           DISPLAY 'Goodbye!'
           STOP RUN.