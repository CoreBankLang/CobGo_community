       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. COBGO-ACCEPTANCE-TESTS.
       DATE-WRITTEN. 2024.
       
       * Basic Hello World program
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING-MESSAGE PIC X(20) VALUE 'Hello, World!'.
       01 COUNTER          PIC 9(3)  VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY GREETING-MESSAGE
           ADD 1 TO COUNTER
           DISPLAY 'Counter: ' COUNTER
           STOP RUN.
