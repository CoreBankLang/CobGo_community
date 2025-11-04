       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARITHMETIC-OPERATIONS.
       AUTHOR. COBGO-ACCEPTANCE-TESTS.
       DATE-WRITTEN. 2024.
       
       * Basic arithmetic operations
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIRST-NUMBER    PIC 9(5)V99 VALUE 123.45.
       01 SECOND-NUMBER   PIC 9(5)V99 VALUE 67.89.
       01 RESULT-ADD      PIC 9(6)V99.
       01 RESULT-SUB      PIC 9(6)V99.
       01 RESULT-MULT     PIC 9(8)V99.
       01 RESULT-DIV      PIC 9(6)V99.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           COMPUTE RESULT-ADD = FIRST-NUMBER + SECOND-NUMBER
           COMPUTE RESULT-SUB = FIRST-NUMBER - SECOND-NUMBER
           COMPUTE RESULT-MULT = FIRST-NUMBER * SECOND-NUMBER
           COMPUTE RESULT-DIV = FIRST-NUMBER / SECOND-NUMBER
           
           DISPLAY 'Addition: ' RESULT-ADD
           DISPLAY 'Subtraction: ' RESULT-SUB
           DISPLAY 'Multiplication: ' RESULT-MULT
           DISPLAY 'Division: ' RESULT-DIV
           
           STOP RUN.
