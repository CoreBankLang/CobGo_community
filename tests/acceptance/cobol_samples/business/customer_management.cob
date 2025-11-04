       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-MANAGEMENT.
       AUTHOR. COBGO-ACCEPTANCE-TESTS.
       DATE-WRITTEN. 2024.
       
       * Customer management system
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID     PIC 9(10) VALUE 1234567890.
           05 CUSTOMER-NAME   PIC X(50) VALUE 'John Doe'.
           05 CUSTOMER-EMAIL  PIC X(100) VALUE 'john.doe@example.com'.
           05 CUSTOMER-BALANCE PIC S9(15)V99 VALUE 1500.75.
           05 CUSTOMER-STATUS PIC X(10) VALUE 'ACTIVE'.
       
       01 TRANSACTION-AMOUNT  PIC S9(10)V99 VALUE 250.50.
       01 NEW-BALANCE         PIC S9(15)V99.
       01 DISPLAY-MESSAGE     PIC X(100).
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM DISPLAY-CUSTOMER-INFO
           PERFORM PROCESS-TRANSACTION
           PERFORM UPDATE-CUSTOMER-STATUS
           STOP RUN.
       
       DISPLAY-CUSTOMER-INFO.
           DISPLAY 'Customer ID: ' CUSTOMER-ID
           DISPLAY 'Customer Name: ' CUSTOMER-NAME
           DISPLAY 'Customer Email: ' CUSTOMER-EMAIL
           DISPLAY 'Current Balance: $' CUSTOMER-BALANCE
           DISPLAY 'Status: ' CUSTOMER-STATUS.
       
       PROCESS-TRANSACTION.
           COMPUTE NEW-BALANCE = CUSTOMER-BALANCE + TRANSACTION-AMOUNT
           MOVE NEW-BALANCE TO CUSTOMER-BALANCE
           
           STRING 'Transaction processed. New balance: $'
                  CUSTOMER-BALANCE
                  INTO DISPLAY-MESSAGE
           DISPLAY DISPLAY-MESSAGE.
       
       UPDATE-CUSTOMER-STATUS.
           IF CUSTOMER-BALANCE < 0
               MOVE 'OVERDRAFT' TO CUSTOMER-STATUS
           ELSE IF CUSTOMER-BALANCE > 10000
               MOVE 'PREMIUM' TO CUSTOMER-STATUS
           ELSE
               MOVE 'STANDARD' TO CUSTOMER-STATUS
           END-IF
           
           DISPLAY 'Updated status: ' CUSTOMER-STATUS.
