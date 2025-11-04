       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-PROCESSING.
       AUTHOR. COBGO-ACCEPTANCE-TESTS.
       DATE-WRITTEN. 2024.
       
       * Order processing system with tax calculation
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ORDER-RECORD.
           05 ORDER-ID        PIC 9(10) VALUE 9876543210.
           05 CUSTOMER-ID     PIC 9(10) VALUE 1234567890.
           05 ORDER-DATE      PIC 9(8) VALUE 20241201.
           05 SUBTOTAL        PIC S9(10)V99 VALUE 1000.00.
           05 TAX-RATE        PIC 9V9999 VALUE 0.0875.
           05 TAX-AMOUNT      PIC S9(10)V99.
           05 TOTAL-AMOUNT    PIC S9(10)V99.
           05 ORDER-STATUS    PIC X(10) VALUE 'PENDING'.
       
       01 DISCOUNT-RATE       PIC 9V9999 VALUE 0.10.
       01 DISCOUNT-AMOUNT     PIC S9(10)V99.
       01 FINAL-AMOUNT        PIC S9(10)V99.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM CALCULATE-TAX
           PERFORM APPLY-DISCOUNT
           PERFORM FINALIZE-ORDER
           STOP RUN.
       
       CALCULATE-TAX.
           COMPUTE TAX-AMOUNT = SUBTOTAL * TAX-RATE
           COMPUTE TOTAL-AMOUNT = SUBTOTAL + TAX-AMOUNT
           
           DISPLAY 'Subtotal: $' SUBTOTAL
           DISPLAY 'Tax (' TAX-RATE '): $' TAX-AMOUNT
           DISPLAY 'Total with tax: $' TOTAL-AMOUNT.
       
       APPLY-DISCOUNT.
           COMPUTE DISCOUNT-AMOUNT = TOTAL-AMOUNT * DISCOUNT-RATE
           COMPUTE FINAL-AMOUNT = TOTAL-AMOUNT - DISCOUNT-AMOUNT
           
           DISPLAY 'Discount (' DISCOUNT-RATE '): $' DISCOUNT-AMOUNT
           DISPLAY 'Final amount: $' FINAL-AMOUNT.
       
       FINALIZE-ORDER.
           IF FINAL-AMOUNT > 0
               MOVE 'CONFIRMED' TO ORDER-STATUS
           ELSE
               MOVE 'ERROR' TO ORDER-STATUS
           END-IF
           
           DISPLAY 'Order ID: ' ORDER-ID
           DISPLAY 'Order Status: ' ORDER-STATUS
           DISPLAY 'Final Amount: $' FINAL-AMOUNT.
