       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT-COPYBOOK.
       AUTHOR. COBGO.
       DATE-WRITTEN. 2024.
       
       * Payment record copybook with redefines
       
       01 PAYMENT-RECORD.
           05 PAYMENT-ID          PIC 9(10).
           05 ORDER-ID            PIC 9(10).
           05 PAYMENT-TYPE        PIC X(1).
           05 PAYMENT-AMOUNT      PIC S9(10)V99.
           05 CASH-PAYMENT        REDEFINES PAYMENT-AMOUNT PIC S9(10)V99.
           05 CHECK-PAYMENT.
              10 CHECK-NUMBER     PIC 9(10).
              10 CHECK-AMOUNT     PIC S9(10)V99.
              10 CHECK-DATE       PIC 9(8).
           05 CREDIT-PAYMENT.
              10 CARD-NUMBER      PIC X(20).
              10 CARD-TYPE        PIC X(10).
              10 EXPIRY-DATE      PIC X(5).
              10 CARD-AMOUNT      PIC S9(10)V99.
           05 PAYMENT-DATE        PIC 9(8).
           05 PAYMENT-STATUS      PIC X(10).
           05 PAYMENT-NOTES       PIC X(200).
