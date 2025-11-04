       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-COPYBOOK.
       AUTHOR. COBGO.
       DATE-WRITTEN. 2024.
       
       * Customer record copybook
       
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID         PIC 9(10).
           05 CUSTOMER-NAME       PIC X(50).
           05 CUSTOMER-EMAIL      PIC X(100).
           05 CUSTOMER-BALANCE    PIC S9(15)V99.
           05 CUSTOMER-STATUS     PIC X(10).
           05 CUSTOMER-CREATED    PIC 9(8).
           05 CUSTOMER-ADDRESS.
              10 STREET           PIC X(30).
              10 CITY             PIC X(20).
              10 STATE            PIC X(2).
              10 ZIP-CODE         PIC X(10).
           05 CUSTOMER-PHONE      PIC X(15) OCCURS 3.
           05 CUSTOMER-NOTES      PIC X(200).
