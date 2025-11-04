       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-COPYBOOK.
       AUTHOR. COBGO.
       DATE-WRITTEN. 2024.
       
       * Order record copybook
       
       01 ORDER-RECORD.
           05 ORDER-ID            PIC 9(10).
           05 CUSTOMER-ID         PIC 9(10).
           05 ORDER-DATE          PIC 9(8).
           05 ORDER-STATUS        PIC X(10).
           05 ORDER-TOTAL         PIC S9(15)V99.
           05 ORDER-ITEMS.
              10 ITEM-COUNT       PIC 9(3).
              10 ORDER-ITEM       OCCURS 50.
                 15 ITEM-ID       PIC 9(10).
                 15 ITEM-NAME     PIC X(50).
                 15 ITEM-QTY      PIC 9(5).
                 15 ITEM-PRICE    PIC S9(7)V99.
                 15 ITEM-TOTAL    PIC S9(9)V99.
           05 ORDER-SHIPPING.
              10 SHIP-METHOD      PIC X(20).
              10 SHIP-ADDRESS     PIC X(100).
              10 SHIP-DATE        PIC 9(8).
           05 ORDER-NOTES         PIC X(500).
