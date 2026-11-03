*> ISO 8583 message image shared by ATM, bank and codec.
       01 ISO-MSG.
          05 ISO-MTI              PIC X(4).
          05 ISO-FLD OCCURS 64 TIMES.
             10 ISO-PRES          PIC X.
             10 ISO-LEN           PIC 9(3).
             10 ISO-VAL           PIC X(40).
