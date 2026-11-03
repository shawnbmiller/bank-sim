*> ISO 8583 (1987-style, ASCII) pack/unpack with primary bitmap
*> and trailing MAC (field 64). Ops: PACK, UNPACK, DUMP.
*> UNPACK RC: 0 ok, 1 format error, 2 MAC failure.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISOCODEC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INITED        PIC X VALUE "N".
       01 DEFS.
          05 DEF OCCURS 64 TIMES.
             10 DEF-TYPE PIC X VALUE " ".
             10 DEF-LEN  PIC 9(3) VALUE 0.
       01 HEXDIGITS     PIC X(16) VALUE "0123456789ABCDEF".
       01 BITS.
          05 BITF        PIC X OCCURS 64 TIMES.
       01 I             PIC 9(4).
       01 J             PIC 9(4).
       01 K             PIC 9(4).
       01 N             PIC 9(4).
       01 W             PIC 9(4).
       01 CPOS           PIC 9(4).
       01 FLEN          PIC 9(4).
       01 LL2           PIC 9(2).
       01 LL3           PIC 9(3).
       01 WS-MAC        PIC X(16).
       01 WS-DUMMY      PIC X(40) VALUE SPACES.
       01 WS-MAC-LEN    PIC 9(4).
       LINKAGE SECTION.
       01 LK-OP         PIC X(6).
       COPY ISOMSG.
       01 LK-BUF        PIC X(512).
       01 LK-LEN        PIC 9(4).
       01 LK-RC         PIC 9(2).
       PROCEDURE DIVISION USING LK-OP ISO-MSG LK-BUF LK-LEN LK-RC.
       MAIN.
           IF INITED = "N" PERFORM INIT-DEFS END-IF
           MOVE 0 TO LK-RC
           EVALUATE LK-OP
              WHEN "PACK  " PERFORM DO-PACK
              WHEN "UNPACK" PERFORM DO-UNPACK
              WHEN "DUMP  " PERFORM DO-DUMP
           END-EVALUATE
           GOBACK.

       INIT-DEFS.
           MOVE "L" TO DEF-TYPE(2)   MOVE 19 TO DEF-LEN(2)
           MOVE "F" TO DEF-TYPE(3)   MOVE 6  TO DEF-LEN(3)
           MOVE "F" TO DEF-TYPE(4)   MOVE 12 TO DEF-LEN(4)
           MOVE "F" TO DEF-TYPE(7)   MOVE 10 TO DEF-LEN(7)
           MOVE "F" TO DEF-TYPE(11)  MOVE 6  TO DEF-LEN(11)
           MOVE "F" TO DEF-TYPE(37)  MOVE 12 TO DEF-LEN(37)
           MOVE "F" TO DEF-TYPE(39)  MOVE 2  TO DEF-LEN(39)
           MOVE "F" TO DEF-TYPE(41)  MOVE 8  TO DEF-LEN(41)
           MOVE "F" TO DEF-TYPE(49)  MOVE 3  TO DEF-LEN(49)
           MOVE "F" TO DEF-TYPE(52)  MOVE 16 TO DEF-LEN(52)
           MOVE "T" TO DEF-TYPE(54)  MOVE 40 TO DEF-LEN(54)
           MOVE "F" TO DEF-TYPE(64)  MOVE 16 TO DEF-LEN(64)
           MOVE "Y" TO INITED.

       DO-PACK.
           MOVE SPACES TO LK-BUF
           MOVE ISO-MTI TO LK-BUF(1:4)
           MOVE "N" TO ISO-PRES(1)
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 16
              MOVE 0 TO N
              MOVE 8 TO W
              PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                 COMPUTE I = (K - 1) * 4 + J
                 IF ISO-PRES(I) = "Y" ADD W TO N END-IF
                 DIVIDE W BY 2 GIVING W
              END-PERFORM
              MOVE HEXDIGITS(N + 1:1) TO LK-BUF(4 + K:1)
           END-PERFORM
           MOVE 21 TO CPOS
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 63
              IF ISO-PRES(I) = "Y"
                 EVALUATE DEF-TYPE(I)
                    WHEN "F"
                       MOVE ISO-VAL(I)(1:DEF-LEN(I))
                         TO LK-BUF(CPOS:DEF-LEN(I))
                       ADD DEF-LEN(I) TO CPOS
                    WHEN "L"
                       COMPUTE FLEN = FUNCTION LENGTH(
                          FUNCTION TRIM(ISO-VAL(I) TRAILING))
                       MOVE FLEN TO LL2
                       MOVE LL2 TO LK-BUF(CPOS:2)
                       ADD 2 TO CPOS
                       MOVE ISO-VAL(I)(1:FLEN) TO LK-BUF(CPOS:FLEN)
                       ADD FLEN TO CPOS
                    WHEN "T"
                       COMPUTE FLEN = FUNCTION LENGTH(
                          FUNCTION TRIM(ISO-VAL(I) TRAILING))
                       MOVE FLEN TO LL3
                       MOVE LL3 TO LK-BUF(CPOS:3)
                       ADD 3 TO CPOS
                       MOVE ISO-VAL(I)(1:FLEN) TO LK-BUF(CPOS:FLEN)
                       ADD FLEN TO CPOS
                 END-EVALUATE
              END-IF
           END-PERFORM
           IF ISO-PRES(64) = "Y"
              COMPUTE WS-MAC-LEN = CPOS - 1
              CALL "CRYPTOSM" USING "MAC   " LK-BUF WS-MAC-LEN
                   WS-DUMMY WS-MAC
              MOVE WS-MAC TO LK-BUF(CPOS:16)
              MOVE WS-MAC TO ISO-VAL(64)
              ADD 16 TO CPOS
           END-IF
           COMPUTE LK-LEN = CPOS - 1.

       DO-UNPACK.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 64
              MOVE "N" TO ISO-PRES(I)
              MOVE 0 TO ISO-LEN(I)
              MOVE SPACES TO ISO-VAL(I)
           END-PERFORM
           IF LK-LEN < 20 MOVE 1 TO LK-RC GOBACK END-IF
           MOVE LK-BUF(1:4) TO ISO-MTI
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 16
              MOVE 0 TO N
              INSPECT HEXDIGITS TALLYING N FOR CHARACTERS
                 BEFORE INITIAL LK-BUF(4 + K:1)
              MOVE 8 TO W
              PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                 COMPUTE I = (K - 1) * 4 + J
                 IF N >= W
                    MOVE "Y" TO BITF(I)
                    SUBTRACT W FROM N
                 ELSE
                    MOVE "N" TO BITF(I)
                 END-IF
                 DIVIDE W BY 2 GIVING W
              END-PERFORM
           END-PERFORM
           MOVE 21 TO CPOS
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 64 OR LK-RC NOT = 0
              IF BITF(I) = "Y"
                 EVALUATE DEF-TYPE(I)
                    WHEN "F"
                       MOVE DEF-LEN(I) TO FLEN
                    WHEN "L"
                       IF CPOS + 1 > LK-LEN OR
                          LK-BUF(CPOS:2) IS NOT NUMERIC
                          MOVE 1 TO LK-RC
                       ELSE
                          MOVE LK-BUF(CPOS:2) TO LL2
                          MOVE LL2 TO FLEN
                          ADD 2 TO CPOS
                       END-IF
                    WHEN "T"
                       IF CPOS + 2 > LK-LEN OR
                          LK-BUF(CPOS:3) IS NOT NUMERIC
                          MOVE 1 TO LK-RC
                       ELSE
                          MOVE LK-BUF(CPOS:3) TO LL3
                          MOVE LL3 TO FLEN
                          ADD 3 TO CPOS
                       END-IF
                    WHEN OTHER
                       MOVE 1 TO LK-RC
                 END-EVALUATE
                 IF LK-RC = 0
                    IF FLEN = 0 OR FLEN > 40
                       OR CPOS + FLEN - 1 > LK-LEN
                       MOVE 1 TO LK-RC
                    ELSE
                       MOVE "Y" TO ISO-PRES(I)
                       MOVE FLEN TO ISO-LEN(I)
                       MOVE LK-BUF(CPOS:FLEN) TO ISO-VAL(I)
                       IF I = 64
                          COMPUTE WS-MAC-LEN = CPOS - 1
                          CALL "CRYPTOSM" USING "MAC   " LK-BUF
                               WS-MAC-LEN WS-DUMMY WS-MAC
                          IF WS-MAC NOT = LK-BUF(CPOS:16)
                             MOVE 2 TO LK-RC
                          END-IF
                       END-IF
                       ADD FLEN TO CPOS
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

       DO-DUMP.
           DISPLAY "   MTI " ISO-MTI
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 64
              IF ISO-PRES(I) = "Y"
                 EVALUATE I
                    WHEN 2
                       COMPUTE FLEN = FUNCTION LENGTH(
                          FUNCTION TRIM(ISO-VAL(I) TRAILING))
                       DISPLAY "   F" I " PAN        "
                          ISO-VAL(I)(1:6) "******"
                          ISO-VAL(I)(FLEN - 3:4)
                    WHEN 52
                       DISPLAY "   F" I " PIN BLOCK  [encrypted]"
                    WHEN 64
                       DISPLAY "   F" I " MAC        " ISO-VAL(I)(1:16)
                    WHEN OTHER
                       DISPLAY "   F" I " " ISO-VAL(I)(1:40)
                 END-EVALUATE
              END-IF
           END-PERFORM.
