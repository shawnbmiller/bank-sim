*> Issuer bank host / authorization switch simulator.
*> Listens on TCP 8583, speaks ISO 8583 (0800/0810, 0200/0210).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKSRV.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO "ACCOUNTS.DAT"
              ORGANIZATION LINE SEQUENTIAL
              FILE STATUS IS FS.
       DATA DIVISION.
       FILE SECTION.
       FD ACCT-FILE.
       01 ACCT-REC.
          05 R-PAN        PIC X(16).
          05 R-PIN        PIC X(4).
          05 R-BAL        PIC 9(12).
          05 R-FAIL       PIC 9.
          05 R-STAT       PIC X.
          05 R-NAME       PIC X(20).
       WORKING-STORAGE SECTION.
       COPY ISOMSG.
       01 FS              PIC XX.
       01 ACCT-TABLE.
          05 ACCT OCCURS 50 TIMES.
             10 A-PAN     PIC X(16).
             10 A-PIN     PIC X(4).
             10 A-BAL     PIC 9(12).
             10 A-FAIL    PIC 9.
             10 A-STAT    PIC X.
             10 A-NAME    PIC X(20).
       01 ACCT-COUNT      PIC 99 VALUE 0.
       01 IDX             PIC 99.
       01 FOUND           PIC 99.
       01 LISTEN-FD       PIC S9(9) COMP-5.
       01 CONN-FD         PIC S9(9) COMP-5.
       01 NET-RC          PIC S9(9) COMP-5.
       01 PORT            PIC 9(5) VALUE 8583.
       01 BUF             PIC X(512).
       01 BUFLEN          PIC 9(4).
       01 CODEC-RC        PIC 9(2).
       01 WS-PIN          PIC X(16).
       01 WS-DUMMY        PIC X(40) VALUE SPACES.
       01 WS-ALEN         PIC 9(4) VALUE 16.
       01 PIN-IN          PIC X(512).
       01 RESP            PIC XX.
       01 AMT             PIC 9(12).
       01 NEWBAL          PIC 9(12).
       01 RRN             PIC 9(12) VALUE 0.
       01 PCODE           PIC XX.
       01 MAX-WD          PIC 9(12) VALUE 50000.
       01 DISP-AMT        PIC Z,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM LOAD-ACCOUNTS
           CALL "NETIO" USING "LSTN" LISTEN-FD PORT BUF BUFLEN
                NET-RC
           IF NET-RC < 0
              DISPLAY "BANK: cannot listen on port " PORT
              STOP RUN
           END-IF
           DISPLAY "BANK: host up, listening on 127.0.0.1:" PORT
           PERFORM FOREVER
              CALL "NETIO" USING "ACPT" LISTEN-FD PORT BUF BUFLEN
                   NET-RC
              IF NET-RC >= 0
                 MOVE NET-RC TO CONN-FD
                 DISPLAY "BANK: ATM connected"
                 PERFORM SERVE-CONNECTION
                 CALL "NETIO" USING "CLOS" CONN-FD PORT BUF BUFLEN
                      NET-RC
                 DISPLAY "BANK: ATM disconnected"
              END-IF
           END-PERFORM.

       SERVE-CONNECTION.
           MOVE 0 TO NET-RC
           PERFORM UNTIL NET-RC < 0
              CALL "NETIO" USING "RECV" CONN-FD PORT BUF BUFLEN NET-RC
              IF NET-RC >= 0
                 DISPLAY "BANK <- ATM  " BUF(1:BUFLEN)
                 CALL "ISOCODEC" USING "UNPACK" ISO-MSG BUF BUFLEN
                      CODEC-RC
                 CALL "ISOCODEC" USING "DUMP  " ISO-MSG BUF BUFLEN
                      CODEC-RC
                 PERFORM HANDLE-REQUEST
                 CALL "ISOCODEC" USING "PACK  " ISO-MSG BUF BUFLEN
                      CODEC-RC
                 DISPLAY "BANK -> ATM  " BUF(1:BUFLEN)
                 CALL "NETIO" USING "SEND" CONN-FD PORT BUF BUFLEN
                      NET-RC
              END-IF
           END-PERFORM.

       HANDLE-REQUEST.
           MOVE "96" TO RESP
           EVALUATE TRUE
              WHEN CODEC-RC = 1
                 MOVE "30" TO RESP
                 PERFORM BUILD-RESPONSE-HEAD
              WHEN CODEC-RC = 2
                 PERFORM BUILD-RESPONSE-HEAD
              WHEN ISO-MTI = "0800"
                 MOVE "00" TO RESP
                 MOVE "0810" TO ISO-MTI
                 MOVE "N" TO ISO-PRES(2) ISO-PRES(52)
                 MOVE RESP TO ISO-VAL(39)
                 MOVE "Y" TO ISO-PRES(39) ISO-PRES(64)
              WHEN ISO-MTI = "0200"
                 PERFORM AUTHORIZE
              WHEN OTHER
                 MOVE "40" TO RESP
                 PERFORM BUILD-RESPONSE-HEAD
           END-EVALUATE
           DISPLAY "BANK: response code " RESP.

       BUILD-RESPONSE-HEAD.
           MOVE "0210" TO ISO-MTI
           MOVE "N" TO ISO-PRES(2) ISO-PRES(52) ISO-PRES(54)
           MOVE RESP TO ISO-VAL(39)
           MOVE "Y" TO ISO-PRES(39) ISO-PRES(64).

       AUTHORIZE.
           MOVE 0 TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > ACCT-COUNT
              IF A-PAN(IDX) = ISO-VAL(2)(1:16) MOVE IDX TO FOUND END-IF
           END-PERFORM
           MOVE ISO-VAL(3)(1:2) TO PCODE
           MOVE ISO-VAL(4)(1:12) TO AMT
           MOVE ISO-VAL(52) TO PIN-IN
           EVALUATE TRUE
              WHEN FOUND = 0
                 MOVE "14" TO RESP
              WHEN A-STAT(FOUND) = "B"
                 MOVE "75" TO RESP
              WHEN OTHER
                 CALL "CRYPTOSM" USING "DECPIN" PIN-IN WS-ALEN
                      ISO-VAL(2) WS-PIN
                 IF WS-PIN(1:4) NOT = A-PIN(FOUND) OR WS-PIN(5:12)
                    NOT = SPACES
                    ADD 1 TO A-FAIL(FOUND)
                    MOVE "55" TO RESP
                    IF A-FAIL(FOUND) >= 3
                       MOVE "B" TO A-STAT(FOUND)
                       MOVE "75" TO RESP
                    END-IF
                    PERFORM SAVE-ACCOUNTS
                 ELSE
                    MOVE 0 TO A-FAIL(FOUND)
                    PERFORM PROCESS-TXN
                 END-IF
           END-EVALUATE
           MOVE "0210" TO ISO-MTI
           MOVE "N" TO ISO-PRES(52) ISO-PRES(54)
           MOVE "Y" TO ISO-PRES(39) ISO-PRES(37) ISO-PRES(64)
           MOVE RESP TO ISO-VAL(39)
           ADD 1 TO RRN
           MOVE RRN TO ISO-VAL(37)
           IF RESP = "00" AND FOUND > 0
              MOVE "Y" TO ISO-PRES(54)
              MOVE A-BAL(FOUND) TO NEWBAL
              STRING "1002840C" NEWBAL DELIMITED BY SIZE
                 INTO ISO-VAL(54)
           END-IF.

       PROCESS-TXN.
           EVALUATE PCODE
              WHEN "31"
                 MOVE "00" TO RESP
              WHEN "01"
                 EVALUATE TRUE
                    WHEN AMT > MAX-WD
                       MOVE "61" TO RESP
                    WHEN AMT > A-BAL(FOUND)
                       MOVE "51" TO RESP
                    WHEN OTHER
                       SUBTRACT AMT FROM A-BAL(FOUND)
                       MOVE "00" TO RESP
                       PERFORM SAVE-ACCOUNTS
                 END-EVALUATE
              WHEN "21"
                 ADD AMT TO A-BAL(FOUND)
                 MOVE "00" TO RESP
                 PERFORM SAVE-ACCOUNTS
              WHEN OTHER
                 MOVE "12" TO RESP
           END-EVALUATE.

       LOAD-ACCOUNTS.
           OPEN INPUT ACCT-FILE
           IF FS NOT = "00"
              PERFORM SEED-ACCOUNTS
              OPEN INPUT ACCT-FILE
           END-IF
           PERFORM UNTIL FS NOT = "00"
              READ ACCT-FILE
                 AT END MOVE "10" TO FS
                 NOT AT END
                    ADD 1 TO ACCT-COUNT
                    MOVE ACCT-REC TO ACCT(ACCT-COUNT)
              END-READ
           END-PERFORM
           CLOSE ACCT-FILE
           DISPLAY "BANK: loaded " ACCT-COUNT " accounts".

       SEED-ACCOUNTS.
           OPEN OUTPUT ACCT-FILE
           MOVE "4111111111111111123400000100000000ALICE ANDERSON"
              TO ACCT-REC
           WRITE ACCT-REC
           MOVE "5500000000000004482100000025050000BOB BAKER"
              TO ACCT-REC
           WRITE ACCT-REC
           CLOSE ACCT-FILE.

       SAVE-ACCOUNTS.
           OPEN OUTPUT ACCT-FILE
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > ACCT-COUNT
              MOVE ACCT(IDX) TO ACCT-REC
              WRITE ACCT-REC
           END-PERFORM
           CLOSE ACCT-FILE.
