*> ATM terminal simulator. Connects to the bank over TCP and
*> exchanges ISO 8583 messages: 0800 sign-on, 0200 financial.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ISOMSG.
       01 SOCK-FD         PIC S9(9) COMP-5.
       01 NET-RC          PIC S9(9) COMP-5.
       01 PORT            PIC 9(5) VALUE 8583.
       01 BUF             PIC X(512).
       01 BUFLEN          PIC 9(4).
       01 CODEC-RC        PIC 9(2).
       01 STAN            PIC 9(6) VALUE 0.
       01 TERM-ID         PIC X(8) VALUE "ATM00001".
       01 WS-CD           PIC X(21).
       01 CARD            PIC X(19).
       01 PIN             PIC X(12).
       01 CHOICE          PIC X.
       01 AMT-IN          PIC 9(7).
       01 AMT-CENTS       PIC 9(12).
       01 PCODE           PIC X(6).
       01 PINBLK          PIC X(16).
       01 PIN-WORK        PIC X(512).
       01 WS-ALEN         PIC 9(4) VALUE 12.
       01 DONE-FLAG       PIC X VALUE "N".
       01 DISP-AMT        PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 BAL-CENTS       PIC 9(12).
       01 WS-DUMMY        PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN.
           CALL "NETIO" USING "CONN" SOCK-FD PORT BUF BUFLEN NET-RC
           IF NET-RC < 0
              DISPLAY "ATM: host unreachable - out of service"
              STOP RUN
           END-IF
           PERFORM SIGN-ON
           PERFORM UNTIL DONE-FLAG = "Y"
              PERFORM CARD-SESSION
           END-PERFORM
           CALL "NETIO" USING "CLOS" SOCK-FD PORT BUF BUFLEN NET-RC
           STOP RUN.

       PREPARE-HEAD.
           PERFORM VARYING NET-RC FROM 1 BY 1 UNTIL NET-RC > 64
              MOVE "N" TO ISO-PRES(NET-RC)
              MOVE SPACES TO ISO-VAL(NET-RC)
           END-PERFORM
           MOVE FUNCTION CURRENT-DATE TO WS-CD
           ADD 1 TO STAN
           MOVE WS-CD(5:10) TO ISO-VAL(7)
           MOVE STAN TO ISO-VAL(11)
           MOVE TERM-ID TO ISO-VAL(41)
           MOVE "Y" TO ISO-PRES(7) ISO-PRES(11) ISO-PRES(41)
                       ISO-PRES(64).

       EXCHANGE.
           CALL "ISOCODEC" USING "PACK  " ISO-MSG BUF BUFLEN CODEC-RC
           DISPLAY "ATM  -> BANK " BUF(1:BUFLEN)
           CALL "ISOCODEC" USING "DUMP  " ISO-MSG BUF BUFLEN CODEC-RC
           CALL "NETIO" USING "SEND" SOCK-FD PORT BUF BUFLEN NET-RC
           IF NET-RC >= 0
              CALL "NETIO" USING "RECV" SOCK-FD PORT BUF BUFLEN NET-RC
           END-IF
           IF NET-RC < 0
              DISPLAY "ATM: communication failure"
              MOVE "Y" TO DONE-FLAG
              MOVE "91" TO ISO-VAL(39)
              MOVE "Y" TO ISO-PRES(39)
           ELSE
              DISPLAY "ATM <- BANK  " BUF(1:BUFLEN)
              CALL "ISOCODEC" USING "UNPACK" ISO-MSG BUF BUFLEN
                   CODEC-RC
              CALL "ISOCODEC" USING "DUMP  " ISO-MSG BUF BUFLEN
                   CODEC-RC
              IF CODEC-RC NOT = 0
                 MOVE "96" TO ISO-VAL(39)
                 MOVE "Y" TO ISO-PRES(39)
              END-IF
           END-IF.

       SIGN-ON.
           PERFORM PREPARE-HEAD
           MOVE "0800" TO ISO-MTI
           MOVE "001000" TO ISO-VAL(3)
           MOVE "Y" TO ISO-PRES(3)
           PERFORM EXCHANGE
           IF ISO-VAL(39)(1:2) = "00"
              DISPLAY "ATM: signed on to host"
           ELSE
              DISPLAY "ATM: sign-on rejected"
              STOP RUN
           END-IF.

       CARD-SESSION.
           DISPLAY " "
           DISPLAY "=== WELCOME - insert card (PAN, blank to quit) ==="
           ACCEPT CARD
           IF CARD = SPACES
              MOVE "Y" TO DONE-FLAG
           ELSE
              DISPLAY "Enter PIN:"
              ACCEPT PIN
              MOVE SPACES TO CHOICE
              PERFORM UNTIL CHOICE = "4" OR DONE-FLAG = "Y"
                 DISPLAY "1) Balance  2) Withdraw  3) Deposit  4) Exit"
                 ACCEPT CHOICE
                 EVALUATE CHOICE
                    WHEN "1" PERFORM DO-BALANCE
                    WHEN "2" PERFORM DO-WITHDRAW
                    WHEN "3" PERFORM DO-DEPOSIT
                    WHEN "4" DISPLAY "Please take your card."
                    WHEN OTHER DISPLAY "Invalid choice"
                 END-EVALUATE
              END-PERFORM
           END-IF.

       DO-BALANCE.
           MOVE "310000" TO PCODE
           MOVE 0 TO AMT-CENTS
           PERFORM FINANCIAL.

       DO-WITHDRAW.
           DISPLAY "Whole-dollar amount:"
           ACCEPT AMT-IN
           COMPUTE AMT-CENTS = AMT-IN * 100
           MOVE "010000" TO PCODE
           PERFORM FINANCIAL.

       DO-DEPOSIT.
           DISPLAY "Whole-dollar amount:"
           ACCEPT AMT-IN
           COMPUTE AMT-CENTS = AMT-IN * 100
           MOVE "210000" TO PCODE
           PERFORM FINANCIAL.

       FINANCIAL.
           PERFORM PREPARE-HEAD
           MOVE "0200" TO ISO-MTI
           MOVE CARD TO ISO-VAL(2)
           MOVE PCODE TO ISO-VAL(3)
           MOVE AMT-CENTS TO ISO-VAL(4)
           MOVE "840" TO ISO-VAL(49)
           MOVE PIN TO PIN-WORK
           CALL "CRYPTOSM" USING "ENCPIN" PIN-WORK WS-ALEN
                ISO-VAL(2) PINBLK
           MOVE PINBLK TO ISO-VAL(52)
           MOVE "Y" TO ISO-PRES(2) ISO-PRES(3) ISO-PRES(4)
                       ISO-PRES(49) ISO-PRES(52)
           PERFORM EXCHANGE
           EVALUATE ISO-VAL(39)(1:2)
              WHEN "00"
                 DISPLAY "APPROVED  (RRN " ISO-VAL(37)(1:12) ")"
                 IF ISO-PRES(54) = "Y"
                    MOVE ISO-VAL(54)(9:12) TO BAL-CENTS
                    COMPUTE DISP-AMT = BAL-CENTS / 100
                    DISPLAY "Available balance: $" DISP-AMT
                 END-IF
              WHEN "14" DISPLAY "DECLINED: invalid card"
              WHEN "51" DISPLAY "DECLINED: insufficient funds"
              WHEN "55" DISPLAY "DECLINED: incorrect PIN"
                 MOVE "4" TO CHOICE
              WHEN "61" DISPLAY "DECLINED: exceeds withdrawal limit"
              WHEN "75" DISPLAY "DECLINED: PIN tries exceeded, card blocked"
                 MOVE "4" TO CHOICE
              WHEN "91" DISPLAY "DECLINED: issuer unavailable"
              WHEN OTHER DISPLAY "DECLINED: code " ISO-VAL(39)(1:2)
           END-EVALUATE.
