*> TCP transport over libc sockets. Frames: 2-byte big-endian
*> length header + payload (as used on real ISO 8583 links).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NETIO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SOCKADDR.
          05 SA-FAMILY   PIC 9(4) COMP-5 VALUE 2.
          05 SA-PORT     PIC X(2).
          05 SA-ADDR     PIC X(4).
          05 SA-ZERO     PIC X(8) VALUE LOW-VALUES.
       01 WS-ONE         PIC S9(9) COMP-5 VALUE 1.
       01 WS-RC9         PIC S9(9) COMP-5.
       01 WS-RC18        PIC S9(18) COMP-5.
       01 WS-NULL        USAGE POINTER VALUE NULL.
       01 WS-HI          PIC 9(4).
       01 WS-LO          PIC 9(4).
       01 WS-OUT         PIC X(522).
       01 WS-RBUF        PIC X(522).
       01 WS-WANT        PIC 9(4).
       01 WS-GOT         PIC 9(4).
       01 WS-REM         PIC 9(4).
       01 WS-FLAGS       PIC S9(9) COMP-5 VALUE 16384.
       LINKAGE SECTION.
       01 LK-OP          PIC X(4).
       01 LK-FD          PIC S9(9) COMP-5.
       01 LK-PORT        PIC 9(5).
       01 LK-BUF         PIC X(512).
       01 LK-LEN         PIC 9(4).
       01 LK-RC          PIC S9(9) COMP-5.
       PROCEDURE DIVISION USING LK-OP LK-FD LK-PORT LK-BUF LK-LEN
                                LK-RC.
       MAIN.
           MOVE 0 TO LK-RC
           EVALUATE LK-OP
              WHEN "LSTN"  PERFORM DO-LISTEN
              WHEN "ACPT"  PERFORM DO-ACCEPT
              WHEN "CONN" PERFORM DO-CONNECT
              WHEN "SEND"    PERFORM DO-SEND
              WHEN "RECV"    PERFORM DO-RECV
              WHEN "CLOS"   CALL "close" USING BY VALUE LK-FD
           END-EVALUATE
           GOBACK.

       SET-ADDR.
           DIVIDE LK-PORT BY 256 GIVING WS-HI REMAINDER WS-LO
           MOVE FUNCTION CHAR(WS-HI + 1) TO SA-PORT(1:1)
           MOVE FUNCTION CHAR(WS-LO + 1) TO SA-PORT(2:1)
           MOVE FUNCTION CHAR(128) TO SA-ADDR(1:1)
           MOVE FUNCTION CHAR(1)   TO SA-ADDR(2:1)
           MOVE FUNCTION CHAR(1)   TO SA-ADDR(3:1)
           MOVE FUNCTION CHAR(2)   TO SA-ADDR(4:1).

       DO-LISTEN.
           PERFORM SET-ADDR
           CALL "socket" USING BY VALUE 2 BY VALUE 1 BY VALUE 0
                         RETURNING WS-RC9
           IF WS-RC9 < 0 MOVE -1 TO LK-RC GOBACK END-IF
           MOVE WS-RC9 TO LK-FD
           CALL "setsockopt" USING BY VALUE LK-FD BY VALUE 1
                BY VALUE 2 BY REFERENCE WS-ONE BY VALUE 4
                RETURNING WS-RC9
           CALL "bind" USING BY VALUE LK-FD BY REFERENCE SOCKADDR
                BY VALUE 16 RETURNING WS-RC9
           IF WS-RC9 < 0 MOVE -2 TO LK-RC GOBACK END-IF
           CALL "listen" USING BY VALUE LK-FD BY VALUE 5
                RETURNING WS-RC9
           IF WS-RC9 < 0 MOVE -3 TO LK-RC END-IF.

       DO-ACCEPT.
           CALL "accept" USING BY VALUE LK-FD BY VALUE WS-NULL
                BY VALUE WS-NULL RETURNING WS-RC9
           MOVE WS-RC9 TO LK-RC.

       DO-CONNECT.
           PERFORM SET-ADDR
           CALL "socket" USING BY VALUE 2 BY VALUE 1 BY VALUE 0
                         RETURNING WS-RC9
           IF WS-RC9 < 0 MOVE -1 TO LK-RC GOBACK END-IF
           MOVE WS-RC9 TO LK-FD
           CALL "connect" USING BY VALUE LK-FD BY REFERENCE SOCKADDR
                BY VALUE 16 RETURNING WS-RC9
           IF WS-RC9 < 0 MOVE -2 TO LK-RC END-IF.

       DO-SEND.
           DIVIDE LK-LEN BY 256 GIVING WS-HI REMAINDER WS-LO
           MOVE FUNCTION CHAR(WS-HI + 1) TO WS-OUT(1:1)
           MOVE FUNCTION CHAR(WS-LO + 1) TO WS-OUT(2:1)
           MOVE LK-BUF(1:LK-LEN) TO WS-OUT(3:LK-LEN)
           COMPUTE WS-WANT = LK-LEN + 2
           MOVE 0 TO WS-GOT
           PERFORM UNTIL WS-GOT >= WS-WANT OR LK-RC < 0
              COMPUTE WS-REM = WS-WANT - WS-GOT
              CALL "send" USING BY VALUE LK-FD
                   BY REFERENCE WS-OUT(WS-GOT + 1:WS-REM)
                   BY VALUE WS-REM BY VALUE WS-FLAGS
                   RETURNING WS-RC18
              IF WS-RC18 <= 0
                 MOVE -1 TO LK-RC
              ELSE
                 ADD WS-RC18 TO WS-GOT
              END-IF
           END-PERFORM.

       DO-RECV.
           MOVE 2 TO WS-WANT
           PERFORM READ-EXACT
           IF LK-RC < 0 GOBACK END-IF
           COMPUTE LK-LEN =
              (FUNCTION ORD(WS-RBUF(1:1)) - 1) * 256
              + FUNCTION ORD(WS-RBUF(2:1)) - 1
           IF LK-LEN > 512 OR LK-LEN = 0
              MOVE -2 TO LK-RC GOBACK
           END-IF
           MOVE LK-LEN TO WS-WANT
           PERFORM READ-EXACT
           IF LK-RC >= 0
              MOVE SPACES TO LK-BUF
              MOVE WS-RBUF(1:LK-LEN) TO LK-BUF(1:LK-LEN)
           END-IF.

       READ-EXACT.
           MOVE 0 TO WS-GOT
           PERFORM UNTIL WS-GOT >= WS-WANT OR LK-RC < 0
              COMPUTE WS-REM = WS-WANT - WS-GOT
              CALL "recv" USING BY VALUE LK-FD
                   BY REFERENCE WS-RBUF(WS-GOT + 1:WS-REM)
                   BY VALUE WS-REM BY VALUE 0
                   RETURNING WS-RC18
              IF WS-RC18 <= 0
                 MOVE -1 TO LK-RC
              ELSE
                 ADD WS-RC18 TO WS-GOT
              END-IF
           END-PERFORM.
