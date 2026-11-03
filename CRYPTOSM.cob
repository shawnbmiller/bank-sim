*> Simulated security module (HSM stand-in).
*> PIN block: ISO 9564 format 0, XORed with the shared terminal
*> PIN key. MAC: keyed checksum standing in for ANSI X9.19.
*> Toy algorithms - NOT real cryptography.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRYPTOSM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 HEXDIGITS    PIC X(16) VALUE "0123456789ABCDEF".
       01 TPK          PIC X(16) VALUE "A1B2C3D4E5F60718".
       01 MAC-K1       PIC 9(18) COMP-5 VALUE 40503.
       01 MAC-K2       PIC 9(18) COMP-5 VALUE 98765.
       01 H1           PIC 9(18) COMP-5.
       01 H2           PIC 9(18) COMP-5.
       01 I            PIC 9(4).
       01 N            PIC 9(4).
       01 C            PIC X.
       01 X1           PIC X(16).
       01 X2           PIC X(16).
       01 X3           PIC X(16).
       01 CLEARPIN     PIC X(16).
       01 PANBLK       PIC X(16).
       01 PLEN         PIC 9(4).
       01 NA           PIC 9(4).
       01 NB           PIC 9(4).
       01 NR           PIC 9(4).
       01 W            PIC 9(4).
       01 TMPH         PIC 9(18) COMP-5.
       LINKAGE SECTION.
       01 LK-OP        PIC X(6).
       01 LK-A         PIC X(512).
       01 LK-ALEN      PIC 9(4).
       01 LK-B         PIC X(40).
       01 LK-OUT       PIC X(16).
       PROCEDURE DIVISION USING LK-OP LK-A LK-ALEN LK-B LK-OUT.
       MAIN.
           EVALUATE LK-OP
              WHEN "MAC   " PERFORM DO-MAC
              WHEN "ENCPIN" PERFORM DO-ENCPIN
              WHEN "DECPIN" PERFORM DO-DECPIN
           END-EVALUATE
           GOBACK.

       DO-MAC.
           MOVE 7919 TO H1
           MOVE 104729 TO H2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-ALEN
              COMPUTE H1 = FUNCTION MOD(H1 * 131
                 + FUNCTION ORD(LK-A(I:1)) + MAC-K1, 4294967291)
              COMPUTE H2 = FUNCTION MOD(H2 * 137 + H1
                 + MAC-K2, 4294967279)
           END-PERFORM
           MOVE H1 TO TMPH
           PERFORM VARYING I FROM 8 BY -1 UNTIL I < 1
              DIVIDE TMPH BY 16 GIVING TMPH REMAINDER N
              MOVE HEXDIGITS(N + 1:1) TO LK-OUT(I:1)
           END-PERFORM
           MOVE H2 TO TMPH
           PERFORM VARYING I FROM 16 BY -1 UNTIL I < 9
              DIVIDE TMPH BY 16 GIVING TMPH REMAINDER N
              MOVE HEXDIGITS(N + 1:1) TO LK-OUT(I:1)
           END-PERFORM.

*> LK-A = clear PIN (4-12 digits), LK-B = PAN
       DO-ENCPIN.
           MOVE ALL "F" TO CLEARPIN
           MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-A)) TO PLEN
           MOVE "0" TO CLEARPIN(1:1)
           MOVE HEXDIGITS(PLEN + 1:1) TO CLEARPIN(2:1)
           MOVE LK-A(1:PLEN) TO CLEARPIN(3:PLEN)
           PERFORM BUILD-PANBLK
           MOVE CLEARPIN TO X1
           MOVE PANBLK TO X2
           PERFORM XOR-HEX
           MOVE X3 TO X1
           MOVE TPK TO X2
           PERFORM XOR-HEX
           MOVE X3 TO LK-OUT.

*> LK-A = encrypted block, LK-B = PAN; LK-OUT = PIN or spaces
       DO-DECPIN.
           PERFORM BUILD-PANBLK
           MOVE LK-A(1:16) TO X1
           MOVE TPK TO X2
           PERFORM XOR-HEX
           MOVE X3 TO X1
           MOVE PANBLK TO X2
           PERFORM XOR-HEX
           MOVE SPACES TO LK-OUT
           MOVE 0 TO PLEN
           IF X3(1:1) = "0"
              MOVE X3(2:1) TO C
              MOVE 0 TO N
              INSPECT HEXDIGITS TALLYING N FOR CHARACTERS
                 BEFORE INITIAL C
              MOVE N TO PLEN
              IF PLEN >= 4 AND PLEN <= 12
                 AND X3(3:PLEN) IS NUMERIC
                 MOVE X3(3:PLEN) TO LK-OUT(1:PLEN)
              END-IF
           END-IF.

       BUILD-PANBLK.
           MOVE "0000000000000000" TO PANBLK
           MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-B)) TO NA
           IF NA > 12
              MOVE LK-B(NA - 12:12) TO PANBLK(5:12)
           END-IF.

       XOR-HEX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 16
              MOVE 0 TO NA
              INSPECT HEXDIGITS TALLYING NA FOR CHARACTERS
                 BEFORE INITIAL X1(I:1)
              MOVE 0 TO NB
              INSPECT HEXDIGITS TALLYING NB FOR CHARACTERS
                 BEFORE INITIAL X2(I:1)
              MOVE 0 TO NR
              MOVE 1 TO W
              PERFORM 4 TIMES
                 IF FUNCTION MOD(NA, 2) NOT = FUNCTION MOD(NB, 2)
                    ADD W TO NR
                 END-IF
                 DIVIDE NA BY 2 GIVING NA
                 DIVIDE NB BY 2 GIVING NB
                 MULTIPLY 2 BY W
              END-PERFORM
              MOVE HEXDIGITS(NR + 1:1) TO X3(I:1)
           END-PERFORM.
