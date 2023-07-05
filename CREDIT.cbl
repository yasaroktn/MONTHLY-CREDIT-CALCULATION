       IDENTIFICATION DIVISION.
      *----
       PROGRAM-ID.    CREDIT.
       AUTHOR.        YASAR OKTEN.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *----
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRT-LINE ASSIGN TO PRTLINE
                           STATUS CHECK-PRT-LINE.
           SELECT ACCT-REC ASSIGN TO ACCTREC
                           STATUS CHECK-ACCT-REC.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *----
       FILE SECTION.
      *----
       FD  PRT-LINE RECORDING MODE F.
       01  PRINT-VARIABLES.
           05 ACCOUNT-NO-O     PIC X(8).
           05 FILLER           PIC X(02) VALUE SPACES.
           05 LAST-NAME-O      PIC X(15).
           05 FILLER           PIC X(02) VALUE SPACES.
           05 DEBIT-O          PIC 9(8).
           05 FILLER           PIC X(02) VALUE SPACES.
           05 START-DATE-O     PIC 9(8).
           05 FILLER           PIC X(02) VALUE SPACES.
           05 FINISH-DATE-O    PIC 9(8).
           05 FILLER           PIC X(02) VALUE SPACES.
           05 MONTHLY-PAY-O    PIC 9(8).
           05 FILLER           PIC X(02) VALUE SPACES.
           05 INSTALLMENTS-O   PIC 9(03).
           05 FILLER           PIC X(02) VALUE SPACES.
      *-----------------------------------------------------------------
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-VARIABLES.
           05 ACCOUNT-NO       PIC X(8).
           05 LAST-NAME        PIC X(15).
           05 DEBIT            PIC 9(8).
           05 START-DATE       PIC 9(8).
           05 FINISH-DATE      PIC 9(8).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *----
       01  INT-DATES.
           05 INT-START-DATE  PIC 9(08).
           05 INT-FINISH-DATE PIC 9(08).
      *----
       01  CHECK-POINTS.
           05 CHECK-PRT-LINE  PIC 9(2).
           05 CHECK-ACCT-REC  PIC 9(2).
      *----
       01  FLAG.
           05 LASTREC         PIC X VALUE SPACE.
      *----
       01  HEADER-1.
           05 FILLER         PIC X(24) VALUE 'CREDIT PAYMENT STATEMENT'.
           05 FILLER         PIC X VALUE SPACES.
      *----
       01  HEADER-2.
           05 FILLER        PIC X(07)  VALUE 'ACCOUNT'.
           05 FILLER        PIC X(03) VALUE SPACES.
           05 FILLER        PIC X(09) VALUE 'LAST NAME'.
           05 FILLER        PIC X(08) VALUE SPACES.
           05 FILLER        PIC X(05) VALUE 'DEBIT'.
           05 FILLER        PIC X(05) VALUE SPACES.
           05 FILLER        PIC X(08) VALUE 'STARTING'.
           05 FILLER        PIC X(02) VALUE SPACES.
           05 FILLER        PIC X(06) VALUE 'FINISH'.
           05 FILLER        PIC X(04) VALUE SPACES.
           05 FILLER        PIC X(07) VALUE 'MNT PAY'.
           05 FILLER        PIC X(03) VALUE SPACES.
           05 FILLER        PIC X(03) VALUE 'MNT'.
           05 FILLER        PIC X(20) VALUE SPACES.
      *----
       01  HEADER-3.
           05 FILLER        PIC X(08) VALUE '________'.
           05 FILLER        PIC X(02) VALUE SPACES.
           05 FILLER        PIC X(15) VALUE '_______________'.
           05 FILLER        PIC X(02) VALUE SPACES.
           05 FILLER        PIC X(08) VALUE '________'.
           05 FILLER        PIC X(02) VALUE SPACES.
           05 FILLER        PIC X(08) VALUE '________'.
           05 FILLER        PIC X(02) VALUE SPACES.
           05 FILLER        PIC X(08) VALUE '________'.
           05 FILLER        PIC X(02) VALUE SPACES.
           05 FILLER        PIC X(08) VALUE '________'.
           05 FILLER        PIC X(02) VALUE SPACES.
           05 FILLER        PIC X(03) VALUE '___'.
           05 FILLER        PIC X(20) VALUE SPACES.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----
       OPEN-FILES.
           OPEN INPUT ACCT-REC.
           OPEN OUTPUT PRT-LINE.
           PERFORM OPEN-CONTROLS.
       OPEN-FILES-END. EXIT.
      *----
       OPEN-CONTROLS.
           IF (CHECK-ACCT-REC NOT EQUAL 97) AND
      -       (CHECK-ACCT-REC NOT EQUAL 00)
              DISPLAY 'INPUT FILE CANNOT OPEN' CHECK-ACCT-REC
              PERFORM CLOSE-FILES
           END-IF.
           IF (CHECK-PRT-LINE NOT EQUAL 97) AND
      -       (CHECK-PRT-LINE NOT EQUAL 00)
              DISPLAY 'OUTPUT FILE CANNOT OPEN' CHECK-PRT-LINE
              PERFORM CLOSE-FILES
           END-IF.
       OPEN-END.EXIT.
      *----
       WRITE-HEADERS.
           WRITE PRINT-VARIABLES FROM HEADER-1.
           MOVE SPACES TO PRINT-VARIABLES.
           WRITE PRINT-VARIABLES AFTER ADVANCING 1 LINES.
           WRITE PRINT-VARIABLES FROM HEADER-2.
           WRITE PRINT-VARIABLES FROM HEADER-3.
           WRITE PRINT-VARIABLES AFTER ADVANCING 1 LINES.
           MOVE SPACES TO PRINT-VARIABLES.
       WRITE-END. EXIT.
      *----
       MAIN.
           PERFORM READ-RECORD.
           PERFORM UNTIL LASTREC = 'Y'
              PERFORM WRITE-RECORD
              PERFORM READ-RECORD
           END-PERFORM.

       READ-RECORD.
           READ ACCT-REC
              AT END MOVE 'Y' TO LASTREC
           END-READ.
       READ-END. EXIT.
      *----
       WRITE-RECORD.
           MOVE  ACCOUNT-NO   TO ACCOUNT-NO-O.
           MOVE  LAST-NAME    TO LAST-NAME-O.
           MOVE  DEBIT        TO DEBIT-O.
           MOVE  START-DATE   TO START-DATE-O.
           MOVE  FINISH-DATE  TO FINISH-DATE-O.
           PERFORM TOTAL-INSTALLMENTS.
           PERFORM MONTHLY-PAYMENT.
           WRITE PRINT-VARIABLES.
       WRITE-END. EXIT.
      *----
       TOTAL-INSTALLMENTS.
           COMPUTE INT-START-DATE=FUNCTION INTEGER-OF-DATE(START-DATE).
           COMPUTE INT-FINISH-DATE=FUNCTION INTEGER-OF-DATE
      -                            (FINISH-DATE).
           COMPUTE INSTALLMENTS-O=(INT-FINISH-DATE - INT-START-DATE)/30.
       TOTAL-END. EXIT.
      *----
       MONTHLY-PAYMENT.
           COMPUTE MONTHLY-PAY-O = DEBIT / INSTALLMENTS-O.
       MONTHLY-END. EXIT.
      *----
       CLOSE-FILES.
           CLOSE ACCT-REC.
           CLOSE PRT-LINE.
           GOBACK.
       CLOSE-END. EXIT.
