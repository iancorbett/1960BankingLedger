       *> ledger_report.cob
       *> Usage:  ledger_report <input.csv> <output.txt>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER-REPORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO DYNAMIC infile-name
               ORGANIZATION LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO DYNAMIC outfile-name
               ORGANIZATION LINE SEQUENTIAL.

        DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  IN-REC                 PIC X(512).

       FD  OUTFILE.
       01  OUT-REC                PIC X(256).

        WORKING-STORAGE SECTION.
       77  infile-name            PIC X(256).
       77  outfile-name           PIC X(256).
       77  ARG-COUNT              PIC 9(4) COMP-5.
       77  WS-LINE                PIC X(512).
       77  WS-PTR                 PIC 9(4) COMP.
       77  FIELDS                 PIC 9(2) COMP.
       77  F-DATE                 PIC X(32).
       77  F-TYPE                 PIC X(8).
       77  F-AMOUNT-TXT           PIC X(32).
       77  F-MEMO                 PIC X(200).
       77  F-USERID               PIC X(64).
       77  AMOUNT                 PIC S9(9)V99 COMP-3.
       77  CUR-DATE               PIC X(32) VALUE SPACES.
       77  DAY-CREDIT             PIC S9(11)V99 COMP-3 VALUE 0.
       77  DAY-DEBIT              PIC S9(11)V99 COMP-3 VALUE 0.
       77  RUN-CREDIT             PIC S9(13)V99 COMP-3 VALUE 0.
       77  RUN-DEBIT              PIC S9(13)V99 COMP-3 VALUE 0.
       77  RUN-BAL                PIC S9(13)V99 COMP-3 VALUE 0.
       77  HEADER-WRITTEN         PIC X VALUE "N".

       01  SPACES-50              PIC X(50) VALUE ALL " ".
       01  DASH-80                PIC X(80) VALUE ALL "-".
       01  DASH-60                PIC X(60) VALUE ALL "-".

        PROCEDURE DIVISION.
        MAIN.
           ACCEPT ARG-COUNT FROM ARGUMENT-NUMBER
           IF ARG-COUNT NOT = 2
                DISPLAY "Usage: ledger_report <input.csv> <output.txt>"
                STOP RUN
           END-IF
           ACCEPT infile-name  FROM ARGUMENT-VALUE
           ACCEPT outfile-name FROM ARGUMENT-VALUE

           OPEN INPUT INFILE
                OUTPUT OUTFILE

           PERFORM WRITE-HEADER

           *> Skip header row
           READ INFILE
                AT END GO TO FINISH-REPORT
           END-READ

           PERFORM UNTIL 1 = 2
               READ INFILE
                   AT END EXIT PERFORM
               END-READ

               MOVE IN-REC TO WS-LINE
               PERFORM PARSE-CSV

               *> Convert amount text to numeric (signed cents with dot)
               MOVE 0 TO AMOUNT
               UNSTRING F-AMOUNT-TXT DELIMITED BY "."
                   INTO F-AMOUNT-TXT, SPACES
               END-UNSTRING
               INSPECT F-AMOUNT-TXT
                   REPLACING ALL "," BY ""
               *> Read signed value (e.g., -12.34 came as -12,34 already dot-stripped earlier)
               MOVE FUNCTION NUMVAL (F-AMOUNT-TXT) TO AMOUNT

               IF CUR-DATE NOT = F-DATE AND CUR-DATE NOT = SPACES
                   PERFORM FLUSH-DAY
               END-IF

               IF CUR-DATE = SPACES
                   MOVE F-DATE TO CUR-DATE
               END-IF

               IF F-TYPE = "Credit"
                   ADD AMOUNT TO DAY-CREDIT
                   ADD AMOUNT TO RUN-CREDIT
                   ADD AMOUNT TO RUN-BAL
               ELSE
                   ADD AMOUNT TO DAY-DEBIT
                   SUBTRACT AMOUNT FROM RUN-DEBIT *> AMOUNT will be negative for debit if you export signed
                   ADD AMOUNT TO RUN-BAL
               END-IF
           END-PERFORM

           IF CUR-DATE NOT = SPACES
               PERFORM FLUSH-DAY
           END-IF

       FINISH-REPORT.
           MOVE DASH-80 TO OUT-REC
           WRITE OUT-REC
           STRING
              "TOTAL CREDITS: ", FUNCTION TRIM(FUNCTION NUMVAL-C (RUN-CREDIT)), SPACES-50
              DELIMITED BY SIZE INTO OUT-REC
           END-STRING
           WRITE OUT-REC
           STRING
              "TOTAL DEBITS : ", FUNCTION TRIM(FUNCTION NUMVAL-C (RUN-DEBIT))
              DELIMITED BY SIZE INTO OUT-REC
           END-STRING
           WRITE OUT-REC
           STRING
              "ENDING BAL   : ", FUNCTION TRIM(FUNCTION NUMVAL-C (RUN-BAL))
              DELIMITED BY SIZE INTO OUT-REC
           END-STRING
           WRITE OUT-REC

           CLOSE INFILE OUTFILE
           GOBACK.


       WRITE-HEADER.
           IF HEADER-WRITTEN = "Y" EXIT PARAGRAPH END-IF
           MOVE "1960Ledger - Daily Statement" TO OUT-REC
           WRITE OUT-REC
           MOVE DASH-80 TO OUT-REC
           WRITE OUT-REC
           MOVE "Date        Credits        Debits         Day Net     Run Bal" TO OUT-REC
           WRITE OUT-REC
           MOVE DASH-80 TO OUT-REC
           WRITE OUT-REC
           MOVE "Y" TO HEADER-WRITTEN
           .