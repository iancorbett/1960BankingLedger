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
