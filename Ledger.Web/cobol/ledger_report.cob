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

