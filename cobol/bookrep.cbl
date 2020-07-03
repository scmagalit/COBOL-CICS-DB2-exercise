      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    BOOKREP.
       AUTHOR.        SAM MAGALIT.
       DATE-WRITTEN.  05/07/2020.
       SECURITY.      HIGHLY CONFIDENTIAL.
      *----------------------------------------------------------------*
      * PROGRAM TO GENERATE REPORT ON UPDATED BOOK DATABASE            *
      *----------------------------------------------------------------*
      * - FETCH ALL ROWS IN BOOK DATABASE                              *
      * - PROCESS BOOK INFO TO BE DISPLAYABLE IN REPORT FORMAT         *
      * - WRITE BOOK INFO IN REPORT FILE                               *
      *                                                                *
      * CHANGELOG:                                                     *
      * MAY 08,2020                                                    *
      *      0508RP - ADDED REPORT HEADERS/FOOTERS                     *
      * MAY 11,2020                                                    *
      *      0511SM - ADDED PAGE SUMMARY                               *
      *                                                                *
      * FILES:                                                         *
      * BKREPOP  (OUTPUT) - IBMUSER.SMAGALIT.BKREPOP                   *
      *                                                                *
      * 0000-MAIN                      2220-WRITE-INFO                 *
      * 1000-INIT                      2221-WRITE-REP-DATA             *
      * 1100-GET-TIMESTAMP             2300-WRITE-SUMMARY              *
      * 1200-OPEN-CURSOR               3000-CLEANUP                    *
      * 1300-OPEN-FILE                 3100-CLOSE-FILE                 *
      * 2000-MAIN-LOGIC                3200-CLOSE-CURSOR               *
      * 2100-WRITE-HEADERS             9999-ERROR-HANDLING             *
      * 2200-FETCH-CURSOR              9999-TERMINATE                  *
      * 2210-MOVE-TO-VARS                                              *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-BKREPOP  ASSIGN       TO    BKREPOP
                              FILE STATUS  IS FS-BKREPOP
                              ORGANIZATION IS SEQUENTIAL.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  FD-BKREPOP
           RECORDING MODE F
           RECORD CONTAINS 132 CHARACTERS.
       01  REC-BKREPOP                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-VARS.
           05  WS-BOOK-INFO.
               10  WS-BOOK-ID-NUM      PIC 9(008)     VALUE 0       .
               10  WS-BOOK-ID-TXT      REDEFINES WS-BOOK-ID-NUM
                                       PIC X(008)                   .
               10  FILLER              PIC XX         VALUE SPACES  .
               10  WS-TITLE            PIC X(076)     VALUE SPACES  .
               10  FILLER              PIC XX         VALUE SPACES  .
               10  WS-TOTAL-PAGES-NUM  PIC 9(004)     VALUE 0       .
               10  WS-TOTAL-PAGES-TXT  REDEFINES WS-TOTAL-PAGES-NUM
                                       PIC X(004)                   .
               10  FILLER              PIC XX         VALUE SPACES  .
               10  WS-RATING-TXT       PIC X(005)     VALUE SPACES  .
               10  WS-RATING-TXT-FMT   REDEFINES WS-RATING-TXT
                                       PIC 99.99                    .
               10  FILLER              PIC XX         VALUE SPACES  .
               10  WS-ISBN             PIC X(013)     VALUE SPACES  .
               10  FILLER              PIC XX         VALUE SPACES  .
               10  WS-PUBLISHED-DATE   PIC X(010)     VALUE SPACES  .
               10  FILLER              PIC XX         VALUE SPACES  .
               10  WS-PUBLISHER-ID-NUM PIC 9(004)     VALUE 0       .
               10  WS-PUBLISHER-ID-TXT REDEFINES WS-PUBLISHER-ID-NUM
                                       PIC X(004)                   .
           05  WS-FIX-FORMATTING.
               10  WS-RATING-NUM       PIC 99V99       VALUE 0     .
               10  WS-TITLE-FULL       PIC X(255)      VALUE SPACES.
           05  WS-NULL-INDICATORS.
               10  IND-TOTAL-PAGES     PIC S9(04) COMP VALUE 0.
               10  IND-RATING          PIC S9(04) COMP VALUE 0.
               10  IND-ISBN            PIC S9(04) COMP VALUE 0.
               10  IND-PUB-DATE        PIC S9(04) COMP VALUE 0.
               10  IND-PUB-ID          PIC S9(04) COMP VALUE 0.
           05  WS-COUNTERS.
               10  WS-REC-TOTAL-CNTR   PIC S9(04) COMP VALUE 0.
0511SM         10  WS-REC-PAGE-CNTR    PIC S9(04) COMP VALUE 0.
               10  WS-TITLE-LEN        PIC S9(04) COMP VALUE 0.
               10  WS-REP-LINE         PIC S9(04) COMP VALUE 0.
0508RP     05  WS-REP-VARS.
0508RP         10  WS-CUR-PAGE         PIC 9(04)            .
0508RP         10  WS-HDR-TITLE        PIC X(13)
0508RP                                 VALUE 'BOOK DATABASE'.
0511SM     05  WS-REP-MSG.
0511SM         10  FILLER               PIC X(053) VALUE SPACES.
0511SM         10  WS-REC-TXT           PIC XXXX   VALUE SPACES.
0511SM         10  WS-REC-NUM           REDEFINES WS-REC-TXT
0511SM                                  PIC 9999               .
0511SM         10  FILLER               PIC X      VALUE SPACES.
0511SM         10  WS-REC-MSG           PIC X(020) VALUE SPACES.
0511SM         10  FILLER               PIC X(054) VALUE SPACES.

       01  WS-SYS-VARS.
           05  WS-TIMESTAMP             PIC X(26)       VALUE SPACES.
0508RP     05  WS-TIMESTAMP-FMT         REDEFINES WS-TIMESTAMP.
0508RP         10  WS-DATE              PIC X(10).
0508RP         10  FILLER               PIC X    .
0508RP         10  WS-TIME              PIC X(08).
0508RP         10  FILLER               PIC X    .
0508RP         10  WS-MICROSEC          PIC X(06).
           05  WS-FILESTAT.
               10  FS-BKREPOP           PIC 99          VALUE 0.
           05  EVAL-CODE                PIC S9(08) COMP VALUE 0.
               88  ERR-OK                               VALUE 0.
               88  SQL-EOC                              VALUE 100.
0508RP     05  WS-PGM-NAME              PIC X(08)       VALUE 'BOOKREP'.

       01  WS-ERROR.
           05  FILLER                   PIC X(09)       VALUE 'ERR AT '.
           05  ERR-LOC                  PIC X(26)       VALUE SPACES   .
           05  FILLER                   PIC X(05)       VALUE ' RC: '  .
           05  ERR-CODE                 PIC X(08)       VALUE SPACES   .
           05  FILLER                   PIC X(06)       VALUE ' MSG: ' .
           05  ERR-MSG                  PIC X(26)       VALUE SPACES   .

0508RP COPY REPVARS.

      *** SQL COPYBOOKS
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLBOOKS END-EXEC.

           EXEC SQL DECLARE CURBOOK CURSOR FOR
                SELECT BOOK_ID
                      ,TITLE
                      ,TOTAL_PAGES
                      ,RATING
                      ,ISBN
                      ,PUBLISHED_DATE
                      ,PUBLISHER_ID
                  FROM IBMUSER.BOOKS
                  ORDER BY BOOK_ID
           END-EXEC.

      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN.
            MOVE '0000-MAIN' TO ERR-LOC

            PERFORM 1000-INIT
            PERFORM 2000-MAIN-LOGIC
            PERFORM 3000-CLEANUP
            .

       1000-INIT.
            MOVE '1000-INIT' TO ERR-LOC

            PERFORM 1100-GET-TIMESTAMP
            PERFORM 1200-OPEN-CURSOR
            PERFORM 1300-OPEN-FILE
0508RP      PERFORM 1400-SET-REPVARS
            .

       1100-GET-TIMESTAMP.
            MOVE '1100-GET-TIMESTAMP' TO ERR-LOC

            EXEC SQL
                 SELECT CURRENT TIMESTAMP
                   INTO :WS-TIMESTAMP
                   FROM SYSIBM.SYSDUMMY1
            END-EXEC
            MOVE SQLCODE TO EVAL-CODE

            IF ERR-OK
               DISPLAY WS-TIMESTAMP
            ELSE
               MOVE 'SELECT CURRENT TIMESTAMP' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            END-IF
            .

       1200-OPEN-CURSOR.
            MOVE '1200-OPEN-CURSOR' TO ERR-LOC

            EXEC SQL
                 OPEN CURBOOK
            END-EXEC
            MOVE SQLCODE TO EVAL-CODE

            IF NOT ERR-OK
               MOVE 'OPEN CURBOOK' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            END-IF
            .

       1300-OPEN-FILE.
            MOVE '1300-OPEN-FILE' TO ERR-LOC

            OPEN OUTPUT FD-BKREPOP
            MOVE FS-BKREPOP TO EVAL-CODE

            IF NOT ERR-OK
               MOVE 'OPEN INPUT FD-BKREPOP' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            END-IF
            .

0508RP 1400-SET-REPVARS.
0508RP      MOVE '1400-SET-REPVARS' TO ERR-LOC

0508RP      MOVE WS-PGM-NAME  TO WS-REP-PGM
0508RP      MOVE WS-HDR-TITLE TO WS-REP-TITLE(32:)
0508RP      MOVE WS-DATE      TO WS-REP-DATE

0508RP      INSPECT WS-TIME REPLACING ALL '.' BY ':'
0508RP      MOVE WS-TIME      TO WS-REP-TIME
0508RP      .

       2000-MAIN-LOGIC.
            MOVE '2000-MAIN-LOGIC' TO ERR-LOC

            INITIALIZE WS-COUNTERS

0508RP      PERFORM 2100-WRITE-HEADERS
            PERFORM 2200-FETCH-CURSOR UNTIL SQL-EOC

            DISPLAY 'SEE FULL REPORT AT IBMUSER.SMAGALIT.BKREPOP'

0511SM      PERFORM 2300-WRITE-SUMMARY
0508RP      WRITE REC-BKREPOP FROM WS-REP-FOOTER
            .

0508RP 2100-WRITE-HEADERS.
0508RP      MOVE '2100-WRITE-HEADERS' TO ERR-LOC

0508RP      ADD  1            TO WS-CUR-PAGE
0508RP      MOVE WS-CUR-PAGE  TO WS-REP-CURP

0508RP      WRITE REC-BKREPOP FROM WS-REP-HEADER1
0508RP      WRITE REC-BKREPOP FROM WS-REP-HEADER2
0508RP      WRITE REC-BKREPOP FROM WS-REP-SPACES
0508RP      .

       2200-FETCH-CURSOR.
            MOVE '2200-FETCH-CURSOR' TO ERR-LOC

            INITIALIZE WS-BOOK-INFO
                       DCLBOOKS

            EXEC SQL
                 FETCH CURBOOK
                  INTO :TBLBKS-BOOK-ID
                      ,:TBLBKS-TITLE
                      ,:TBLBKS-TOTAL-PAGES    :IND-TOTAL-PAGES
                      ,:TBLBKS-RATING         :IND-RATING
                      ,:TBLBKS-ISBN           :IND-ISBN
                      ,:TBLBKS-PUBLISHED-DATE :IND-PUB-DATE
                      ,:TBLBKS-PUBLISHER-ID   :IND-PUB-ID
            END-EXEC
            MOVE SQLCODE TO EVAL-CODE

            EVALUATE TRUE
                WHEN ERR-OK
                     ADD 1 TO WS-REC-TOTAL-CNTR
0511SM                        WS-REC-PAGE-CNTR
0508RP               PERFORM 2210-MOVE-TO-VARS
0508RP               PERFORM 2220-WRITE-INFO
                WHEN SQL-EOC
                     DISPLAY 'END OF FILE REACHED'
                     DISPLAY WS-REC-TOTAL-CNTR ' ROWS READ FROM TABLE'
                WHEN OTHER
                     MOVE 'FETCH CURBOOKS' TO ERR-MSG
                     PERFORM 9999-ERROR-HANDLING
            END-EVALUATE
            .

       2210-MOVE-TO-VARS.
            MOVE '2220-MOVE-TO-VARS' TO ERR-LOC

            MOVE TBLBKS-BOOK-ID           TO WS-BOOK-ID-NUM
            MOVE TBLBKS-TITLE-TEXT        TO WS-TITLE-FULL

            IF IND-TOTAL-PAGES = -1
               MOVE ALL '-'               TO WS-TOTAL-PAGES-TXT
            ELSE
               MOVE TBLBKS-TOTAL-PAGES    TO WS-TOTAL-PAGES-NUM
            END-IF

            IF IND-RATING      = -1
               MOVE ALL '-'               TO WS-RATING-TXT
            ELSE
               MOVE TBLBKS-RATING         TO WS-RATING-NUM
               MOVE WS-RATING-NUM         TO WS-RATING-TXT-FMT
            END-IF

            IF IND-ISBN        = -1
               MOVE ALL '-'               TO WS-ISBN
            ELSE
               MOVE TBLBKS-ISBN-TEXT      TO WS-ISBN
            END-IF

            IF IND-PUB-DATE    = -1
               MOVE ALL '-'               TO WS-PUBLISHED-DATE
            ELSE
               MOVE TBLBKS-PUBLISHED-DATE TO WS-PUBLISHED-DATE
            END-IF

            IF IND-PUB-ID      = -1
               MOVE ALL '-'               TO WS-PUBLISHER-ID-TXT
            ELSE
               MOVE TBLBKS-PUBLISHER-ID   TO WS-PUBLISHER-ID-NUM
            END-IF
            .

       2220-WRITE-INFO.
            MOVE '2220-MOVE-TO-VARS' TO ERR-LOC

            INITIALIZE REC-BKREPOP

            MOVE WS-TITLE-FULL      TO WS-TITLE
            MOVE LENGTH OF WS-TITLE TO WS-TITLE-LEN

0508RP      PERFORM 2221-WRITE-REP-DATA
0508RP      PERFORM 2222-WRITE-REMAINING-TITLE
0508RP        UNTIL WS-TITLE-LEN >= TBLBKS-TITLE-LEN
            .

0508RP 2221-WRITE-REP-DATA.
0508RP      MOVE '2221-WRITE-REP-DATA' TO ERR-LOC

0508RP      WRITE REC-BKREPOP FROM WS-BOOK-INFO
0508RP      ADD 1 TO WS-REP-LINE

0508RP      IF WS-REP-LINE = 30
0511SM         PERFORM 2300-WRITE-SUMMARY
0511SM         WRITE REC-BKREPOP FROM WS-REP-SPACES
0511SM         MOVE 0 TO WS-REC-PAGE-CNTR

0508RP         PERFORM 2100-WRITE-HEADERS
0508RP         MOVE 0 TO WS-REP-LINE
0508RP      END-IF
0508RP      .

0508RP 2222-WRITE-REMAINING-TITLE.
0508RP      MOVE '2222-WRITE-REMAINING-TITLE' TO ERR-LOC

            MOVE SPACES TO WS-BOOK-INFO
                           REC-BKREPOP

            MOVE WS-TITLE-FULL(WS-TITLE-LEN + 1:) TO WS-TITLE
            ADD  LENGTH OF WS-TITLE               TO WS-TITLE-LEN

0508RP      PERFORM 2221-WRITE-REP-DATA
0508RP      .

0511SM 2300-WRITE-SUMMARY.
0511SM      MOVE '3200-WRITE-SUMMARY' TO ERR-LOC

0511SM      WRITE REC-BKREPOP FROM WS-REP-SPACES

0511SM      MOVE  WS-REC-PAGE-CNTR       TO WS-REC-NUM
0511SM      MOVE  'RECORDS ON THIS PAGE' TO WS-REC-MSG
0511SM      WRITE REC-BKREPOP FROM WS-REP-MSG

0511SM      MOVE  WS-REC-TOTAL-CNTR      TO WS-REC-NUM
0511SM      MOVE  'TOTAL RECORDS READ  ' TO WS-REC-MSG
0511SM      WRITE REC-BKREPOP FROM WS-REP-MSG

0511SM      WRITE REC-BKREPOP FROM WS-REP-SPACES
0511SM      .

       3000-CLEANUP.
            MOVE '3000-CLEANUP' TO ERR-LOC

            PERFORM 3100-CLOSE-FILE
            PERFORM 3200-CLOSE-CURSOR

            PERFORM 9999-TERMINATE
            .

       3100-CLOSE-FILE.
            MOVE '3100-CLOSE-FILE' TO ERR-LOC

            CLOSE FD-BKREPOP
            MOVE FS-BKREPOP TO EVAL-CODE

            IF NOT ERR-OK
               MOVE 'CLOSE FD-BKREPOP' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            END-IF
            .

       3200-CLOSE-CURSOR.
            MOVE '3200-CLOSE-CURSOR' TO ERR-LOC

            EXEC SQL
                 CLOSE CURBOOK
            END-EXEC
            MOVE SQLCODE TO ERR-LOC

            IF NOT ERR-OK
               MOVE 'CLOSE CURBOOKS' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            END-IF
            .

       9999-ERROR-HANDLING.
            MOVE EVAL-CODE TO ERR-CODE
            DISPLAY WS-ERROR
            PERFORM 9999-TERMINATE
            .

       9999-TERMINATE.
            STOP RUN
            .
