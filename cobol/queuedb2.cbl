      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    QUEUEDB2.
       AUTHOR.        SAM MAGALIT.
       DATE-WRITTEN.  04/21/2020.
       SECURITY.      HIGHLY CONFIDENTIAL.
      *----------------------------------------------------------------*
      * SUBPROGRAM TO LOAD BOOKS DATABASE INTO TSQ                     *
      *----------------------------------------------------------------*
      * - SET OF 15 BOOKS = 1 PAGE = 1 TSQ ENTRY                       *
      * - QUEUE NAME PASSED FROM MAIN TRANSACTION (T1DB)               *
      * - ONLY T1DB IS ALLOWED TO CALL THIS PROGRAM                    *
      * - TSQS ARE DELETED ON STARTUP                                  *
      * - REBUILD FLAG TO REFRESH QUEUE                                *
      * - SEARCH QUEUE FOR PAGING IN SEARCH MODE                       *
      *                                                                *
      * CHANGELOG:                                                     *
      * APRIL 22,2020 - CHANGED CODE TO COBOL 2               (0422C2) *
      *               - FETCH DATA FROM DB2 INSTEAD OF VSAM   (0422DB) *
      * APRIL 23,2020 - ADDED QUEUE FOR SEARCH RESULTS        (0423SQ) *
      * APRIL 24,2020 - EDIT MAP AESTHETICS                   (0424MP) *
      * APRIL 27,2020 - FLAG TO REBUILD MAIN QUEUE            (0427RQ) *
      * APRIL 27,2020 - CASE INSENSITIVE SEARCH QUERY         (0427UC) *
      *               - DELETE RECORDS                        (0427DE) *
      *               - UPDATE RECORDS                        (0427UP) *
      * APRIL 30,2020 - ADD RECORD                            (0430AD) *
      * MAY   15,2020 - LINK TO SUBPGM FOR JCL BATCH REPORT   (0515RP) *
      *                                                                *
      * PARAGRAPHS:                                                    *
      * 0000-MAIN                      1400-WRITE-TO-SRCH-QUEUE        *
      * 1000-CREATE-TSQ                1410-FETCH-SRCH-ROWS            *
      * 1100-DELETE-TSQ                1500-CLOSE-CURSOR               *
      * 1200-OPEN-CURSOR               9999-ERROR-HANDLING             *
      * 1300-WRITE-TO-MAIN-QUEUE       9999-TERMINATE                  *
      * 1310-FETCH-MAIN-ROWS                                           *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *--------------------
      *-------------
       DATA DIVISION.
      *-------------
       WORKING-STORAGE SECTION.
       01  WS-VARS.
           05  WS-COUNTERS.
               10  BK-IDX           PIC S9(04) COMP   VALUE 1.

       01  WS-CONST-VARS.
           05  WS-TRNIDS.
               10  WS-LIST-TRNID    PIC X(04)         VALUE 'T1DB'.
               10  WS-INFO-TRNID    PIC X(04)         VALUE 'T2DB'.
           05  WS-PGMIDS.
               10  WS-CBKQ-PGMID    PIC X(08)         VALUE 'QUEUEDB2'.
           05  WS-VSAMIDS.
               10 WS-BKS-VSAM-NAME  PIC X(08)         VALUE 'BOOKS'.
0423SQ     05  WS-DUMMY-PERCENT     PIC X             VALUE '%'.
           05  WS-TBL-PAGE.
               10  WS-TBL-BOOK
               OCCURS 15 TIMES.
                   15  WS-TBL-ID    PIC 9(008).
0424MP             15  WS-TBL-TITLE PIC X(062).

       01  WS-TRANS-VARS.
           05  WS-SEND-MSG          PIC  X(80)        VALUE SPACES.
0422DB     05  EVAL-CODE            PIC S9(08) COMP.
0422DB         88  ERR-OK                             VALUE 0.
0422DB         88  SQL-EOC                            VALUE 100.

       01  WS-ERROR.
           05  FILLER               PIC X(09)         VALUE 'ERROR AT '.
           05  ERR-LOC              PIC X(32)         VALUE SPACES     .
           05  FILLER               PIC X(05)         VALUE ' RC: '    .
           05  ERR-CODE             PIC X(08)         VALUE SPACES     .
           05  FILLER               PIC X(06)         VALUE ' MSG: '   .
           05  ERR-MSG              PIC X(20)         VALUE SPACES     .

       COPY DFHAID.

       01  WS-COMMAREA.
           05  WS-PG-NUM            PIC S9(04) COMP   VALUE 1.
           05  WS-TOTAL-PG          PIC S9(04) COMP   VALUE 1.
           05  WS-SEL-NUM           PIC S9(04) COMP   VALUE 1.
           05  WS-TOTAL-SEL         PIC S9(04) COMP   VALUE 1.
0423SQ     05  WS-SRCH-NUM          PIC S9(04) COMP   VALUE 1.
0423SQ     05  WS-TOTAL-SRCH        PIC S9(04) COMP   VALUE 1.
0427DE     05  WS-DEL-NUM           PIC S9(04) COMP   VALUE 1.
0427DE     05  WS-TOTAL-DEL         PIC S9(04) COMP   VALUE 1.
0427UP     05  WS-UPD-NUM           PIC S9(04) COMP   VALUE 1.
0427UP     05  WS-TOTAL-UPD         PIC S9(04) COMP   VALUE 1.
           05  WS-SEARCH-STR        PIC  X(58)        VALUE SPACES.
               88  NOSEARCH                           VALUE SPACES.
           05  WS-PAGE-QUEUE-NAME.
               10  WS-PQ-TRNID      PIC X(04)         VALUE 'T1DB'.
               10  WS-PQ-TRMID      PIC X(04)         VALUE 'L702'.
           05  WS-SEL-QUEUE-NAME.
               10  WS-SL-TRNID      PIC X(04)         VALUE 'SELQ'.
               10  WS-SL-TRMID      PIC X(04)         VALUE 'L702'.
0423SQ     05  WS-SRCH-QUEUE-NAME.
0423SQ         10  WS-SR-TRNID      PIC X(04)         VALUE 'T1SQ'.
0423SQ         10  WS-SR-TRMID      PIC X(04)         VALUE 'L702'.
0427DE     05  WS-DEL-QUEUE-NAME.
0427DE         10  WS-DL-TRNID      PIC X(04)         VALUE 'DELQ'.
0427DE         10  WS-DL-TRMID      PIC X(04)         VALUE 'L702'.
0427UP     05  WS-UPD-QUEUE-NAME.
0427UP         10  WS-UP-TRNID      PIC X(04)         VALUE 'UPDQ'.
0427UP         10  WS-UP-TRMID      PIC X(04)         VALUE 'L702'.
0427RQ     05  WS-REBUILD-SW        PIC 9             VALUE 0.
0427RQ         88  REBUILD                            VALUE 1.
0430AD     05  WS-ADD-RECORD-SW     PIC 9             VALUE 0.
0430AD         88  ADD-RECORD                         VALUE 1.
0515RP     05  WS-RJCL-PGMID        PIC X(08)         VALUE 'CICSRJCL'.

0422DB     EXEC SQL INCLUDE SQLCA END-EXEC.
0422DB     EXEC SQL INCLUDE DCLBOOKS END-EXEC.

0422DB     EXEC SQL DECLARE CURBOOKS CURSOR FOR
0422DB          SELECT BOOK_ID
0422DB                ,TITLE
0422DB            FROM IBMUSER.BOOKS
0430AD           ORDER BY BOOK_ID
0422DB     END-EXEC.

0423SQ     EXEC SQL DECLARE CURSEARCH CURSOR FOR
0423SQ          SELECT BOOK_ID
0423SQ                ,TITLE
0423SQ            FROM IBMUSER.BOOKS
0427UC           WHERE UPPER(TITLE)
0427UC                 LIKE :WS-DUMMY-PERCENT
0427UC                   || RTRIM(LTRIM(UPPER(:WS-SEARCH-STR)))
0427UC                   || :WS-DUMMY-PERCENT
0430AD           ORDER BY BOOK_ID
0423SQ     END-EXEC.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  LS-PG-NUM            PIC S9(04) COMP.
           05  LS-TOTAL-PG          PIC S9(04) COMP.
           05  LS-SEL-NUM           PIC S9(04) COMP.
           05  LS-TOTAL-SEL         PIC S9(04) COMP.
0423SQ     05  LS-SRCH-NUM          PIC S9(04) COMP.
0423SQ     05  LS-TOTAL-SRCH        PIC S9(04) COMP.
0427DE     05  LS-DEL-NUM           PIC S9(04) COMP.
0427DE     05  LS-TOTAL-DEL         PIC S9(04) COMP.
0427UP     05  LS-UPD-NUM           PIC S9(04) COMP.
0427UP     05  LS-TOTAL-UPD         PIC S9(04) COMP.
           05  LS-SEARCH-STR        PIC  X(58).
           05  LS-PAGE-QUEUE-NAME   PIC X(08).
           05  LS-SEL-QUEUE-NAME    PIC X(08).
0423SQ     05  LS-SRCH-QUEUE-NAME   PIC X(08).
0427DE     05  LS-DEL-QUEUE-NAME    PIC X(08).
0427UP     05  LS-UPD-QUEUE-NAME    PIC X(08).
0427RQ     05  LS-REBUILD-SW        PIC 9.
0430AD     05  LS-ADD-RECORD-SW     PIC 9.
0515RP     05  WS-RJCL-PGMID        PIC X(08).

      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN.
            MOVE '0000-MAIN' TO ERR-LOC

            INITIALIZE WS-TBL-PAGE

            IF EIBCALEN = 0
               MOVE 'CALL FROM TERMINAL NOT ALLOWED' TO WS-SEND-MSG
               PERFORM 9999-TERMINATE
            ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA

               IF EIBTRNID = WS-LIST-TRNID OR WS-INFO-TRNID
                  MOVE 'LOADING DATA TO QUEUE...' TO WS-SEND-MSG

                  EXEC CICS
                       SEND TEXT
                            FROM   (WS-SEND-MSG)
                            ERASE
                            FREEKB
                  END-EXEC

                  PERFORM 1000-CREATE-TSQ
               ELSE
                  MOVE 'INVALID CALLEE TRANSACTION' TO WS-SEND-MSG
                  PERFORM 9999-TERMINATE
0422C2         END-IF
0422C2      END-IF

            MOVE WS-COMMAREA TO DFHCOMMAREA

            EXEC CICS
                 RETURN
            END-EXEC
            .

       1000-CREATE-TSQ.
            MOVE '1000-CREATE-TSQ' TO ERR-LOC

            PERFORM 1100-DELETE-TSQ
            PERFORM 1200-OPEN-CURSOR

0427RQ      IF REBUILD
               PERFORM 1300-WRITE-TO-MAIN-QUEUE UNTIL SQL-EOC
0427RQ      END-IF

            INITIALIZE EVAL-CODE
0427RQ      IF NOT NOSEARCH
               PERFORM 1400-WRITE-TO-SRCH-QUEUE UNTIL SQL-EOC
0427RQ      END-IF

            PERFORM 1500-CLOSE-CURSOR
            .

       1100-DELETE-TSQ.
            MOVE '1100-DELETE-TSQ' TO ERR-LOC

0427RQ      IF REBUILD
               EXEC CICS
                    DELETEQ TS
                            QUEUE (WS-PAGE-QUEUE-NAME)
                            RESP  (EVAL-CODE)
               END-EXEC
0427RQ      END-IF

0427RQ      IF NOT NOSEARCH
0423SQ         EXEC CICS
0423SQ              DELETEQ TS
0423SQ                      QUEUE (WS-SRCH-QUEUE-NAME)
0423SQ                      RESP  (EVAL-CODE)
0423SQ         END-EXEC
0423SQ      END-IF
            .

       1200-OPEN-CURSOR.
            MOVE '1200-OPEN-CURSOR' TO ERR-LOC

0427RQ      IF REBUILD
0422DB         EXEC SQL OPEN CURBOOKS END-EXEC
0422DB         MOVE SQLCODE TO EVAL-CODE

               IF NOT ERR-OK
0427RQ            MOVE 'OPEN CURBOOKS' TO ERR-MSG
                  PERFORM 9999-ERROR-HANDLING
0422C2         END-IF
0423SQ      END-IF

0427RQ      IF NOT NOSEARCH
0423SQ         EXEC SQL OPEN CURSEARCH END-EXEC
0422DB         MOVE SQLCODE TO EVAL-CODE

               IF NOT ERR-OK
0427RQ            MOVE 'OPEN CURSEARCH' TO ERR-MSG
                  PERFORM 9999-ERROR-HANDLING
0422C2         END-IF
0423SQ      END-IF
            .

       1300-WRITE-TO-MAIN-QUEUE.
            MOVE '1300-WRITE-TO-MAIN-QUEUE' TO ERR-LOC

            PERFORM 1310-FETCH-MAIN-ROWS VARYING BK-IDX
                         FROM 1 BY 1    UNTIL BK-IDX > 15

            IF WS-TBL-PAGE NOT = SPACES
               EXEC CICS
                    WRITEQ TS
                           QUEUE    (WS-PAGE-QUEUE-NAME)
                           FROM     (WS-TBL-PAGE)
                           NUMITEMS (WS-TOTAL-PG)
                           RESP     (EVAL-CODE)
               END-EXEC

               IF EVAL-CODE NOT = DFHRESP(NORMAL)
                  MOVE 'WRITEQ TS' TO ERR-MSG
                  PERFORM 9999-ERROR-HANDLING
0422C2         END-IF
0422C2      END-IF
            .

       1310-FETCH-MAIN-ROWS.
            MOVE '1310-FETCH-MAIN-ROWS' TO ERR-LOC

            INITIALIZE WS-TBL-BOOK (BK-IDX)
                       TBLBKS-BOOK-ID
                       TBLBKS-TITLE

0422DB      EXEC SQL
0422DB           FETCH CURBOOKS
0422DB            INTO :TBLBKS-BOOK-ID
0422DB                ,:TBLBKS-TITLE
0422DB      END-EXEC
0422DB      MOVE SQLCODE TO EVAL-CODE

0422C2      EVALUATE TRUE
0422C2          WHEN ERR-OK
                     MOVE TBLBKS-BOOK-ID    TO WS-TBL-ID    (BK-IDX)
                     MOVE TBLBKS-TITLE-TEXT TO WS-TBL-TITLE (BK-IDX)
0422C2          WHEN SQL-EOC
                     MOVE SPACES    TO WS-TBL-BOOK (BK-IDX)
0422C2          WHEN OTHER
                     MOVE 'FETCH INTO' TO ERR-MSG
                     PERFORM 9999-ERROR-HANDLING
0422C2      END-EVALUATE
            .

0427RQ 1400-WRITE-TO-SRCH-QUEUE.
0427RQ      MOVE '1400-WRITE-TO-SRCH-QUEUE' TO ERR-LOC

0427RQ      PERFORM 1410-FETCH-SRCH-ROWS VARYING BK-IDX
                         FROM 1 BY 1    UNTIL BK-IDX > 15

            IF WS-TBL-PAGE NOT = SPACES
0423SQ         EXEC CICS
0423SQ              WRITEQ TS
0423SQ                     QUEUE    (WS-SRCH-QUEUE-NAME)
0423SQ                     FROM     (WS-TBL-PAGE)
0423SQ                     NUMITEMS (WS-TOTAL-SRCH)
0423SQ                     RESP     (EVAL-CODE)
0423SQ         END-EXEC

               IF EVAL-CODE NOT = DFHRESP(NORMAL)
                  MOVE 'WRITEQ TS' TO ERR-MSG
                  PERFORM 9999-ERROR-HANDLING
0422C2         END-IF
0422C2      END-IF
            .

0427RQ 1410-FETCH-SRCH-ROWS.
0427RQ      MOVE '1410-FETCH-SRCH-ROWS' TO ERR-LOC

            INITIALIZE WS-TBL-BOOK (BK-IDX)
                       TBLBKS-BOOK-ID
                       TBLBKS-TITLE

0423SQ      EXEC SQL
0423SQ           FETCH CURSEARCH
0423SQ            INTO :TBLBKS-BOOK-ID
0423SQ                ,:TBLBKS-TITLE
0423SQ      END-EXEC
0422DB      MOVE SQLCODE TO EVAL-CODE

0422C2      EVALUATE TRUE
0422C2          WHEN ERR-OK
                     MOVE TBLBKS-BOOK-ID    TO WS-TBL-ID    (BK-IDX)
                     MOVE TBLBKS-TITLE-TEXT TO WS-TBL-TITLE (BK-IDX)
0422C2          WHEN SQL-EOC
                     MOVE SPACES    TO WS-TBL-BOOK (BK-IDX)
0422C2          WHEN OTHER
                     MOVE 'FETCH INTO' TO ERR-MSG
                     PERFORM 9999-ERROR-HANDLING
0422C2      END-EVALUATE
            .

       1500-CLOSE-CURSOR.
            MOVE '1500-CLOSE-CURSOR' TO ERR-LOC

0427RQ      IF REBUILD
0422DB         EXEC SQL CLOSE CURBOOKS END-EXEC
0422DB         MOVE SQLCODE TO EVAL-CODE

               IF NOT ERR-OK
0427RQ            MOVE 'CLOSE CURBOOKS' TO ERR-MSG
                  PERFORM 9999-ERROR-HANDLING
0422C2         END-IF
0427RQ      END-IF

0427RQ      IF NOT NOSEARCH
0423SQ         EXEC SQL CLOSE CURSEARCH END-EXEC
0422DB         MOVE SQLCODE TO EVAL-CODE

               IF NOT ERR-OK
0427RQ            MOVE 'CLOSE CURSEARCH' TO ERR-MSG
                  PERFORM 9999-ERROR-HANDLING
0422C2         END-IF
0423SQ      END-IF
            .

       9999-ERROR-HANDLING.
            MOVE EVAL-CODE TO ERR-CODE
            MOVE WS-ERROR  TO WS-SEND-MSG
            PERFORM 9999-TERMINATE
            .

       9999-TERMINATE.
            EXEC CICS
                 SEND TEXT
                      FROM   (WS-SEND-MSG)
                      ERASE
                      FREEKB
            END-EXEC

            EXEC CICS
                 RETURN
            END-EXEC
            .
