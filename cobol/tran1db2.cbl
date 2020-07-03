      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    TRAN1DB2.
       AUTHOR.        SAM MAGALIT.
       DATE-WRITTEN.  03/04/2020.
       SECURITY.      HIGHLY CONFIDENTIAL.
      *----------------------------------------------------------------*
      * PROGRAM FOR BOOK LIST MAIN MENU - DB2 VERSION                  *
      *----------------------------------------------------------------*
      * - LINK TO A SUBPROGRAM (QUEUEDB2) TO LOAD DATA FROM DB2 TO TSQ *
      * - DISPLAY 15 BOOKS AT A TIME                                   *
      * - PRESS PF7 TO GO UP BY 1 PAGE                                 *
      * - PRESS PF8 TO GO DOWN BY 1 PAGE                               *
      * - ENTER S ON SELECTION FIELD TO DISPLAY BOOK INFO              *
      * - ENTER D ON SELECTION FIELD TO DELETE BOOK                    *
      * - ENTER U ON SELECTION FIELD TO UPDATE BOOK INFO               *
      * - HANDLES MIXED AND MULTIPLE SELECTIONS                        *
      * - DISPLAY BOOK INFO USES XCTL TO A SUBPROGRAM (TRAN2DB2)       *
      * - SEARCH TITLE FUNCTIONALITY USING WILDCARD QUERY              *
      * - PRESS F2 TO ADD RECORD                                       *
      * - REFRESH BOOK LIST QUEUE AFTER ADD/DELETE/UPDATE              *
      * - EXECUTES JCL TO GENERATE REPORT UPON TERMINATION             *
      *                                                                *
      * CHANGELOG:                                                     *
      * APRIL 22,2020 - CHANGED CODE TO COBOL 2               (0422C2) *
      *               X SEARCH FUNCTION USING DB2             (0422SF) *
      *               X CREATE QUEUE FOR USE IN TRAN2DB2      (0422SQ) *
      * APRIL 23,2020 - FIX INITIAL CURSOR PLACEMENT          (0423IC) *
      *               - FIXED SEARCH ALGORITHM                (0423SQ) *
      * APRIL 24,2020 - EDIT MAP AESTHETICS                   (0424MP) *
      * APRIL 27,2020 - REBUILD FLAG WHEN COMING FROM TERMINAL(0427RQ) *
      *               - SELECT/ADD/DELETE/UPDATE MODE         (0427MD) *
      *               - DELETE RECORDS                        (0427DE) *
      *               - UPDATE RECORDS                        (0427UP) *
      * APRIL 28,2020 - REBUILD QUEUE AFTER ADD/DELETE/UPDATE (0428RQ) *
      *               - SET BOOK MDT FOR QUEUE WRITING        (0428WQ) *
      * APRIL 30,2020 - ADD RECORD                            (0430AD) *
      * MAY   04,2020 - FIX INTERFACE                         (0504FI) *
      * MAY   15,2020 - LINK TO SUBPGM FOR JCL BATCH REPORT   (0515RP) *
      *                                                                *
      * PARAGRAPHS:                                                    *
      * 0000-MAIN                      4300-CHECK-INPUT                *
      * 0100-CREATE-TSQ                4310-FIND-SEL-INPUTS            *
      * 1000-FILL-PAGE                 4311-WRITE-SELQ                 *
      * 1100-MOVE-TO-MAP               4312-WRITE-DELQ                 *
      * 1110-LOCK-INPUT                4313-WRITE-UPDQ                 *
      * 2000-DISPLAY-MAP               4320-CHECK-VALID-SELECTION      *
      * 2100-GET-DATETIME              5000-REFRESH-PAGE               *
      * 3000-RECEIVE-MAP               5200-CLEAR-INPUT                *
      * 4000-CHECK-PFKEYS              9999-ERROR-HANDLING             *
      * 4100-PAGE-UP                   9999-TERMINATE                  *
      * 4200-PAGE-DOWN                                                 *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *--------------------
      *-------------
       DATA DIVISION.
      *-------------
       WORKING-STORAGE SECTION.
       01  WS-VARS.
0424MP     05  WS-RECORD            PIC X(08)         VALUE SPACES.
           05  WS-COUNTERS.
               10  BK-IDX           PIC S9(04) COMP   VALUE 1.
0427MD         10  WS-VALID-CTR     PIC S9(04) COMP   VALUE 0.
           05  WS-SWITCHES.
               10  WS-KEY-SW        PIC 9             VALUE 0.
                   88  INVALID-CHAR                   VALUE 1.
               10  WS-CLEAR-SW      PIC 9             VALUE 0.
                   88  CLEAR                          VALUE 1.
           05  WS-TBL-PAGE.
               10  WS-TBL-BOOK
               OCCURS 15 TIMES.
                   15  WS-TBL-ID    PIC 9(008).
0424MP             15  WS-TBL-TITLE PIC X(062).

       01  WS-CONST-VARS.
           05  WS-TRNIDS.
               10  WS-LIST-TRNID    PIC X(04)         VALUE 'T1DB'.
               10  WS-INFO-TRNID    PIC X(04)         VALUE 'T2DB'.
           05  WS-PGMIDS.
               10  WS-CBKQ-PGMID    PIC X(08)         VALUE 'QUEUEDB2'.
               10  WS-INFO-PGMID    PIC X(08)         VALUE 'TRAN2DB2'.
           05  WS-MAPIDS.
               10  WS-LISTMAP-NAME  PIC X(07)         VALUE 'LISTMAP'.
               10  WS-LISTSET-NAME  PIC X(07)         VALUE 'LISTSET'.

       01  WS-SYS-VARS.
           05  WS-SEND-MSG          PIC  X(80)        VALUE SPACES.
           05  EVAL-CODE            PIC S9(08) COMP.
           05  WS-TIME.
               10  WS-ABS-TIME      PIC S9(15) COMP-3 VALUE 0.
               10  WS-FMT-DATE      PIC  X(10)        VALUE SPACES.
               10  WS-FMT-TIME      PIC  X(08)        VALUE SPACES.

       01  WS-ERROR.
           05  FILLER               PIC X(09)         VALUE 'ERROR AT '.
           05  ERR-LOC              PIC X(32)         VALUE SPACES     .
           05  FILLER               PIC X(05)         VALUE ' RC: '    .
           05  ERR-CODE             PIC X(08)         VALUE SPACES     .
           05  FILLER               PIC X(06)         VALUE ' MSG: '   .
           05  ERR-MSG              PIC X(20)         VALUE SPACES     .

      **COPYBOOK FOR SYMBOLIC MAP
       COPY LISTSET.

       COPY DFHAID.
       COPY DFHBMSCA.

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
0515RP     05  LS-RJCL-PGMID        PIC X(08).

      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN.
            MOVE '0000-MAIN' TO ERR-LOC

0422C2      MOVE LOW-VALUES  TO LISTMAPO
0423IC      MOVE -1          TO SELBKL (1)
0427RQ      INITIALIZE WS-REBUILD-SW
0430AD                 WS-ADD-RECORD-SW

            IF EIBCALEN = 0
0504FI         EXEC CICS SET
0504FI              TERMINAL   (EIBTRMID)
0504FI              RESP       (EVAL-CODE)
0504FI              TRANIDONLY
0504FI         END-EXEC

0504FI         IF EVAL-CODE NOT = DFHRESP(NORMAL)
0504FI            MOVE 'SET TERMINAL' TO ERR-MSG
0504FI            PERFORM 9999-ERROR-HANDLING
0504FI         END-IF

               MOVE 1 TO WS-PG-NUM
0427RQ         SET REBUILD TO TRUE

               MOVE EIBTRNID TO WS-PQ-TRNID
               MOVE EIBTRMID TO WS-PQ-TRMID
                                WS-SL-TRMID
0423SQ                          WS-SR-TRMID
0427DE                          WS-DL-TRMID
0427UP                          WS-UP-TRMID

               PERFORM 0100-CREATE-TSQ

               PERFORM 1000-FILL-PAGE
               PERFORM 2000-DISPLAY-MAP
            ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA

0422C2         EVALUATE EIBTRNID
0422C2             WHEN WS-LIST-TRNID
                        PERFORM 3000-RECEIVE-MAP
                        PERFORM 4000-CHECK-PFKEYS
0422C2             WHEN WS-INFO-TRNID
0428RQ                  IF REBUILD
0428RQ                     PERFORM 0100-CREATE-TSQ
0428RQ                     INITIALIZE WS-REBUILD-SW
0428RQ                  END-IF

                        PERFORM 5000-REFRESH-PAGE
0422C2             WHEN OTHER
                        MOVE 'INVALID CALLEE TRANSACTION' TO WS-SEND-MSG
                        PERFORM 9999-TERMINATE
0422C2         END-EVALUATE
0422C2      END-IF

            EXEC CICS
                 RETURN TRANSID  (WS-LIST-TRNID)
                        COMMAREA (WS-COMMAREA)
                        RESP     (EVAL-CODE)
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'RETURN TRANSID' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
0422C2      END-IF
            .

       0100-CREATE-TSQ.
            MOVE '0100-CREATE-TSQ' TO ERR-LOC

            EXEC CICS LINK
                 PROGRAM  (WS-CBKQ-PGMID)
                 COMMAREA (WS-COMMAREA)
                 RESP     (EVAL-CODE)
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'LINK PROGRAM' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
0422C2      END-IF
            .

       1000-FILL-PAGE.
            MOVE '1000-FILL-PAGE' TO ERR-LOC

            MOVE LOW-VALUES TO WS-TBL-PAGE

0423SQ      IF NOSEARCH
0428RQ         IF WS-PG-NUM > WS-TOTAL-PG
0428RQ            MOVE WS-TOTAL-PG TO WS-PG-NUM
0428RQ         END-IF

               EXEC CICS READQ TS
                         QUEUE (WS-PAGE-QUEUE-NAME)
                         INTO  (WS-TBL-PAGE)
                         ITEM  (WS-PG-NUM)
                         RESP  (EVAL-CODE)
               END-EXEC
0423SQ      ELSE
0428RQ         IF WS-SRCH-NUM > WS-TOTAL-SRCH
0428RQ            MOVE WS-TOTAL-SRCH TO WS-SRCH-NUM
0428RQ         END-IF

0423SQ         EXEC CICS READQ TS
0423SQ                   QUEUE (WS-SRCH-QUEUE-NAME)
0423SQ                   INTO  (WS-TBL-PAGE)
0423SQ                   ITEM  (WS-SRCH-NUM)
0423SQ                   RESP  (EVAL-CODE)
0423SQ         END-EXEC
0423SQ      END-IF

0428RQ      EVALUATE EVAL-CODE
0428RQ          WHEN DFHRESP (NORMAL)
                     PERFORM 1100-MOVE-TO-MAP VARYING BK-IDX
                                  FROM 1 BY 1   UNTIL BK-IDX > 15
0428RQ          WHEN DFHRESP (QIDERR)
0423SQ               IF NOSEARCH
0428RQ                  MOVE 'QUEUE NOT CREATED. DATABASE MAY BE EMPTY'
0428RQ                    TO MESSAGEO

0428RQ                  MOVE DFHBMASK TO TTLSRCHA
0428RQ                  MOVE 0        TO TTLSRCHH
0428RQ                  MOVE SPACES   TO WS-TBL-PAGE
0428RQ                  PERFORM 1100-MOVE-TO-MAP VARYING BK-IDX
0428RQ                               FROM 1 BY 1   UNTIL BK-IDX > 15
0423SQ               ELSE
0423SQ                  MOVE 'SEARCH STRING NOT FOUND' TO MESSAGEO
0423SQ                  SET NOSEARCH TO TRUE

0423IC                  MOVE -1 TO TTLSRCHL
0423IC**                SET MDT TO 1
0423IC                  MOVE DFHBMFSE TO TTLSRCHF

0423SQ                  PERFORM 1000-FILL-PAGE
0428RQ               END-IF
0423SQ          WHEN OTHER
                     MOVE 'READQ TS' TO ERR-MSG
                     PERFORM 9999-ERROR-HANDLING
0422C2      END-EVALUATE
            .

       1100-MOVE-TO-MAP.
            MOVE '1100-MOVE-TO-MAP' TO ERR-LOC

            MOVE WS-TBL-ID    (BK-IDX) TO IDNUMO (BK-IDX)
0428RQ      MOVE DFHBMPRF              TO IDNUMF (BK-IDX)
            MOVE WS-TBL-TITLE (BK-IDX) TO TITLEO (BK-IDX)

            IF WS-TBL-BOOK (BK-IDX) = SPACES
               PERFORM 1110-LOCK-INPUT
0422C2      END-IF
            .

       1110-LOCK-INPUT.
            MOVE '1110-LOCK-INPUT' TO ERR-LOC
      **    ATTRB = ASKIP,HILIGHT = NONE
      **    FOR BLANK FIELDS
            MOVE DFHBMASK TO SELBKA (BK-IDX)
            MOVE 0        TO SELBKH (BK-IDX)
            .

       2000-DISPLAY-MAP.
            MOVE '2000-DISPLAY-MAP' TO ERR-LOC

            IF CLEAR
               PERFORM 5200-CLEAR-INPUT VARYING BK-IDX
                            FROM 1 BY 1   UNTIL BK-IDX > 15
0423IC         MOVE -1 TO SELBKL (1)
0422C2      END-IF

0423SQ      IF NOT NOSEARCH
0423SQ         MOVE WS-SEARCH-STR TO TTLSRCHO
0424MP         MOVE WS-SRCH-NUM   TO CURPAGEO
0424MP         MOVE WS-TOTAL-SRCH TO TOTPAGEO
0424MP         MOVE 'ON'          TO SRCHMODO
0424MP      ELSE
0424MP         MOVE WS-PG-NUM     TO CURPAGEO
0424MP         MOVE WS-TOTAL-PG   TO TOTPAGEO
0424MP         MOVE 'OFF'         TO SRCHMODO
0423SQ      END-IF

            PERFORM 2100-GET-DATETIME
            MOVE WS-LIST-TRNID TO TRANSIDO

            EXEC CICS
                 SEND MAP    (WS-LISTMAP-NAME)
                      MAPSET (WS-LISTSET-NAME)
                      FROM   (LISTMAPO)
                      RESP   (EVAL-CODE)
0423IC                CURSOR
                      ERASE
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'SEND MAP' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
0422C2      END-IF
            .

       2100-GET-DATETIME.
            MOVE '2100-GET-DATETIME' TO ERR-LOC

            EXEC CICS
                 ASKTIME ABSTIME(WS-ABS-TIME)
            END-EXEC

            EXEC CICS
                 FORMATTIME ABSTIME  (WS-ABS-TIME)
                            MMDDYYYY (WS-FMT-DATE)
                            DATESEP  ('/')
            END-EXEC

            EXEC CICS
                 FORMATTIME ABSTIME  (WS-ABS-TIME)
                            TIME     (WS-FMT-TIME)
                            TIMESEP  (':')
            END-EXEC

            MOVE WS-FMT-DATE TO SYSDATEO
            MOVE WS-FMT-TIME TO SYSTIMEO
            .

       3000-RECEIVE-MAP.
            MOVE '3000-RECEIVE-MAP' TO ERR-LOC

            EXEC CICS
                 RECEIVE MAP    (WS-LISTMAP-NAME)
                         MAPSET (WS-LISTSET-NAME)
                         INTO   (LISTMAPI)
                         RESP   (EVAL-CODE)
                         ASIS
            END-EXEC

0422C2      IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'RECEIVE MAP' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
0422C2      END-IF
            .

       4000-CHECK-PFKEYS.
            MOVE '4000-CHECK-PFKEYS' TO ERR-LOC

0422C2      EVALUATE EIBAID
0430AD          WHEN DFHPF2
0430AD               SET ADD-RECORD TO TRUE

0430AD               MOVE 0 TO WS-TOTAL-SEL
0430AD                         WS-TOTAL-DEL
0430AD                         WS-TOTAL-UPD

0430AD               PERFORM 1110-LOCK-INPUT VARYING BK-IDX
0430AD                            FROM 1 BY 1  UNTIL BK-IDX > 15

0430AD**             ATTRB = ASKIP,HILIGHT = NONE
0430AD               MOVE DFHBMASK TO TTLSRCHA
0430AD               SET CLEAR TO TRUE

0430AD               PERFORM 5000-REFRESH-PAGE

0430AD               EXEC CICS
0430AD                    XCTL PROGRAM  (WS-INFO-PGMID)
0430AD                         COMMAREA (WS-COMMAREA)
0430AD               END-EXEC
0422C2          WHEN DFHPF3
0422C2          WHEN DFHPF12
0515RP               EXEC CICS LINK
0515RP                    PROGRAM (WS-RJCL-PGMID)
0515RP                    RESP    (EVAL-CODE)
0515RP               END-EXEC

0515RP               IF EVAL-CODE NOT = DFHRESP (NORMAL)
0515RP                  MOVE 'LINK PROGRAM' TO ERR-MSG
0515RP                  PERFORM 9999-ERROR-HANDLING
0515RP               END-IF

                     MOVE 'TRANSACTION TERMINATED' TO WS-SEND-MSG
                     PERFORM 9999-TERMINATE
0422C2          WHEN DFHPF7
                     PERFORM 4100-PAGE-UP
0422C2          WHEN DFHPF8
                     PERFORM 4200-PAGE-DOWN
0422C2          WHEN DFHPF9
0423SQ               IF NOSEARCH
                        MOVE 1 TO WS-PG-NUM
0423SQ               ELSE
0423SQ                  MOVE 1 TO WS-SRCH-NUM
0423SQ               END-IF

                     PERFORM 5000-REFRESH-PAGE
                     PERFORM 4100-PAGE-UP
0422C2          WHEN DFHPF10
0423SQ               IF NOSEARCH
                        MOVE WS-TOTAL-PG   TO WS-PG-NUM
0423SQ               ELSE
0423SQ                  MOVE WS-TOTAL-SRCH TO WS-SRCH-NUM
0423SQ               END-IF

                     PERFORM 5000-REFRESH-PAGE
                     PERFORM 4200-PAGE-DOWN
0422C2          WHEN DFHENTER
                     PERFORM 4300-CHECK-INPUT
0422C2          WHEN OTHER
                     MOVE 'INVALID KEY PRESSED' TO MESSAGEO
                     PERFORM 5000-REFRESH-PAGE
                     PERFORM 2000-DISPLAY-MAP
0422C2      END-EVALUATE
            .

       4100-PAGE-UP.
            MOVE '4100-PAGE-UP' TO ERR-LOC

0422C2*     SET NOSEARCH TO TRUE
0423IC      SET CLEAR    TO TRUE
0423IC      MOVE -1      TO SELBKL (1)

0423SQ      IF NOSEARCH
               IF WS-PG-NUM > 1
                  SUBTRACT 1 FROM WS-PG-NUM
                  MOVE SPACES TO MESSAGEO
               ELSE
                  MOVE 1                     TO WS-PG-NUM
                  MOVE 'TOP OF DATA REACHED' TO MESSAGEO
0422C2         END-IF
0423SQ      ELSE
0423SQ         IF WS-SRCH-NUM > 1
0423SQ            SUBTRACT 1 FROM WS-SRCH-NUM
0423SQ            MOVE SPACES TO MESSAGEO
0423SQ         ELSE
0423SQ            MOVE 1                     TO WS-SRCH-NUM
0423SQ            MOVE 'TOP OF DATA REACHED' TO MESSAGEO
0423SQ         END-IF
0423SQ      END-IF

            PERFORM 1000-FILL-PAGE
            PERFORM 2000-DISPLAY-MAP
            .

       4200-PAGE-DOWN.
            MOVE '4200-PAGE-DOWN' TO ERR-LOC

0422C2*     SET NOSEARCH TO TRUE
0423IC      SET CLEAR    TO TRUE
0423IC      MOVE -1      TO SELBKL(1)

0423SQ      IF NOSEARCH
               IF WS-PG-NUM < WS-TOTAL-PG
                  ADD 1 TO WS-PG-NUM
                  MOVE SPACES TO MESSAGEO
               ELSE
                  MOVE WS-TOTAL-PG           TO WS-PG-NUM
                  MOVE 'END OF DATA REACHED' TO MESSAGEO
0422C2         END-IF
0423SQ      ELSE
0423SQ         IF WS-SRCH-NUM < WS-TOTAL-SRCH
0423SQ            ADD 1 TO WS-SRCH-NUM
0423SQ            MOVE SPACES TO MESSAGEO
0423SQ         ELSE
0423SQ            MOVE WS-TOTAL-SRCH         TO WS-SRCH-NUM
0423SQ            MOVE 'END OF DATA REACHED' TO MESSAGEO
0423SQ         END-IF
0423SQ      END-IF

            PERFORM 1000-FILL-PAGE
            PERFORM 2000-DISPLAY-MAP
            .

       4300-CHECK-INPUT.
            MOVE '4300-CHECK-INPUT' TO ERR-LOC

0422C2      INITIALIZE WS-KEY-SW

            IF TTLSRCHL > 0
               MOVE TTLSRCHI TO WS-SEARCH-STR
0423SQ         MOVE 0        TO WS-TOTAL-SRCH
0423SQ         MOVE 1        TO WS-SRCH-NUM

0423SQ         IF NOT NOSEARCH
0423SQ            PERFORM 0100-CREATE-TSQ
0423SQ         END-IF

0423IC         SET CLEAR TO TRUE
               PERFORM 5000-REFRESH-PAGE
            ELSE
0427MD         MOVE 0 TO WS-TOTAL-SEL
0427MD                   WS-TOTAL-DEL
0427MD                   WS-TOTAL-UPD

               EXEC CICS
                    DELETEQ TS
                            QUEUE (WS-SEL-QUEUE-NAME)
                            RESP  (EVAL-CODE)
               END-EXEC

               EXEC CICS
                    DELETEQ TS
0427DE                      QUEUE (WS-DEL-QUEUE-NAME)
                            RESP  (EVAL-CODE)
               END-EXEC

               EXEC CICS
                    DELETEQ TS
0427UP                      QUEUE (WS-UPD-QUEUE-NAME)
                            RESP  (EVAL-CODE)
               END-EXEC

               PERFORM 4310-FIND-SEL-INPUTS VARYING BK-IDX
                            FROM 1 BY 1       UNTIL BK-IDX > 15
               PERFORM 4320-CHECK-VALID-SELECTION
0422C2      END-IF
            .

       4310-FIND-SEL-INPUTS.
            MOVE '4310-FIND-SEL-INPUTS' TO ERR-LOC

      **    CHECK MODIFIED FIELDS WITH NON-SPACE CHARACTERS ONLY
            IF  SELBKL (BK-IDX) > 0
            AND SELBKI (BK-IDX) NOT = SPACES
0423IC**        SET MDT TO 1
0423IC          MOVE DFHBMFSE TO SELBKF (BK-IDX)
0427MD          EVALUATE FUNCTION UPPER-CASE (SELBKI (BK-IDX))
                    WHEN 'S'
0427MD                   ADD 1 TO WS-VALID-CTR
0428RQ                   MOVE IDNUMO (BK-IDX) TO WS-RECORD
                         PERFORM 4311-WRITE-SELQ
0427DE              WHEN 'D'
0427MD                   ADD 1 TO WS-VALID-CTR
0428RQ                   MOVE IDNUMO (BK-IDX) TO WS-RECORD
0427DE                   PERFORM 4312-WRITE-DELQ
0427UP              WHEN 'U'
0427MD                   ADD 1 TO WS-VALID-CTR
0428RQ                   MOVE IDNUMO (BK-IDX) TO WS-RECORD
0427UP                   PERFORM 4313-WRITE-UPDQ
                    WHEN OTHER
0422C2                   SET INVALID-CHAR TO TRUE
0423IC                   MOVE -1 TO SELBKL (BK-IDX)
0422C2          END-EVALUATE
0422C2      END-IF
            .

       4311-WRITE-SELQ.
            MOVE '4311-WRITE-SELQ' TO ERR-LOC

            EXEC CICS
                 WRITEQ TS
                        QUEUE    (WS-SEL-QUEUE-NAME)
                        FROM     (WS-RECORD)
                        NUMITEMS (WS-TOTAL-SEL)
                        RESP     (EVAL-CODE)
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'WRITEQ TS' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
0422C2      END-IF
            .

0427DE 4312-WRITE-DELQ.
0427DE      MOVE '4312-WRITE-DELQ' TO ERR-LOC

            EXEC CICS
                 WRITEQ TS
0427DE                  QUEUE    (WS-DEL-QUEUE-NAME)
                        FROM     (WS-RECORD)
0427DE                  NUMITEMS (WS-TOTAL-DEL)
                        RESP     (EVAL-CODE)
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'WRITEQ TS' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
0422C2      END-IF
            .

0427UP 4313-WRITE-UPDQ.
0427UP      MOVE '4313-WRITE-UPDQ' TO ERR-LOC

            EXEC CICS
                 WRITEQ TS
0427UP                  QUEUE    (WS-UPD-QUEUE-NAME)
                        FROM     (WS-RECORD)
0427UP                  NUMITEMS (WS-TOTAL-UPD)
                        RESP     (EVAL-CODE)
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'WRITEQ TS' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
0422C2      END-IF
            .

       4320-CHECK-VALID-SELECTION.
            MOVE '4320-CHECK-VALID-SELECTION' TO ERR-LOC

            IF INVALID-CHAR
               MOVE 'ONLY VALID INPUTS ARE ''S'', ''U'', AND ''D'''
0515RP              TO MESSAGEO
               PERFORM 5000-REFRESH-PAGE
            ELSE
0427MD         IF WS-VALID-CTR > 0
                  PERFORM 1110-LOCK-INPUT VARYING BK-IDX
                               FROM 1 BY 1  UNTIL BK-IDX > 15

      **          ATTRB = ASKIP,HILIGHT = NONE
                  MOVE DFHBMASK TO TTLSRCHA
                  INITIALIZE WS-CLEAR-SW

                  PERFORM 5000-REFRESH-PAGE

                  EXEC CICS
                       XCTL PROGRAM  (WS-INFO-PGMID)
                            COMMAREA (WS-COMMAREA)
                  END-EXEC
               ELSE
                  MOVE -1 TO SELBKL (1)
                  MOVE 'PLEASE USE THE SEARCH BAR OR SELECT A BOOK'
                       TO MESSAGEO
                  PERFORM 5000-REFRESH-PAGE
0422C2         END-IF
0422C2      END-IF
            .

       5000-REFRESH-PAGE.
            MOVE '5000-REFRESH-PAGE' TO ERR-LOC

            PERFORM 1000-FILL-PAGE
            PERFORM 2000-DISPLAY-MAP
            .

       5200-CLEAR-INPUT.
            MOVE '5200-CLEAR-INPUT' TO ERR-LOC

            MOVE SPACE TO SELBKO (BK-IDX)
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