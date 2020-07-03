      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CICSRJCL.
       AUTHOR.        SAM MAGALIT.
       DATE-WRITTEN.  05/15/2020.
       SECURITY.      HIGHLY CONFIDENTIAL.
      *----------------------------------------------------------------*
      * SUBPROGRAM TO EXECUTE BATCH JCL STORED IN VSAM                 *
      *----------------------------------------------------------------*
      * - READ VSAM WHICH CONTAINS JCL LINES TO BE EXECUTED            *
      * - WRITE TO SPOOL TO BE EXECUTED BY JES INTERNAL READER         *
      * - EXECUTES JCL WRITTEN TO SPOOL UPON CLOSING THE SPOOL         *
      *                                                                *
      * CHANGELOG:                                                     *
      * JUN 01,2020                                                    *
      *      0601TD - USE TDQ INSTEAD OF SPOOL                         *
      *                                                                *
      * FILES:                                                         *
      * JCLBKREP (INPUT ) - IBMUSER.SMAGALIT.VSAM.JCLBKREP             *
      *                                                                *
      * 1000-INIT                      3000-CLEANUP                    *
      * 1100-STARTBR                   3100-ENDBR                      *
      * 2000-MAIN-LOGIC                9999-ERROR-HANDLING             *
      * 2100-READ-JCL                  9999-TERMINATE                  *
      * 2110-WRITE-IRDR                                                *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *--------------------
      *-------------
       DATA DIVISION.
      *-------------
       WORKING-STORAGE SECTION.
       01  WS-VARS.
           05  WS-JCL-FILE           PIC X(08)       VALUE 'JCLBKREP'.
           05  WS-JCL-LINE           PIC X(80)       VALUE SPACES    .
           05  WS-VSAM-KEY           PIC X(08)       VALUE LOW-VALUES.
           05  WS-TOKEN              PIC X(08)       VALUE SPACES.
           05  WS-VSAM-EOF-SW        PIC 9           VALUE 0.
               88  VSAM-EOF                          VALUE 1.

       01  WS-SYS-VARS.
           05  EVAL-CODE             PIC S9(08) COMP VALUE 0.
           05  WS-SEND-MSG           PIC X(80)       VALUE SPACES.

       01  WS-ERROR.
           05  FILLER                PIC X(09)       VALUE 'ERROR AT '.
           05  ERR-LOC               PIC X(26)       VALUE SPACES     .
           05  FILLER                PIC X(05)       VALUE ' RC: '    .
           05  ERR-CODE              PIC X(08)       VALUE SPACES     .
           05  FILLER                PIC X(06)       VALUE ' MSG: '   .
           05  ERR-MSG               PIC X(26)       VALUE SPACES     .

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

            MOVE 'EXECUTING JOB...' TO WS-SEND-MSG
            EXEC CICS
                 SEND TEXT
                      FROM  (WS-SEND-MSG)
                      RESP  (EVAL-CODE)
                      ERASE
            END-EXEC

            PERFORM 1100-STARTBR
            .

       1100-STARTBR.
            MOVE '1100-STARTBR' TO ERR-LOC

            MOVE LOW-VALUES TO WS-VSAM-KEY
            INITIALIZE WS-VSAM-EOF-SW

            EXEC CICS
                 STARTBR FILE   (WS-JCL-FILE)
                         RIDFLD (WS-VSAM-KEY)
                         RESP   (EVAL-CODE)
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'STARTBR' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            END-IF
            .

       2000-MAIN-LOGIC.
            MOVE 'X000-MAIN-LOGIC' TO ERR-LOC

            PERFORM 2100-READ-JCL UNTIL VSAM-EOF
            .

       2100-READ-JCL.
            MOVE '2100-READ-JCL' TO ERR-LOC

            INITIALIZE WS-JCL-LINE

            EXEC CICS
                 READNEXT FILE   (WS-JCL-FILE)
                          INTO   (WS-JCL-LINE)
                          RIDFLD (WS-VSAM-KEY)
                          RESP   (EVAL-CODE)
            END-EXEC


            EVALUATE EVAL-CODE
                WHEN DFHRESP (NORMAL)
                     PERFORM 2110-WRITE-IRDR
                WHEN DFHRESP (ENDFILE)
                     SET VSAM-EOF TO TRUE
                WHEN OTHER
                     MOVE 'READNEXT' TO ERR-MSG
                     PERFORM 9999-ERROR-HANDLING
            END-EVALUATE
            .

       2110-WRITE-IRDR.
            MOVE '2110-WRITE-IRDR' TO ERR-LOC

0601TD      EXEC CICS WRITEQ TD
0601TD           QUEUE ('IRDR')
0601TD           FROM  (WS-JCL-LINE)
0601TD           RESP  (EVAL-CODE)
0601TD      END-EXEC

            IF EVAL-CODE NOT = DFHRESP(NORMAL)
0601TD         MOVE 'WRITEQ TD' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            END-IF
            .

       3000-CLEANUP.
            MOVE '3000-CLEANUP' TO ERR-LOC

            PERFORM 3100-ENDBR

            MOVE 'JOB EXECUTED' TO WS-SEND-MSG
            PERFORM 9999-TERMINATE
            .

       3100-ENDBR.
            MOVE '3100-ENDBR' TO ERR-LOC

            EXEC CICS
                 ENDBR FILE (WS-JCL-FILE)
                       RESP (EVAL-CODE)
            END-EXEC

            IF EVAL-CODE NOT = DFHRESP (NORMAL)
               MOVE 'ENDBR' TO ERR-MSG
               PERFORM 9999-ERROR-HANDLING
            .

       9999-ERROR-HANDLING.
            MOVE EVAL-CODE TO ERR-CODE
            MOVE WS-ERROR  TO WS-SEND-MSG
            PERFORM 9999-TERMINATE
            .

       9999-TERMINATE.
            EXEC CICS
                 SEND TEXT
                      FROM  (WS-SEND-MSG)
                      RESP  (EVAL-CODE)
                      ERASE
            END-EXEC

            EXEC CICS
                 RETURN
            END-EXEC
            .
