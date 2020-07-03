      ******************************************************************
      * DCLGEN TABLE(IBMUSER.BOOKS)                                    *
      *        LIBRARY(IBMUSER.SMAGALIT.COPYLIB(DCLBOOKS))             *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(TBLBKS-)                                          *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IBMUSER.BOOKS TABLE
           ( BOOK_ID                        INTEGER NOT NULL,
             TITLE                          VARCHAR(255) NOT NULL,
             TOTAL_PAGES                    INTEGER,
             RATING                         DECIMAL(4, 2),
             ISBN                           VARCHAR(13),
             PUBLISHED_DATE                 DATE,
             PUBLISHER_ID                   INTEGER
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE IBMUSER.BOOKS                      *
      ******************************************************************
       01  DCLBOOKS.
      *                       BOOK_ID
           10 TBLBKS-BOOK-ID       PIC S9(9) USAGE COMP.
           10 TBLBKS-TITLE.
      *                       TITLE LENGTH
              49 TBLBKS-TITLE-LEN
                 PIC S9(4) USAGE COMP.
      *                       TITLE
              49 TBLBKS-TITLE-TEXT
                 PIC X(255).
      *                       TOTAL_PAGES
           10 TBLBKS-TOTAL-PAGES   PIC S9(9) USAGE COMP.
      *                       RATING
           10 TBLBKS-RATING        PIC S9(2)V9(2) USAGE COMP-3.
           10 TBLBKS-ISBN.
      *                       ISBN LENGTH
              49 TBLBKS-ISBN-LEN   PIC S9(4) USAGE COMP.
      *                       ISBN
              49 TBLBKS-ISBN-TEXT
                 PIC X(13).
      *                       PUBLISHED_DATE
           10 TBLBKS-PUBLISHED-DATE
              PIC X(10).
      *                       PUBLISHER_ID
           10 TBLBKS-PUBLISHER-ID  PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
