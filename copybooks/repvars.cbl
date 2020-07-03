      * COPYBOOK FOR REPORT TEMPLATE OF HEADERS AND FOOTERS
       01  WS-REPORT-TEMPLATE.
           05  WS-REP-HEADER1.
               10  FILLER       PIC X(019) VALUE 'CREATED BY PROGRAM '.
               10  WS-REP-PGM   PIC X(008) VALUE '--------'           .
               10  FILLER       PIC X      VALUE SPACES               .
               10  WS-REP-TITLE PIC X(076) VALUE SPACES               .
               10  FILLER       PIC X(018) VALUE SPACES               .
               10  WS-REP-DATE  PIC X(010) VALUE '0000-00-00'         .
           05  WS-REP-HEADER2.
               10  FILLER       PIC X(005) VALUE 'PAGE '              .
               10  WS-REP-CURP  PIC 9(004) VALUE ZEROES               .
               10  FILLER       PIC X(115) VALUE SPACES               .
               10  WS-REP-TIME  PIC X(008) VALUE '00:00:00'           .
           05  WS-REP-SPACES    PIC X(132) VALUE SPACES               .
           05  WS-REP-FOOTER.
               10  FILLER       PIC X(057) VALUE ALL '*'              .
               10  FILLER       PIC XX     VALUE SPACES               .
               10  FILLER       PIC X(013) VALUE 'END OF REPORT'      .
               10  FILLER       PIC XX     VALUE SPACES               .
               10  FILLER       PIC X(058) VALUE ALL '*'              .
