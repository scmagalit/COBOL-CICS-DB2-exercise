       01  LISTMAPI.
           02  FILLER PIC X(12).
           02  TRANSIDL    COMP  PIC  S9(4).
           02  TRANSIDF    PICTURE X.
           02  FILLER REDEFINES TRANSIDF.
             03 TRANSIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TRANSIDI  PIC X(4).
           02  FILLER PIC X.
           02  SYSDATEL    COMP  PIC  S9(4).
           02  SYSDATEF    PICTURE X.
           02  FILLER REDEFINES SYSDATEF.
             03 SYSDATEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SYSDATEI  PIC X(10).
           02  FILLER PIC X.
           02  SYSTIMEL    COMP  PIC  S9(4).
           02  SYSTIMEF    PICTURE X.
           02  FILLER REDEFINES SYSTIMEF.
             03 SYSTIMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SYSTIMEI  PIC X(8).
           02  FILLER PIC X.
           02  CURPAGEL    COMP  PIC  S9(4).
           02  CURPAGEF    PICTURE X.
           02  FILLER REDEFINES CURPAGEF.
             03 CURPAGEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CURPAGEI  PIC X(4).
           02  FILLER PIC X.
           02  TOTPAGEL    COMP  PIC  S9(4).
           02  TOTPAGEF    PICTURE X.
           02  FILLER REDEFINES TOTPAGEF.
             03 TOTPAGEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TOTPAGEI  PIC X(4).
           02  FILLER PIC X.
           02  TTLSRCHL    COMP  PIC  S9(4).
           02  TTLSRCHF    PICTURE X.
           02  FILLER REDEFINES TTLSRCHF.
             03 TTLSRCHA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TTLSRCHI  PIC X(58).
           02  LISTLINEI OCCURS 15 TIMES.
             03  FILLER PIC X.
             03  SELBKL    COMP  PIC  S9(4).
             03  SELBKF    PICTURE X.
             03  FILLER REDEFINES SELBKF.
               04 SELBKA    PICTURE X.
             03  FILLER   PICTURE X(2).
             03  SELBKI  PIC X(1).
             03  IDNUML    COMP  PIC  S9(4).
             03  IDNUMF    PICTURE X.
             03  FILLER REDEFINES IDNUMF.
               04 IDNUMA    PICTURE X.
             03  FILLER   PICTURE X(2).
             03  IDNUMI  PIC X(8).
             03  FILLER PIC X.
             03  TITLEL    COMP  PIC  S9(4).
             03  TITLEF    PICTURE X.
             03  FILLER REDEFINES TITLEF.
               04 TITLEA    PICTURE X.
             03  FILLER   PICTURE X(2).
             03  TITLEI  PIC X(62).
           02  FILLER PIC X.
           02  SRCHMODL    COMP  PIC  S9(4).
           02  SRCHMODF    PICTURE X.
           02  FILLER REDEFINES SRCHMODF.
             03 SRCHMODA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SRCHMODI  PIC X(3).
           02  MESSAGEL    COMP  PIC  S9(4).
           02  MESSAGEF    PICTURE X.
           02  FILLER REDEFINES MESSAGEF.
             03 MESSAGEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MESSAGEI  PIC X(77).

       01  LISTMAPO REDEFINES LISTMAPI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANSIDC    PICTURE X.
           02  TRANSIDH    PICTURE X.
           02  TRANSIDO  PIC X(4).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  SYSDATEC    PICTURE X.
           02  SYSDATEH    PICTURE X.
           02  SYSDATEO  PIC X(10).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  SYSTIMEC    PICTURE X.
           02  SYSTIMEH    PICTURE X.
           02  SYSTIMEO  PIC X(8).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  CURPAGEC    PICTURE X.
           02  CURPAGEH    PICTURE X.
           02  CURPAGEO  PIC X(4).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  TOTPAGEC    PICTURE X.
           02  TOTPAGEH    PICTURE X.
           02  TOTPAGEO  PIC X(4).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  TTLSRCHC    PICTURE X.
           02  TTLSRCHH    PICTURE X.
           02  TTLSRCHO  PIC X(58).
           02  LISTLINEO OCCURS 15 TIMES.
             03  FILLER PIC X.
             03  FILLER PICTURE X(3).
             03  SELBKC    PICTURE X.
             03  SELBKH    PICTURE X.
             03  SELBKO  PIC X(1).
             03  FILLER PICTURE X(3).
             03  IDNUMC    PICTURE X.
             03  IDNUMH    PICTURE X.
             03  IDNUMO  PIC X(8).
             03  FILLER PIC X.
             03  FILLER PICTURE X(3).
             03  TITLEC    PICTURE X.
             03  TITLEH    PICTURE X.
             03  TITLEO  PIC X(62).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  SRCHMODC    PICTURE X.
           02  SRCHMODH    PICTURE X.
           02  SRCHMODO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  MESSAGEC    PICTURE X.
           02  MESSAGEH    PICTURE X.
           02  MESSAGEO  PIC X(77).
