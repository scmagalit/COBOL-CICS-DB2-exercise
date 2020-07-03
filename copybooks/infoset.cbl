       01  INFOMAPI.
           02  FILLER PIC X(12).
           02  TRANSIDL    COMP  PIC  S9(4).
           02  TRANSIDF    PICTURE X.
           02  FILLER REDEFINES TRANSIDF.
             03 TRANSIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TRANSIDI  PIC X(4).
           02  FILLER PIC X.
           02  BKIDNUML    COMP  PIC  S9(4).
           02  BKIDNUMF    PICTURE X.
           02  FILLER REDEFINES BKIDNUMF.
             03 BKIDNUMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BKIDNUMI  PIC X(8).
           02  FILLER PIC X.
           02  BTITLE1L    COMP  PIC  S9(4).
           02  BTITLE1F    PICTURE X.
           02  FILLER REDEFINES BTITLE1F.
             03 BTITLE1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BTITLE1I  PIC X(42).
           02  FILLER PIC X.
           02  BTITLE2L    COMP  PIC  S9(4).
           02  BTITLE2F    PICTURE X.
           02  FILLER REDEFINES BTITLE2F.
             03 BTITLE2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BTITLE2I  PIC X(42).
           02  FILLER PIC X.
           02  BTITLE3L    COMP  PIC  S9(4).
           02  BTITLE3F    PICTURE X.
           02  FILLER REDEFINES BTITLE3F.
             03 BTITLE3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BTITLE3I  PIC X(42).
           02  FILLER PIC X.
           02  BTITLE4L    COMP  PIC  S9(4).
           02  BTITLE4F    PICTURE X.
           02  FILLER REDEFINES BTITLE4F.
             03 BTITLE4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BTITLE4I  PIC X(42).
           02  FILLER PIC X.
           02  BKPAGESL    COMP  PIC  S9(4).
           02  BKPAGESF    PICTURE X.
           02  FILLER REDEFINES BKPAGESF.
             03 BKPAGESA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BKPAGESI  PIC X(4).
           02  FILLER PIC X.
           02  BKRATNGL    COMP  PIC  S9(4).
           02  BKRATNGF    PICTURE X.
           02  FILLER REDEFINES BKRATNGF.
             03 BKRATNGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BKRATNGI  PIC X(5).
           02  BKISBNL    COMP  PIC  S9(4).
           02  BKISBNF    PICTURE X.
           02  FILLER REDEFINES BKISBNF.
             03 BKISBNA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BKISBNI  PIC X(13).
           02  BKPBDATL    COMP  PIC  S9(4).
           02  BKPBDATF    PICTURE X.
           02  FILLER REDEFINES BKPBDATF.
             03 BKPBDATA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BKPBDATI  PIC X(10).
           02  FILLER PIC X.
           02  BKPUBIDL    COMP  PIC  S9(4).
           02  BKPUBIDF    PICTURE X.
           02  FILLER REDEFINES BKPUBIDF.
             03 BKPUBIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  BKPUBIDI  PIC X(4).
           02  FILLER PIC X.
           02  MODEMSGL    COMP  PIC  S9(4).
           02  MODEMSGF    PICTURE X.
           02  FILLER REDEFINES MODEMSGF.
             03 MODEMSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MODEMSGI  PIC X(32).
           02  FILLER PIC X.
           02  CONFRMIL    COMP  PIC  S9(4).
           02  CONFRMIF    PICTURE X.
           02  FILLER REDEFINES CONFRMIF.
             03 CONFRMIA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CONFRMII  PIC X(1).
           02  MESSAGEL    COMP  PIC  S9(4).
           02  MESSAGEF    PICTURE X.
           02  FILLER REDEFINES MESSAGEF.
             03 MESSAGEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MESSAGEI  PIC X(60).
           02  FILLER PIC X.
           02  PAGEUL    COMP  PIC  S9(4).
           02  PAGEUF    PICTURE X.
           02  FILLER REDEFINES PAGEUF.
             03 PAGEUA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PAGEUI  PIC X(11).
           02  PAGEDL    COMP  PIC  S9(4).
           02  PAGEDF    PICTURE X.
           02  FILLER REDEFINES PAGEDF.
             03 PAGEDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PAGEDI  PIC X(13).
       01  INFOMAPO REDEFINES INFOMAPI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANSIDC    PICTURE X.
           02  TRANSIDH    PICTURE X.
           02  TRANSIDO  PIC X(4).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BKIDNUMC    PICTURE X.
           02  BKIDNUMH    PICTURE X.
           02  BKIDNUMO  PIC X(8).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BTITLE1C    PICTURE X.
           02  BTITLE1H    PICTURE X.
           02  BTITLE1O  PIC X(42).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BTITLE2C    PICTURE X.
           02  BTITLE2H    PICTURE X.
           02  BTITLE2O  PIC X(42).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BTITLE3C    PICTURE X.
           02  BTITLE3H    PICTURE X.
           02  BTITLE3O  PIC X(42).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BTITLE4C    PICTURE X.
           02  BTITLE4H    PICTURE X.
           02  BTITLE4O  PIC X(42).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BKPAGESC    PICTURE X.
           02  BKPAGESH    PICTURE X.
           02  BKPAGESO  PIC X(4).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BKRATNGC    PICTURE X.
           02  BKRATNGH    PICTURE X.
           02  BKRATNGO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  BKISBNC    PICTURE X.
           02  BKISBNH    PICTURE X.
           02  BKISBNO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  BKPBDATC    PICTURE X.
           02  BKPBDATH    PICTURE X.
           02  BKPBDATO  PIC X(10).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  BKPUBIDC    PICTURE X.
           02  BKPUBIDH    PICTURE X.
           02  BKPUBIDO  PIC X(4).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  MODEMSGC    PICTURE X.
           02  MODEMSGH    PICTURE X.
           02  MODEMSGO  PIC X(32).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  CONFRMIC    PICTURE X.
           02  CONFRMIH    PICTURE X.
           02  CONFRMIO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MESSAGEC    PICTURE X.
           02  MESSAGEH    PICTURE X.
           02  MESSAGEO  PIC X(60).
           02  FILLER PIC X.
           02  FILLER PICTURE X(3).
           02  PAGEUC    PICTURE X.
           02  PAGEUH    PICTURE X.
           02  PAGEUO  PIC X(11).
           02  FILLER PICTURE X(3).
           02  PAGEDC    PICTURE X.
           02  PAGEDH    PICTURE X.
           02  PAGEDO  PIC X(13).
