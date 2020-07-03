//JBOOKREP JOB CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),REGION=2048K,          00000001
//          NOTIFY=IBMUSER                                              00000002
//*                                                                     00000003
//GO       EXEC PGM=IKJEFT01,DYNAMNBR=20,REGION=4M,COND=(4,LT)          00000004
//STEPLIB  DD DSN=DSN910.DB9G.RUNLIB.LOAD,DISP=SHR                      00000005
//         DD DISP=SHR,DSN=DSN910.DB9G.SDSNEXIT                         00000006
//         DD DISP=SHR,DSN=DSN910.SDSNLOAD                              00000007
//         DD DISP=SHR,DSN=ISP.SISPLOAD                                 00000008
//         DD DISP=SHR,DSN=GDDM.SADMMOD                                 00000009
//SYSOUT   DD SYSOUT=*                                                  00000010
//SYUDUMP  DD SYSOUT=*                                                  00000011
//CEEDUMP  DD SYSOUT=*                                                  00000012
//SYSPRINT DD SYSOUT=*                                                  00000013
//SYSTSPRT DD SYSOUT=*                                                  00000014
//BKREPOP  DD DSN=IBMUSER.SMAGALIT.GDG.BKREPOP(+1),                     00000015
//            UNIT=SYSDA,DISP=(NEW,CATLG),                              00000016
//            SPACE=(0,(40,100)),                                       00000017
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=0)                        00000018
//SYSTSIN  DD *                                                         00000019
  DSN SYSTEM(DB9G)                                                      00000020
  RUN PROGRAM(BOOKREP) PLAN(DSNTIA91)                                   00000021
  END                                                                   00000022
/*EOF                                                                   00000023
