!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:22:47 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZS1S2(ZRR, ZRI, S1R, S1I, S2R, S2I, NZ, ASCLE, ALIM,   EILI6433
     * IUF)                                                             EILI6434
C***BEGIN PROLOGUE  ZS1S2                                               EILI6435
C***REFER TO  ZBESK,ZAIRY                                               EILI6436
C                                                                       EILI6437
C     ZS1S2 TESTS FOR A POSSIBLE UNDERFLOW RESULTING FROM THE           EILI6438
C     ADDITION OF THE I AND K FUNCTIONS IN THE ANALYTIC CON-            EILI6439
C     TINUATION FORMULA WHERE S1=K FUNCTION AND S2=I FUNCTION.          EILI6440
C     ON KODE=1 THE I AND K FUNCTIONS ARE DIFFERENT ORDERS OF           EILI6441
C     MAGNITUDE, BUT FOR KODE=2 THEY CAN BE OF THE SAME ORDER           EILI6442
C     OF MAGNITUDE AND THE MAXIMUM MUST BE AT LEAST ONE                 EILI6443
C     PRECISION ABOVE THE UNDERFLOW LIMIT.                              EILI6444
C                                                                       EILI6445
C***ROUTINES CALLED  ZABS,ZEXP,ZLOG                                     EILI6446
C***END PROLOGUE  ZS1S2                                                 EILI6447
C     COMPLEX CZERO,C1,S1,S1D,S2,ZR                                     EILI6448
      DOUBLE PRECISION AA, ALIM, ALN, ASCLE, AS1, AS2, C1I, C1R, S1DI,  EILI6449
     * S1DR, S1I, S1R, S2I, S2R, ZEROI, ZEROR, ZRI, ZRR, ZABS           EILI6450
      INTEGER IUF, IDUM, NZ                                             EILI6451
CNEA                                                                    EILI6452
      EXTERNAL ZLOG                                                     EILI6453
CNEA                                                                    EILI6454
      EXTERNAL ZABS                                                     EILI6455
      DATA ZEROR,ZEROI  / 0.0D0 , 0.0D0 /                               EILI6456
      NZ = 0                                                            EILI6457
      AS1 = ZABS(S1R,S1I)                                               EILI6458
      AS2 = ZABS(S2R,S2I)                                               EILI6459
      IF (S1R.EQ.0.0D0 .AND. S1I.EQ.0.0D0) GO TO 10                     EILI6460
      IF (AS1.EQ.0.0D0) GO TO 10                                        EILI6461
      ALN = -ZRR - ZRR + DLOG(AS1)                                      EILI6462
      S1DR = S1R                                                        EILI6463
      S1DI = S1I                                                        EILI6464
      S1R = ZEROR                                                       EILI6465
      S1I = ZEROI                                                       EILI6466
      AS1 = ZEROR                                                       EILI6467
      IF (ALN.LT.(-ALIM)) GO TO 10                                      EILI6468
      CALL ZLOG(S1DR, S1DI, C1R, C1I, IDUM)                             EILI6469
      C1R = C1R - ZRR - ZRR                                             EILI6470
      C1I = C1I - ZRI - ZRI                                             EILI6471
      CALL RRZEXP(C1R, C1I, S1R, S1I)                                   EILI6472
      AS1 = ZABS(S1R,S1I)                                               EILI6473
      IUF = IUF + 1                                                     EILI6474
   10 CONTINUE                                                          EILI6475
      AA = DMAX1(AS1,AS2)                                               EILI6476
      IF (AA.GT.ASCLE) RETURN                                           EILI6477
      S1R = ZEROR                                                       EILI6478
      S1I = ZEROI                                                       EILI6479
      S2R = ZEROR                                                       EILI6480
      S2I = ZEROI                                                       EILI6481
      NZ = 1                                                            EILI6482
      IUF = 0                                                           EILI6483
      RETURN                                                            EILI6484
      END       