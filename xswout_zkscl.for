!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:19:47 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZKSCL(ZRR,ZRI,FNU,N,YR,YI,NZ,RZR,RZI,ASCLE,TOL,ELIM)   EILI5488
C***BEGIN PROLOGUE  ZKSCL                                               EILI5489
C***REFER TO  ZBESK                                                     EILI5490
C                                                                       EILI5491
C     SET K FUNCTIONS TO ZERO ON UNDERFLOW, CONTINUE RECURRENCE         EILI5492
C     ON SCALED FUNCTIONS UNTIL TWO MEMBERS COME ON SCALE, THEN         EILI5493
C     RETURN WITH MIN(NZ+2,N) VALUES SCALED BY 1/TOL.                   EILI5494
C                                                                       EILI5495
C***ROUTINES CALLED  ZUCHK,ZABS,ZLOG                                    EILI5496
C***END PROLOGUE  ZKSCL                                                 EILI5497
C     COMPLEX CK,CS,CY,CZERO,RZ,S1,S2,Y,ZR,ZD,CELM                      EILI5498
      DOUBLE PRECISION ACS, AS, ASCLE, CKI, CKR, CSI, CSR, CYI,         EILI5499
     * CYR, ELIM, FN, FNU, RZI, RZR, STR, S1I, S1R, S2I,                EILI5500
     * S2R, TOL, YI, YR, ZEROI, ZEROR, ZRI, ZRR, ZABS,                  EILI5501
     * ZDR, ZDI, CELMR, ELM, HELIM, ALAS                                EILI5502
      INTEGER I, IC, IDUM, KK, N, NN, NW, NZ                            EILI5503
CNEA                                                                    EILI5504
      EXTERNAL ZLOG                                                     EILI5505
CNEA                                                                    EILI5506
      EXTERNAL ZABS                                                     EILI5507
      DIMENSION YR(N), YI(N), CYR(2), CYI(2)                            EILI5508
      DATA ZEROR,ZEROI / 0.0D0 , 0.0D0 /                                EILI5509
C                                                                       EILI5510
      NZ = 0                                                            EILI5511
      IC = 0                                                            EILI5512
      NN = MIN0(2,N)                                                    EILI5513
      DO 10 I=1,NN                                                      EILI5514
        S1R = YR(I)                                                     EILI5515
        S1I = YI(I)                                                     EILI5516
        CYR(I) = S1R                                                    EILI5517
        CYI(I) = S1I                                                    EILI5518
        AS = ZABS(S1R,S1I)                                              EILI5519
        ACS = -ZRR + DLOG(AS)                                           EILI5520
        NZ = NZ + 1                                                     EILI5521
        YR(I) = ZEROR                                                   EILI5522
        YI(I) = ZEROI                                                   EILI5523
        IF (ACS.LT.(-ELIM)) GO TO 10                                    EILI5524
        CALL ZLOG(S1R, S1I, CSR, CSI, IDUM)                             EILI5525
        CSR = CSR - ZRR                                                 EILI5526
        CSI = CSI - ZRI                                                 EILI5527
        STR = DEXP(CSR)/TOL                                             EILI5528
        CSR = STR*DCOS(CSI)                                             EILI5529
        CSI = STR*DSIN(CSI)                                             EILI5530
        CALL ZUCHK(CSR, CSI, NW, ASCLE, TOL)                            EILI5531
        IF (NW.NE.0) GO TO 10                                           EILI5532
        YR(I) = CSR                                                     EILI5533
        YI(I) = CSI                                                     EILI5534
        IC = I                                                          EILI5535
        NZ = NZ - 1                                                     EILI5536
   10 CONTINUE                                                          EILI5537
      IF (N.EQ.1) RETURN                                                EILI5538
      IF (IC.GT.1) GO TO 20                                             EILI5539
      YR(1) = ZEROR                                                     EILI5540
      YI(1) = ZEROI                                                     EILI5541
      NZ = 2                                                            EILI5542
   20 CONTINUE                                                          EILI5543
      IF (N.EQ.2) RETURN                                                EILI5544
      IF (NZ.EQ.0) RETURN                                               EILI5545
      FN = FNU + 1.0D0                                                  EILI5546
      CKR = FN*RZR                                                      EILI5547
      CKI = FN*RZI                                                      EILI5548
      S1R = CYR(1)                                                      EILI5549
      S1I = CYI(1)                                                      EILI5550
      S2R = CYR(2)                                                      EILI5551
      S2I = CYI(2)                                                      EILI5552
      HELIM = 0.5D0*ELIM                                                EILI5553
      ELM = DEXP(-ELIM)                                                 EILI5554
      CELMR = ELM                                                       EILI5555
      ZDR = ZRR                                                         EILI5556
      ZDI = ZRI                                                         EILI5557
C                                                                       EILI5558
C     FIND TWO CONSECUTIVE Y VALUES ON SCALE. SCALE RECURRENCE IF       EILI5559
C     S2 GETS LARGER THAN EXP(ELIM/2)                                   EILI5560
C                                                                       EILI5561
      DO 30 I=3,N                                                       EILI5562
        KK = I                                                          EILI5563
        CSR = S2R                                                       EILI5564
        CSI = S2I                                                       EILI5565
        S2R = CKR*CSR - CKI*CSI + S1R                                   EILI5566
        S2I = CKI*CSR + CKR*CSI + S1I                                   EILI5567
        S1R = CSR                                                       EILI5568
        S1I = CSI                                                       EILI5569
        CKR = CKR + RZR                                                 EILI5570
        CKI = CKI + RZI                                                 EILI5571
        AS = ZABS(S2R,S2I)                                              EILI5572
        ALAS = DLOG(AS)                                                 EILI5573
        ACS = -ZDR + ALAS                                               EILI5574
        NZ = NZ + 1                                                     EILI5575
        YR(I) = ZEROR                                                   EILI5576
        YI(I) = ZEROI                                                   EILI5577
        IF (ACS.LT.(-ELIM)) GO TO 25                                    EILI5578
        CALL ZLOG(S2R, S2I, CSR, CSI, IDUM)                             EILI5579
        CSR = CSR - ZDR                                                 EILI5580
        CSI = CSI - ZDI                                                 EILI5581
        STR = DEXP(CSR)/TOL                                             EILI5582
        CSR = STR*DCOS(CSI)                                             EILI5583
        CSI = STR*DSIN(CSI)                                             EILI5584
        CALL ZUCHK(CSR, CSI, NW, ASCLE, TOL)                            EILI5585
        IF (NW.NE.0) GO TO 25                                           EILI5586
        YR(I) = CSR                                                     EILI5587
        YI(I) = CSI                                                     EILI5588
        NZ = NZ - 1                                                     EILI5589
        IF (IC.EQ.KK-1) GO TO 40                                        EILI5590
        IC = KK                                                         EILI5591
        GO TO 30                                                        EILI5592
   25   CONTINUE                                                        EILI5593
        IF(ALAS.LT.HELIM) GO TO 30                                      EILI5594
        ZDR = ZDR - ELIM                                                EILI5595
        S1R = S1R*CELMR                                                 EILI5596
        S1I = S1I*CELMR                                                 EILI5597
        S2R = S2R*CELMR                                                 EILI5598
        S2I = S2I*CELMR                                                 EILI5599
   30 CONTINUE                                                          EILI5600
      NZ = N                                                            EILI5601
      IF(IC.EQ.N) NZ=N-1                                                EILI5602
      GO TO 45                                                          EILI5603
   40 CONTINUE                                                          EILI5604
      NZ = KK - 2                                                       EILI5605
   45 CONTINUE                                                          EILI5606
      DO 50 I=1,NZ                                                      EILI5607
        YR(I) = ZEROR                                                   EILI5608
        YI(I) = ZEROI                                                   EILI5609
   50 CONTINUE                                                          EILI5610
      RETURN                                                            EILI5611
      END      