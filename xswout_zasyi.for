!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:58:05 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZASYI(ZR, ZI, FNU, KODE, N, YR, YI, NZ, RL, TOL, ELIM, EILI7489
     * ALIM)                                                            EILI7490
C***BEN PROLOGUE  ZASYI                                               EILI7491
C***REFER TO  ZBESI,ZBESK                                               EILI7492
C                                                                       EILI7493
C     ZASYI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY        EILI7494
C     MEANS OF THE ASYMPTOTIC EXPANSION FOR LARGE CABS(Z) IN THE        EILI7495
C     REGION CABS(Z).GT.MAX(RL,FNU*FNU/2). NZ=0 IS A NORMAL RETURN.     EILI7496
C     NZ.LT.0 INDICATES AN OVERFLOW ON KODE=1.                          EILI7497
C                                                                       EILI7498
C***ROUTINES CALLED  D1MACH,ZABS,ZDIV,ZEXP,ZMLT,ZSQRT                   EILI7499
C***END PROLOGUE  ZASYI                                                 EILI7500
C     COMPLEX AK1,CK,CONE,CS1,CS2,CZ,CZERO,DK,EZ,P1,RZ,S2,Y,Z           EILI7501
      DOUBLE PRECISION AA, AEZ, AK, AK1I, AK1R, ALIM, ARG, ARM, ATOL,   EILI7502
     * AZ, BB, BK, CKI, CKR, CONEI, CONER, CS1I, CS1R, CS2I, CS2R, CZI, EILI7503
     * CZR, DFNU, DKI, DKR, DNU2, ELIM, EZI, EZR, FDN, FNU, PI, P1I,    EILI7504
     * P1R, RAZ, RL, RTPI, RTR1, RZI, RZR, S, SGN, SQK, STI, STR, S2I,  EILI7505
     * S2R, TOL, TZI, TZR, YI, YR, ZEROI, ZEROR, ZI, ZR, D1MACH, ZABS   EILI7506
      INTEGER I, IB, IL, INU, J, JL, K, KODE, KODED, M, N, NN, NZ       EILI7507
      DIMENSION YR(N), YI(N)                                            EILI7508
      EXTERNAL ZABS                                                     EILI7509
      DATA PI, RTPI  /3.14159265358979324D0 , 0.159154943091895336D0 /  EILI7510
      DATA ZEROR,ZEROI,CONER,CONEI / 0.0D0, 0.0D0, 1.0D0, 0.0D0 /       EILI7511
C                                                                       EILI7512
      NZ = 0                                                            EILI7513
      AZ = ZABS(ZR,ZI)                                                  EILI7514
      ARM = 1.0D+3*D1MACH(1)                                            EILI7515
      RTR1 = DSQRT(ARM)                                                 EILI7516
      IL = MIN0(2,N)                                                    EILI7517
      DFNU = FNU + DBLE(FLOAT(N-IL))                                    EILI7518
C-----------------------------------------------------------------------EILI7519
C     OVERFLOW TEST                                                     EILI7520
C-----------------------------------------------------------------------EILI7521
      RAZ = 1.0D0/AZ                                                    EILI7522
      STR = ZR*RAZ                                                      EILI7523
      STI = -ZI*RAZ                                                     EILI7524
      AK1R = RTPI*STR*RAZ                                               EILI7525
      AK1I = RTPI*STI*RAZ                                               EILI7526
      CALL RRZSQRT(AK1R, AK1I, AK1R, AK1I)                              EILI7527
      CZR = ZR                                                          EILI7528
      CZI = ZI                                                          EILI7529
      IF (KODE.NE.2) GO TO 10                                           EILI7530
      CZR = ZEROR                                                       EILI7531
      CZI = ZI                                                          EILI7532
   10 CONTINUE                                                          EILI7533
      IF (DABS(CZR).GT.ELIM) GO TO 100                                  EILI7534
      DNU2 = DFNU + DFNU                                                EILI7535
      KODED = 1                                                         EILI7536
      IF ((DABS(CZR).GT.ALIM) .AND. (N.GT.2)) GO TO 20                  EILI7537
      KODED = 0                                                         EILI7538
      CALL RRZEXP(CZR, CZI, STR, STI)                                   EILI7539
      CALL ZMLT(AK1R, AK1I, STR, STI, AK1R, AK1I)                       EILI7540
   20 CONTINUE                                                          EILI7541
      FDN = 0.0D0                                                       EILI7542
      IF (DNU2.GT.RTR1) FDN = DNU2*DNU2                                 EILI7543
      EZR = ZR*8.0D0                                                    EILI7544
      EZI = ZI*8.0D0                                                    EILI7545
C-----------------------------------------------------------------------EILI7546
C     WHEN Z IS IMAGINARY, THE ERROR TEST MUST BE MADE RELATIVE TO THE  EILI7547
C     FIRST RECIPROCAL POWER SINCE THIS IS THE LEADING TERM OF THE      EILI7548
C     EXPANSION FOR THE IMAGINARY PART.                                 EILI7549
C-----------------------------------------------------------------------EILI7550
      AEZ = 8.0D0*AZ                                                    EILI7551
      S = TOL/AEZ                                                       EILI7552
      JL = INT(SNGL(RL+RL)) + 2                                         EILI7553
      P1R = ZEROR                                                       EILI7554
      P1I = ZEROI                                                       EILI7555
      IF (ZI.EQ.0.0D0) GO TO 30                                         EILI7556
C-----------------------------------------------------------------------EILI7557
C     CALCULATE EXP(PI*(0.5+FNU+N-IL)*I) TO MINIMIZE LOSSES OF          EILI7558
C     SIGNIFICANCE WHEN FNU OR N IS LARGE                               EILI7559
C-----------------------------------------------------------------------EILI7560
      INU = INT(SNGL(FNU))                                              EILI7561
      ARG = (FNU-DBLE(FLOAT(INU)))*PI                                   EILI7562
      INU = INU + N - IL                                                EILI7563
      AK = -DSIN(ARG)                                                   EILI7564
      BK = DCOS(ARG)                                                    EILI7565
      IF (ZI.LT.0.0D0) BK = -BK                                         EILI7566
      P1R = AK                                                          EILI7567
      P1I = BK                                                          EILI7568
      IF (MOD(INU,2).EQ.0) GO TO 30                                     EILI7569
      P1R = -P1R                                                        EILI7570
      P1I = -P1I                                                        EILI7571
   30 CONTINUE                                                          EILI7572
      DO 70 K=1,IL                                                      EILI7573
        SQK = FDN - 1.0D0                                               EILI7574
        ATOL = S*DABS(SQK)                                              EILI7575
        SGN = 1.0D0                                                     EILI7576
        CS1R = CONER                                                    EILI7577
        CS1I = CONEI                                                    EILI7578
        CS2R = CONER                                                    EILI7579
        CS2I = CONEI                                                    EILI7580
        CKR = CONER                                                     EILI7581
        CKI = CONEI                                                     EILI7582
        AK = 0.0D0                                                      EILI7583
        AA = 1.0D0                                                      EILI7584
        BB = AEZ                                                        EILI7585
        DKR = EZR                                                       EILI7586
        DKI = EZI                                                       EILI7587
        DO 40 J=1,JL                                                    EILI7588
          CALL ZDIV(CKR, CKI, DKR, DKI, STR, STI)                       EILI7589
          CKR = STR*SQK                                                 EILI7590
          CKI = STI*SQK                                                 EILI7591
          CS2R = CS2R + CKR                                             EILI7592
          CS2I = CS2I + CKI                                             EILI7593
          SGN = -SGN                                                    EILI7594
          CS1R = CS1R + CKR*SGN                                         EILI7595
          CS1I = CS1I + CKI*SGN                                         EILI7596
          DKR = DKR + EZR                                               EILI7597
          DKI = DKI + EZI                                               EILI7598
          AA = AA*DABS(SQK)/BB                                          EILI7599
          BB = BB + AEZ                                                 EILI7600
          AK = AK + 8.0D0                                               EILI7601
          SQK = SQK - AK                                                EILI7602
          IF (AA.LE.ATOL) GO TO 50                                      EILI7603
   40   CONTINUE                                                        EILI7604
        GO TO 110                                                       EILI7605
   50   CONTINUE                                                        EILI7606
        S2R = CS1R                                                      EILI7607
        S2I = CS1I                                                      EILI7608
        IF (ZR+ZR.GE.ELIM) GO TO 60                                     EILI7609
        TZR = ZR + ZR                                                   EILI7610
        TZI = ZI + ZI                                                   EILI7611
        CALL RRZEXP(-TZR, -TZI, STR, STI)                               EILI7612
        CALL ZMLT(STR, STI, P1R, P1I, STR, STI)                         EILI7613
        CALL ZMLT(STR, STI, CS2R, CS2I, STR, STI)                       EILI7614
        S2R = S2R + STR                                                 EILI7615
        S2I = S2I + STI                                                 EILI7616
   60   CONTINUE                                                        EILI7617
        FDN = FDN + 8.0D0*DFNU + 4.0D0                                  EILI7618
        P1R = -P1R                                                      EILI7619
        P1I = -P1I                                                      EILI7620
        M = N - IL + K                                                  EILI7621
        YR(M) = S2R*AK1R - S2I*AK1I                                     EILI7622
        YI(M) = S2R*AK1I + S2I*AK1R                                     EILI7623
   70 CONTINUE                                                          EILI7624
      IF (N.LE.2) RETURN                                                EILI7625
      NN = N                                                            EILI7626
      K = NN - 2                                                        EILI7627
      AK = DBLE(FLOAT(K))                                               EILI7628
      STR = ZR*RAZ                                                      EILI7629
      STI = -ZI*RAZ                                                     EILI7630
      RZR = (STR+STR)*RAZ                                               EILI7631
      RZI = (STI+STI)*RAZ                                               EILI7632
      IB = 3                                                            EILI7633
      DO 80 I=IB,NN                                                     EILI7634
        YR(K) = (AK+FNU)*(RZR*YR(K+1)-RZI*YI(K+1)) + YR(K+2)            EILI7635
        YI(K) = (AK+FNU)*(RZR*YI(K+1)+RZI*YR(K+1)) + YI(K+2)            EILI7636
        AK = AK - 1.0D0                                                 EILI7637
        K = K - 1                                                       EILI7638
   80 CONTINUE                                                          EILI7639
      IF (KODED.EQ.0) RETURN                                            EILI7640
      CALL RRZEXP(CZR, CZI, CKR, CKI)                                   EILI7641
      DO 90 I=1,NN                                                      EILI7642
        STR = YR(I)*CKR - YI(I)*CKI                                     EILI7643
        YI(I) = YR(I)*CKI + YI(I)*CKR                                   EILI7644
        YR(I) = STR                                                     EILI7645
   90 CONTINUE                                                          EILI7646
      RETURN                                                            EILI7647
  100 CONTINUE                                                          EILI7648
      NZ = -1                                                           EILI7649
      RETURN                                                            EILI7650
  110 CONTINUE                                                          EILI7651
      NZ=-2                                                             EILI7652
      RETURN                                                            EILI7653
      END                                                               EILI7654