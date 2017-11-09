!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:58:58 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZUOIK(ZR, ZI, FNU, KODE, IKFLG, N, YR, YI, NUF, TOL,   EILI7655
     * ELIM, ALIM)                                                      EILI7656
C***BEGIN PROLOGUE  ZUOIK                                               EILI7657
C***REFER TO  ZBESI,ZBESK,ZBESH                                         EILI7658
C                                                                       EILI7659
C     ZUOIK COMPUTES THE LEADING TERMS OF THE UNIFORM ASYMPTOTIC        EILI7660
C     EXPANSIONS FOR THE I AND K FUNCTIONS AND COMPARES THEM            EILI7661
C     (IN LOGARITHMIC FORM) TO ALIM AND ELIM FOR OVER AND UNDERFLOW     EILI7662
C     WHERE ALIM.LT.ELIM. IF THE MAGNITUDE, BASED ON THE LEADING        EILI7663
C     EXPONENTIAL, IS LESS THAN ALIM OR GREATER THAN -ALIM, THEN        EILI7664
C     THE RESULT IS ON SCALE. IF NOT, THEN A REFINED TEST USING OTHER   EILI7665
C     MULTIPLIERS (IN LOGARITHMIC FORM) IS MADE BASED ON ELIM. HERE     EILI7666
C     EXP(-ELIM)=SMALLEST MACHINE NUMBER*1.0E+3 AND EXP(-ALIM)=         EILI7667
C     EXP(-ELIM)/TOL                                                    EILI7668
C                                                                       EILI7669
C     IKFLG=1 MEANS THE I SEQUENCE IS TESTED                            EILI7670
C          =2 MEANS THE K SEQUENCE IS TESTED                            EILI7671
C     NUF = 0 MEANS THE LAST MEMBER OF THE SEQUENCE IS ON SCALE         EILI7672
C         =-1 MEANS AN OVERFLOW WOULD OCCUR                             EILI7673
C     IKFLG=1 AND NUF.GT.0 MEANS THE LAST NUF Y VALUES WERE SET TO ZERO EILI7674
C             THE FIRST N-NUF VALUES MUST BE SET BY ANOTHER ROUTINE     EILI7675
C     IKFLG=2 AND NUF.EQ.N MEANS ALL Y VALUES WERE SET TO ZERO          EILI7676
C     IKFLG=2 AND 0.LT.NUF.LT.N NOT CONSIDERED. Y MUST BE SET BY        EILI7677
C             ANOTHER ROUTINE                                           EILI7678
C                                                                       EILI7679
C***ROUTINES CALLED  ZUCHK,ZUNHJ,ZUNIK,D1MACH,ZABS,ZLOG                 EILI7680
C***END PROLOGUE  ZUOIK                                                 EILI7681
C     COMPLEX ARG,ASUM,BSUM,CWRK,CZ,CZERO,PHI,SUM,Y,Z,ZB,ZETA1,ZETA2,ZN,EILI7682
C    *ZR                                                                EILI7683
      DOUBLE PRECISION AARG, AIC, ALIM, APHI, ARGI, ARGR, ASUMI, ASUMR, EILI7684
     * ASCLE, AX, AY, BSUMI, BSUMR, CWRKI, CWRKR, CZI, CZR, ELIM, FNN,  EILI7685
     * FNU, GNN, GNU, PHII, PHIR, RCZ, STR, STI, SUMI, SUMR, TOL, YI,   EILI7686
     * YR, ZBI, ZBR, ZEROI, ZEROR, ZETA1I, ZETA1R, ZETA2I, ZETA2R, ZI,  EILI7687
     * ZNI, ZNR, ZR, ZRI, ZRR, D1MACH, ZABS                             EILI7688
      INTEGER I, IDUM, IFORM, IKFLG, INIT, KODE, N, NN, NUF, NW         EILI7689
      DIMENSION YR(N), YI(N), CWRKR(16), CWRKI(16)                      EILI7690
CNEA                                                                    EILI7691
      EXTERNAL ZLOG                                                     EILI7692
CNEA                                                                    EILI7693
      EXTERNAL ZABS                                                     EILI7694
      DATA ZEROR,ZEROI / 0.0D0, 0.0D0 /                                 EILI7695
      DATA AIC / 1.265512123484645396D+00 /                             EILI7696
      NUF = 0                                                           EILI7697
      NN = N                                                            EILI7698
      ZRR = ZR                                                          EILI7699
      ZRI = ZI                                                          EILI7700
      IF (ZR.GE.0.0D0) GO TO 10                                         EILI7701
      ZRR = -ZR                                                         EILI7702
      ZRI = -ZI                                                         EILI7703
   10 CONTINUE                                                          EILI7704
      ZBR = ZRR                                                         EILI7705
      ZBI = ZRI                                                         EILI7706
      AX = DABS(ZR)*1.7321D0                                            EILI7707
      AY = DABS(ZI)                                                     EILI7708
      IFORM = 1                                                         EILI7709
      IF (AY.GT.AX) IFORM = 2                                           EILI7710
      GNU = DMAX1(FNU,1.0D0)                                            EILI7711
      IF (IKFLG.EQ.1) GO TO 20                                          EILI7712
      FNN = DBLE(FLOAT(NN))                                             EILI7713
      GNN = FNU + FNN - 1.0D0                                           EILI7714
      GNU = DMAX1(GNN,FNN)                                              EILI7715
   20 CONTINUE                                                          EILI7716
C-----------------------------------------------------------------------EILI7717
C     ONLY THE MAGNITUDE OF ARG AND PHI ARE NEEDED ALONG WITH THE       EILI7718
C     REAL PARTS OF ZETA1, ZETA2 AND ZB. NO ATTEMPT IS MADE TO GET      EILI7719
C     THE SIGN OF THE IMAGINARY PART CORRECT.                           EILI7720
C-----------------------------------------------------------------------EILI7721
      IF (IFORM.EQ.2) GO TO 30                                          EILI7722
      INIT = 0                                                          EILI7723
      CALL ZUNIK(ZRR, ZRI, GNU, IKFLG, 1, TOL, INIT, PHIR, PHII,        EILI7724
     * ZETA1R, ZETA1I, ZETA2R, ZETA2I, SUMR, SUMI, CWRKR, CWRKI)        EILI7725
      CZR = -ZETA1R + ZETA2R                                            EILI7726
      CZI = -ZETA1I + ZETA2I                                            EILI7727
      GO TO 50                                                          EILI7728
   30 CONTINUE                                                          EILI7729
      ZNR = ZRI                                                         EILI7730
      ZNI = -ZRR                                                        EILI7731
      IF (ZI.GT.0.0D0) GO TO 40                                         EILI7732
      ZNR = -ZNR                                                        EILI7733
   40 CONTINUE                                                          EILI7734
      CALL ZUNHJ(ZNR, ZNI, GNU, 1, TOL, PHIR, PHII, ARGR, ARGI, ZETA1R, EILI7735
     * ZETA1I, ZETA2R, ZETA2I, ASUMR, ASUMI, BSUMR, BSUMI)              EILI7736
      CZR = -ZETA1R + ZETA2R                                            EILI7737
      CZI = -ZETA1I + ZETA2I                                            EILI7738
      AARG = ZABS(ARGR,ARGI)                                            EILI7739
   50 CONTINUE                                                          EILI7740
      IF (KODE.EQ.1) GO TO 60                                           EILI7741
      CZR = CZR - ZBR                                                   EILI7742
      CZI = CZI - ZBI                                                   EILI7743
   60 CONTINUE                                                          EILI7744
      IF (IKFLG.EQ.1) GO TO 70                                          EILI7745
      CZR = -CZR                                                        EILI7746
      CZI = -CZI                                                        EILI7747
   70 CONTINUE                                                          EILI7748
      APHI = ZABS(PHIR,PHII)                                            EILI7749
      RCZ = CZR                                                         EILI7750
C-----------------------------------------------------------------------EILI7751
C     OVERFLOW TEST                                                     EILI7752
C-----------------------------------------------------------------------EILI7753
      IF (RCZ.GT.ELIM) GO TO 210                                        EILI7754
      IF (RCZ.LT.ALIM) GO TO 80                                         EILI7755
      RCZ = RCZ + DLOG(APHI)                                            EILI7756
      IF (IFORM.EQ.2) RCZ = RCZ - 0.25D0*DLOG(AARG) - AIC               EILI7757
      IF (RCZ.GT.ELIM) GO TO 210                                        EILI7758
      GO TO 130                                                         EILI7759
   80 CONTINUE                                                          EILI7760
C-----------------------------------------------------------------------EILI7761
C     UNDERFLOW TEST                                                    EILI7762
C-----------------------------------------------------------------------EILI7763
      IF (RCZ.LT.(-ELIM)) GO TO 90                                      EILI7764
      IF (RCZ.GT.(-ALIM)) GO TO 130                                     EILI7765
      RCZ = RCZ + DLOG(APHI)                                            EILI7766
      IF (IFORM.EQ.2) RCZ = RCZ - 0.25D0*DLOG(AARG) - AIC               EILI7767
      IF (RCZ.GT.(-ELIM)) GO TO 110                                     EILI7768
   90 CONTINUE                                                          EILI7769
      DO 100 I=1,NN                                                     EILI7770
        YR(I) = ZEROR                                                   EILI7771
        YI(I) = ZEROI                                                   EILI7772
  100 CONTINUE                                                          EILI7773
      NUF = NN                                                          EILI7774
      RETURN                                                            EILI7775
  110 CONTINUE                                                          EILI7776
      ASCLE = 1.0D+3*D1MACH(1)/TOL                                      EILI7777
      CALL ZLOG(PHIR, PHII, STR, STI, IDUM)                             EILI7778
      CZR = CZR + STR                                                   EILI7779
      CZI = CZI + STI                                                   EILI7780
      IF (IFORM.EQ.1) GO TO 120                                         EILI7781
      CALL ZLOG(ARGR, ARGI, STR, STI, IDUM)                             EILI7782
      CZR = CZR - 0.25D0*STR - AIC                                      EILI7783
      CZI = CZI - 0.25D0*STI                                            EILI7784
  120 CONTINUE                                                          EILI7785
      AX = DEXP(RCZ)/TOL                                                EILI7786
      AY = CZI                                                          EILI7787
      CZR = AX*DCOS(AY)                                                 EILI7788
      CZI = AX*DSIN(AY)                                                 EILI7789
      CALL ZUCHK(CZR, CZI, NW, ASCLE, TOL)                              EILI7790
      IF (NW.NE.0) GO TO 90                                             EILI7791
  130 CONTINUE                                                          EILI7792
      IF (IKFLG.EQ.2) RETURN                                            EILI7793
      IF (N.EQ.1) RETURN                                                EILI7794
C-----------------------------------------------------------------------EILI7795
C     SET UNDERFLOWS ON I SEQUENCE                                      EILI7796
C-----------------------------------------------------------------------EILI7797
  140 CONTINUE                                                          EILI7798
      GNU = FNU + DBLE(FLOAT(NN-1))                                     EILI7799
      IF (IFORM.EQ.2) GO TO 150                                         EILI7800
      INIT = 0                                                          EILI7801
      CALL ZUNIK(ZRR, ZRI, GNU, IKFLG, 1, TOL, INIT, PHIR, PHII,        EILI7802
     * ZETA1R, ZETA1I, ZETA2R, ZETA2I, SUMR, SUMI, CWRKR, CWRKI)        EILI7803
      CZR = -ZETA1R + ZETA2R                                            EILI7804
      CZI = -ZETA1I + ZETA2I                                            EILI7805
      GO TO 160                                                         EILI7806
  150 CONTINUE                                                          EILI7807
      CALL ZUNHJ(ZNR, ZNI, GNU, 1, TOL, PHIR, PHII, ARGR, ARGI, ZETA1R, EILI7808
     * ZETA1I, ZETA2R, ZETA2I, ASUMR, ASUMI, BSUMR, BSUMI)              EILI7809
      CZR = -ZETA1R + ZETA2R                                            EILI7810
      CZI = -ZETA1I + ZETA2I                                            EILI7811
      AARG = ZABS(ARGR,ARGI)                                            EILI7812
  160 CONTINUE                                                          EILI7813
      IF (KODE.EQ.1) GO TO 170                                          EILI7814
      CZR = CZR - ZBR                                                   EILI7815
      CZI = CZI - ZBI                                                   EILI7816
  170 CONTINUE                                                          EILI7817
      APHI = ZABS(PHIR,PHII)                                            EILI7818
      RCZ = CZR                                                         EILI7819
      IF (RCZ.LT.(-ELIM)) GO TO 180                                     EILI7820
      IF (RCZ.GT.(-ALIM)) RETURN                                        EILI7821
      RCZ = RCZ + DLOG(APHI)                                            EILI7822
      IF (IFORM.EQ.2) RCZ = RCZ - 0.25D0*DLOG(AARG) - AIC               EILI7823
      IF (RCZ.GT.(-ELIM)) GO TO 190                                     EILI7824
  180 CONTINUE                                                          EILI7825
      YR(NN) = ZEROR                                                    EILI7826
      YI(NN) = ZEROI                                                    EILI7827
      NN = NN - 1                                                       EILI7828
      NUF = NUF + 1                                                     EILI7829
      IF (NN.EQ.0) RETURN                                               EILI7830
      GO TO 140                                                         EILI7831
  190 CONTINUE                                                          EILI7832
      ASCLE = 1.0D+3*D1MACH(1)/TOL                                      EILI7833
      CALL ZLOG(PHIR, PHII, STR, STI, IDUM)                             EILI7834
      CZR = CZR + STR                                                   EILI7835
      CZI = CZI + STI                                                   EILI7836
      IF (IFORM.EQ.1) GO TO 200                                         EILI7837
      CALL ZLOG(ARGR, ARGI, STR, STI, IDUM)                             EILI7838
      CZR = CZR - 0.25D0*STR - AIC                                      EILI7839
      CZI = CZI - 0.25D0*STI                                            EILI7840
  200 CONTINUE                                                          EILI7841
      AX = DEXP(RCZ)/TOL                                                EILI7842
      AY = CZI                                                          EILI7843
      CZR = AX*DCOS(AY)                                                 EILI7844
      CZI = AX*DSIN(AY)                                                 EILI7845
      CALL ZUCHK(CZR, CZI, NW, ASCLE, TOL)                              EILI7846
      IF (NW.NE.0) GO TO 180                                            EILI7847
      RETURN                                                            EILI7848
  210 CONTINUE                                                          EILI7849
      NUF = -1                                                          EILI7850
      RETURN                                                            EILI7851
      END       