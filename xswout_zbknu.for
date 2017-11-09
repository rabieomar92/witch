!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:16:08 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZBKNU(ZR, ZI, FNU, KODE, N, YR, YI, NZ, TOL, ELIM,     EILI5671
     * ALIM)                                                            EILI5672
C***BEGIN PROLOGUE  ZBKNU                                               EILI5673
C***REFER TO  ZBESI,ZBESK,ZAIRY,ZBESH                                   EILI5674
C                                                                       EILI5675
C     ZBKNU COMPUTES THE K BESSEL FUNCTION IN THE RIGHT HALF Z PLANE.   EILI5676
C                                                                       EILI5677
C***ROUTINES CALLED  DGAMLN,I1MACH,D1MACH,ZKSCL,ZSHCH,ZUCHK,ZABS,ZDIV,  EILI5678
C                    ZEXP,ZLOG,ZMLT,ZSQRT                               EILI5679
C***END PROLOGUE  ZBKNU                                                 EILI5680
C                                                                       EILI5681
      DOUBLE PRECISION AA, AK, ALIM, ASCLE, A1, A2, BB, BK, BRY, CAZ,   EILI5682
     * CBI, CBR, CC, CCHI, CCHR, CKI, CKR, COEFI, COEFR, CONEI, CONER,  EILI5683
     * CRSCR, CSCLR, CSHI, CSHR, CSI, CSR, CSRR, CSSR, CTWOR,           EILI5684
     * CZEROI, CZEROR, CZI, CZR, DNU, DNU2, DPI, ELIM, ETEST, FC, FHS,  EILI5685
     * FI, FK, FKS, FMUI, FMUR, FNU, FPI, FR, G1, G2, HPI, PI, PR, PTI, EILI5686
     * PTR, P1I, P1R, P2I, P2M, P2R, QI, QR, RAK, RCAZ, RTHPI, RZI,     EILI5687
     * RZR, R1, S, SMUI, SMUR, SPI, STI, STR, S1I, S1R, S2I, S2R, TM,   EILI5688
     * TOL, TTH, T1, T2, YI, YR, ZI, ZR, DGAMLN, D1MACH, ZABS, ELM,     EILI5689
     * CELMR, ZDR, ZDI, AS, ALAS, HELIM, CYR, CYI                       EILI5690
      INTEGER I, IFLAG, INU, K, KFLAG, KK, KMAX, KODE, KODED, N, NZ,    EILI5691
     * IDUM, I1MACH, J, IC, INUB, NW                                    EILI5692
CNEA                                                                    EILI5693
      EXTERNAL ZLOG                                                     EILI5694
CNEA                                                                    EILI5695
      EXTERNAL ZABS                                                     EILI5696
      DIMENSION YR(N), YI(N), CC(8), CSSR(3), CSRR(3), BRY(3), CYR(2),  EILI5697
     * CYI(2)                                                           EILI5698
C     COMPLEX Z,Y,A,B,RZ,SMU,FU,FMU,F,FLRZ,CZ,S1,S2,CSH,CCH             EILI5699
C     COMPLEX CK,P,Q,COEF,P1,P2,CBK,PT,CZERO,CONE,CTWO,ST,EZ,CS,DK      EILI5700
C                                                                       EILI5701
      DATA KMAX / 30 /                                                  EILI5702
      DATA CZEROR,CZEROI,CONER,CONEI,CTWOR,R1/                          EILI5703
     1  0.0D0 , 0.0D0 , 1.0D0 , 0.0D0 , 2.0D0 , 2.0D0 /                 EILI5704
      DATA DPI, RTHPI, SPI ,HPI, FPI, TTH /                             EILI5705
     1     3.14159265358979324D0,       1.25331413731550025D0,          EILI5706
     2     1.90985931710274403D0,       1.57079632679489662D0,          EILI5707
     3     1.89769999331517738D0,       6.66666666666666666D-01/        EILI5708
      DATA CC(1), CC(2), CC(3), CC(4), CC(5), CC(6), CC(7), CC(8)/      EILI5709
     1     5.77215664901532861D-01,    -4.20026350340952355D-02,        EILI5710
     2    -4.21977345555443367D-02,     7.21894324666309954D-03,        EILI5711
     3    -2.15241674114950973D-04,    -2.01348547807882387D-05,        EILI5712
     4     1.13302723198169588D-06,     6.11609510448141582D-09/        EILI5713
C                                                                       EILI5714
      CAZ = ZABS(ZR,ZI)                                                 EILI5715
      CSCLR = 1.0D0/TOL                                                 EILI5716
      CRSCR = TOL                                                       EILI5717
      CSSR(1) = CSCLR                                                   EILI5718
      CSSR(2) = 1.0D0                                                   EILI5719
      CSSR(3) = CRSCR                                                   EILI5720
      CSRR(1) = CRSCR                                                   EILI5721
      CSRR(2) = 1.0D0                                                   EILI5722
      CSRR(3) = CSCLR                                                   EILI5723
      BRY(1) = 1.0D+3*D1MACH(1)/TOL                                     EILI5724
      BRY(2) = 1.0D0/BRY(1)                                             EILI5725
      BRY(3) = D1MACH(2)                                                EILI5726
      NZ = 0                                                            EILI5727
      IFLAG = 0                                                         EILI5728
      KODED = KODE                                                      EILI5729
      RCAZ = 1.0D0/CAZ                                                  EILI5730
      STR = ZR*RCAZ                                                     EILI5731
      STI = -ZI*RCAZ                                                    EILI5732
      RZR = (STR+STR)*RCAZ                                              EILI5733
      RZI = (STI+STI)*RCAZ                                              EILI5734
      INU = INT(SNGL(FNU+0.5D0))                                        EILI5735
      DNU = FNU - DBLE(FLOAT(INU))                                      EILI5736
      IF (DABS(DNU).EQ.0.5D0) GO TO 110                                 EILI5737
      DNU2 = 0.0D0                                                      EILI5738
      IF (DABS(DNU).GT.TOL) DNU2 = DNU*DNU                              EILI5739
      IF (CAZ.GT.R1) GO TO 110                                          EILI5740
C-----------------------------------------------------------------------EILI5741
C     SERIES FOR CABS(Z).LE.R1                                          EILI5742
C-----------------------------------------------------------------------EILI5743
      FC = 1.0D0                                                        EILI5744
      CALL ZLOG(RZR, RZI, SMUR, SMUI, IDUM)                             EILI5745
      FMUR = SMUR*DNU                                                   EILI5746
      FMUI = SMUI*DNU                                                   EILI5747
      CALL ZSHCH(FMUR, FMUI, CSHR, CSHI, CCHR, CCHI)                    EILI5748
      IF (DNU.EQ.0.0D0) GO TO 10                                        EILI5749
      FC = DNU*DPI                                                      EILI5750
      FC = FC/DSIN(FC)                                                  EILI5751
      SMUR = CSHR/DNU                                                   EILI5752
      SMUI = CSHI/DNU                                                   EILI5753
   10 CONTINUE                                                          EILI5754
      A2 = 1.0D0 + DNU                                                  EILI5755
C-----------------------------------------------------------------------EILI5756
C     GAM(1-Z)*GAM(1+Z)=PI*Z/SIN(PI*Z), T1=1/GAM(1-DNU), T2=1/GAM(1+DNU)EILI5757
C-----------------------------------------------------------------------EILI5758
      T2 = DEXP(-DGAMLN(A2,IDUM))                                       EILI5759
      T1 = 1.0D0/(T2*FC)                                                EILI5760
      IF (DABS(DNU).GT.0.1D0) GO TO 40                                  EILI5761
C-----------------------------------------------------------------------EILI5762
C     SERIES FOR F0 TO RESOLVE INDETERMINACY FOR SMALL ABS(DNU)         EILI5763
C-----------------------------------------------------------------------EILI5764
      AK = 1.0D0                                                        EILI5765
      S = CC(1)                                                         EILI5766
      DO 20 K=2,8                                                       EILI5767
        AK = AK*DNU2                                                    EILI5768
        TM = CC(K)*AK                                                   EILI5769
        S = S + TM                                                      EILI5770
        IF (DABS(TM).LT.TOL) GO TO 30                                   EILI5771
   20 CONTINUE                                                          EILI5772
   30 G1 = -S                                                           EILI5773
      GO TO 50                                                          EILI5774
   40 CONTINUE                                                          EILI5775
      G1 = (T1-T2)/(DNU+DNU)                                            EILI5776
   50 CONTINUE                                                          EILI5777
      G2 = (T1+T2)*0.5D0                                                EILI5778
      FR = FC*(CCHR*G1+SMUR*G2)                                         EILI5779
      FI = FC*(CCHI*G1+SMUI*G2)                                         EILI5780
      CALL RRZEXP(FMUR, FMUI, STR, STI)                                 EILI5781
      PR = 0.5D0*STR/T2                                                 EILI5782
      PI = 0.5D0*STI/T2                                                 EILI5783
      CALL ZDIV(0.5D0, 0.0D0, STR, STI, PTR, PTI)                       EILI5784
      QR = PTR/T1                                                       EILI5785
      QI = PTI/T1                                                       EILI5786
      S1R = FR                                                          EILI5787
      S1I = FI                                                          EILI5788
      S2R = PR                                                          EILI5789
      S2I = PI                                                          EILI5790
      AK = 1.0D0                                                        EILI5791
      A1 = 1.0D0                                                        EILI5792
      CKR = CONER                                                       EILI5793
      CKI = CONEI                                                       EILI5794
      BK = 1.0D0 - DNU2                                                 EILI5795
      IF (INU.GT.0 .OR. N.GT.1) GO TO 80                                EILI5796
C-----------------------------------------------------------------------EILI5797
C     GENERATE K(FNU,Z), 0.0D0 .LE. FNU .LT. 0.5D0 AND N=1              EILI5798
C-----------------------------------------------------------------------EILI5799
      IF (CAZ.LT.TOL) GO TO 70                                          EILI5800
      CALL ZMLT(ZR, ZI, ZR, ZI, CZR, CZI)                               EILI5801
      CZR = 0.25D0*CZR                                                  EILI5802
      CZI = 0.25D0*CZI                                                  EILI5803
      T1 = 0.25D0*CAZ*CAZ                                               EILI5804
   60 CONTINUE                                                          EILI5805
      FR = (FR*AK+PR+QR)/BK                                             EILI5806
      FI = (FI*AK+PI+QI)/BK                                             EILI5807
      STR = 1.0D0/(AK-DNU)                                              EILI5808
      PR = PR*STR                                                       EILI5809
      PI = PI*STR                                                       EILI5810
      STR = 1.0D0/(AK+DNU)                                              EILI5811
      QR = QR*STR                                                       EILI5812
      QI = QI*STR                                                       EILI5813
      STR = CKR*CZR - CKI*CZI                                           EILI5814
      RAK = 1.0D0/AK                                                    EILI5815
      CKI = (CKR*CZI+CKI*CZR)*RAK                                       EILI5816
      CKR = STR*RAK                                                     EILI5817
      S1R = CKR*FR - CKI*FI + S1R                                       EILI5818
      S1I = CKR*FI + CKI*FR + S1I                                       EILI5819
      A1 = A1*T1*RAK                                                    EILI5820
      BK = BK + AK + AK + 1.0D0                                         EILI5821
      AK = AK + 1.0D0                                                   EILI5822
      IF (A1.GT.TOL) GO TO 60                                           EILI5823
   70 CONTINUE                                                          EILI5824
      YR(1) = S1R                                                       EILI5825
      YI(1) = S1I                                                       EILI5826
      IF (KODED.EQ.1) RETURN                                            EILI5827
      CALL RRZEXP(ZR, ZI, STR, STI)                                     EILI5828
      CALL ZMLT(S1R, S1I, STR, STI, YR(1), YI(1))                       EILI5829
      RETURN                                                            EILI5830
C-----------------------------------------------------------------------EILI5831
C     GENERATE K(DNU,Z) AND K(DNU+1,Z) FOR FORWARD RECURRENCE           EILI5832
C-----------------------------------------------------------------------EILI5833
   80 CONTINUE                                                          EILI5834
      IF (CAZ.LT.TOL) GO TO 100                                         EILI5835
      CALL ZMLT(ZR, ZI, ZR, ZI, CZR, CZI)                               EILI5836
      CZR = 0.25D0*CZR                                                  EILI5837
      CZI = 0.25D0*CZI                                                  EILI5838
      T1 = 0.25D0*CAZ*CAZ                                               EILI5839
   90 CONTINUE                                                          EILI5840
      FR = (FR*AK+PR+QR)/BK                                             EILI5841
      FI = (FI*AK+PI+QI)/BK                                             EILI5842
      STR = 1.0D0/(AK-DNU)                                              EILI5843
      PR = PR*STR                                                       EILI5844
      PI = PI*STR                                                       EILI5845
      STR = 1.0D0/(AK+DNU)                                              EILI5846
      QR = QR*STR                                                       EILI5847
      QI = QI*STR                                                       EILI5848
      STR = CKR*CZR - CKI*CZI                                           EILI5849
      RAK = 1.0D0/AK                                                    EILI5850
      CKI = (CKR*CZI+CKI*CZR)*RAK                                       EILI5851
      CKR = STR*RAK                                                     EILI5852
      S1R = CKR*FR - CKI*FI + S1R                                       EILI5853
      S1I = CKR*FI + CKI*FR + S1I                                       EILI5854
      STR = PR - FR*AK                                                  EILI5855
      STI = PI - FI*AK                                                  EILI5856
      S2R = CKR*STR - CKI*STI + S2R                                     EILI5857
      S2I = CKR*STI + CKI*STR + S2I                                     EILI5858
      A1 = A1*T1*RAK                                                    EILI5859
      BK = BK + AK + AK + 1.0D0                                         EILI5860
      AK = AK + 1.0D0                                                   EILI5861
      IF (A1.GT.TOL) GO TO 90                                           EILI5862
  100 CONTINUE                                                          EILI5863
      KFLAG = 2                                                         EILI5864
      A1 = FNU + 1.0D0                                                  EILI5865
      AK = A1*DABS(SMUR)                                                EILI5866
      IF (AK.GT.ALIM) KFLAG = 3                                         EILI5867
      STR = CSSR(KFLAG)                                                 EILI5868
      P2R = S2R*STR                                                     EILI5869
      P2I = S2I*STR                                                     EILI5870
      CALL ZMLT(P2R, P2I, RZR, RZI, S2R, S2I)                           EILI5871
      S1R = S1R*STR                                                     EILI5872
      S1I = S1I*STR                                                     EILI5873
      IF (KODED.EQ.1) GO TO 210                                         EILI5874
      CALL RRZEXP(ZR, ZI, FR, FI)                                       EILI5875
      CALL ZMLT(S1R, S1I, FR, FI, S1R, S1I)                             EILI5876
      CALL ZMLT(S2R, S2I, FR, FI, S2R, S2I)                             EILI5877
      GO TO 210                                                         EILI5878
C-----------------------------------------------------------------------EILI5879
C     IFLAG=0 MEANS NO UNDERFLOW OCCURRED                               EILI5880
C     IFLAG=1 MEANS AN UNDERFLOW OCCURRED- COMPUTATION PROCEEDS WITH    EILI5881
C     KODED=2 AND A TEST FOR ON SCALE VALUES IS MADE DURING FORWARD     EILI5882
C     RECURSION                                                         EILI5883
C-----------------------------------------------------------------------EILI5884
  110 CONTINUE                                                          EILI5885
      CALL RRZSQRT(ZR, ZI, STR, STI)                                    EILI5886
      CALL ZDIV(RTHPI, CZEROI, STR, STI, COEFR, COEFI)                  EILI5887
      KFLAG = 2                                                         EILI5888
      IF (KODED.EQ.2) GO TO 120                                         EILI5889
      IF (ZR.GT.ALIM) GO TO 290                                         EILI5890
C     BLANK LINE                                                        EILI5891
      STR = DEXP(-ZR)*CSSR(KFLAG)                                       EILI5892
      STI = -STR*DSIN(ZI)                                               EILI5893
      STR = STR*DCOS(ZI)                                                EILI5894
      CALL ZMLT(COEFR, COEFI, STR, STI, COEFR, COEFI)                   EILI5895
  120 CONTINUE                                                          EILI5896
      IF (DABS(DNU).EQ.0.5D0) GO TO 300                                 EILI5897
C-----------------------------------------------------------------------EILI5898
C     MILLER ALGORITHM FOR CABS(Z).GT.R1                                EILI5899
C-----------------------------------------------------------------------EILI5900
      AK = DCOS(DPI*DNU)                                                EILI5901
      AK = DABS(AK)                                                     EILI5902
      IF (AK.EQ.CZEROR) GO TO 300                                       EILI5903
      FHS = DABS(0.25D0-DNU2)                                           EILI5904
      IF (FHS.EQ.CZEROR) GO TO 300                                      EILI5905
C-----------------------------------------------------------------------EILI5906
C     COMPUTE R2=F(E). IF CABS(Z).GE.R2, USE FORWARD RECURRENCE TO      EILI5907
C     DETERMINE THE BACKWARD INDEX K. R2=F(E) IS A STRAIGHT LINE ON     EILI5908
C     12.LE.E.LE.60. E IS COMPUTED FROM 2**(-E)=B**(1-I1MACH(14))=      EILI5909
C     TOL WHERE B IS THE BASE OF THE ARITHMETIC.                        EILI5910
C-----------------------------------------------------------------------EILI5911
      T1 = DBLE(FLOAT(I1MACH(14)-1))                                    EILI5912
      T1 = T1*D1MACH(5)*3.321928094D0                                   EILI5913
      T1 = DMAX1(T1,12.0D0)                                             EILI5914
      T1 = DMIN1(T1,60.0D0)                                             EILI5915
      T2 = TTH*T1 - 6.0D0                                               EILI5916
      IF (ZR.NE.0.0D0) GO TO 130                                        EILI5917
      T1 = HPI                                                          EILI5918
      GO TO 140                                                         EILI5919
  130 CONTINUE                                                          EILI5920
      T1 = DATAN(ZI/ZR)                                                 EILI5921
      T1 = DABS(T1)                                                     EILI5922
  140 CONTINUE                                                          EILI5923
      IF (T2.GT.CAZ) GO TO 170                                          EILI5924
C-----------------------------------------------------------------------EILI5925
C     FORWARD RECURRENCE LOOP WHEN CABS(Z).GE.R2                        EILI5926
C-----------------------------------------------------------------------EILI5927
      ETEST = AK/(DPI*CAZ*TOL)                                          EILI5928
      FK = CONER                                                        EILI5929
      IF (ETEST.LT.CONER) GO TO 180                                     EILI5930
      FKS = CTWOR                                                       EILI5931
      CKR = CAZ + CAZ + CTWOR                                           EILI5932
      P1R = CZEROR                                                      EILI5933
      P2R = CONER                                                       EILI5934
      DO 150 I=1,KMAX                                                   EILI5935
        AK = FHS/FKS                                                    EILI5936
        CBR = CKR/(FK+CONER)                                            EILI5937
        PTR = P2R                                                       EILI5938
        P2R = CBR*P2R - P1R*AK                                          EILI5939
        P1R = PTR                                                       EILI5940
        CKR = CKR + CTWOR                                               EILI5941
        FKS = FKS + FK + FK + CTWOR                                     EILI5942
        FHS = FHS + FK + FK                                             EILI5943
        FK = FK + CONER                                                 EILI5944
        STR = DABS(P2R)*FK                                              EILI5945
        IF (ETEST.LT.STR) GO TO 160                                     EILI5946
  150 CONTINUE                                                          EILI5947
      GO TO 310                                                         EILI5948
  160 CONTINUE                                                          EILI5949
      FK = FK + SPI*T1*DSQRT(T2/CAZ)                                    EILI5950
      FHS = DABS(0.25D0-DNU2)                                           EILI5951
      GO TO 180                                                         EILI5952
  170 CONTINUE                                                          EILI5953
C-----------------------------------------------------------------------EILI5954
C     COMPUTE BACKWARD INDEX K FOR CABS(Z).LT.R2                        EILI5955
C-----------------------------------------------------------------------EILI5956
      A2 = DSQRT(CAZ)                                                   EILI5957
      AK = FPI*AK/(TOL*DSQRT(A2))                                       EILI5958
      AA = 3.0D0*T1/(1.0D0+CAZ)                                         EILI5959
      BB = 14.7D0*T1/(28.0D0+CAZ)                                       EILI5960
      AK = (DLOG(AK)+CAZ*DCOS(AA)/(1.0D0+0.008D0*CAZ))/DCOS(BB)         EILI5961
      FK = 0.12125D0*AK*AK/CAZ + 1.5D0                                  EILI5962
  180 CONTINUE                                                          EILI5963
C-----------------------------------------------------------------------EILI5964
C     BACKWARD RECURRENCE LOOP FOR MILLER ALGORITHM                     EILI5965
C-----------------------------------------------------------------------EILI5966
      K = INT(SNGL(FK))                                                 EILI5967
      FK = DBLE(FLOAT(K))                                               EILI5968
      FKS = FK*FK                                                       EILI5969
      P1R = CZEROR                                                      EILI5970
      P1I = CZEROI                                                      EILI5971
      P2R = TOL                                                         EILI5972
      P2I = CZEROI                                                      EILI5973
      CSR = P2R                                                         EILI5974
      CSI = P2I                                                         EILI5975
      DO 190 I=1,K                                                      EILI5976
        A1 = FKS - FK                                                   EILI5977
        AK = (FKS+FK)/(A1+FHS)                                          EILI5978
        RAK = 2.0D0/(FK+CONER)                                          EILI5979
        CBR = (FK+ZR)*RAK                                               EILI5980
        CBI = ZI*RAK                                                    EILI5981
        PTR = P2R                                                       EILI5982
        PTI = P2I                                                       EILI5983
        P2R = (PTR*CBR-PTI*CBI-P1R)*AK                                  EILI5984
        P2I = (PTI*CBR+PTR*CBI-P1I)*AK                                  EILI5985
        P1R = PTR                                                       EILI5986
        P1I = PTI                                                       EILI5987
        CSR = CSR + P2R                                                 EILI5988
        CSI = CSI + P2I                                                 EILI5989
        FKS = A1 - FK + CONER                                           EILI5990
        FK = FK - CONER                                                 EILI5991
  190 CONTINUE                                                          EILI5992
C-----------------------------------------------------------------------EILI5993
C     COMPUTE (P2/CS)=(P2/CABS(CS))*(CONJG(CS)/CABS(CS)) FOR BETTER     EILI5994
C     SCALING                                                           EILI5995
C-----------------------------------------------------------------------EILI5996
      TM = ZABS(CSR,CSI)                                                EILI5997
      PTR = 1.0D0/TM                                                    EILI5998
      S1R = P2R*PTR                                                     EILI5999
      S1I = P2I*PTR                                                     EILI6000
      CSR = CSR*PTR                                                     EILI6001
      CSI = -CSI*PTR                                                    EILI6002
      CALL ZMLT(COEFR, COEFI, S1R, S1I, STR, STI)                       EILI6003
      CALL ZMLT(STR, STI, CSR, CSI, S1R, S1I)                           EILI6004
      IF (INU.GT.0 .OR. N.GT.1) GO TO 200                               EILI6005
      ZDR = ZR                                                          EILI6006
      ZDI = ZI                                                          EILI6007
      IF(IFLAG.EQ.1) GO TO 270                                          EILI6008
      GO TO 240                                                         EILI6009
  200 CONTINUE                                                          EILI6010
C-----------------------------------------------------------------------EILI6011
C     COMPUTE P1/P2=(P1/CABS(P2)*CONJG(P2)/CABS(P2) FOR SCALING         EILI6012
C-----------------------------------------------------------------------EILI6013
      TM = ZABS(P2R,P2I)                                                EILI6014
      PTR = 1.0D0/TM                                                    EILI6015
      P1R = P1R*PTR                                                     EILI6016
      P1I = P1I*PTR                                                     EILI6017
      P2R = P2R*PTR                                                     EILI6018
      P2I = -P2I*PTR                                                    EILI6019
      CALL ZMLT(P1R, P1I, P2R, P2I, PTR, PTI)                           EILI6020
      STR = DNU + 0.5D0 - PTR                                           EILI6021
      STI = -PTI                                                        EILI6022
      CALL ZDIV(STR, STI, ZR, ZI, STR, STI)                             EILI6023
      STR = STR + 1.0D0                                                 EILI6024
      CALL ZMLT(STR, STI, S1R, S1I, S2R, S2I)                           EILI6025
C-----------------------------------------------------------------------EILI6026
C     FORWARD RECURSION ON THE THREE TERM RECURSION WITH RELATION WITH  EILI6027
C     SCALING NEAR EXPONENT EXTREMES ON KFLAG=1 OR KFLAG=3              EILI6028
C-----------------------------------------------------------------------EILI6029
  210 CONTINUE                                                          EILI6030
      STR = DNU + 1.0D0                                                 EILI6031
      CKR = STR*RZR                                                     EILI6032
      CKI = STR*RZI                                                     EILI6033
      IF (N.EQ.1) INU = INU - 1                                         EILI6034
      IF (INU.GT.0) GO TO 220                                           EILI6035
      IF (N.GT.1) GO TO 215                                             EILI6036
      S1R = S2R                                                         EILI6037
      S1I = S2I                                                         EILI6038
  215 CONTINUE                                                          EILI6039
      ZDR = ZR                                                          EILI6040
      ZDI = ZI                                                          EILI6041
      IF(IFLAG.EQ.1) GO TO 270                                          EILI6042
      GO TO 240                                                         EILI6043
  220 CONTINUE                                                          EILI6044
      INUB = 1                                                          EILI6045
      IF(IFLAG.EQ.1) GO TO 261                                          EILI6046
  225 CONTINUE                                                          EILI6047
      P1R = CSRR(KFLAG)                                                 EILI6048
      ASCLE = BRY(KFLAG)                                                EILI6049
      DO 230 I=INUB,INU                                                 EILI6050
        STR = S2R                                                       EILI6051
        STI = S2I                                                       EILI6052
        S2R = CKR*STR - CKI*STI + S1R                                   EILI6053
        S2I = CKR*STI + CKI*STR + S1I                                   EILI6054
        S1R = STR                                                       EILI6055
        S1I = STI                                                       EILI6056
        CKR = CKR + RZR                                                 EILI6057
        CKI = CKI + RZI                                                 EILI6058
        IF (KFLAG.GE.3) GO TO 230                                       EILI6059
        P2R = S2R*P1R                                                   EILI6060
        P2I = S2I*P1R                                                   EILI6061
        STR = DABS(P2R)                                                 EILI6062
        STI = DABS(P2I)                                                 EILI6063
        P2M = DMAX1(STR,STI)                                            EILI6064
        IF (P2M.LE.ASCLE) GO TO 230                                     EILI6065
        KFLAG = KFLAG + 1                                               EILI6066
        ASCLE = BRY(KFLAG)                                              EILI6067
        S1R = S1R*P1R                                                   EILI6068
        S1I = S1I*P1R                                                   EILI6069
        S2R = P2R                                                       EILI6070
        S2I = P2I                                                       EILI6071
        STR = CSSR(KFLAG)                                               EILI6072
        S1R = S1R*STR                                                   EILI6073
        S1I = S1I*STR                                                   EILI6074
        S2R = S2R*STR                                                   EILI6075
        S2I = S2I*STR                                                   EILI6076
        P1R = CSRR(KFLAG)                                               EILI6077
  230 CONTINUE                                                          EILI6078
      IF (N.NE.1) GO TO 240                                             EILI6079
      S1R = S2R                                                         EILI6080
      S1I = S2I                                                         EILI6081
  240 CONTINUE                                                          EILI6082
      STR = CSRR(KFLAG)                                                 EILI6083
      YR(1) = S1R*STR                                                   EILI6084
      YI(1) = S1I*STR                                                   EILI6085
      IF (N.EQ.1) RETURN                                                EILI6086
      YR(2) = S2R*STR                                                   EILI6087
      YI(2) = S2I*STR                                                   EILI6088
      IF (N.EQ.2) RETURN                                                EILI6089
      KK = 2                                                            EILI6090
  250 CONTINUE                                                          EILI6091
      KK = KK + 1                                                       EILI6092
      IF (KK.GT.N) RETURN                                               EILI6093
      P1R = CSRR(KFLAG)                                                 EILI6094
      ASCLE = BRY(KFLAG)                                                EILI6095
      DO 260 I=KK,N                                                     EILI6096
        P2R = S2R                                                       EILI6097
        P2I = S2I                                                       EILI6098
        S2R = CKR*P2R - CKI*P2I + S1R                                   EILI6099
        S2I = CKI*P2R + CKR*P2I + S1I                                   EILI6100
        S1R = P2R                                                       EILI6101
        S1I = P2I                                                       EILI6102
        CKR = CKR + RZR                                                 EILI6103
        CKI = CKI + RZI                                                 EILI6104
        P2R = S2R*P1R                                                   EILI6105
        P2I = S2I*P1R                                                   EILI6106
        YR(I) = P2R                                                     EILI6107
        YI(I) = P2I                                                     EILI6108
        IF (KFLAG.GE.3) GO TO 260                                       EILI6109
        STR = DABS(P2R)                                                 EILI6110
        STI = DABS(P2I)                                                 EILI6111
        P2M = DMAX1(STR,STI)                                            EILI6112
        IF (P2M.LE.ASCLE) GO TO 260                                     EILI6113
        KFLAG = KFLAG + 1                                               EILI6114
        ASCLE = BRY(KFLAG)                                              EILI6115
        S1R = S1R*P1R                                                   EILI6116
        S1I = S1I*P1R                                                   EILI6117
        S2R = P2R                                                       EILI6118
        S2I = P2I                                                       EILI6119
        STR = CSSR(KFLAG)                                               EILI6120
        S1R = S1R*STR                                                   EILI6121
        S1I = S1I*STR                                                   EILI6122
        S2R = S2R*STR                                                   EILI6123
        S2I = S2I*STR                                                   EILI6124
        P1R = CSRR(KFLAG)                                               EILI6125
  260 CONTINUE                                                          EILI6126
      RETURN                                                            EILI6127
C-----------------------------------------------------------------------EILI6128
C     IFLAG=1 CASES, FORWARD RECURRENCE ON SCALED VALUES ON UNDERFLOW   EILI6129
C-----------------------------------------------------------------------EILI6130
  261 CONTINUE                                                          EILI6131
      HELIM = 0.5D0*ELIM                                                EILI6132
      ELM = DEXP(-ELIM)                                                 EILI6133
      CELMR = ELM                                                       EILI6134
      ASCLE = BRY(1)                                                    EILI6135
      ZDR = ZR                                                          EILI6136
      ZDI = ZI                                                          EILI6137
      IC = -1                                                           EILI6138
      J = 2                                                             EILI6139
      DO 262 I=1,INU                                                    EILI6140
        STR = S2R                                                       EILI6141
        STI = S2I                                                       EILI6142
        S2R = STR*CKR-STI*CKI+S1R                                       EILI6143
        S2I = STI*CKR+STR*CKI+S1I                                       EILI6144
        S1R = STR                                                       EILI6145
        S1I = STI                                                       EILI6146
        CKR = CKR+RZR                                                   EILI6147
        CKI = CKI+RZI                                                   EILI6148
        AS = ZABS(S2R,S2I)                                              EILI6149
        ALAS = DLOG(AS)                                                 EILI6150
        P2R = -ZDR+ALAS                                                 EILI6151
        IF(P2R.LT.(-ELIM)) GO TO 263                                    EILI6152
        CALL ZLOG(S2R,S2I,STR,STI,IDUM)                                 EILI6153
        P2R = -ZDR+STR                                                  EILI6154
        P2I = -ZDI+STI                                                  EILI6155
        P2M = DEXP(P2R)/TOL                                             EILI6156
        P1R = P2M*DCOS(P2I)                                             EILI6157
        P1I = P2M*DSIN(P2I)                                             EILI6158
        CALL ZUCHK(P1R,P1I,NW,ASCLE,TOL)                                EILI6159
        IF(NW.NE.0) GO TO 263                                           EILI6160
        J = 3 - J                                                       EILI6161
        CYR(J) = P1R                                                    EILI6162
        CYI(J) = P1I                                                    EILI6163
        IF(IC.EQ.(I-1)) GO TO 264                                       EILI6164
        IC = I                                                          EILI6165
        GO TO 262                                                       EILI6166
  263   CONTINUE                                                        EILI6167
        IF(ALAS.LT.HELIM) GO TO 262                                     EILI6168
        ZDR = ZDR-ELIM                                                  EILI6169
        S1R = S1R*CELMR                                                 EILI6170
        S1I = S1I*CELMR                                                 EILI6171
        S2R = S2R*CELMR                                                 EILI6172
        S2I = S2I*CELMR                                                 EILI6173
  262 CONTINUE                                                          EILI6174
      IF(N.NE.1) GO TO 270                                              EILI6175
      S1R = S2R                                                         EILI6176
      S1I = S2I                                                         EILI6177
      GO TO 270                                                         EILI6178
  264 CONTINUE                                                          EILI6179
      KFLAG = 1                                                         EILI6180
      INUB = I+1                                                        EILI6181
      S2R = CYR(J)                                                      EILI6182
      S2I = CYI(J)                                                      EILI6183
      J = 3 - J                                                         EILI6184
      S1R = CYR(J)                                                      EILI6185
      S1I = CYI(J)                                                      EILI6186
      IF(INUB.LE.INU) GO TO 225                                         EILI6187
      IF(N.NE.1) GO TO 240                                              EILI6188
      S1R = S2R                                                         EILI6189
      S1I = S2I                                                         EILI6190
      GO TO 240                                                         EILI6191
  270 CONTINUE                                                          EILI6192
      YR(1) = S1R                                                       EILI6193
      YI(1) = S1I                                                       EILI6194
      IF(N.EQ.1) GO TO 280                                              EILI6195
      YR(2) = S2R                                                       EILI6196
      YI(2) = S2I                                                       EILI6197
  280 CONTINUE                                                          EILI6198
      ASCLE = BRY(1)                                                    EILI6199
      CALL ZKSCL(ZDR,ZDI,FNU,N,YR,YI,NZ,RZR,RZI,ASCLE,TOL,ELIM)         EILI6200
      INU = N - NZ                                                      EILI6201
      IF (INU.LE.0) RETURN                                              EILI6202
      KK = NZ + 1                                                       EILI6203
      S1R = YR(KK)                                                      EILI6204
      S1I = YI(KK)                                                      EILI6205
      YR(KK) = S1R*CSRR(1)                                              EILI6206
      YI(KK) = S1I*CSRR(1)                                              EILI6207
      IF (INU.EQ.1) RETURN                                              EILI6208
      KK = NZ + 2                                                       EILI6209
      S2R = YR(KK)                                                      EILI6210
      S2I = YI(KK)                                                      EILI6211
      YR(KK) = S2R*CSRR(1)                                              EILI6212
      YI(KK) = S2I*CSRR(1)                                              EILI6213
      IF (INU.EQ.2) RETURN                                              EILI6214
      T2 = FNU + DBLE(FLOAT(KK-1))                                      EILI6215
      CKR = T2*RZR                                                      EILI6216
      CKI = T2*RZI                                                      EILI6217
      KFLAG = 1                                                         EILI6218
      GO TO 250                                                         EILI6219
  290 CONTINUE                                                          EILI6220
C-----------------------------------------------------------------------EILI6221
C     SCALE BY DEXP(Z), IFLAG = 1 CASES                                 EILI6222
C-----------------------------------------------------------------------EILI6223
      KODED = 2                                                         EILI6224
      IFLAG = 1                                                         EILI6225
      KFLAG = 2                                                         EILI6226
      GO TO 120                                                         EILI6227
C-----------------------------------------------------------------------EILI6228
C     FNU=HALF ODD INTEGER CASE, DNU=-0.5                               EILI6229
C-----------------------------------------------------------------------EILI6230
  300 CONTINUE                                                          EILI6231
      S1R = COEFR                                                       EILI6232
      S1I = COEFI                                                       EILI6233
      S2R = COEFR                                                       EILI6234
      S2I = COEFI                                                       EILI6235
      GO TO 210                                                         EILI6236
C                                                                       EILI6237
C                                                                       EILI6238
  310 CONTINUE                                                          EILI6239
      NZ=-2                                                             EILI6240
      RETURN                                                            EILI6241
      END      