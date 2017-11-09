!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:20:23 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZAIRY(ZR, ZI, ID, KODE, AIR, AII, NZ, IERR)            EILI4694
C***BEGIN PROLOGUE  ZAIRY                                               EILI4695
C***DATE WRITTEN   830501   (YYMMDD)                                    EILI4696
C***REVISION DATE  890801   (YYMMDD)                                    EILI4697
C***CATEGORY NO.  B5K                                                   EILI4698
C***KEYWORDS  AIRY FUNCTION,BESSEL FUNCTIONS OF ORDER ONE THIRD         EILI4699
C***AUTHOR  AMOS, DONALD E., SANDIA NATIONAL LABORATORIES               EILI4700
C***PURPOSE  TO COMPUTE AIRY FUNCTIONS AI(Z) AND DAI(Z) FOR COMPLEX Z   EILI4701
C***DESCRIPTION                                                         EILI4702
C                                                                       EILI4703
C                      ***A DOUBLE PRECISION ROUTINE***                 EILI4704
C         ON KODE=1, ZAIRY COMPUTES THE COMPLEX AIRY FUNCTION AI(Z) OR  EILI4705
C         ITS DERIVATIVE DAI(Z)/DZ ON ID=0 OR ID=1 RESPECTIVELY. ON     EILI4706
C         KODE=2, A SCALING OPTION CEXP(ZTA)*AI(Z) OR CEXP(ZTA)*        EILI4707
C         DAI(Z)/DZ IS PROVIDED TO REMOVE THE EXPONENTIAL DECAY IN      EILI4708
C         -PI/3.LT.ARG(Z).LT.PI/3 AND THE EXPONENTIAL GROWTH IN         EILI4709
C         PI/3.LT.ABS(ARG(Z)).LT.PI WHERE ZTA=(2/3)*Z*CSQRT(Z).         EILI4710
C                                                                       EILI4711
C         WHILE THE AIRY FUNCTIONS AI(Z) AND DAI(Z)/DZ ARE ANALYTIC IN  EILI4712
C         THE WHOLE Z PLANE, THE CORRESPONDING SCALED FUNCTIONS DEFINED EILI4713
C         FOR KODE=2 HAVE A CUT ALONG THE NEGATIVE REAL AXIS.           EILI4714
C         DEFINTIONS AND NOTATION ARE FOUND IN THE NBS HANDBOOK OF      EILI4715
C         MATHEMATICAL FUNCTIONS (REF. 1).                              EILI4716
C                                                                       EILI4717
C         INPUT      ZR,ZI ARE DOUBLE PRECISION                         EILI4718
C           ZR,ZI  - Z=CMPLX(ZR,ZI)                                     EILI4719
C           ID     - ORDER OF DERIVATIVE, ID=0 OR ID=1                  EILI4720
C           KODE   - A PARAMETER TO INDICATE THE SCALING OPTION         EILI4721
C                    KODE= 1  RETURNS                                   EILI4722
C                             AI=AI(Z)                ON ID=0 OR        EILI4723
C                             AI=DAI(Z)/DZ            ON ID=1           EILI4724
C                        = 2  RETURNS                                   EILI4725
C                             AI=CEXP(ZTA)*AI(Z)       ON ID=0 OR       EILI4726
C                             AI=CEXP(ZTA)*DAI(Z)/DZ   ON ID=1 WHERE    EILI4727
C                             ZTA=(2/3)*Z*CSQRT(Z)                      EILI4728
C                                                                       EILI4729
C         OUTPUT     AIR,AII ARE DOUBLE PRECISION                       EILI4730
C           AIR,AII- COMPLEX ANSWER DEPENDING ON THE CHOICES FOR ID AND EILI4731
C                    KODE                                               EILI4732
C           NZ     - UNDERFLOW INDICATOR                                EILI4733
C                    NZ= 0   , NORMAL RETURN                            EILI4734
C                    NZ= 1   , AI=CMPLX(0.0D0,0.0D0) DUE TO UNDERFLOW INEILI4735
C                              -PI/3.LT.ARG(Z).LT.PI/3 ON KODE=1        EILI4736
C           IERR   - ERROR FLAG                                         EILI4737
C                    IERR=0, NORMAL RETURN - COMPUTATION COMPLETED      EILI4738
C                    IERR=1, INPUT ERROR   - NO COMPUTATION             EILI4739
C                    IERR=2, OVERFLOW      - NO COMPUTATION, REAL(ZTA)  EILI4740
C                            TOO LARGE ON KODE=1                        EILI4741
C                    IERR=3, CABS(Z) LARGE      - COMPUTATION COMPLETED EILI4742
C                            LOSSES OF SIGNIFCANCE BY ARGUMENT REDUCTIONEILI4743
C                            PRODUCE LESS THAN HALF OF MACHINE ACCURACY EILI4744
C                    IERR=4, CABS(Z) TOO LARGE  - NO COMPUTATION        EILI4745
C                            COMPLETE LOSS OF ACCURACY BY ARGUMENT      EILI4746
C                            REDUCTION                                  EILI4747
C                    IERR=5, ERROR              - NO COMPUTATION,       EILI4748
C                            ALGORITHM TERMINATION CONDITION NOT MET    EILI4749
C                                                                       EILI4750
C***LONG DESCRIPTION                                                    EILI4751
C                                                                       EILI4752
C         AI AND DAI ARE COMPUTED FOR CABS(Z).GT.1.0 FROM THE K BESSEL  EILI4753
C         FUNCTIONS BY                                                  EILI4754
C                                                                       EILI4755
C            AI(Z)=C*SQRT(Z)*K(1/3,ZTA) , DAI(Z)=-C*Z*K(2/3,ZTA)        EILI4756
C                           C=1.0/(PI*SQRT(3.0))                        EILI4757
C                            ZTA=(2/3)*Z**(3/2)                         EILI4758
C                                                                       EILI4759
C         WITH THE POWER SERIES FOR CABS(Z).LE.1.0.                     EILI4760
C                                                                       EILI4761
C         IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST EVALUATE ELE-  EILI4762
C         MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z IS LARGE, LOSSES   EILI4763
C         OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR. CONSEQUENTLY, IF EILI4764
C         THE MAGNITUDE OF ZETA=(2/3)*Z**1.5 EXCEEDS U1=SQRT(0.5/UR),   EILI4765
C         THEN LOSSES EXCEEDING HALF PRECISION ARE LIKELY AND AN ERROR  EILI4766
C         FLAG IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS EILI4767
C         DOUBLE PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION.EILI4768
C         ALSO, IF THE MAGNITUDE OF ZETA IS LARGER THAN U2=0.5/UR, THEN EILI4769
C         ALL SIGNIFICANCE IS LOST AND IERR=4. IN ORDER TO USE THE INT  EILI4770
C         FUNCTION, ZETA MUST BE FURTHER RESTRICTED NOT TO EXCEED THE   EILI4771
C         LARGEST INTEGER, U3=I1MACH(9). THUS, THE MAGNITUDE OF ZETA    EILI4772
C         MUST BE RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2,  EILI4773
C         AND U3 ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN SINGLE     EILI4774
C         PRECISION ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE    EILI4775
C         PRECISION ARITHMETIC RESPECTIVELY. THIS MAKES U2 AND U3 LIMIT-EILI4776
C         ING IN THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT THE MAG- EILI4777
C         NITUDE OF Z CANNOT EXCEED 3.1E+4 IN SINGLE AND 2.1E+6 IN      EILI4778
C         DOUBLE PRECISION ARITHMETIC. THIS ALSO MEANS THAT ONE CAN     EILI4779
C         EXPECT TO RETAIN, IN THE WORST CASES ON 32 BIT MACHINES,      EILI4780
C         NO DIGITS IN SINGLE PRECISION AND ONLY 7 DIGITS IN DOUBLE     EILI4781
C         PRECISION ARITHMETIC. SIMILAR CONSIDERATIONS HOLD FOR OTHER   EILI4782
C         MACHINES.                                                     EILI4783
C                                                                       EILI4784
C         THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A COMPLEX  EILI4785
C         BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P=MAX(UNIT  EILI4786
C         ROUNDOFF,1.0E-18) IS THE NOMINAL PRECISION AND 10**S REPRE-   EILI4787
C         SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE  EILI4788
C         ELEMENTARY FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))),      EILI4789
C         ABS(LOG10(FNU))) APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF  EILI4790
C         CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY EILI4791
C         HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST LIKELY TO OCCUR WHENEILI4792
C         ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY EILI4793
C         SEVERAL ORDERS OF MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER EILI4794
C         THAN THE OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, EILI4795
C         0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS EILI4796
C         THE EXPONENT OF P, NO SIGNIFICANT DIGITS REMAIN IN THE SMALLEREILI4797
C         COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY EILI4798
C         BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P, THE SMALLER  EILI4799
C         COMPONENT WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE     EILI4800
C         MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES,    EILI4801
C         THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P, -P, PI/2-P,  EILI4802
C         OR -PI/2+P.                                                   EILI4803
C                                                                       EILI4804
C***REFERENCES  HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ     EILI4805
C                 AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF    EILI4806
C                 COMMERCE, 1955.                                       EILI4807
C                                                                       EILI4808
C               COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT     EILI4809
C                 AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983 EILI4810
C                                                                       EILI4811
C               A SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX  EILI4812
C                 ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85- EILI4813
C                 1018, MAY, 1985                                       EILI4814
C                                                                       EILI4815
C               A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX    EILI4816
C                 ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, TRANS.  EILI4817
C                 MATH. SOFTWARE, 1986                                  EILI4818
C                                                                       EILI4819
C***ROUTINES CALLED  ZACAI,ZBKNU,ZEXP,ZSQRT,I1MACH,D1MACH               EILI4820
C***END PROLOGUE  ZAIRY                                                 EILI4821
C     COMPLEX AI,CONE,CSQ,CY,S1,S2,TRM1,TRM2,Z,ZTA,Z3                   EILI4822
      DOUBLE PRECISION AA, AD, AII, AIR, AK, ALIM, ATRM, AZ, AZ3, BK,   EILI4823
     * CC, CK, COEF, CONEI, CONER, CSQI, CSQR, CYI, CYR, C1, C2, DIG,   EILI4824
     * DK, D1, D2, ELIM, FID, FNU, PTR, RL, R1M5, SFAC, STI, STR,       EILI4825
     * S1I, S1R, S2I, S2R, TOL, TRM1I, TRM1R, TRM2I, TRM2R, TTH, ZEROI, EILI4826
     * ZEROR, ZI, ZR, ZTAI, ZTAR, Z3I, Z3R, D1MACH, ZABS, ALAZ, BB      EILI4827
      INTEGER ID, IERR, IFLAG, K, KODE, K1, K2, MR, NN, NZ, I1MACH      EILI4828
      EXTERNAL ZABS                                                     EILI4829
      DIMENSION CYR(1), CYI(1)                                          EILI4830
      DATA TTH, C1, C2, COEF /6.66666666666666667D-01,                  EILI4831
     * 3.55028053887817240D-01,2.58819403792806799D-01,                 EILI4832
     * 1.83776298473930683D-01/                                         EILI4833
      DATA ZEROR, ZEROI, CONER, CONEI /0.0D0,0.0D0,1.0D0,0.0D0/         EILI4834
C***FIRST EXECUTABLE STATEMENT  ZAIRY                                   EILI4835
      IERR = 0                                                          EILI4836
      NZ=0                                                              EILI4837
      IF (ID.LT.0 .OR. ID.GT.1) IERR=1                                  EILI4838
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1                              EILI4839
      IF (IERR.NE.0) RETURN                                             EILI4840
      AZ = ZABS(ZR,ZI)                                                  EILI4841
      TOL = DMAX1(D1MACH(4),1.0D-18)                                    EILI4842
      FID = DBLE(FLOAT(ID))                                             EILI4843
      IF (AZ.GT.1.0D0) GO TO 70                                         EILI4844
C-----------------------------------------------------------------------EILI4845
C     POWER SERIES FOR CABS(Z).LE.1.                                    EILI4846
C-----------------------------------------------------------------------EILI4847
      S1R = CONER                                                       EILI4848
      S1I = CONEI                                                       EILI4849
      S2R = CONER                                                       EILI4850
      S2I = CONEI                                                       EILI4851
      IF (AZ.LT.TOL) GO TO 170                                          EILI4852
      AA = AZ*AZ                                                        EILI4853
      IF (AA.LT.TOL/AZ) GO TO 40                                        EILI4854
      TRM1R = CONER                                                     EILI4855
      TRM1I = CONEI                                                     EILI4856
      TRM2R = CONER                                                     EILI4857
      TRM2I = CONEI                                                     EILI4858
      ATRM = 1.0D0                                                      EILI4859
      STR = ZR*ZR - ZI*ZI                                               EILI4860
      STI = ZR*ZI + ZI*ZR                                               EILI4861
      Z3R = STR*ZR - STI*ZI                                             EILI4862
      Z3I = STR*ZI + STI*ZR                                             EILI4863
      AZ3 = AZ*AA                                                       EILI4864
      AK = 2.0D0 + FID                                                  EILI4865
      BK = 3.0D0 - FID - FID                                            EILI4866
      CK = 4.0D0 - FID                                                  EILI4867
      DK = 3.0D0 + FID + FID                                            EILI4868
      D1 = AK*DK                                                        EILI4869
      D2 = BK*CK                                                        EILI4870
      AD = DMIN1(D1,D2)                                                 EILI4871
      AK = 24.0D0 + 9.0D0*FID                                           EILI4872
      BK = 30.0D0 - 9.0D0*FID                                           EILI4873
      DO 30 K=1,25                                                      EILI4874
        STR = (TRM1R*Z3R-TRM1I*Z3I)/D1                                  EILI4875
        TRM1I = (TRM1R*Z3I+TRM1I*Z3R)/D1                                EILI4876
        TRM1R = STR                                                     EILI4877
        S1R = S1R + TRM1R                                               EILI4878
        S1I = S1I + TRM1I                                               EILI4879
        STR = (TRM2R*Z3R-TRM2I*Z3I)/D2                                  EILI4880
        TRM2I = (TRM2R*Z3I+TRM2I*Z3R)/D2                                EILI4881
        TRM2R = STR                                                     EILI4882
        S2R = S2R + TRM2R                                               EILI4883
        S2I = S2I + TRM2I                                               EILI4884
        ATRM = ATRM*AZ3/AD                                              EILI4885
        D1 = D1 + AK                                                    EILI4886
        D2 = D2 + BK                                                    EILI4887
        AD = DMIN1(D1,D2)                                               EILI4888
        IF (ATRM.LT.TOL*AD) GO TO 40                                    EILI4889
        AK = AK + 18.0D0                                                EILI4890
        BK = BK + 18.0D0                                                EILI4891
   30 CONTINUE                                                          EILI4892
   40 CONTINUE                                                          EILI4893
      IF (ID.EQ.1) GO TO 50                                             EILI4894
      AIR = S1R*C1 - C2*(ZR*S2R-ZI*S2I)                                 EILI4895
      AII = S1I*C1 - C2*(ZR*S2I+ZI*S2R)                                 EILI4896
      IF (KODE.EQ.1) RETURN                                             EILI4897
      CALL RRZSQRT(ZR, ZI, STR, STI)                                    EILI4898
      ZTAR = TTH*(ZR*STR-ZI*STI)                                        EILI4899
      ZTAI = TTH*(ZR*STI+ZI*STR)                                        EILI4900
      CALL RRZEXP(ZTAR, ZTAI, STR, STI)                                 EILI4901
      PTR = AIR*STR - AII*STI                                           EILI4902
      AII = AIR*STI + AII*STR                                           EILI4903
      AIR = PTR                                                         EILI4904
      RETURN                                                            EILI4905
   50 CONTINUE                                                          EILI4906
      AIR = -S2R*C2                                                     EILI4907
      AII = -S2I*C2                                                     EILI4908
      IF (AZ.LE.TOL) GO TO 60                                           EILI4909
      STR = ZR*S1R - ZI*S1I                                             EILI4910
      STI = ZR*S1I + ZI*S1R                                             EILI4911
      CC = C1/(1.0D0+FID)                                               EILI4912
      AIR = AIR + CC*(STR*ZR-STI*ZI)                                    EILI4913
      AII = AII + CC*(STR*ZI+STI*ZR)                                    EILI4914
   60 CONTINUE                                                          EILI4915
      IF (KODE.EQ.1) RETURN                                             EILI4916
      CALL RRZSQRT(ZR, ZI, STR, STI)                                    EILI4917
      ZTAR = TTH*(ZR*STR-ZI*STI)                                        EILI4918
      ZTAI = TTH*(ZR*STI+ZI*STR)                                        EILI4919
      CALL RRZEXP(ZTAR, ZTAI, STR, STI)                                 EILI4920
      PTR = STR*AIR - STI*AII                                           EILI4921
      AII = STR*AII + STI*AIR                                           EILI4922
      AIR = PTR                                                         EILI4923
      RETURN                                                            EILI4924
C-----------------------------------------------------------------------EILI4925
C     CASE FOR CABS(Z).GT.1.0                                           EILI4926
C-----------------------------------------------------------------------EILI4927
   70 CONTINUE                                                          EILI4928
      FNU = (1.0D0+FID)/3.0D0                                           EILI4929
C-----------------------------------------------------------------------EILI4930
C     SET PARAMETERS RELATED TO MACHINE CONSTANTS.                      EILI4931
C     TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0D-18.          EILI4932
C     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.    EILI4933
C     EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL    AND                    EILI4934
C     EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL       ARE INTERVALS NEAR     EILI4935
C     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.    EILI4936
C     RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. EILI4937
C     DIG = NUMBER OF BASE 10 DIGITS IN TOL = 10**(-DIG).               EILI4938
C-----------------------------------------------------------------------EILI4939
      K1 = I1MACH(15)                                                   EILI4940
      K2 = I1MACH(16)                                                   EILI4941
      R1M5 = D1MACH(5)                                                  EILI4942
      K = MIN0(IABS(K1),IABS(K2))                                       EILI4943
      ELIM = 2.303D0*(DBLE(FLOAT(K))*R1M5-3.0D0)                        EILI4944
      K1 = I1MACH(14) - 1                                               EILI4945
      AA = R1M5*DBLE(FLOAT(K1))                                         EILI4946
      DIG = DMIN1(AA,18.0D0)                                            EILI4947
      AA = AA*2.303D0                                                   EILI4948
      ALIM = ELIM + DMAX1(-AA,-41.45D0)                                 EILI4949
      RL = 1.2D0*DIG + 3.0D0                                            EILI4950
      ALAZ = DLOG(AZ)                                                   EILI4951
C-----------------------------------------------------------------------EILI4952
C     TEST FOR PROPER RANGE                                             EILI4953
C-----------------------------------------------------------------------EILI4954
      AA=0.5D0/TOL                                                      EILI4955
      BB=DBLE(FLOAT(I1MACH(9)))*0.5D0                                   EILI4956
      AA=DMIN1(AA,BB)                                                   EILI4957
      AA=AA**TTH                                                        EILI4958
      IF (AZ.GT.AA) GO TO 260                                           EILI4959
      AA=DSQRT(AA)                                                      EILI4960
      IF (AZ.GT.AA) IERR=3                                              EILI4961
      CALL RRZSQRT(ZR, ZI, CSQR, CSQI)                                  EILI4962
      ZTAR = TTH*(ZR*CSQR-ZI*CSQI)                                      EILI4963
      ZTAI = TTH*(ZR*CSQI+ZI*CSQR)                                      EILI4964
C-----------------------------------------------------------------------EILI4965
C     RE(ZTA).LE.0 WHEN RE(Z).LT.0, ESPECIALLY WHEN IM(Z) IS SMALL      EILI4966
C-----------------------------------------------------------------------EILI4967
      IFLAG = 0                                                         EILI4968
      SFAC = 1.0D0                                                      EILI4969
      AK = ZTAI                                                         EILI4970
      IF (ZR.GE.0.0D0) GO TO 80                                         EILI4971
      BK = ZTAR                                                         EILI4972
      CK = -DABS(BK)                                                    EILI4973
      ZTAR = CK                                                         EILI4974
      ZTAI = AK                                                         EILI4975
   80 CONTINUE                                                          EILI4976
      IF (ZI.NE.0.0D0) GO TO 90                                         EILI4977
      IF (ZR.GT.0.0D0) GO TO 90                                         EILI4978
      ZTAR = 0.0D0                                                      EILI4979
      ZTAI = AK                                                         EILI4980
   90 CONTINUE                                                          EILI4981
      AA = ZTAR                                                         EILI4982
      IF (AA.GE.0.0D0 .AND. ZR.GT.0.0D0) GO TO 110                      EILI4983
      IF (KODE.EQ.2) GO TO 100                                          EILI4984
C-----------------------------------------------------------------------EILI4985
C     OVERFLOW TEST                                                     EILI4986
C-----------------------------------------------------------------------EILI4987
      IF (AA.GT.(-ALIM)) GO TO 100                                      EILI4988
      AA = -AA + 0.25D0*ALAZ                                            EILI4989
      IFLAG = 1                                                         EILI4990
      SFAC = TOL                                                        EILI4991
      IF (AA.GT.ELIM) GO TO 270                                         EILI4992
  100 CONTINUE                                                          EILI4993
C-----------------------------------------------------------------------EILI4994
C     CBKNU AND CACON RETURN EXP(ZTA)*K(FNU,ZTA) ON KODE=2              EILI4995
C-----------------------------------------------------------------------EILI4996
      MR = 1                                                            EILI4997
      IF (ZI.LT.0.0D0) MR = -1                                          EILI4998
      CALL ZACAI(ZTAR, ZTAI, FNU, KODE, MR, 1, CYR, CYI, NN, RL, TOL,   EILI4999
     * ELIM, ALIM)                                                      EILI5000
      IF (NN.LT.0) GO TO 280                                            EILI5001
      NZ = NZ + NN                                                      EILI5002
      GO TO 130                                                         EILI5003
  110 CONTINUE                                                          EILI5004
      IF (KODE.EQ.2) GO TO 120                                          EILI5005
C-----------------------------------------------------------------------EILI5006
C     UNDERFLOW TEST                                                    EILI5007
C-----------------------------------------------------------------------EILI5008
      IF (AA.LT.ALIM) GO TO 120                                         EILI5009
      AA = -AA - 0.25D0*ALAZ                                            EILI5010
      IFLAG = 2                                                         EILI5011
      SFAC = 1.0D0/TOL                                                  EILI5012
      IF (AA.LT.(-ELIM)) GO TO 210                                      EILI5013
  120 CONTINUE                                                          EILI5014
      CALL ZBKNU(ZTAR, ZTAI, FNU, KODE, 1, CYR, CYI, NZ, TOL, ELIM,     EILI5015
     * ALIM)                                                            EILI5016
  130 CONTINUE                                                          EILI5017
      S1R = CYR(1)*COEF                                                 EILI5018
      S1I = CYI(1)*COEF                                                 EILI5019
      IF (IFLAG.NE.0) GO TO 150                                         EILI5020
      IF (ID.EQ.1) GO TO 140                                            EILI5021
      AIR = CSQR*S1R - CSQI*S1I                                         EILI5022
      AII = CSQR*S1I + CSQI*S1R                                         EILI5023
      RETURN                                                            EILI5024
  140 CONTINUE                                                          EILI5025
      AIR = -(ZR*S1R-ZI*S1I)                                            EILI5026
      AII = -(ZR*S1I+ZI*S1R)                                            EILI5027
      RETURN                                                            EILI5028
  150 CONTINUE                                                          EILI5029
      S1R = S1R*SFAC                                                    EILI5030
      S1I = S1I*SFAC                                                    EILI5031
      IF (ID.EQ.1) GO TO 160                                            EILI5032
      STR = S1R*CSQR - S1I*CSQI                                         EILI5033
      S1I = S1R*CSQI + S1I*CSQR                                         EILI5034
      S1R = STR                                                         EILI5035
      AIR = S1R/SFAC                                                    EILI5036
      AII = S1I/SFAC                                                    EILI5037
      RETURN                                                            EILI5038
  160 CONTINUE                                                          EILI5039
      STR = -(S1R*ZR-S1I*ZI)                                            EILI5040
      S1I = -(S1R*ZI+S1I*ZR)                                            EILI5041
      S1R = STR                                                         EILI5042
      AIR = S1R/SFAC                                                    EILI5043
      AII = S1I/SFAC                                                    EILI5044
      RETURN                                                            EILI5045
  170 CONTINUE                                                          EILI5046
      AA = 1.0D+3*D1MACH(1)                                             EILI5047
      S1R = ZEROR                                                       EILI5048
      S1I = ZEROI                                                       EILI5049
      IF (ID.EQ.1) GO TO 190                                            EILI5050
      IF (AZ.LE.AA) GO TO 180                                           EILI5051
      S1R = C2*ZR                                                       EILI5052
      S1I = C2*ZI                                                       EILI5053
  180 CONTINUE                                                          EILI5054
      AIR = C1 - S1R                                                    EILI5055
      AII = -S1I                                                        EILI5056
      RETURN                                                            EILI5057
  190 CONTINUE                                                          EILI5058
      AIR = -C2                                                         EILI5059
      AII = 0.0D0                                                       EILI5060
      AA = DSQRT(AA)                                                    EILI5061
      IF (AZ.LE.AA) GO TO 200                                           EILI5062
      S1R = 0.5D0*(ZR*ZR-ZI*ZI)                                         EILI5063
      S1I = ZR*ZI                                                       EILI5064
  200 CONTINUE                                                          EILI5065
      AIR = AIR + C1*S1R                                                EILI5066
      AII = AII + C1*S1I                                                EILI5067
      RETURN                                                            EILI5068
  210 CONTINUE                                                          EILI5069
      NZ = 1                                                            EILI5070
      AIR = ZEROR                                                       EILI5071
      AII = ZEROI                                                       EILI5072
      RETURN                                                            EILI5073
  270 CONTINUE                                                          EILI5074
      NZ = 0                                                            EILI5075
      IERR=2                                                            EILI5076
      RETURN                                                            EILI5077
  280 CONTINUE                                                          EILI5078
      IF(NN.EQ.(-1)) GO TO 270                                          EILI5079
      NZ=0                                                              EILI5080
      IERR=5                                                            EILI5081
      RETURN                                                            EILI5082
  260 CONTINUE                                                          EILI5083
      IERR=4                                                            EILI5084
      NZ=0                                                              EILI5085
      RETURN                                                            EILI5086
      END       