!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 1:35:36 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZBESI(ZR, ZI, FNU, KODE, N, CYR, CYI, NZ, IERR)        EILI3631
C***BEGIN PROLOGUE  ZBESI                                               EILI3632
C***DATE WRITTEN   830501   (YYMMDD)                                    EILI3633
C***REVISION DATE  890801   (YYMMDD)                                    EILI3634
C***CATEGORY NO.  B5K                                                   EILI3635
C***KEYWORDS  I-BESSEL FUNCTION,COMPLEX BESSEL FUNCTION,                EILI3636
C             MODIFIED BESSEL FUNCTION OF THE FIRST KIND                EILI3637
C***AUTHOR  AMOS, DONALD E., SANDIA NATIONAL LABORATORIES               EILI3638
C***PURPOSE  TO COMPUTE I-BESSEL FUNCTIONS OF COMPLEX ARGUMENT          EILI3639
C***DESCRIPTION                                                         EILI3640
C                                                                       EILI3641
C                    ***A DOUBLE PRECISION ROUTINE***                   EILI3642
C         ON KODE=1, ZBESI COMPUTES AN N MEMBER SEQUENCE OF COMPLEX     EILI3643
C         BESSEL FUNCTIONS CY(J)=I(FNU+J-1,Z) FOR REAL, NONNEGATIVE     EILI3644
C         ORDERS FNU+J-1, J=1,...,N AND COMPLEX Z IN THE CUT PLANE      EILI3645
C         -PI.LT.ARG(Z).LE.PI. ON KODE=2, ZBESI RETURNS THE SCALED      EILI3646
C         FUNCTIONS                                                     EILI3647
C                                                                       EILI3648
C         CY(J)=EXP(-ABS(X))*I(FNU+J-1,Z)   J = 1,...,N , X=REAL(Z)     EILI3649
C                                                                       EILI3650
C         WITH THE EXPONENTIAL GROWTH REMOVED IN BOTH THE LEFT AND      EILI3651
C         RIGHT HALF PLANES FOR Z TO INFINITY. DEFINITIONS AND NOTATION EILI3652
C         ARE FOUND IN THE NBS HANDBOOK OF MATHEMATICAL FUNCTIONS       EILI3653
C         (REF. 1).                                                     EILI3654
C                                                                       EILI3655
C         INPUT      ZR,ZI,FNU ARE DOUBLE PRECISION                     EILI3656
C           ZR,ZI  - Z=CMPLX(ZR,ZI),  -PI.LT.ARG(Z).LE.PI               EILI3657
C           FNU    - ORDER OF INITIAL I FUNCTION, FNU.GE.0.0D0          EILI3658
C           KODE   - A PARAMETER TO INDICATE THE SCALING OPTION         EILI3659
C                    KODE= 1  RETURNS                                   EILI3660
C                             CY(J)=I(FNU+J-1,Z), J=1,...,N             EILI3661
C                        = 2  RETURNS                                   EILI3662
C                             CY(J)=I(FNU+J-1,Z)*EXP(-ABS(X)), J=1,...,NEILI3663
C           N      - NUMBER OF MEMBERS OF THE SEQUENCE, N.GE.1          EILI3664
C                                                                       EILI3665
C         OUTPUT     CYR,CYI ARE DOUBLE PRECISION                       EILI3666
C           CYR,CYI- DOUBLE PRECISION VECTORS WHOSE FIRST N COMPONENTS  EILI3667
C                    CONTAIN REAL AND IMAGINARY PARTS FOR THE SEQUENCE  EILI3668
C                    CY(J)=I(FNU+J-1,Z)  OR                             EILI3669
C                    CY(J)=I(FNU+J-1,Z)*EXP(-ABS(X))  J=1,...,N         EILI3670
C                    DEPENDING ON KODE, X=REAL(Z)                       EILI3671
C           NZ     - NUMBER OF COMPONENTS SET TO ZERO DUE TO UNDERFLOW, EILI3672
C                    NZ= 0   , NORMAL RETURN                            EILI3673
C                    NZ.GT.0 , LAST NZ COMPONENTS OF CY SET TO ZERO     EILI3674
C                              TO UNDERFLOW, CY(J)=CMPLX(0.0D0,0.0D0)   EILI3675
C                              J = N-NZ+1,...,N                         EILI3676
C           IERR   - ERROR FLAG                                         EILI3677
C                    IERR=0, NORMAL RETURN - COMPUTATION COMPLETED      EILI3678
C                    IERR=1, INPUT ERROR   - NO COMPUTATION             EILI3679
C                    IERR=2, OVERFLOW      - NO COMPUTATION, REAL(Z) TOOEILI3680
C                            LARGE ON KODE=1                            EILI3681
C                    IERR=3, CABS(Z) OR FNU+N-1 LARGE - COMPUTATION DONEEILI3682
C                            BUT LOSSES OF SIGNIFCANCE BY ARGUMENT      EILI3683
C                            REDUCTION PRODUCE LESS THAN HALF OF MACHINEEILI3684
C                            ACCURACY                                   EILI3685
C                    IERR=4, CABS(Z) OR FNU+N-1 TOO LARGE - NO COMPUTA- EILI3686
C                            TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI-EILI3687
C                            CANCE BY ARGUMENT REDUCTION                EILI3688
C                    IERR=5, ERROR              - NO COMPUTATION,       EILI3689
C                            ALGORITHM TERMINATION CONDITION NOT MET    EILI3690
C                                                                       EILI3691
C***LONG DESCRIPTION                                                    EILI3692
C                                                                       EILI3693
C         THE COMPUTATION IS CARRIED OUT BY THE POWER SERIES FOR        EILI3694
C         SMALL CABS(Z), THE ASYMPTOTIC EXPANSION FOR LARGE CABS(Z),    EILI3695
C         THE MILLER ALGORITHM NORMALIZED BY THE WRONSKIAN AND A        EILI3696
C         NEUMANN SERIES FOR IMTERMEDIATE MAGNITUDES, AND THE           EILI3697
C         UNIFORM ASYMPTOTIC EXPANSIONS FOR I(FNU,Z) AND J(FNU,Z)       EILI3698
C         FOR LARGE ORDERS. BACKWARD RECURRENCE IS USED TO GENERATE     EILI3699
C         SEQUENCES OR REDUCE ORDERS WHEN NECESSARY.                    EILI3700
C                                                                       EILI3701
C         THE CALCULATIONS ABOVE ARE DONE IN THE RIGHT HALF PLANE AND   EILI3702
C         CONTINUED INTO THE LEFT HALF PLANE BY THE FORMULA             EILI3703
C                                                                       EILI3704
C         I(FNU,Z*EXP(M*PI)) = EXP(M*PI*FNU)*I(FNU,Z)  REAL(Z).GT.0.0   EILI3705
C                       M = +I OR -I,  I**2=-1                          EILI3706
C                                                                       EILI3707
C         FOR NEGATIVE ORDERS,THE FORMULA                               EILI3708
C                                                                       EILI3709
C              I(-FNU,Z) = I(FNU,Z) + (2/PI)*SIN(PI*FNU)*K(FNU,Z)       EILI3710
C                                                                       EILI3711
C         CAN BE USED. HOWEVER,FOR LARGE ORDERS CLOSE TO INTEGERS, THE  EILI3712
C         THE FUNCTION CHANGES RADICALLY. WHEN FNU IS A LARGE POSITIVE  EILI3713
C         INTEGER,THE MAGNITUDE OF I(-FNU,Z)=I(FNU,Z) IS A LARGE        EILI3714
C         NEGATIVE POWER OF TEN. BUT WHEN FNU IS NOT AN INTEGER,        EILI3715
C         K(FNU,Z) DOMINATES IN MAGNITUDE WITH A LARGE POSITIVE POWER OFEILI3716
C         TEN AND THE MOST THAT THE SECOND TERM CAN BE REDUCED IS BY    EILI3717
C         UNIT ROUNDOFF FROM THE COEFFICIENT. THUS, WIDE CHANGES CAN    EILI3718
C         OCCUR WITHIN UNIT ROUNDOFF OF A LARGE INTEGER FOR FNU. HERE,  EILI3719
C         LARGE MEANS FNU.GT.CABS(Z).                                   EILI3720
C                                                                       EILI3721
C         IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST EVALUATE ELE-  EILI3722
C         MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z OR FNU+N-1 IS      EILI3723
C         LARGE, LOSSES OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR.    EILI3724
C         CONSEQUENTLY, IF EITHER ONE EXCEEDS U1=SQRT(0.5/UR), THEN     EILI3725
C         LOSSES EXCEEDING HALF PRECISION ARE LIKELY AND AN ERROR FLAG  EILI3726
C         IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS      EILI3727
C         DOUBLE PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION.EILI3728
C         IF EITHER IS LARGER THAN U2=0.5/UR, THEN ALL SIGNIFICANCE IS  EILI3729
C         LOST AND IERR=4. IN ORDER TO USE THE INT FUNCTION, ARGUMENTS  EILI3730
C         MUST BE FURTHER RESTRICTED NOT TO EXCEED THE LARGEST MACHINE  EILI3731
C         INTEGER, U3=I1MACH(9). THUS, THE MAGNITUDE OF Z AND FNU+N-1 ISEILI3732
C         RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, AND U3   EILI3733
C         ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN SINGLE PRECISION  EILI3734
C         ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE PRECISION    EILI3735
C         ARITHMETIC RESPECTIVELY. THIS MAKES U2 AND U3 LIMITING IN     EILI3736
C         THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT ONE CAN EXPECT  EILI3737
C         TO RETAIN, IN THE WORST CASES ON 32 BIT MACHINES, NO DIGITS   EILI3738
C         IN SINGLE AND ONLY 7 DIGITS IN DOUBLE PRECISION ARITHMETIC.   EILI3739
C         SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES.               EILI3740
C                                                                       EILI3741
C         THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A COMPLEX  EILI3742
C         BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P=MAX(UNIT  EILI3743
C         ROUNDOFF,1.0E-18) IS THE NOMINAL PRECISION AND 10**S REPRE-   EILI3744
C         SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE  EILI3745
C         ELEMENTARY FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))),      EILI3746
C         ABS(LOG10(FNU))) APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF  EILI3747
C         CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY EILI3748
C         HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST LIKELY TO OCCUR WHENEILI3749
C         ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY EILI3750
C         SEVERAL ORDERS OF MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER EILI3751
C         THAN THE OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, EILI3752
C         0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS EILI3753
C         THE EXPONENT OF P, NO SIGNIFICANT DIGITS REMAIN IN THE SMALLEREILI3754
C         COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY EILI3755
C         BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P, THE SMALLER  EILI3756
C         COMPONENT WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE     EILI3757
C         MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES,    EILI3758
C         THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P, -P, PI/2-P,  EILI3759
C         OR -PI/2+P.                                                   EILI3760
C                                                                       EILI3761
C***REFERENCES  HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ     EILI3762
C                 AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF    EILI3763
C                 COMMERCE, 1955.                                       EILI3764
C                                                                       EILI3765
C               COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT     EILI3766
C                 BY D. E. AMOS, SAND83-0083, MAY, 1983.                EILI3767
C                                                                       EILI3768
C               COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT     EILI3769
C                 AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983 EILI3770
C                                                                       EILI3771
C               A SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX  EILI3772
C                 ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85- EILI3773
C                 1018, MAY, 1985                                       EILI3774
C                                                                       EILI3775
C               A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX    EILI3776
C                 ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, TRANS.  EILI3777
C                 MATH. SOFTWARE, 1986                                  EILI3778
C                                                                       EILI3779
C***ROUTINES CALLED  ZBINU,I1MACH,D1MACH                                EILI3780
C***END PROLOGUE  ZBESI                                                 EILI3781
C     COMPLEX CONE,CSGN,CW,CY,CZERO,Z,ZN                                EILI3782
      DOUBLE PRECISION AA, ALIM, ARG, CONEI, CONER, CSGNI, CSGNR, CYI,  EILI3783
     * CYR, DIG, ELIM, FNU, FNUL, PI, RL, R1M5, STR, TOL, ZI, ZNI, ZNR, EILI3784
     * ZR, D1MACH, AZ, BB, FN, ZABS, ASCLE, RTOL, ATOL, STI             EILI3785
      INTEGER I, IERR, INU, K, KODE, K1,K2,N,NZ,NN, I1MACH              EILI3786
      EXTERNAL ZABS                                                     EILI3787
      DIMENSION CYR(N), CYI(N)                                          EILI3788
      DATA PI /3.14159265358979324D0/                                   EILI3789
      DATA CONER, CONEI /1.0D0,0.0D0/                                   EILI3790
C                                                                       EILI3791
C***FIRST EXECUTABLE STATEMENT  ZBESI                                   EILI3792
      IERR = 0                                                          EILI3793
      NZ=0                                                              EILI3794
      IF (FNU.LT.0.0D0) IERR=1                                          EILI3795
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1                              EILI3796
      IF (N.LT.1) IERR=1                                                EILI3797
      IF (IERR.NE.0) RETURN                                             EILI3798
C-----------------------------------------------------------------------EILI3799
C     SET PARAMETERS RELATED TO MACHINE CONSTANTS.                      EILI3800
C     TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.          EILI3801
C     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.    EILI3802
C     EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL    AND                    EILI3803
C     EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL       ARE INTERVALS NEAR     EILI3804
C     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.    EILI3805
C     RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. EILI3806
C     DIG = NUMBER OF BASE 10 DIGITS IN TOL = 10**(-DIG).               EILI3807
C     FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.EILI3808
C-----------------------------------------------------------------------EILI3809
      TOL = DMAX1(D1MACH(4),1.0D-18)                                    EILI3810
      K1 = I1MACH(15)                                                   EILI3811
      K2 = I1MACH(16)                                                   EILI3812
      R1M5 = D1MACH(5)                                                  EILI3813
      K = MIN0(IABS(K1),IABS(K2))                                       EILI3814
      ELIM = 2.303D0*(DBLE(FLOAT(K))*R1M5-3.0D0)                        EILI3815
      K1 = I1MACH(14) - 1                                               EILI3816
      AA = R1M5*DBLE(FLOAT(K1))                                         EILI3817
      DIG = DMIN1(AA,18.0D0)                                            EILI3818
      AA = AA*2.303D0                                                   EILI3819
      ALIM = ELIM + DMAX1(-AA,-41.45D0)                                 EILI3820
      RL = 1.2D0*DIG + 3.0D0                                            EILI3821
      FNUL = 10.0D0 + 6.0D0*(DIG-3.0D0)                                 EILI3822
C-----------------------------------------------------------------------EILI3823
C     TEST FOR PROPER RANGE                                             EILI3824
C-----------------------------------------------------------------------EILI3825
      AZ = ZABS(ZR,ZI)                                                  EILI3826
      FN = FNU+DBLE(FLOAT(N-1))                                         EILI3827
      AA = 0.5D0/TOL                                                    EILI3828
      BB=DBLE(FLOAT(I1MACH(9)))*0.5D0                                   EILI3829
      AA = DMIN1(AA,BB)                                                 EILI3830
      IF (AZ.GT.AA) GO TO 260                                           EILI3831
      IF (FN.GT.AA) GO TO 260                                           EILI3832
      AA = DSQRT(AA)                                                    EILI3833
      IF (AZ.GT.AA) IERR=3                                              EILI3834
      IF (FN.GT.AA) IERR=3                                              EILI3835
      ZNR = ZR                                                          EILI3836
      ZNI = ZI                                                          EILI3837
      CSGNR = CONER                                                     EILI3838
      CSGNI = CONEI                                                     EILI3839
      IF (ZR.GE.0.0D0) GO TO 40                                         EILI3840
      ZNR = -ZR                                                         EILI3841
      ZNI = -ZI                                                         EILI3842
C-----------------------------------------------------------------------EILI3843
C     CALCULATE CSGN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE   EILI3844
C     WHEN FNU IS LARGE                                                 EILI3845
C-----------------------------------------------------------------------EILI3846
      INU = INT(SNGL(FNU))                                              EILI3847
      ARG = (FNU-DBLE(FLOAT(INU)))*PI                                   EILI3848
      IF (ZI.LT.0.0D0) ARG = -ARG                                       EILI3849
      CSGNR = DCOS(ARG)                                                 EILI3850
      CSGNI = DSIN(ARG)                                                 EILI3851
      IF (MOD(INU,2).EQ.0) GO TO 40                                     EILI3852
      CSGNR = -CSGNR                                                    EILI3853
      CSGNI = -CSGNI                                                    EILI3854
   40 CONTINUE                                                          EILI3855
      CALL ZBINU(ZNR, ZNI, FNU, KODE, N, CYR, CYI, NZ, RL, FNUL, TOL,   EILI3856
     * ELIM, ALIM)                                                      EILI3857
      IF (NZ.LT.0) GO TO 120                                            EILI3858
      IF (ZR.GE.0.0D0) RETURN                                           EILI3859
C-----------------------------------------------------------------------EILI3860
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE                      EILI3861
C-----------------------------------------------------------------------EILI3862
      NN = N - NZ                                                       EILI3863
      IF (NN.EQ.0) RETURN                                               EILI3864
      RTOL = 1.0D0/TOL                                                  EILI3865
      ASCLE = D1MACH(1)*RTOL*1.0D+3                                     EILI3866
      DO 50 I=1,NN                                                      EILI3867
C       STR = CYR(I)*CSGNR - CYI(I)*CSGNI                               EILI3868
C       CYI(I) = CYR(I)*CSGNI + CYI(I)*CSGNR                            EILI3869
C       CYR(I) = STR                                                    EILI3870
        AA = CYR(I)                                                     EILI3871
        BB = CYI(I)                                                     EILI3872
        ATOL = 1.0D0                                                    EILI3873
        IF (DMAX1(DABS(AA),DABS(BB)).GT.ASCLE) GO TO 55                 EILI3874
          AA = AA*RTOL                                                  EILI3875
          BB = BB*RTOL                                                  EILI3876
          ATOL = TOL                                                    EILI3877
   55   CONTINUE                                                        EILI3878
        STR = AA*CSGNR - BB*CSGNI                                       EILI3879
        STI = AA*CSGNI + BB*CSGNR                                       EILI3880
        CYR(I) = STR*ATOL                                               EILI3881
        CYI(I) = STI*ATOL                                               EILI3882
        CSGNR = -CSGNR                                                  EILI3883
        CSGNI = -CSGNI                                                  EILI3884
   50 CONTINUE                                                          EILI3885
      RETURN                                                            EILI3886
  120 CONTINUE                                                          EILI3887
      IF(NZ.EQ.(-2)) GO TO 130                                          EILI3888
      NZ = 0                                                            EILI3889
      IERR=2                                                            EILI3890
      RETURN                                                            EILI3891
  130 CONTINUE                                                          EILI3892
      NZ=0                                                              EILI3893
      IERR=5                                                            EILI3894
      RETURN                                                            EILI3895
  260 CONTINUE                                                          EILI3896
      NZ=0                                                              EILI3897
      IERR=4                                                            EILI3898
      RETURN                                                            EILI3899
      END         