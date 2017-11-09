!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:52:06 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE HQR2(NM,N,LOW,IGH,H,WR,WI,Z,IERR)                      EILI 784
C                                                                       EILI 785
      INTEGER I,J,K,L,M,N,EN,II,JJ,LL,MM,NA,NM,NN,                      EILI 786
     X        IGH,ITN,ITS,LOW,MP2,ENM2,IERR                             EILI 787
      DOUBLE PRECISION H(NM,N),WR(N),WI(N),Z(NM,N)                      EILI 788
      DOUBLE PRECISION P,Q,R,S,T,W,X,Y,RA,SA,VI,VR,ZZ,NORM,TST1,TST2    EILI 789
      LOGICAL NOTLAS                                                    EILI 790
C                                                                       EILI 791
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2,     EILI 792
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.             EILI 793
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   EILI 794
C                                                                       EILI 795
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS            EILI 796
C     OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE          EILI 797
C     EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND           EILI 798
C     IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE            EILI 799
C     BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM        EILI 800
C     AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS.                 EILI 801
C                                                                       EILI 802
C     ON INPUT                                                          EILI 803
C                                                                       EILI 804
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         EILI 805
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          EILI 806
C          DIMENSION STATEMENT.                                         EILI 807
C                                                                       EILI 808
C        N IS THE ORDER OF THE MATRIX.                                  EILI 809
C                                                                       EILI 810
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING           EILI 811
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,          EILI 812
C          SET LOW=1, IGH=N.                                            EILI 813
C                                                                       EILI 814
C        H CONTAINS THE UPPER HESSENBERG MATRIX.                        EILI 815
C                                                                       EILI 816
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN       EILI 817
C          AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE     EILI 818
C          REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS     EILI 819
C          OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE     EILI 820
C          IDENTITY MATRIX.                                             EILI 821
C                                                                       EILI 822
C     ON OUTPUT                                                         EILI 823
C                                                                       EILI 824
C        H HAS BEEN DESTROYED.                                          EILI 825
C                                                                       EILI 826
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,                EILI 827
C          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES           EILI 828
C          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS            EILI 829
C          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE           EILI 830
C          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN             EILI 831
C          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT        EILI 832
C          FOR INDICES IERR+1,...,N.                                    EILI 833
C                                                                       EILI 834
C        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   EILI 835
C          IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z         EILI 836
C          CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX EILI 837
C          WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH          EILI 838
C          COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS     EILI 839
C          EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN      EILI 840
C          ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND. EILI 841
C                                                                       EILI 842
C        IERR IS SET TO                                                 EILI 843
C          ZERO       FOR NORMAL RETURN,                                EILI 844
C          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED      EILI 845
C                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.        EILI 846
C                                                                       EILI 847
C     CALLS CDIV FOR COMPLEX DIVISION.                                  EILI 848
C                                                                       EILI 849
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,    EILI 850
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY EILI 851
C                                                                       EILI 852
C     THIS VERSION DATED AUGUST 1983.                                   EILI 853
C                                                                       EILI 854
C     ------------------------------------------------------------------EILI 855
C                                                                       EILI 856
      IERR = 0                                                          EILI 857
      NORM = 0.0D0                                                      EILI 858
      K = 1                                                             EILI 859
C     .......... STORE ROOTS ISOLATED BY BALANC                         EILI 860
C                AND COMPUTE MATRIX NORM ..........                     EILI 861
      DO 50 I = 1, N                                                    EILI 862
C                                                                       EILI 863
         DO 40 J = K, N                                                 EILI 864
   40    NORM = NORM + DABS(H(I,J))                                     EILI 865
C                                                                       EILI 866
         K = I                                                          EILI 867
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 50                      EILI 868
         WR(I) = H(I,I)                                                 EILI 869
         WI(I) = 0.0D0                                                  EILI 870
   50 CONTINUE                                                          EILI 871
C                                                                       EILI 872
      EN = IGH                                                          EILI 873
      T = 0.0D0                                                         EILI 874
      ITN = 30*N                                                        EILI 875
C     .......... SEARCH FOR NEXT EIGENVALUES ..........                 EILI 876
   60 IF (EN .LT. LOW) GO TO 340                                        EILI 877
      ITS = 0                                                           EILI 878
      NA = EN - 1                                                       EILI 879
      ENM2 = NA - 1                                                     EILI 880
C     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT             EILI 881
C                FOR L=EN STEP -1 UNTIL LOW DO -- ..........            EILI 882
   70 DO 80 LL = LOW, EN                                                EILI 883
         L = EN + LOW - LL                                              EILI 884
         IF (L .EQ. LOW) GO TO 100                                      EILI 885
         S = DABS(H(L-1,L-1)) + DABS(H(L,L))                            EILI 886
         IF (S .EQ. 0.0D0) S = NORM                                     EILI 887
         TST1 = S                                                       EILI 888
         TST2 = TST1 + DABS(H(L,L-1))                                   EILI 889
         IF (TST2 .EQ. TST1) GO TO 100                                  EILI 890
   80 CONTINUE                                                          EILI 891
C     .......... FORM SHIFT ..........                                  EILI 892
  100 X = H(EN,EN)                                                      EILI 893
      IF (L .EQ. EN) GO TO 270                                          EILI 894
      Y = H(NA,NA)                                                      EILI 895
      W = H(EN,NA) * H(NA,EN)                                           EILI 896
      IF (L .EQ. NA) GO TO 280                                          EILI 897
      IF (ITN .EQ. 0) GO TO 1000                                        EILI 898
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130                      EILI 899
C     .......... FORM EXCEPTIONAL SHIFT ..........                      EILI 900
      T = T + X                                                         EILI 901
C                                                                       EILI 902
      DO 120 I = LOW, EN                                                EILI 903
  120 H(I,I) = H(I,I) - X                                               EILI 904
C                                                                       EILI 905
      S = DABS(H(EN,NA)) + DABS(H(NA,ENM2))                             EILI 906
      X = 0.75D0 * S                                                    EILI 907
      Y = X                                                             EILI 908
      W = -0.4375D0 * S * S                                             EILI 909
  130 ITS = ITS + 1                                                     EILI 910
      ITN = ITN - 1                                                     EILI 911
C     .......... LOOK FOR TWO CONSECUTIVE SMALL                         EILI 912
C                SUB-DIAGONAL ELEMENTS.                                 EILI 913
C                FOR M=EN-2 STEP -1 UNTIL L DO -- ..........            EILI 914
      DO 140 MM = L, ENM2                                               EILI 915
         M = ENM2 + L - MM                                              EILI 916
         ZZ = H(M,M)                                                    EILI 917
         R = X - ZZ                                                     EILI 918
         S = Y - ZZ                                                     EILI 919
         P = (R * S - W) / H(M+1,M) + H(M,M+1)                          EILI 920
         Q = H(M+1,M+1) - ZZ - R - S                                    EILI 921
         R = H(M+2,M+1)                                                 EILI 922
         S = DABS(P) + DABS(Q) + DABS(R)                                EILI 923
         P = P / S                                                      EILI 924
         Q = Q / S                                                      EILI 925
         R = R / S                                                      EILI 926
         IF (M .EQ. L) GO TO 150                                        EILI 927
         TST1 = DABS(P)*(DABS(H(M-1,M-1)) + DABS(ZZ) + DABS(H(M+1,M+1)))EILI 928
         TST2 = TST1 + DABS(H(M,M-1))*(DABS(Q) + DABS(R))               EILI 929
         IF (TST2 .EQ. TST1) GO TO 150                                  EILI 930
  140 CONTINUE                                                          EILI 931
C                                                                       EILI 932
  150 MP2 = M + 2                                                       EILI 933
C                                                                       EILI 934
      DO 160 I = MP2, EN                                                EILI 935
         H(I,I-2) = 0.0D0                                               EILI 936
         IF (I .EQ. MP2) GO TO 160                                      EILI 937
         H(I,I-3) = 0.0D0                                               EILI 938
  160 CONTINUE                                                          EILI 939
C     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND              EILI 940
C                COLUMNS M TO EN ..........                             EILI 941
      DO 260 K = M, NA                                                  EILI 942
         NOTLAS = K .NE. NA                                             EILI 943
         IF (K .EQ. M) GO TO 170                                        EILI 944
         P = H(K,K-1)                                                   EILI 945
         Q = H(K+1,K-1)                                                 EILI 946
         R = 0.0D0                                                      EILI 947
         IF (NOTLAS) R = H(K+2,K-1)                                     EILI 948
         X = DABS(P) + DABS(Q) + DABS(R)                                EILI 949
         IF (X .EQ. 0.0D0) GO TO 260                                    EILI 950
         P = P / X                                                      EILI 951
         Q = Q / X                                                      EILI 952
         R = R / X                                                      EILI 953
  170    S = DSIGN(DSQRT(P*P+Q*Q+R*R),P)                                EILI 954
         IF (K .EQ. M) GO TO 180                                        EILI 955
         H(K,K-1) = -S * X                                              EILI 956
         GO TO 190                                                      EILI 957
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)                             EILI 958
  190    P = P + S                                                      EILI 959
         X = P / S                                                      EILI 960
         Y = Q / S                                                      EILI 961
         ZZ = R / S                                                     EILI 962
         Q = Q / P                                                      EILI 963
         R = R / P                                                      EILI 964
         IF (NOTLAS) GO TO 225                                          EILI 965
C     .......... ROW MODIFICATION ..........                            EILI 966
         DO 200 J = K, N                                                EILI 967
            P = H(K,J) + Q * H(K+1,J)                                   EILI 968
            H(K,J) = H(K,J) - P * X                                     EILI 969
            H(K+1,J) = H(K+1,J) - P * Y                                 EILI 970
  200    CONTINUE                                                       EILI 971
C                                                                       EILI 972
         J = MIN0(EN,K+3)                                               EILI 973
C     .......... COLUMN MODIFICATION ..........                         EILI 974
         DO 210 I = 1, J                                                EILI 975
            P = X * H(I,K) + Y * H(I,K+1)                               EILI 976
            H(I,K) = H(I,K) - P                                         EILI 977
            H(I,K+1) = H(I,K+1) - P * Q                                 EILI 978
  210    CONTINUE                                                       EILI 979
C     .......... ACCUMULATE TRANSFORMATIONS ..........                  EILI 980
         DO 220 I = LOW, IGH                                            EILI 981
            P = X * Z(I,K) + Y * Z(I,K+1)                               EILI 982
            Z(I,K) = Z(I,K) - P                                         EILI 983
            Z(I,K+1) = Z(I,K+1) - P * Q                                 EILI 984
  220    CONTINUE                                                       EILI 985
         GO TO 255                                                      EILI 986
  225    CONTINUE                                                       EILI 987
C     .......... ROW MODIFICATION ..........                            EILI 988
         DO 230 J = K, N                                                EILI 989
            P = H(K,J) + Q * H(K+1,J) + R * H(K+2,J)                    EILI 990
            H(K,J) = H(K,J) - P * X                                     EILI 991
            H(K+1,J) = H(K+1,J) - P * Y                                 EILI 992
            H(K+2,J) = H(K+2,J) - P * ZZ                                EILI 993
  230    CONTINUE                                                       EILI 994
C                                                                       EILI 995
         J = MIN0(EN,K+3)                                               EILI 996
C     .......... COLUMN MODIFICATION ..........                         EILI 997
         DO 240 I = 1, J                                                EILI 998
            P = X * H(I,K) + Y * H(I,K+1) + ZZ * H(I,K+2)               EILI 999
            H(I,K) = H(I,K) - P                                         EILI1000
            H(I,K+1) = H(I,K+1) - P * Q                                 EILI1001
            H(I,K+2) = H(I,K+2) - P * R                                 EILI1002
  240    CONTINUE                                                       EILI1003
C     .......... ACCUMULATE TRANSFORMATIONS ..........                  EILI1004
         DO 250 I = LOW, IGH                                            EILI1005
            P = X * Z(I,K) + Y * Z(I,K+1) + ZZ * Z(I,K+2)               EILI1006
            Z(I,K) = Z(I,K) - P                                         EILI1007
            Z(I,K+1) = Z(I,K+1) - P * Q                                 EILI1008
            Z(I,K+2) = Z(I,K+2) - P * R                                 EILI1009
  250    CONTINUE                                                       EILI1010
  255    CONTINUE                                                       EILI1011
C                                                                       EILI1012
  260 CONTINUE                                                          EILI1013
C                                                                       EILI1014
      GO TO 70                                                          EILI1015
C     .......... ONE ROOT FOUND ..........                              EILI1016
  270 H(EN,EN) = X + T                                                  EILI1017
      WR(EN) = H(EN,EN)                                                 EILI1018
      WI(EN) = 0.0D0                                                    EILI1019
      EN = NA                                                           EILI1020
      GO TO 60                                                          EILI1021
C     .......... TWO ROOTS FOUND ..........                             EILI1022
  280 P = (Y - X) / 2.0D0                                               EILI1023
      Q = P * P + W                                                     EILI1024
      ZZ = DSQRT(DABS(Q))                                               EILI1025
      H(EN,EN) = X + T                                                  EILI1026
      X = H(EN,EN)                                                      EILI1027
      H(NA,NA) = Y + T                                                  EILI1028
      IF (Q .LT. 0.0D0) GO TO 320                                       EILI1029
C     .......... REAL PAIR ..........                                   EILI1030
      ZZ = P + DSIGN(ZZ,P)                                              EILI1031
      WR(NA) = X + ZZ                                                   EILI1032
      WR(EN) = WR(NA)                                                   EILI1033
      IF (ZZ .NE. 0.0D0) WR(EN) = X - W / ZZ                            EILI1034
      WI(NA) = 0.0D0                                                    EILI1035
      WI(EN) = 0.0D0                                                    EILI1036
      X = H(EN,NA)                                                      EILI1037
      S = DABS(X) + DABS(ZZ)                                            EILI1038
      P = X / S                                                         EILI1039
      Q = ZZ / S                                                        EILI1040
      R = DSQRT(P*P+Q*Q)                                                EILI1041
      P = P / R                                                         EILI1042
      Q = Q / R                                                         EILI1043
C     .......... ROW MODIFICATION ..........                            EILI1044
      DO 290 J = NA, N                                                  EILI1045
         ZZ = H(NA,J)                                                   EILI1046
         H(NA,J) = Q * ZZ + P * H(EN,J)                                 EILI1047
         H(EN,J) = Q * H(EN,J) - P * ZZ                                 EILI1048
  290 CONTINUE                                                          EILI1049
C     .......... COLUMN MODIFICATION ..........                         EILI1050
      DO 300 I = 1, EN                                                  EILI1051
         ZZ = H(I,NA)                                                   EILI1052
         H(I,NA) = Q * ZZ + P * H(I,EN)                                 EILI1053
         H(I,EN) = Q * H(I,EN) - P * ZZ                                 EILI1054
  300 CONTINUE                                                          EILI1055
C     .......... ACCUMULATE TRANSFORMATIONS ..........                  EILI1056
      DO 310 I = LOW, IGH                                               EILI1057
         ZZ = Z(I,NA)                                                   EILI1058
         Z(I,NA) = Q * ZZ + P * Z(I,EN)                                 EILI1059
         Z(I,EN) = Q * Z(I,EN) - P * ZZ                                 EILI1060
  310 CONTINUE                                                          EILI1061
C                                                                       EILI1062
      GO TO 330                                                         EILI1063
C     .......... COMPLEX PAIR ..........                                EILI1064
  320 WR(NA) = X + P                                                    EILI1065
      WR(EN) = X + P                                                    EILI1066
      WI(NA) = ZZ                                                       EILI1067
      WI(EN) = -ZZ                                                      EILI1068
  330 EN = ENM2                                                         EILI1069
      GO TO 60                                                          EILI1070
C     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND               EILI1071
C                VECTORS OF UPPER TRIANGULAR FORM ..........            EILI1072
  340 IF (NORM .EQ. 0.0D0) GO TO 1001                                   EILI1073
C     .......... FOR EN=N STEP -1 UNTIL 1 DO -- ..........              EILI1074
      DO 800 NN = 1, N                                                  EILI1075
         EN = N + 1 - NN                                                EILI1076
         P = WR(EN)                                                     EILI1077
         Q = WI(EN)                                                     EILI1078
         NA = EN - 1                                                    EILI1079
         IF (Q) 710, 600, 800                                           EILI1080
C     .......... REAL VECTOR ..........                                 EILI1081
  600    M = EN                                                         EILI1082
         H(EN,EN) = 1.0D0                                               EILI1083
         IF (NA .EQ. 0) GO TO 800                                       EILI1084
C     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........            EILI1085
         DO 700 II = 1, NA                                              EILI1086
            I = EN - II                                                 EILI1087
            W = H(I,I) - P                                              EILI1088
            R = 0.0D0                                                   EILI1089
C                                                                       EILI1090
            DO 610 J = M, EN                                            EILI1091
  610       R = R + H(I,J) * H(J,EN)                                    EILI1092
C                                                                       EILI1093
            IF (WI(I) .GE. 0.0D0) GO TO 630                             EILI1094
            ZZ = W                                                      EILI1095
            S = R                                                       EILI1096
            GO TO 700                                                   EILI1097
  630       M = I                                                       EILI1098
            IF (WI(I) .NE. 0.0D0) GO TO 640                             EILI1099
            T = W                                                       EILI1100
            IF (T .NE. 0.0D0) GO TO 635                                 EILI1101
               TST1 = NORM                                              EILI1102
               T = TST1                                                 EILI1103
  632          T = 0.01D0 * T                                           EILI1104
               TST2 = NORM + T                                          EILI1105
               IF (TST2 .GT. TST1) GO TO 632                            EILI1106
  635       H(I,EN) = -R / T                                            EILI1107
            GO TO 680                                                   EILI1108
C     .......... SOLVE REAL EQUATIONS ..........                        EILI1109
  640       X = H(I,I+1)                                                EILI1110
            Y = H(I+1,I)                                                EILI1111
            Q = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I)               EILI1112
            T = (X * S - ZZ * R) / Q                                    EILI1113
            H(I,EN) = T                                                 EILI1114
            IF (DABS(X) .LE. DABS(ZZ)) GO TO 650                        EILI1115
            H(I+1,EN) = (-R - W * T) / X                                EILI1116
            GO TO 680                                                   EILI1117
  650       H(I+1,EN) = (-S - Y * T) / ZZ                               EILI1118
C                                                                       EILI1119
C     .......... OVERFLOW CONTROL ..........                            EILI1120
  680       T = DABS(H(I,EN))                                           EILI1121
            IF (T .EQ. 0.0D0) GO TO 700                                 EILI1122
            TST1 = T                                                    EILI1123
            TST2 = TST1 + 1.0D0/TST1                                    EILI1124
            IF (TST2 .GT. TST1) GO TO 700                               EILI1125
            DO 690 J = I, EN                                            EILI1126
               H(J,EN) = H(J,EN)/T                                      EILI1127
  690       CONTINUE                                                    EILI1128
C                                                                       EILI1129
  700    CONTINUE                                                       EILI1130
C     .......... END REAL VECTOR ..........                             EILI1131
         GO TO 800                                                      EILI1132
C     .......... COMPLEX VECTOR ..........                              EILI1133
  710    M = NA                                                         EILI1134
C     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT         EILI1135
C                EIGENVECTOR MATRIX IS TRIANGULAR ..........            EILI1136
         IF (DABS(H(EN,NA)) .LE. DABS(H(NA,EN))) GO TO 720              EILI1137
         H(NA,NA) = Q / H(EN,NA)                                        EILI1138
         H(NA,EN) = -(H(EN,EN) - P) / H(EN,NA)                          EILI1139
         GO TO 730                                                      EILI1140
  720    CALL CDIV(0.0D0,-H(NA,EN),H(NA,NA)-P,Q,H(NA,NA),H(NA,EN))      EILI1141
  730    H(EN,NA) = 0.0D0                                               EILI1142
         H(EN,EN) = 1.0D0                                               EILI1143
         ENM2 = NA - 1                                                  EILI1144
         IF (ENM2 .EQ. 0) GO TO 800                                     EILI1145
C     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- ..........            EILI1146
         DO 795 II = 1, ENM2                                            EILI1147
            I = NA - II                                                 EILI1148
            W = H(I,I) - P                                              EILI1149
            RA = 0.0D0                                                  EILI1150
            SA = 0.0D0                                                  EILI1151
C                                                                       EILI1152
            DO 760 J = M, EN                                            EILI1153
               RA = RA + H(I,J) * H(J,NA)                               EILI1154
               SA = SA + H(I,J) * H(J,EN)                               EILI1155
  760       CONTINUE                                                    EILI1156
C                                                                       EILI1157
            IF (WI(I) .GE. 0.0D0) GO TO 770                             EILI1158
            ZZ = W                                                      EILI1159
            R = RA                                                      EILI1160
            S = SA                                                      EILI1161
            GO TO 795                                                   EILI1162
  770       M = I                                                       EILI1163
            IF (WI(I) .NE. 0.0D0) GO TO 780                             EILI1164
            CALL CDIV(-RA,-SA,W,Q,H(I,NA),H(I,EN))                      EILI1165
            GO TO 790                                                   EILI1166
C     .......... SOLVE COMPLEX EQUATIONS ..........                     EILI1167
  780       X = H(I,I+1)                                                EILI1168
            Y = H(I+1,I)                                                EILI1169
            VR = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I) - Q * Q      EILI1170
            VI = (WR(I) - P) * 2.0D0 * Q                                EILI1171
            IF (VR .NE. 0.0D0 .OR. VI .NE. 0.0D0) GO TO 784             EILI1172
               TST1 = NORM * (DABS(W) + DABS(Q) + DABS(X)               EILI1173
     X                      + DABS(Y) + DABS(ZZ))                       EILI1174
               VR = TST1                                                EILI1175
  783          VR = 0.01D0 * VR                                         EILI1176
               TST2 = TST1 + VR                                         EILI1177
               IF (TST2 .GT. TST1) GO TO 783                            EILI1178
  784       CALL CDIV(X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA,VR,VI,              EILI1179
     X                H(I,NA),H(I,EN))                                  EILI1180
            IF (DABS(X) .LE. DABS(ZZ) + DABS(Q)) GO TO 785              EILI1181
            H(I+1,NA) = (-RA - W * H(I,NA) + Q * H(I,EN)) / X           EILI1182
            H(I+1,EN) = (-SA - W * H(I,EN) - Q * H(I,NA)) / X           EILI1183
            GO TO 790                                                   EILI1184
  785       CALL CDIV(-R-Y*H(I,NA),-S-Y*H(I,EN),ZZ,Q,                   EILI1185
     X                H(I+1,NA),H(I+1,EN))                              EILI1186
C                                                                       EILI1187
C     .......... OVERFLOW CONTROL ..........                            EILI1188
  790       T = DMAX1(DABS(H(I,NA)), DABS(H(I,EN)))                     EILI1189
            IF (T .EQ. 0.0D0) GO TO 795                                 EILI1190
            TST1 = T                                                    EILI1191
            TST2 = TST1 + 1.0D0/TST1                                    EILI1192
            IF (TST2 .GT. TST1) GO TO 795                               EILI1193
            DO 792 J = I, EN                                            EILI1194
               H(J,NA) = H(J,NA)/T                                      EILI1195
               H(J,EN) = H(J,EN)/T                                      EILI1196
  792       CONTINUE                                                    EILI1197
C                                                                       EILI1198
  795    CONTINUE                                                       EILI1199
C     .......... END COMPLEX VECTOR ..........                          EILI1200
  800 CONTINUE                                                          EILI1201
C     .......... END BACK SUBSTITUTION.                                 EILI1202
C                VECTORS OF ISOLATED ROOTS ..........                   EILI1203
      DO 840 I = 1, N                                                   EILI1204
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 840                     EILI1205
C                                                                       EILI1206
         DO 820 J = I, N                                                EILI1207
  820    Z(I,J) = H(I,J)                                                EILI1208
C                                                                       EILI1209
  840 CONTINUE                                                          EILI1210
C     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE              EILI1211
C                VECTORS OF ORIGINAL FULL MATRIX.                       EILI1212
C                FOR J=N STEP -1 UNTIL LOW DO -- ..........             EILI1213
      DO 880 JJ = LOW, N                                                EILI1214
         J = N + LOW - JJ                                               EILI1215
         M = MIN0(J,IGH)                                                EILI1216
C                                                                       EILI1217
         DO 880 I = LOW, IGH                                            EILI1218
            ZZ = 0.0D0                                                  EILI1219
C                                                                       EILI1220
            DO 860 K = LOW, M                                           EILI1221
  860       ZZ = ZZ + Z(I,K) * H(K,J)                                   EILI1222
C                                                                       EILI1223
            Z(I,J) = ZZ                                                 EILI1224
  880 CONTINUE                                                          EILI1225
C                                                                       EILI1226
      GO TO 1001                                                        EILI1227
C     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT                  EILI1228
C                CONVERGED AFTER 30*N ITERATIONS ..........             EILI1229
 1000 IERR = EN                                                         EILI1230
 1001 RETURN                                                            EILI1231
      END      