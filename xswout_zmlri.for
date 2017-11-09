!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:59:33 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZMLRI(ZR, ZI, FNU, KODE, N, YR, YI, NZ, TOL)           EILI6992
C***BEGIN PROLOGUE  ZMLRI                                               EILI6993
C***REFER TO  ZBESI,ZBESK                                               EILI6994
C                                                                       EILI6995
C     ZMLRI COMPUTES THE I BESSEL FUNCTION FOR RE(Z).GE.0.0 BY THE      EILI6996
C     MILLER ALGORITHM NORMALIZED BY A NEUMANN SERIES.                  EILI6997
C                                                                       EILI6998
C***ROUTINES CALLED  DGAMLN,D1MACH,ZABS,ZEXP,ZLOG,ZMLT                  EILI6999
C***END PROLOGUE  ZMLRI                                                 EILI7000
C     COMPLEX CK,CNORM,CONE,CTWO,CZERO,PT,P1,P2,RZ,SUM,Y,Z              EILI7001
      DOUBLE PRECISION ACK, AK, AP, AT, AZ, BK, CKI, CKR, CNORMI,       EILI7002
     * CNORMR, CONEI, CONER, FKAP, FKK, FLAM, FNF, FNU, PTI, PTR, P1I,  EILI7003
     * P1R, P2I, P2R, RAZ, RHO, RHO2, RZI, RZR, SCLE, STI, STR, SUMI,   EILI7004
     * SUMR, TFNF, TOL, TST, YI, YR, ZEROI, ZEROR, ZI, ZR, DGAMLN,      EILI7005
     * D1MACH, ZABS                                                     EILI7006
      INTEGER I, IAZ, IDUM, IFNU, INU, ITIME, K, KK, KM, KODE, M, N, NZ EILI7007
CNEA                                                                    EILI7008
      EXTERNAL ZLOG                                                     EILI7009
CNEA                                                                    EILI7010
      EXTERNAL ZABS                                                     EILI7011
      DIMENSION YR(N), YI(N)                                            EILI7012
      DATA ZEROR,ZEROI,CONER,CONEI / 0.0D0, 0.0D0, 1.0D0, 0.0D0 /       EILI7013
      SCLE = D1MACH(1)/TOL                                              EILI7014
      NZ=0                                                              EILI7015
      AZ = ZABS(ZR,ZI)                                                  EILI7016
      IAZ = INT(SNGL(AZ))                                               EILI7017
      IFNU = INT(SNGL(FNU))                                             EILI7018
      INU = IFNU + N - 1                                                EILI7019
      AT = DBLE(FLOAT(IAZ)) + 1.0D0                                     EILI7020
      RAZ = 1.0D0/AZ                                                    EILI7021
      STR = ZR*RAZ                                                      EILI7022
      STI = -ZI*RAZ                                                     EILI7023
      CKR = STR*AT*RAZ                                                  EILI7024
      CKI = STI*AT*RAZ                                                  EILI7025
      RZR = (STR+STR)*RAZ                                               EILI7026
      RZI = (STI+STI)*RAZ                                               EILI7027
      P1R = ZEROR                                                       EILI7028
      P1I = ZEROI                                                       EILI7029
      P2R = CONER                                                       EILI7030
      P2I = CONEI                                                       EILI7031
      ACK = (AT+1.0D0)*RAZ                                              EILI7032
      RHO = ACK + DSQRT(ACK*ACK-1.0D0)                                  EILI7033
      RHO2 = RHO*RHO                                                    EILI7034
      TST = (RHO2+RHO2)/((RHO2-1.0D0)*(RHO-1.0D0))                      EILI7035
      TST = TST/TOL                                                     EILI7036
C-----------------------------------------------------------------------EILI7037
C     COMPUTE RELATIVE TRUNCATION ERROR INDEX FOR SERIES                EILI7038
C-----------------------------------------------------------------------EILI7039
      AK = AT                                                           EILI7040
      DO 10 I=1,80                                                      EILI7041
        PTR = P2R                                                       EILI7042
        PTI = P2I                                                       EILI7043
        P2R = P1R - (CKR*PTR-CKI*PTI)                                   EILI7044
        P2I = P1I - (CKI*PTR+CKR*PTI)                                   EILI7045
        P1R = PTR                                                       EILI7046
        P1I = PTI                                                       EILI7047
        CKR = CKR + RZR                                                 EILI7048
        CKI = CKI + RZI                                                 EILI7049
        AP = ZABS(P2R,P2I)                                              EILI7050
        IF (AP.GT.TST*AK*AK) GO TO 20                                   EILI7051
        AK = AK + 1.0D0                                                 EILI7052
   10 CONTINUE                                                          EILI7053
      GO TO 110                                                         EILI7054
   20 CONTINUE                                                          EILI7055
      I = I + 1                                                         EILI7056
      K = 0                                                             EILI7057
      IF (INU.LT.IAZ) GO TO 40                                          EILI7058
C-----------------------------------------------------------------------EILI7059
C     COMPUTE RELATIVE TRUNCATION ERROR FOR RATIOS                      EILI7060
C-----------------------------------------------------------------------EILI7061
      P1R = ZEROR                                                       EILI7062
      P1I = ZEROI                                                       EILI7063
      P2R = CONER                                                       EILI7064
      P2I = CONEI                                                       EILI7065
      AT = DBLE(FLOAT(INU)) + 1.0D0                                     EILI7066
      STR = ZR*RAZ                                                      EILI7067
      STI = -ZI*RAZ                                                     EILI7068
      CKR = STR*AT*RAZ                                                  EILI7069
      CKI = STI*AT*RAZ                                                  EILI7070
      ACK = AT*RAZ                                                      EILI7071
      TST = DSQRT(ACK/TOL)                                              EILI7072
      ITIME = 1                                                         EILI7073
      DO 30 K=1,80                                                      EILI7074
        PTR = P2R                                                       EILI7075
        PTI = P2I                                                       EILI7076
        P2R = P1R - (CKR*PTR-CKI*PTI)                                   EILI7077
        P2I = P1I - (CKR*PTI+CKI*PTR)                                   EILI7078
        P1R = PTR                                                       EILI7079
        P1I = PTI                                                       EILI7080
        CKR = CKR + RZR                                                 EILI7081
        CKI = CKI + RZI                                                 EILI7082
        AP = ZABS(P2R,P2I)                                              EILI7083
        IF (AP.LT.TST) GO TO 30                                         EILI7084
        IF (ITIME.EQ.2) GO TO 40                                        EILI7085
        ACK = ZABS(CKR,CKI)                                             EILI7086
        FLAM = ACK + DSQRT(ACK*ACK-1.0D0)                               EILI7087
        FKAP = AP/ZABS(P1R,P1I)                                         EILI7088
        RHO = DMIN1(FLAM,FKAP)                                          EILI7089
        TST = TST*DSQRT(RHO/(RHO*RHO-1.0D0))                            EILI7090
        ITIME = 2                                                       EILI7091
   30 CONTINUE                                                          EILI7092
      GO TO 110                                                         EILI7093
   40 CONTINUE                                                          EILI7094
C-----------------------------------------------------------------------EILI7095
C     BACKWARD RECURRENCE AND SUM NORMALIZING RELATION                  EILI7096
C-----------------------------------------------------------------------EILI7097
      K = K + 1                                                         EILI7098
      KK = MAX0(I+IAZ,K+INU)                                            EILI7099
      FKK = DBLE(FLOAT(KK))                                             EILI7100
      P1R = ZEROR                                                       EILI7101
      P1I = ZEROI                                                       EILI7102
C-----------------------------------------------------------------------EILI7103
C     SCALE P2 AND SUM BY SCLE                                          EILI7104
C-----------------------------------------------------------------------EILI7105
      P2R = SCLE                                                        EILI7106
      P2I = ZEROI                                                       EILI7107
      FNF = FNU - DBLE(FLOAT(IFNU))                                     EILI7108
      TFNF = FNF + FNF                                                  EILI7109
      BK = DGAMLN(FKK+TFNF+1.0D0,IDUM) - DGAMLN(FKK+1.0D0,IDUM) -       EILI7110
     * DGAMLN(TFNF+1.0D0,IDUM)                                          EILI7111
      BK = DEXP(BK)                                                     EILI7112
      SUMR = ZEROR                                                      EILI7113
      SUMI = ZEROI                                                      EILI7114
      KM = KK - INU                                                     EILI7115
      DO 50 I=1,KM                                                      EILI7116
        PTR = P2R                                                       EILI7117
        PTI = P2I                                                       EILI7118
        P2R = P1R + (FKK+FNF)*(RZR*PTR-RZI*PTI)                         EILI7119
        P2I = P1I + (FKK+FNF)*(RZI*PTR+RZR*PTI)                         EILI7120
        P1R = PTR                                                       EILI7121
        P1I = PTI                                                       EILI7122
        AK = 1.0D0 - TFNF/(FKK+TFNF)                                    EILI7123
        ACK = BK*AK                                                     EILI7124
        SUMR = SUMR + (ACK+BK)*P1R                                      EILI7125
        SUMI = SUMI + (ACK+BK)*P1I                                      EILI7126
        BK = ACK                                                        EILI7127
        FKK = FKK - 1.0D0                                               EILI7128
   50 CONTINUE                                                          EILI7129
      YR(N) = P2R                                                       EILI7130
      YI(N) = P2I                                                       EILI7131
      IF (N.EQ.1) GO TO 70                                              EILI7132
      DO 60 I=2,N                                                       EILI7133
        PTR = P2R                                                       EILI7134
        PTI = P2I                                                       EILI7135
        P2R = P1R + (FKK+FNF)*(RZR*PTR-RZI*PTI)                         EILI7136
        P2I = P1I + (FKK+FNF)*(RZI*PTR+RZR*PTI)                         EILI7137
        P1R = PTR                                                       EILI7138
        P1I = PTI                                                       EILI7139
        AK = 1.0D0 - TFNF/(FKK+TFNF)                                    EILI7140
        ACK = BK*AK                                                     EILI7141
        SUMR = SUMR + (ACK+BK)*P1R                                      EILI7142
        SUMI = SUMI + (ACK+BK)*P1I                                      EILI7143
        BK = ACK                                                        EILI7144
        FKK = FKK - 1.0D0                                               EILI7145
        M = N - I + 1                                                   EILI7146
        YR(M) = P2R                                                     EILI7147
        YI(M) = P2I                                                     EILI7148
   60 CONTINUE                                                          EILI7149
   70 CONTINUE                                                          EILI7150
      IF (IFNU.LE.0) GO TO 90                                           EILI7151
      DO 80 I=1,IFNU                                                    EILI7152
        PTR = P2R                                                       EILI7153
        PTI = P2I                                                       EILI7154
        P2R = P1R + (FKK+FNF)*(RZR*PTR-RZI*PTI)                         EILI7155
        P2I = P1I + (FKK+FNF)*(RZR*PTI+RZI*PTR)                         EILI7156
        P1R = PTR                                                       EILI7157
        P1I = PTI                                                       EILI7158
        AK = 1.0D0 - TFNF/(FKK+TFNF)                                    EILI7159
        ACK = BK*AK                                                     EILI7160
        SUMR = SUMR + (ACK+BK)*P1R                                      EILI7161
        SUMI = SUMI + (ACK+BK)*P1I                                      EILI7162
        BK = ACK                                                        EILI7163
        FKK = FKK - 1.0D0                                               EILI7164
   80 CONTINUE                                                          EILI7165
   90 CONTINUE                                                          EILI7166
      PTR = ZR                                                          EILI7167
      PTI = ZI                                                          EILI7168
      IF (KODE.EQ.2) PTR = ZEROR                                        EILI7169
      CALL ZLOG(RZR, RZI, STR, STI, IDUM)                               EILI7170
      P1R = -FNF*STR + PTR                                              EILI7171
      P1I = -FNF*STI + PTI                                              EILI7172
      AP = DGAMLN(1.0D0+FNF,IDUM)                                       EILI7173
      PTR = P1R - AP                                                    EILI7174
      PTI = P1I                                                         EILI7175
C-----------------------------------------------------------------------EILI7176
C     THE DIVISION CEXP(PT)/(SUM+P2) IS ALTERED TO AVOID OVERFLOW       EILI7177
C     IN THE DENOMINATOR BY SQUARING LARGE QUANTITIES                   EILI7178
C-----------------------------------------------------------------------EILI7179
      P2R = P2R + SUMR                                                  EILI7180
      P2I = P2I + SUMI                                                  EILI7181
      AP = ZABS(P2R,P2I)                                                EILI7182
      P1R = 1.0D0/AP                                                    EILI7183
      CALL RRZEXP(PTR, PTI, STR, STI)                                   EILI7184
      CKR = STR*P1R                                                     EILI7185
      CKI = STI*P1R                                                     EILI7186
      PTR = P2R*P1R                                                     EILI7187
      PTI = -P2I*P1R                                                    EILI7188
      CALL ZMLT(CKR, CKI, PTR, PTI, CNORMR, CNORMI)                     EILI7189
      DO 100 I=1,N                                                      EILI7190
        STR = YR(I)*CNORMR - YI(I)*CNORMI                               EILI7191
        YI(I) = YR(I)*CNORMI + YI(I)*CNORMR                             EILI7192
        YR(I) = STR                                                     EILI7193
  100 CONTINUE                                                          EILI7194
      RETURN                                                            EILI7195
  110 CONTINUE                                                          EILI7196
      NZ=-2                                                             EILI7197
      RETURN                                                            EILI7198
      END       