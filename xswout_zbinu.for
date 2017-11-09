!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:48:28 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZBINU(ZR, ZI, FNU, KODE, N, CYR, CYI, NZ, RL, FNUL,    EILI8057
     * TOL, ELIM, ALIM)                                                 EILI8058
C***BEGIN PROLOGUE  ZBINU                                               EILI8059
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZAIRY,ZBIRY                       EILI8060
C                                                                       EILI8061
C     ZBINU COMPUTES THE I FUNCTION IN THE RIGHT HALF Z PLANE           EILI8062
C                                                                       EILI8063
C***ROUTINES CALLED  ZABS,ZASYI,ZBUNI,ZMLRI,ZSERI,ZUOIK,ZWRSK           EILI8064
C***END PROLOGUE  ZBINU                                                 EILI8065
      DOUBLE PRECISION ALIM, AZ, CWI, CWR, CYI, CYR, DFNU, ELIM, FNU,   EILI8066
     * FNUL, RL, TOL, ZEROI, ZEROR, ZI, ZR, ZABS                        EILI8067
      INTEGER I, INW, KODE, N, NLAST, NN, NUI, NW, NZ                   EILI8068
      EXTERNAL ZABS                                                     EILI8069
      DIMENSION CYR(N), CYI(N), CWR(2), CWI(2)                          EILI8070
      DATA ZEROR,ZEROI / 0.0D0, 0.0D0 /                                 EILI8071
C                                                                       EILI8072
      NZ = 0                                                            EILI8073
      AZ = ZABS(ZR,ZI)                                                  EILI8074
      NN = N                                                            EILI8075
      DFNU = FNU + DBLE(FLOAT(N-1))                                     EILI8076
      IF (AZ.LE.2.0D0) GO TO 10                                         EILI8077
      IF (AZ*AZ*0.25D0.GT.DFNU+1.0D0) GO TO 20                          EILI8078
   10 CONTINUE                                                          EILI8079
C-----------------------------------------------------------------------EILI8080
C     POWER SERIES                                                      EILI8081
C-----------------------------------------------------------------------EILI8082
      CALL ZSERI(ZR, ZI, FNU, KODE, NN, CYR, CYI, NW, TOL, ELIM, ALIM)  EILI8083
      INW = IABS(NW)                                                    EILI8084
      NZ = NZ + INW                                                     EILI8085
      NN = NN - INW                                                     EILI8086
      IF (NN.EQ.0) RETURN                                               EILI8087
      IF (NW.GE.0) GO TO 120                                            EILI8088
      DFNU = FNU + DBLE(FLOAT(NN-1))                                    EILI8089
   20 CONTINUE                                                          EILI8090
      IF (AZ.LT.RL) GO TO 40                                            EILI8091
      IF (DFNU.LE.1.0D0) GO TO 30                                       EILI8092
      IF (AZ+AZ.LT.DFNU*DFNU) GO TO 50                                  EILI8093
C-----------------------------------------------------------------------EILI8094
C     ASYMPTOTIC EXPANSION FOR LARGE Z                                  EILI8095
C-----------------------------------------------------------------------EILI8096
   30 CONTINUE                                                          EILI8097
      CALL ZASYI(ZR, ZI, FNU, KODE, NN, CYR, CYI, NW, RL, TOL, ELIM,    EILI8098
     * ALIM)                                                            EILI8099
      IF (NW.LT.0) GO TO 130                                            EILI8100
      GO TO 120                                                         EILI8101
   40 CONTINUE                                                          EILI8102
      IF (DFNU.LE.1.0D0) GO TO 70                                       EILI8103
   50 CONTINUE                                                          EILI8104
C-----------------------------------------------------------------------EILI8105
C     OVERFLOW AND UNDERFLOW TEST ON I SEQUENCE FOR MILLER ALGORITHM    EILI8106
C-----------------------------------------------------------------------EILI8107
      CALL ZUOIK(ZR, ZI, FNU, KODE, 1, NN, CYR, CYI, NW, TOL, ELIM,     EILI8108
     * ALIM)                                                            EILI8109
      IF (NW.LT.0) GO TO 130                                            EILI8110
      NZ = NZ + NW                                                      EILI8111
      NN = NN - NW                                                      EILI8112
      IF (NN.EQ.0) RETURN                                               EILI8113
      DFNU = FNU+DBLE(FLOAT(NN-1))                                      EILI8114
      IF (DFNU.GT.FNUL) GO TO 110                                       EILI8115
      IF (AZ.GT.FNUL) GO TO 110                                         EILI8116
   60 CONTINUE                                                          EILI8117
      IF (AZ.GT.RL) GO TO 80                                            EILI8118
   70 CONTINUE                                                          EILI8119
C-----------------------------------------------------------------------EILI8120
C     MILLER ALGORITHM NORMALIZED BY THE SERIES                         EILI8121
C-----------------------------------------------------------------------EILI8122
      CALL ZMLRI(ZR, ZI, FNU, KODE, NN, CYR, CYI, NW, TOL)              EILI8123
      IF(NW.LT.0) GO TO 130                                             EILI8124
      GO TO 120                                                         EILI8125
   80 CONTINUE                                                          EILI8126
C-----------------------------------------------------------------------EILI8127
C     MILLER ALGORITHM NORMALIZED BY THE WRONSKIAN                      EILI8128
C-----------------------------------------------------------------------EILI8129
C-----------------------------------------------------------------------EILI8130
C     OVERFLOW TEST ON K FUNCTIONS USED IN WRONSKIAN                    EILI8131
C-----------------------------------------------------------------------EILI8132
      CALL ZUOIK(ZR, ZI, FNU, KODE, 2, 2, CWR, CWI, NW, TOL, ELIM,      EILI8133
     * ALIM)                                                            EILI8134
      IF (NW.GE.0) GO TO 100                                            EILI8135
      NZ = NN                                                           EILI8136
      DO 90 I=1,NN                                                      EILI8137
        CYR(I) = ZEROR                                                  EILI8138
        CYI(I) = ZEROI                                                  EILI8139
   90 CONTINUE                                                          EILI8140
      RETURN                                                            EILI8141
  100 CONTINUE                                                          EILI8142
      IF (NW.GT.0) GO TO 130                                            EILI8143
      CALL ZWRSK(ZR, ZI, FNU, KODE, NN, CYR, CYI, NW, CWR, CWI, TOL,    EILI8144
     * ELIM, ALIM)                                                      EILI8145
      IF (NW.LT.0) GO TO 130                                            EILI8146
      GO TO 120                                                         EILI8147
  110 CONTINUE                                                          EILI8148
C-----------------------------------------------------------------------EILI8149
C     INCREMENT FNU+NN-1 UP TO FNUL, COMPUTE AND RECUR BACKWARD         EILI8150
C-----------------------------------------------------------------------EILI8151
      NUI = INT(SNGL(FNUL-DFNU)) + 1                                    EILI8152
      NUI = MAX0(NUI,0)                                                 EILI8153
      CALL ZBUNI(ZR, ZI, FNU, KODE, NN, CYR, CYI, NW, NUI, NLAST, FNUL, EILI8154
     * TOL, ELIM, ALIM)                                                 EILI8155
      IF (NW.LT.0) GO TO 130                                            EILI8156
      NZ = NZ + NW                                                      EILI8157
      IF (NLAST.EQ.0) GO TO 120                                         EILI8158
      NN = NLAST                                                        EILI8159
      GO TO 60                                                          EILI8160
  120 CONTINUE                                                          EILI8161
      RETURN                                                            EILI8162
  130 CONTINUE                                                          EILI8163
      NZ = -1                                                           EILI8164
      IF(NW.EQ.(-2)) NZ=-2                                              EILI8165
      RETURN                                                            EILI8166
      END      