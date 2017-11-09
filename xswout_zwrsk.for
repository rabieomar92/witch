!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:00:14 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZWRSK(ZRR, ZRI, FNU, KODE, N, YR, YI, NZ, CWR, CWI,    EILI7200
     * TOL, ELIM, ALIM)                                                 EILI7201
C***BEGIN PROLOGUE  ZWRSK                                               EILI7202
C***REFER TO  ZBESI,ZBESK                                               EILI7203
C                                                                       EILI7204
C     ZWRSK COMPUTES THE I BESSEL FUNCTION FOR RE(Z).GE.0.0 BY          EILI7205
C     NORMALIZING THE I FUNCTION RATIOS FROM ZRATI BY THE WRONSKIAN     EILI7206
C                                                                       EILI7207
C***ROUTINES CALLED  D1MACH,ZBKNU,ZRATI,ZABS                            EILI7208
C***END PROLOGUE  ZWRSK                                                 EILI7209
C     COMPLEX CINU,CSCL,CT,CW,C1,C2,RCT,ST,Y,ZR                         EILI7210
      DOUBLE PRECISION ACT, ACW, ALIM, ASCLE, CINUI, CINUR, CSCLR, CTI, EILI7211
     * CTR, CWI, CWR, C1I, C1R, C2I, C2R, ELIM, FNU, PTI, PTR, RACT,    EILI7212
     * STI, STR, TOL, YI, YR, ZRI, ZRR, ZABS, D1MACH                    EILI7213
      INTEGER I, KODE, N, NW, NZ                                        EILI7214
      EXTERNAL ZABS                                                     EILI7215
      DIMENSION YR(N), YI(N), CWR(2), CWI(2)                            EILI7216
C-----------------------------------------------------------------------EILI7217
C     I(FNU+I-1,Z) BY BACKWARD RECURRENCE FOR RATIOS                    EILI7218
C     Y(I)=I(FNU+I,Z)/I(FNU+I-1,Z) FROM CRATI NORMALIZED BY THE         EILI7219
C     WRONSKIAN WITH K(FNU,Z) AND K(FNU+1,Z) FROM CBKNU.                EILI7220
C-----------------------------------------------------------------------EILI7221
      NZ = 0                                                            EILI7222
      CALL ZBKNU(ZRR, ZRI, FNU, KODE, 2, CWR, CWI, NW, TOL, ELIM, ALIM) EILI7223
      IF (NW.NE.0) GO TO 50                                             EILI7224
      CALL ZRATI(ZRR, ZRI, FNU, N, YR, YI, TOL)                         EILI7225
C-----------------------------------------------------------------------EILI7226
C     RECUR FORWARD ON I(FNU+1,Z) = R(FNU,Z)*I(FNU,Z),                  EILI7227
C     R(FNU+J-1,Z)=Y(J),  J=1,...,N                                     EILI7228
C-----------------------------------------------------------------------EILI7229
      CINUR = 1.0D0                                                     EILI7230
      CINUI = 0.0D0                                                     EILI7231
      IF (KODE.EQ.1) GO TO 10                                           EILI7232
      CINUR = DCOS(ZRI)                                                 EILI7233
      CINUI = DSIN(ZRI)                                                 EILI7234
   10 CONTINUE                                                          EILI7235
C-----------------------------------------------------------------------EILI7236
C     ON LOW EXPONENT MACHINES THE K FUNCTIONS CAN BE CLOSE TO BOTH     EILI7237
C     THE UNDER AND OVERFLOW LIMITS AND THE NORMALIZATION MUST BE       EILI7238
C     SCALED TO PREVENT OVER OR UNDERFLOW. CUOIK HAS DETERMINED THAT    EILI7239
C     THE RESULT IS ON SCALE.                                           EILI7240
C-----------------------------------------------------------------------EILI7241
      ACW = ZABS(CWR(2),CWI(2))                                         EILI7242
      ASCLE = 1.0D+3*D1MACH(1)/TOL                                      EILI7243
      CSCLR = 1.0D0                                                     EILI7244
      IF (ACW.GT.ASCLE) GO TO 20                                        EILI7245
      CSCLR = 1.0D0/TOL                                                 EILI7246
      GO TO 30                                                          EILI7247
   20 CONTINUE                                                          EILI7248
      ASCLE = 1.0D0/ASCLE                                               EILI7249
      IF (ACW.LT.ASCLE) GO TO 30                                        EILI7250
      CSCLR = TOL                                                       EILI7251
   30 CONTINUE                                                          EILI7252
      C1R = CWR(1)*CSCLR                                                EILI7253
      C1I = CWI(1)*CSCLR                                                EILI7254
      C2R = CWR(2)*CSCLR                                                EILI7255
      C2I = CWI(2)*CSCLR                                                EILI7256
      STR = YR(1)                                                       EILI7257
      STI = YI(1)                                                       EILI7258
C-----------------------------------------------------------------------EILI7259
C     CINU=CINU*(CONJG(CT)/CABS(CT))*(1.0D0/CABS(CT) PREVENTS           EILI7260
C     UNDER- OR OVERFLOW PREMATURELY BY SQUARING CABS(CT)               EILI7261
C-----------------------------------------------------------------------EILI7262
      PTR = STR*C1R - STI*C1I                                           EILI7263
      PTI = STR*C1I + STI*C1R                                           EILI7264
      PTR = PTR + C2R                                                   EILI7265
      PTI = PTI + C2I                                                   EILI7266
      CTR = ZRR*PTR - ZRI*PTI                                           EILI7267
      CTI = ZRR*PTI + ZRI*PTR                                           EILI7268
      ACT = ZABS(CTR,CTI)                                               EILI7269
      RACT = 1.0D0/ACT                                                  EILI7270
      CTR = CTR*RACT                                                    EILI7271
      CTI = -CTI*RACT                                                   EILI7272
      PTR = CINUR*RACT                                                  EILI7273
      PTI = CINUI*RACT                                                  EILI7274
      CINUR = PTR*CTR - PTI*CTI                                         EILI7275
      CINUI = PTR*CTI + PTI*CTR                                         EILI7276
      YR(1) = CINUR*CSCLR                                               EILI7277
      YI(1) = CINUI*CSCLR                                               EILI7278
      IF (N.EQ.1) RETURN                                                EILI7279
      DO 40 I=2,N                                                       EILI7280
        PTR = STR*CINUR - STI*CINUI                                     EILI7281
        CINUI = STR*CINUI + STI*CINUR                                   EILI7282
        CINUR = PTR                                                     EILI7283
        STR = YR(I)                                                     EILI7284
        STI = YI(I)                                                     EILI7285
        YR(I) = CINUR*CSCLR                                             EILI7286
        YI(I) = CINUI*CSCLR                                             EILI7287
   40 CONTINUE                                                          EILI7288
      RETURN                                                            EILI7289
   50 CONTINUE                                                          EILI7290
      NZ = -1                                                           EILI7291
      IF(NW.EQ.(-2)) NZ=-2                                              EILI7292
      RETURN                                                            EILI7293
      END       