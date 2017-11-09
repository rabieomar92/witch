!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:22:10 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZACAI(ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, RL, TOL,   EILI8168
     * ELIM, ALIM)                                                      EILI8169
C***BEGIN PROLOGUE  ZACAI                                               EILI8170
C***REFER TO  ZAIRY                                                     EILI8171
C                                                                       EILI8172
C     ZACAI APPLIES THE ANALYTIC CONTINUATION FORMULA                   EILI8173
C                                                                       EILI8174
C         K(FNU,ZN*EXP(MP))=K(FNU,ZN)*EXP(-MP*FNU) - MP*I(FNU,ZN)       EILI8175
C                 MP=PI*MR*CMPLX(0.0,1.0)                               EILI8176
C                                                                       EILI8177
C     TO CONTINUE THE K FUNCTION FROM THE RIGHT HALF TO THE LEFT        EILI8178
C     HALF Z PLANE FOR USE WITH ZAIRY WHERE FNU=1/3 OR 2/3 AND N=1.     EILI8179
C     ZACAI IS THE SAME AS ZACON WITH THE PARTS FOR LARGER ORDERS AND   EILI8180
C     RECURRENCE REMOVED. A RECURSIVE CALL TO ZACON CAN RESULT IF ZACON EILI8181
C     IS CALLED FROM ZAIRY.                                             EILI8182
C                                                                       EILI8183
C***ROUTINES CALLED  ZASYI,ZBKNU,ZMLRI,ZSERI,ZS1S2,D1MACH,ZABS          EILI8184
C***END PROLOGUE  ZACAI                                                 EILI8185
C     COMPLEX CSGN,CSPN,C1,C2,Y,Z,ZN,CY                                 EILI8186
      DOUBLE PRECISION ALIM, ARG, ASCLE, AZ, CSGNR, CSGNI, CSPNR,       EILI8187
     * CSPNI, C1R, C1I, C2R, C2I, CYR, CYI, DFNU, ELIM, FMR, FNU, PI,   EILI8188
     * RL, SGN, TOL, YY, YR, YI, ZR, ZI, ZNR, ZNI, D1MACH, ZABS         EILI8189
      INTEGER INU, IUF, KODE, MR, N, NN, NW, NZ                         EILI8190
      EXTERNAL ZABS                                                     EILI8191
      DIMENSION YR(N), YI(N), CYR(2), CYI(2)                            EILI8192
      DATA PI / 3.14159265358979324D0 /                                 EILI8193
      NZ = 0                                                            EILI8194
      ZNR = -ZR                                                         EILI8195
      ZNI = -ZI                                                         EILI8196
      AZ = ZABS(ZR,ZI)                                                  EILI8197
      NN = N                                                            EILI8198
      DFNU = FNU + DBLE(FLOAT(N-1))                                     EILI8199
      IF (AZ.LE.2.0D0) GO TO 10                                         EILI8200
      IF (AZ*AZ*0.25D0.GT.DFNU+1.0D0) GO TO 20                          EILI8201
   10 CONTINUE                                                          EILI8202
C-----------------------------------------------------------------------EILI8203
C     POWER SERIES FOR THE I FUNCTION                                   EILI8204
C-----------------------------------------------------------------------EILI8205
      CALL ZSERI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, TOL, ELIM, ALIM)  EILI8206
      GO TO 40                                                          EILI8207
   20 CONTINUE                                                          EILI8208
      IF (AZ.LT.RL) GO TO 30                                            EILI8209
C-----------------------------------------------------------------------EILI8210
C     ASYMPTOTIC EXPANSION FOR LARGE Z FOR THE I FUNCTION               EILI8211
C-----------------------------------------------------------------------EILI8212
      CALL ZASYI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, RL, TOL, ELIM,    EILI8213
     * ALIM)                                                            EILI8214
      IF (NW.LT.0) GO TO 80                                             EILI8215
      GO TO 40                                                          EILI8216
   30 CONTINUE                                                          EILI8217
C-----------------------------------------------------------------------EILI8218
C     MILLER ALGORITHM NORMALIZED BY THE SERIES FOR THE I FUNCTION      EILI8219
C-----------------------------------------------------------------------EILI8220
      CALL ZMLRI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, TOL)              EILI8221
      IF(NW.LT.0) GO TO 80                                              EILI8222
   40 CONTINUE                                                          EILI8223
C-----------------------------------------------------------------------EILI8224
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE FOR THE K FUNCTION   EILI8225
C-----------------------------------------------------------------------EILI8226
      CALL ZBKNU(ZNR, ZNI, FNU, KODE, 1, CYR, CYI, NW, TOL, ELIM, ALIM) EILI8227
      IF (NW.NE.0) GO TO 80                                             EILI8228
      FMR = DBLE(FLOAT(MR))                                             EILI8229
      SGN = -DSIGN(PI,FMR)                                              EILI8230
      CSGNR = 0.0D0                                                     EILI8231
      CSGNI = SGN                                                       EILI8232
      IF (KODE.EQ.1) GO TO 50                                           EILI8233
      YY = -ZNI                                                         EILI8234
      CSGNR = -CSGNI*DSIN(YY)                                           EILI8235
      CSGNI = CSGNI*DCOS(YY)                                            EILI8236
   50 CONTINUE                                                          EILI8237
C-----------------------------------------------------------------------EILI8238
C     CALCULATE CSPN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE   EILI8239
C     WHEN FNU IS LARGE                                                 EILI8240
C-----------------------------------------------------------------------EILI8241
      INU = INT(SNGL(FNU))                                              EILI8242
      ARG = (FNU-DBLE(FLOAT(INU)))*SGN                                  EILI8243
      CSPNR = DCOS(ARG)                                                 EILI8244
      CSPNI = DSIN(ARG)                                                 EILI8245
      IF (MOD(INU,2).EQ.0) GO TO 60                                     EILI8246
      CSPNR = -CSPNR                                                    EILI8247
      CSPNI = -CSPNI                                                    EILI8248
   60 CONTINUE                                                          EILI8249
      C1R = CYR(1)                                                      EILI8250
      C1I = CYI(1)                                                      EILI8251
      C2R = YR(1)                                                       EILI8252
      C2I = YI(1)                                                       EILI8253
      IF (KODE.EQ.1) GO TO 70                                           EILI8254
      IUF = 0                                                           EILI8255
      ASCLE = 1.0D+3*D1MACH(1)/TOL                                      EILI8256
      CALL ZS1S2(ZNR, ZNI, C1R, C1I, C2R, C2I, NW, ASCLE, ALIM, IUF)    EILI8257
      NZ = NZ + NW                                                      EILI8258
   70 CONTINUE                                                          EILI8259
      YR(1) = CSPNR*C1R - CSPNI*C1I + CSGNR*C2R - CSGNI*C2I             EILI8260
      YI(1) = CSPNR*C1I + CSPNI*C1R + CSGNR*C2I + CSGNI*C2R             EILI8261
      RETURN                                                            EILI8262
   80 CONTINUE                                                          EILI8263
      NZ = -1                                                           EILI8264
      IF(NW.EQ.(-2)) NZ=-2                                              EILI8265
      RETURN                                                            EILI8266
      END      