!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:07:08 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZBUNI(ZR, ZI, FNU, KODE, N, YR, YI, NZ, NUI, NLAST,    EILI9196
     * FNUL, TOL, ELIM, ALIM)                                           EILI9197
C***BEGIN PROLOGUE  ZBUNI                                               EILI9198
C***REFER TO  ZBESI,ZBESK                                               EILI9199
C                                                                       EILI9200
C     ZBUNI COMPUTES THE I BESSEL FUNCTION FOR LARGE CABS(Z).GT.        EILI9201
C     FNUL AND FNU+N-1.LT.FNUL. THE ORDER IS INCREASED FROM             EILI9202
C     FNU+N-1 GREATER THAN FNUL BY ADDING NUI AND COMPUTING             EILI9203
C     ACCORDING TO THE UNIFORM ASYMPTOTIC EXPANSION FOR I(FNU,Z)        EILI9204
C     ON IFORM=1 AND THE EXPANSION FOR J(FNU,Z) ON IFORM=2              EILI9205
C                                                                       EILI9206
C***ROUTINES CALLED  ZUNI1,ZUNI2,ZABS,D1MACH                            EILI9207
C***END PROLOGUE  ZBUNI                                                 EILI9208
C     COMPLEX CSCL,CSCR,CY,RZ,ST,S1,S2,Y,Z                              EILI9209
      DOUBLE PRECISION ALIM, AX, AY, CSCLR, CSCRR, CYI, CYR, DFNU,      EILI9210
     * ELIM, FNU, FNUI, FNUL, GNU, RAZ, RZI, RZR, STI, STR, S1I, S1R,   EILI9211
     * S2I, S2R, TOL, YI, YR, ZI, ZR, ZABS, ASCLE, BRY, C1R, C1I, C1M,  EILI9212
     * D1MACH                                                           EILI9213
      INTEGER I, IFLAG, IFORM, K, KODE, N, NL, NLAST, NUI, NW, NZ       EILI9214
      EXTERNAL ZABS                                                     EILI9215
      DIMENSION YR(N), YI(N), CYR(2), CYI(2), BRY(3)                    EILI9216
      NZ = 0                                                            EILI9217
      AX = DABS(ZR)*1.7321D0                                            EILI9218
      AY = DABS(ZI)                                                     EILI9219
      IFORM = 1                                                         EILI9220
      IF (AY.GT.AX) IFORM = 2                                           EILI9221
      IF (NUI.EQ.0) GO TO 60                                            EILI9222
      FNUI = DBLE(FLOAT(NUI))                                           EILI9223
      DFNU = FNU + DBLE(FLOAT(N-1))                                     EILI9224
      GNU = DFNU + FNUI                                                 EILI9225
      IF (IFORM.EQ.2) GO TO 10                                          EILI9226
C-----------------------------------------------------------------------EILI9227
C     ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN        EILI9228
C     -PI/3.LE.ARG(Z).LE.PI/3                                           EILI9229
C-----------------------------------------------------------------------EILI9230
      CALL ZUNI1(ZR, ZI, GNU, KODE, 2, CYR, CYI, NW, NLAST, FNUL, TOL,  EILI9231
     * ELIM, ALIM)                                                      EILI9232
      GO TO 20                                                          EILI9233
   10 CONTINUE                                                          EILI9234
C-----------------------------------------------------------------------EILI9235
C     ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU        EILI9236
C     APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I           EILI9237
C     AND HPI=PI/2                                                      EILI9238
C-----------------------------------------------------------------------EILI9239
      CALL ZUNI2(ZR, ZI, GNU, KODE, 2, CYR, CYI, NW, NLAST, FNUL, TOL,  EILI9240
     * ELIM, ALIM)                                                      EILI9241
   20 CONTINUE                                                          EILI9242
      IF (NW.LT.0) GO TO 50                                             EILI9243
      IF (NW.NE.0) GO TO 90                                             EILI9244
      STR = ZABS(CYR(1),CYI(1))                                         EILI9245
C---------------------------------------------------------------------- EILI9246
C     SCALE BACKWARD RECURRENCE, BRY(3) IS DEFINED BUT NEVER USED       EILI9247
C---------------------------------------------------------------------- EILI9248
      BRY(1)=1.0D+3*D1MACH(1)/TOL                                       EILI9249
      BRY(2) = 1.0D0/BRY(1)                                             EILI9250
      BRY(3) = BRY(2)                                                   EILI9251
      IFLAG = 2                                                         EILI9252
      ASCLE = BRY(2)                                                    EILI9253
      CSCLR = 1.0D0                                                     EILI9254
      IF (STR.GT.BRY(1)) GO TO 21                                       EILI9255
      IFLAG = 1                                                         EILI9256
      ASCLE = BRY(1)                                                    EILI9257
      CSCLR = 1.0D0/TOL                                                 EILI9258
      GO TO 25                                                          EILI9259
   21 CONTINUE                                                          EILI9260
      IF (STR.LT.BRY(2)) GO TO 25                                       EILI9261
      IFLAG = 3                                                         EILI9262
      ASCLE=BRY(3)                                                      EILI9263
      CSCLR = TOL                                                       EILI9264
   25 CONTINUE                                                          EILI9265
      CSCRR = 1.0D0/CSCLR                                               EILI9266
      S1R = CYR(2)*CSCLR                                                EILI9267
      S1I = CYI(2)*CSCLR                                                EILI9268
      S2R = CYR(1)*CSCLR                                                EILI9269
      S2I = CYI(1)*CSCLR                                                EILI9270
      RAZ = 1.0D0/ZABS(ZR,ZI)                                           EILI9271
      STR = ZR*RAZ                                                      EILI9272
      STI = -ZI*RAZ                                                     EILI9273
      RZR = (STR+STR)*RAZ                                               EILI9274
      RZI = (STI+STI)*RAZ                                               EILI9275
      DO 30 I=1,NUI                                                     EILI9276
        STR = S2R                                                       EILI9277
        STI = S2I                                                       EILI9278
        S2R = (DFNU+FNUI)*(RZR*STR-RZI*STI) + S1R                       EILI9279
        S2I = (DFNU+FNUI)*(RZR*STI+RZI*STR) + S1I                       EILI9280
        S1R = STR                                                       EILI9281
        S1I = STI                                                       EILI9282
        FNUI = FNUI - 1.0D0                                             EILI9283
        IF (IFLAG.GE.3) GO TO 30                                        EILI9284
        STR = S2R*CSCRR                                                 EILI9285
        STI = S2I*CSCRR                                                 EILI9286
        C1R = DABS(STR)                                                 EILI9287
        C1I = DABS(STI)                                                 EILI9288
        C1M = DMAX1(C1R,C1I)                                            EILI9289
        IF (C1M.LE.ASCLE) GO TO 30                                      EILI9290
        IFLAG = IFLAG+1                                                 EILI9291
        ASCLE = BRY(IFLAG)                                              EILI9292
        S1R = S1R*CSCRR                                                 EILI9293
        S1I = S1I*CSCRR                                                 EILI9294
        S2R = STR                                                       EILI9295
        S2I = STI                                                       EILI9296
        CSCLR = CSCLR*TOL                                               EILI9297
        CSCRR = 1.0D0/CSCLR                                             EILI9298
        S1R = S1R*CSCLR                                                 EILI9299
        S1I = S1I*CSCLR                                                 EILI9300
        S2R = S2R*CSCLR                                                 EILI9301
        S2I = S2I*CSCLR                                                 EILI9302
   30 CONTINUE                                                          EILI9303
      YR(N) = S2R*CSCRR                                                 EILI9304
      YI(N) = S2I*CSCRR                                                 EILI9305
      IF (N.EQ.1) RETURN                                                EILI9306
      NL = N - 1                                                        EILI9307
      FNUI = DBLE(FLOAT(NL))                                            EILI9308
      K = NL                                                            EILI9309
      DO 40 I=1,NL                                                      EILI9310
        STR = S2R                                                       EILI9311
        STI = S2I                                                       EILI9312
        S2R = (FNU+FNUI)*(RZR*STR-RZI*STI) + S1R                        EILI9313
        S2I = (FNU+FNUI)*(RZR*STI+RZI*STR) + S1I                        EILI9314
        S1R = STR                                                       EILI9315
        S1I = STI                                                       EILI9316
        STR = S2R*CSCRR                                                 EILI9317
        STI = S2I*CSCRR                                                 EILI9318
        YR(K) = STR                                                     EILI9319
        YI(K) = STI                                                     EILI9320
        FNUI = FNUI - 1.0D0                                             EILI9321
        K = K - 1                                                       EILI9322
        IF (IFLAG.GE.3) GO TO 40                                        EILI9323
        C1R = DABS(STR)                                                 EILI9324
        C1I = DABS(STI)                                                 EILI9325
        C1M = DMAX1(C1R,C1I)                                            EILI9326
        IF (C1M.LE.ASCLE) GO TO 40                                      EILI9327
        IFLAG = IFLAG+1                                                 EILI9328
        ASCLE = BRY(IFLAG)                                              EILI9329
        S1R = S1R*CSCRR                                                 EILI9330
        S1I = S1I*CSCRR                                                 EILI9331
        S2R = STR                                                       EILI9332
        S2I = STI                                                       EILI9333
        CSCLR = CSCLR*TOL                                               EILI9334
        CSCRR = 1.0D0/CSCLR                                             EILI9335
        S1R = S1R*CSCLR                                                 EILI9336
        S1I = S1I*CSCLR                                                 EILI9337
        S2R = S2R*CSCLR                                                 EILI9338
        S2I = S2I*CSCLR                                                 EILI9339
   40 CONTINUE                                                          EILI9340
      RETURN                                                            EILI9341
   50 CONTINUE                                                          EILI9342
      NZ = -1                                                           EILI9343
      IF(NW.EQ.(-2)) NZ=-2                                              EILI9344
      RETURN                                                            EILI9345
   60 CONTINUE                                                          EILI9346
      IF (IFORM.EQ.2) GO TO 70                                          EILI9347
C-----------------------------------------------------------------------EILI9348
C     ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN        EILI9349
C     -PI/3.LE.ARG(Z).LE.PI/3                                           EILI9350
C-----------------------------------------------------------------------EILI9351
      CALL ZUNI1(ZR, ZI, FNU, KODE, N, YR, YI, NW, NLAST, FNUL, TOL,    EILI9352
     * ELIM, ALIM)                                                      EILI9353
      GO TO 80                                                          EILI9354
   70 CONTINUE                                                          EILI9355
C-----------------------------------------------------------------------EILI9356
C     ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU        EILI9357
C     APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I           EILI9358
C     AND HPI=PI/2                                                      EILI9359
C-----------------------------------------------------------------------EILI9360
      CALL ZUNI2(ZR, ZI, FNU, KODE, N, YR, YI, NW, NLAST, FNUL, TOL,    EILI9361
     * ELIM, ALIM)                                                      EILI9362
   80 CONTINUE                                                          EILI9363
      IF (NW.LT.0) GO TO 50                                             EILI9364
      NZ = NW                                                           EILI9365
      RETURN                                                            EILI9366
   90 CONTINUE                                                          EILI9367
      NLAST = N                                                         EILI9368
      RETURN                                                            EILI9369
      END       