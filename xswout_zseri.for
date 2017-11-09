!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:56:40 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZSERI(ZR, ZI, FNU, KODE, N, YR, YI, NZ, TOL, ELIM,     EILI7295
     * ALIM)                                                            EILI7296
C***BEGIN PROLOGUE  ZSERI                                               EILI7297
C***REFER TO  ZBESI,ZBESK                                               EILI7298
C                                                                       EILI7299
C     ZSERI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY        EILI7300
C     MEANS OF THE POWER SERIES FOR LARGE CABS(Z) IN THE                EILI7301
C     REGION CABS(Z).LE.2*SQRT(FNU+1). NZ=0 IS A NORMAL RETURN.         EILI7302
C     NZ.GT.0 MEANS THAT THE LAST NZ COMPONENTS WERE SET TO ZERO        EILI7303
C     DUE TO UNDERFLOW. NZ.LT.0 MEANS UNDERFLOW OCCURRED, BUT THE       EILI7304
C     CONDITION CABS(Z).LE.2*SQRT(FNU+1) WAS VIOLATED AND THE           EILI7305
C     COMPUTATION MUST BE COMPLETED IN ANOTHER ROUTINE WITH N=N-ABS(NZ).EILI7306
C                                                                       EILI7307
C***ROUTINES CALLED  DGAMLN,D1MACH,ZUCHK,ZABS,ZDIV,ZLOG,ZMLT            EILI7308
C***END PROLOGUE  ZSERI                                                 EILI7309
C     COMPLEX AK1,CK,COEF,CONE,CRSC,CSCL,CZ,CZERO,HZ,RZ,S1,S2,Y,Z       EILI7310
      DOUBLE PRECISION AA, ACZ, AK, AK1I, AK1R, ALIM, ARM, ASCLE, ATOL, EILI7311
     * AZ, CKI, CKR, COEFI, COEFR, CONEI, CONER, CRSCR, CZI, CZR, DFNU, EILI7312
     * ELIM, FNU, FNUP, HZI, HZR, RAZ, RS, RTR1, RZI, RZR, S, SS, STI,  EILI7313
     * STR, S1I, S1R, S2I, S2R, TOL, YI, YR, WI, WR, ZEROI, ZEROR, ZI,  EILI7314
     * ZR, DGAMLN, D1MACH, ZABS                                         EILI7315
      INTEGER I, IB, IDUM, IFLAG, IL, K, KODE, L, M, N, NN, NZ, NW      EILI7316
CNEA                                                                    EILI7317
      EXTERNAL ZLOG                                                     EILI7318
CNEA                                                                    EILI7319
      EXTERNAL ZABS                                                     EILI7320
      DIMENSION YR(N), YI(N), WR(2), WI(2)                              EILI7321
      DATA ZEROR,ZEROI,CONER,CONEI / 0.0D0, 0.0D0, 1.0D0, 0.0D0 /       EILI7322
C                                                                       EILI7323
      NZ = 0                                                            EILI7324
      AZ = ZABS(ZR,ZI)                                                  EILI7325
      IF (AZ.EQ.0.0D0) GO TO 160                                        EILI7326
      ARM = 1.0D+3*D1MACH(1)                                            EILI7327
      RTR1 = DSQRT(ARM)                                                 EILI7328
      CRSCR = 1.0D0                                                     EILI7329
      IFLAG = 0                                                         EILI7330
      IF (AZ.LT.ARM) GO TO 150                                          EILI7331
      HZR = 0.5D0*ZR                                                    EILI7332
      HZI = 0.5D0*ZI                                                    EILI7333
      CZR = ZEROR                                                       EILI7334
      CZI = ZEROI                                                       EILI7335
      IF (AZ.LE.RTR1) GO TO 10                                          EILI7336
      CALL ZMLT(HZR, HZI, HZR, HZI, CZR, CZI)                           EILI7337
   10 CONTINUE                                                          EILI7338
      ACZ = ZABS(CZR,CZI)                                               EILI7339
      NN = N                                                            EILI7340
      CALL ZLOG(HZR, HZI, CKR, CKI, IDUM)                               EILI7341
   20 CONTINUE                                                          EILI7342
      DFNU = FNU + DBLE(FLOAT(NN-1))                                    EILI7343
      FNUP = DFNU + 1.0D0                                               EILI7344
C-----------------------------------------------------------------------EILI7345
C     UNDERFLOW TEST                                                    EILI7346
C-----------------------------------------------------------------------EILI7347
      AK1R = CKR*DFNU                                                   EILI7348
      AK1I = CKI*DFNU                                                   EILI7349
      AK = DGAMLN(FNUP,IDUM)                                            EILI7350
      AK1R = AK1R - AK                                                  EILI7351
      IF (KODE.EQ.2) AK1R = AK1R - ZR                                   EILI7352
      IF (AK1R.GT.(-ELIM)) GO TO 40                                     EILI7353
   30 CONTINUE                                                          EILI7354
      NZ = NZ + 1                                                       EILI7355
      YR(NN) = ZEROR                                                    EILI7356
      YI(NN) = ZEROI                                                    EILI7357
      IF (ACZ.GT.DFNU) GO TO 190                                        EILI7358
      NN = NN - 1                                                       EILI7359
      IF (NN.EQ.0) RETURN                                               EILI7360
      GO TO 20                                                          EILI7361
   40 CONTINUE                                                          EILI7362
      IF (AK1R.GT.(-ALIM)) GO TO 50                                     EILI7363
      IFLAG = 1                                                         EILI7364
      SS = 1.0D0/TOL                                                    EILI7365
      CRSCR = TOL                                                       EILI7366
      ASCLE = ARM*SS                                                    EILI7367
   50 CONTINUE                                                          EILI7368
      AA = DEXP(AK1R)                                                   EILI7369
      IF (IFLAG.EQ.1) AA = AA*SS                                        EILI7370
      COEFR = AA*DCOS(AK1I)                                             EILI7371
      COEFI = AA*DSIN(AK1I)                                             EILI7372
      ATOL = TOL*ACZ/FNUP                                               EILI7373
      IL = MIN0(2,NN)                                                   EILI7374
      DO 90 I=1,IL                                                      EILI7375
        DFNU = FNU + DBLE(FLOAT(NN-I))                                  EILI7376
        FNUP = DFNU + 1.0D0                                             EILI7377
        S1R = CONER                                                     EILI7378
        S1I = CONEI                                                     EILI7379
        IF (ACZ.LT.TOL*FNUP) GO TO 70                                   EILI7380
        AK1R = CONER                                                    EILI7381
        AK1I = CONEI                                                    EILI7382
        AK = FNUP + 2.0D0                                               EILI7383
        S = FNUP                                                        EILI7384
        AA = 2.0D0                                                      EILI7385
   60   CONTINUE                                                        EILI7386
        RS = 1.0D0/S                                                    EILI7387
        STR = AK1R*CZR - AK1I*CZI                                       EILI7388
        STI = AK1R*CZI + AK1I*CZR                                       EILI7389
        AK1R = STR*RS                                                   EILI7390
        AK1I = STI*RS                                                   EILI7391
        S1R = S1R + AK1R                                                EILI7392
        S1I = S1I + AK1I                                                EILI7393
        S = S + AK                                                      EILI7394
        AK = AK + 2.0D0                                                 EILI7395
        AA = AA*ACZ*RS                                                  EILI7396
        IF (AA.GT.ATOL) GO TO 60                                        EILI7397
   70   CONTINUE                                                        EILI7398
        S2R = S1R*COEFR - S1I*COEFI                                     EILI7399
        S2I = S1R*COEFI + S1I*COEFR                                     EILI7400
        WR(I) = S2R                                                     EILI7401
        WI(I) = S2I                                                     EILI7402
        IF (IFLAG.EQ.0) GO TO 80                                        EILI7403
        CALL ZUCHK(S2R, S2I, NW, ASCLE, TOL)                            EILI7404
        IF (NW.NE.0) GO TO 30                                           EILI7405
   80   CONTINUE                                                        EILI7406
        M = NN - I + 1                                                  EILI7407
        YR(M) = S2R*CRSCR                                               EILI7408
        YI(M) = S2I*CRSCR                                               EILI7409
        IF (I.EQ.IL) GO TO 90                                           EILI7410
        CALL ZDIV(COEFR, COEFI, HZR, HZI, STR, STI)                     EILI7411
        COEFR = STR*DFNU                                                EILI7412
        COEFI = STI*DFNU                                                EILI7413
   90 CONTINUE                                                          EILI7414
      IF (NN.LE.2) RETURN                                               EILI7415
      K = NN - 2                                                        EILI7416
      AK = DBLE(FLOAT(K))                                               EILI7417
      RAZ = 1.0D0/AZ                                                    EILI7418
      STR = ZR*RAZ                                                      EILI7419
      STI = -ZI*RAZ                                                     EILI7420
      RZR = (STR+STR)*RAZ                                               EILI7421
      RZI = (STI+STI)*RAZ                                               EILI7422
      IF (IFLAG.EQ.1) GO TO 120                                         EILI7423
      IB = 3                                                            EILI7424
  100 CONTINUE                                                          EILI7425
      DO 110 I=IB,NN                                                    EILI7426
        YR(K) = (AK+FNU)*(RZR*YR(K+1)-RZI*YI(K+1)) + YR(K+2)            EILI7427
        YI(K) = (AK+FNU)*(RZR*YI(K+1)+RZI*YR(K+1)) + YI(K+2)            EILI7428
        AK = AK - 1.0D0                                                 EILI7429
        K = K - 1                                                       EILI7430
  110 CONTINUE                                                          EILI7431
      RETURN                                                            EILI7432
C-----------------------------------------------------------------------EILI7433
C     RECUR BACKWARD WITH SCALED VALUES                                 EILI7434
C-----------------------------------------------------------------------EILI7435
  120 CONTINUE                                                          EILI7436
C-----------------------------------------------------------------------EILI7437
C     EXP(-ALIM)=EXP(-ELIM)/TOL=APPROX. ONE PRECISION ABOVE THE         EILI7438
C     UNDERFLOW LIMIT = ASCLE = D1MACH(1)*SS*1.0D+3                     EILI7439
C-----------------------------------------------------------------------EILI7440
      S1R = WR(1)                                                       EILI7441
      S1I = WI(1)                                                       EILI7442
      S2R = WR(2)                                                       EILI7443
      S2I = WI(2)                                                       EILI7444
      DO 130 L=3,NN                                                     EILI7445
        CKR = S2R                                                       EILI7446
        CKI = S2I                                                       EILI7447
        S2R = S1R + (AK+FNU)*(RZR*CKR-RZI*CKI)                          EILI7448
        S2I = S1I + (AK+FNU)*(RZR*CKI+RZI*CKR)                          EILI7449
        S1R = CKR                                                       EILI7450
        S1I = CKI                                                       EILI7451
        CKR = S2R*CRSCR                                                 EILI7452
        CKI = S2I*CRSCR                                                 EILI7453
        YR(K) = CKR                                                     EILI7454
        YI(K) = CKI                                                     EILI7455
        AK = AK - 1.0D0                                                 EILI7456
        K = K - 1                                                       EILI7457
        IF (ZABS(CKR,CKI).GT.ASCLE) GO TO 140                           EILI7458
  130 CONTINUE                                                          EILI7459
      RETURN                                                            EILI7460
  140 CONTINUE                                                          EILI7461
      IB = L + 1                                                        EILI7462
      IF (IB.GT.NN) RETURN                                              EILI7463
      GO TO 100                                                         EILI7464
  150 CONTINUE                                                          EILI7465
      NZ = N                                                            EILI7466
      IF (FNU.EQ.0.0D0) NZ = NZ - 1                                     EILI7467
  160 CONTINUE                                                          EILI7468
      YR(1) = ZEROR                                                     EILI7469
      YI(1) = ZEROI                                                     EILI7470
      IF (FNU.NE.0.0D0) GO TO 170                                       EILI7471
      YR(1) = CONER                                                     EILI7472
      YI(1) = CONEI                                                     EILI7473
  170 CONTINUE                                                          EILI7474
      IF (N.EQ.1) RETURN                                                EILI7475
      DO 180 I=2,N                                                      EILI7476
        YR(I) = ZEROR                                                   EILI7477
        YI(I) = ZEROI                                                   EILI7478
  180 CONTINUE                                                          EILI7479
      RETURN                                                            EILI7480
C-----------------------------------------------------------------------EILI7481
C     RETURN WITH NZ.LT.0 IF CABS(Z*Z/4).GT.FNU+N-NZ-1 COMPLETE         EILI7482
C     THE CALCULATION IN CBINU WITH N=N-IABS(NZ)                        EILI7483
C-----------------------------------------------------------------------EILI7484
  190 CONTINUE                                                          EILI7485
      NZ = -NZ                                                          EILI7486
      RETURN                                                            EILI7487
      END      