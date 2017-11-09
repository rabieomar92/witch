!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:18:36 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZUNI1(ZR, ZI, FNU, KODE, N, YR, YI, NZ, NLAST, FNUL,   EILI8296
     * TOL, ELIM, ALIM)                                                 EILI8297
C***BEGIN PROLOGUE  ZUNI1                                               EILI8298
C***REFER TO  ZBESI,ZBESK                                               EILI8299
C                                                                       EILI8300
C     ZUNI1 COMPUTES I(FNU,Z)  BY MEANS OF THE UNIFORM ASYMPTOTIC       EILI8301
C     EXPANSION FOR I(FNU,Z) IN -PI/3.LE.ARG Z.LE.PI/3.                 EILI8302
C                                                                       EILI8303
C     FNUL IS THE SMALLEST ORDER PERMITTED FOR THE ASYMPTOTIC           EILI8304
C     EXPANSION. NLAST=0 MEANS ALL OF THE Y VALUES WERE SET.            EILI8305
C     NLAST.NE.0 IS THE NUMBER LEFT TO BE COMPUTED BY ANOTHER           EILI8306
C     FORMULA FOR ORDERS FNU TO FNU+NLAST-1 BECAUSE FNU+NLAST-1.LT.FNUL.EILI8307
C     Y(I)=CZERO FOR I=NLAST+1,N                                        EILI8308
C                                                                       EILI8309
C***ROUTINES CALLED  ZUCHK,ZUNIK,ZUOIK,D1MACH,ZABS                      EILI8310
C***END PROLOGUE  ZUNI1                                                 EILI8311
C     COMPLEX CFN,CONE,CRSC,CSCL,CSR,CSS,CWRK,CZERO,C1,C2,PHI,RZ,SUM,S1,EILI8312
C    *S2,Y,Z,ZETA1,ZETA2                                                EILI8313
      DOUBLE PRECISION ALIM, APHI, ASCLE, BRY, CONER, CRSC,             EILI8314
     * CSCL, CSRR, CSSR, CWRKI, CWRKR, C1R, C2I, C2M, C2R, ELIM, FN,    EILI8315
     * FNU, FNUL, PHII, PHIR, RAST, RS1, RZI, RZR, STI, STR, SUMI,      EILI8316
     * SUMR, S1I, S1R, S2I, S2R, TOL, YI, YR, ZEROI, ZEROR, ZETA1I,     EILI8317
     * ZETA1R, ZETA2I, ZETA2R, ZI, ZR, CYR, CYI, D1MACH, ZABS           EILI8318
      INTEGER I, IFLAG, INIT, K, KODE, M, N, ND, NLAST, NN, NUF, NW, NZ EILI8319
      EXTERNAL ZABS                                                     EILI8320
      DIMENSION BRY(3), YR(N), YI(N), CWRKR(16), CWRKI(16), CSSR(3),    EILI8321
     * CSRR(3), CYR(2), CYI(2)                                          EILI8322
      DATA ZEROR,ZEROI,CONER / 0.0D0, 0.0D0, 1.0D0 /                    EILI8323
C                                                                       EILI8324
      NZ = 0                                                            EILI8325
      ND = N                                                            EILI8326
      NLAST = 0                                                         EILI8327
C-----------------------------------------------------------------------EILI8328
C     COMPUTED VALUES WITH EXPONENTS BETWEEN ALIM AND ELIM IN MAG-      EILI8329
C     NITUDE ARE SCALED TO KEEP INTERMEDIATE ARITHMETIC ON SCALE,       EILI8330
C     EXP(ALIM)=EXP(ELIM)*TOL                                           EILI8331
C-----------------------------------------------------------------------EILI8332
      CSCL = 1.0D0/TOL                                                  EILI8333
      CRSC = TOL                                                        EILI8334
      CSSR(1) = CSCL                                                    EILI8335
      CSSR(2) = CONER                                                   EILI8336
      CSSR(3) = CRSC                                                    EILI8337
      CSRR(1) = CRSC                                                    EILI8338
      CSRR(2) = CONER                                                   EILI8339
      CSRR(3) = CSCL                                                    EILI8340
      BRY(1) = 1.0D+3*D1MACH(1)/TOL                                     EILI8341
C-----------------------------------------------------------------------EILI8342
C     CHECK FOR UNDERFLOW AND OVERFLOW ON FIRST MEMBER                  EILI8343
C-----------------------------------------------------------------------EILI8344
      FN = DMAX1(FNU,1.0D0)                                             EILI8345
      INIT = 0                                                          EILI8346
      CALL ZUNIK(ZR, ZI, FN, 1, 1, TOL, INIT, PHIR, PHII, ZETA1R,       EILI8347
     * ZETA1I, ZETA2R, ZETA2I, SUMR, SUMI, CWRKR, CWRKI)                EILI8348
      IF (KODE.EQ.1) GO TO 10                                           EILI8349
      STR = ZR + ZETA2R                                                 EILI8350
      STI = ZI + ZETA2I                                                 EILI8351
      RAST = FN/ZABS(STR,STI)                                           EILI8352
      STR = STR*RAST*RAST                                               EILI8353
      STI = -STI*RAST*RAST                                              EILI8354
      S1R = -ZETA1R + STR                                               EILI8355
      S1I = -ZETA1I + STI                                               EILI8356
      GO TO 20                                                          EILI8357
   10 CONTINUE                                                          EILI8358
      S1R = -ZETA1R + ZETA2R                                            EILI8359
      S1I = -ZETA1I + ZETA2I                                            EILI8360
   20 CONTINUE                                                          EILI8361
      RS1 = S1R                                                         EILI8362
      IF (DABS(RS1).GT.ELIM) GO TO 130                                  EILI8363
   30 CONTINUE                                                          EILI8364
      NN = MIN0(2,ND)                                                   EILI8365
      DO 80 I=1,NN                                                      EILI8366
        FN = FNU + DBLE(FLOAT(ND-I))                                    EILI8367
        INIT = 0                                                        EILI8368
        CALL ZUNIK(ZR, ZI, FN, 1, 0, TOL, INIT, PHIR, PHII, ZETA1R,     EILI8369
     *   ZETA1I, ZETA2R, ZETA2I, SUMR, SUMI, CWRKR, CWRKI)              EILI8370
        IF (KODE.EQ.1) GO TO 40                                         EILI8371
        STR = ZR + ZETA2R                                               EILI8372
        STI = ZI + ZETA2I                                               EILI8373
        RAST = FN/ZABS(STR,STI)                                         EILI8374
        STR = STR*RAST*RAST                                             EILI8375
        STI = -STI*RAST*RAST                                            EILI8376
        S1R = -ZETA1R + STR                                             EILI8377
        S1I = -ZETA1I + STI + ZI                                        EILI8378
        GO TO 50                                                        EILI8379
   40   CONTINUE                                                        EILI8380
        S1R = -ZETA1R + ZETA2R                                          EILI8381
        S1I = -ZETA1I + ZETA2I                                          EILI8382
   50   CONTINUE                                                        EILI8383
C-----------------------------------------------------------------------EILI8384
C     TEST FOR UNDERFLOW AND OVERFLOW                                   EILI8385
C-----------------------------------------------------------------------EILI8386
        RS1 = S1R                                                       EILI8387
        IF (DABS(RS1).GT.ELIM) GO TO 110                                EILI8388
        IF (I.EQ.1) IFLAG = 2                                           EILI8389
        IF (DABS(RS1).LT.ALIM) GO TO 60                                 EILI8390
C-----------------------------------------------------------------------EILI8391
C     REFINE  TEST AND SCALE                                            EILI8392
C-----------------------------------------------------------------------EILI8393
        APHI = ZABS(PHIR,PHII)                                          EILI8394
        RS1 = RS1 + DLOG(APHI)                                          EILI8395
        IF (DABS(RS1).GT.ELIM) GO TO 110                                EILI8396
        IF (I.EQ.1) IFLAG = 1                                           EILI8397
        IF (RS1.LT.0.0D0) GO TO 60                                      EILI8398
        IF (I.EQ.1) IFLAG = 3                                           EILI8399
   60   CONTINUE                                                        EILI8400
C-----------------------------------------------------------------------EILI8401
C     SCALE S1 IF CABS(S1).LT.ASCLE                                     EILI8402
C-----------------------------------------------------------------------EILI8403
        S2R = PHIR*SUMR - PHII*SUMI                                     EILI8404
        S2I = PHIR*SUMI + PHII*SUMR                                     EILI8405
        STR = DEXP(S1R)*CSSR(IFLAG)                                     EILI8406
        S1R = STR*DCOS(S1I)                                             EILI8407
        S1I = STR*DSIN(S1I)                                             EILI8408
        STR = S2R*S1R - S2I*S1I                                         EILI8409
        S2I = S2R*S1I + S2I*S1R                                         EILI8410
        S2R = STR                                                       EILI8411
        IF (IFLAG.NE.1) GO TO 70                                        EILI8412
        CALL ZUCHK(S2R, S2I, NW, BRY(1), TOL)                           EILI8413
        IF (NW.NE.0) GO TO 110                                          EILI8414
   70   CONTINUE                                                        EILI8415
        CYR(I) = S2R                                                    EILI8416
        CYI(I) = S2I                                                    EILI8417
        M = ND - I + 1                                                  EILI8418
        YR(M) = S2R*CSRR(IFLAG)                                         EILI8419
        YI(M) = S2I*CSRR(IFLAG)                                         EILI8420
   80 CONTINUE                                                          EILI8421
      IF (ND.LE.2) GO TO 100                                            EILI8422
      RAST = 1.0D0/ZABS(ZR,ZI)                                          EILI8423
      STR = ZR*RAST                                                     EILI8424
      STI = -ZI*RAST                                                    EILI8425
      RZR = (STR+STR)*RAST                                              EILI8426
      RZI = (STI+STI)*RAST                                              EILI8427
      BRY(2) = 1.0D0/BRY(1)                                             EILI8428
      BRY(3) = D1MACH(2)                                                EILI8429
      S1R = CYR(1)                                                      EILI8430
      S1I = CYI(1)                                                      EILI8431
      S2R = CYR(2)                                                      EILI8432
      S2I = CYI(2)                                                      EILI8433
      C1R = CSRR(IFLAG)                                                 EILI8434
      ASCLE = BRY(IFLAG)                                                EILI8435
      K = ND - 2                                                        EILI8436
      FN = DBLE(FLOAT(K))                                               EILI8437
      DO 90 I=3,ND                                                      EILI8438
        C2R = S2R                                                       EILI8439
        C2I = S2I                                                       EILI8440
        S2R = S1R + (FNU+FN)*(RZR*C2R-RZI*C2I)                          EILI8441
        S2I = S1I + (FNU+FN)*(RZR*C2I+RZI*C2R)                          EILI8442
        S1R = C2R                                                       EILI8443
        S1I = C2I                                                       EILI8444
        C2R = S2R*C1R                                                   EILI8445
        C2I = S2I*C1R                                                   EILI8446
        YR(K) = C2R                                                     EILI8447
        YI(K) = C2I                                                     EILI8448
        K = K - 1                                                       EILI8449
        FN = FN - 1.0D0                                                 EILI8450
        IF (IFLAG.GE.3) GO TO 90                                        EILI8451
        STR = DABS(C2R)                                                 EILI8452
        STI = DABS(C2I)                                                 EILI8453
        C2M = DMAX1(STR,STI)                                            EILI8454
        IF (C2M.LE.ASCLE) GO TO 90                                      EILI8455
        IFLAG = IFLAG + 1                                               EILI8456
        ASCLE = BRY(IFLAG)                                              EILI8457
        S1R = S1R*C1R                                                   EILI8458
        S1I = S1I*C1R                                                   EILI8459
        S2R = C2R                                                       EILI8460
        S2I = C2I                                                       EILI8461
        S1R = S1R*CSSR(IFLAG)                                           EILI8462
        S1I = S1I*CSSR(IFLAG)                                           EILI8463
        S2R = S2R*CSSR(IFLAG)                                           EILI8464
        S2I = S2I*CSSR(IFLAG)                                           EILI8465
        C1R = CSRR(IFLAG)                                               EILI8466
   90 CONTINUE                                                          EILI8467
  100 CONTINUE                                                          EILI8468
      RETURN                                                            EILI8469
C-----------------------------------------------------------------------EILI8470
C     SET UNDERFLOW AND UPDATE PARAMETERS                               EILI8471
C-----------------------------------------------------------------------EILI8472
  110 CONTINUE                                                          EILI8473
      IF (RS1.GT.0.0D0) GO TO 120                                       EILI8474
      YR(ND) = ZEROR                                                    EILI8475
      YI(ND) = ZEROI                                                    EILI8476
      NZ = NZ + 1                                                       EILI8477
      ND = ND - 1                                                       EILI8478
      IF (ND.EQ.0) GO TO 100                                            EILI8479
      CALL ZUOIK(ZR, ZI, FNU, KODE, 1, ND, YR, YI, NUF, TOL, ELIM, ALIM)EILI8480
      IF (NUF.LT.0) GO TO 120                                           EILI8481
      ND = ND - NUF                                                     EILI8482
      NZ = NZ + NUF                                                     EILI8483
      IF (ND.EQ.0) GO TO 100                                            EILI8484
      FN = FNU + DBLE(FLOAT(ND-1))                                      EILI8485
      IF (FN.GE.FNUL) GO TO 30                                          EILI8486
      NLAST = ND                                                        EILI8487
      RETURN                                                            EILI8488
  120 CONTINUE                                                          EILI8489
      NZ = -1                                                           EILI8490
      RETURN                                                            EILI8491
  130 CONTINUE                                                          EILI8492
      IF (RS1.GT.0.0D0) GO TO 120                                       EILI8493
      NZ = N                                                            EILI8494
      DO 140 I=1,N                                                      EILI8495
        YR(I) = ZEROR                                                   EILI8496
        YI(I) = ZEROI                                                   EILI8497
  140 CONTINUE                                                          EILI8498
      RETURN                                                            EILI8499
      END                                                               EILI8500
      SUBROUTINE ZUNI2(ZR, ZI, FNU, KODE, N, YR, YI, NZ, NLAST, FNUL,   EILI8501
     * TOL, ELIM, ALIM)                                                 EILI8502
C***BEGIN PROLOGUE  ZUNI2                                               EILI8503
C***REFER TO  ZBESI,ZBESK                                               EILI8504
C                                                                       EILI8505
C     ZUNI2 COMPUTES I(FNU,Z) IN THE RIGHT HALF PLANE BY MEANS OF       EILI8506
C     UNIFORM ASYMPTOTIC EXPANSION FOR J(FNU,ZN) WHERE ZN IS Z*I        EILI8507
C     OR -Z*I AND ZN IS IN THE RIGHT HALF PLANE ALSO.                   EILI8508
C                                                                       EILI8509
C     FNUL IS THE SMALLEST ORDER PERMITTED FOR THE ASYMPTOTIC           EILI8510
C     EXPANSION. NLAST=0 MEANS ALL OF THE Y VALUES WERE SET.            EILI8511
C     NLAST.NE.0 IS THE NUMBER LEFT TO BE COMPUTED BY ANOTHER           EILI8512
C     FORMULA FOR ORDERS FNU TO FNU+NLAST-1 BECAUSE FNU+NLAST-1.LT.FNUL.EILI8513
C     Y(I)=CZERO FOR I=NLAST+1,N                                        EILI8514
C                                                                       EILI8515
C***ROUTINES CALLED  ZAIRY,ZUCHK,ZUNHJ,ZUOIK,D1MACH,ZABS                EILI8516
C***END PROLOGUE  ZUNI2                                                 EILI8517
C     COMPLEX AI,ARG,ASUM,BSUM,CFN,CI,CID,CIP,CONE,CRSC,CSCL,CSR,CSS,   EILI8518
C    *CZERO,C1,C2,DAI,PHI,RZ,S1,S2,Y,Z,ZB,ZETA1,ZETA2,ZN                EILI8519
      DOUBLE PRECISION AARG, AIC, AII, AIR, ALIM, ANG, APHI, ARGI,      EILI8520
     * ARGR, ASCLE, ASUMI, ASUMR, BRY, BSUMI, BSUMR, CIDI, CIPI, CIPR,  EILI8521
     * CONER, CRSC, CSCL, CSRR, CSSR, C1R, C2I, C2M, C2R, DAII,         EILI8522
     * DAIR, ELIM, FN, FNU, FNUL, HPI, PHII, PHIR, RAST, RAZ, RS1, RZI, EILI8523
     * RZR, STI, STR, S1I, S1R, S2I, S2R, TOL, YI, YR, ZBI, ZBR, ZEROI, EILI8524
     * ZEROR, ZETA1I, ZETA1R, ZETA2I, ZETA2R, ZI, ZNI, ZNR, ZR, CYR,    EILI8525
     * CYI, D1MACH, ZABS, CAR, SAR                                      EILI8526
      INTEGER I, IFLAG, IN, INU, J, K, KODE, N, NAI, ND, NDAI, NLAST,   EILI8527
     * NN, NUF, NW, NZ, IDUM                                            EILI8528
      EXTERNAL ZABS                                                     EILI8529
      DIMENSION BRY(3), YR(N), YI(N), CIPR(4), CIPI(4), CSSR(3),        EILI8530
     * CSRR(3), CYR(2), CYI(2)                                          EILI8531
      DATA ZEROR,ZEROI,CONER / 0.0D0, 0.0D0, 1.0D0 /                    EILI8532
      DATA CIPR(1),CIPI(1),CIPR(2),CIPI(2),CIPR(3),CIPI(3),CIPR(4),     EILI8533
     * CIPI(4)/ 1.0D0,0.0D0, 0.0D0,1.0D0, -1.0D0,0.0D0, 0.0D0,-1.0D0/   EILI8534
      DATA HPI, AIC  /                                                  EILI8535
     1      1.57079632679489662D+00,     1.265512123484645396D+00/      EILI8536
C                                                                       EILI8537
      NZ = 0                                                            EILI8538
      ND = N                                                            EILI8539
      NLAST = 0                                                         EILI8540
C-----------------------------------------------------------------------EILI8541
C     COMPUTED VALUES WITH EXPONENTS BETWEEN ALIM AND ELIM IN MAG-      EILI8542
C     NITUDE ARE SCALED TO KEEP INTERMEDIATE ARITHMETIC ON SCALE,       EILI8543
C     EXP(ALIM)=EXP(ELIM)*TOL                                           EILI8544
C-----------------------------------------------------------------------EILI8545
      CSCL = 1.0D0/TOL                                                  EILI8546
      CRSC = TOL                                                        EILI8547
      CSSR(1) = CSCL                                                    EILI8548
      CSSR(2) = CONER                                                   EILI8549
      CSSR(3) = CRSC                                                    EILI8550
      CSRR(1) = CRSC                                                    EILI8551
      CSRR(2) = CONER                                                   EILI8552
      CSRR(3) = CSCL                                                    EILI8553
      BRY(1) = 1.0D+3*D1MACH(1)/TOL                                     EILI8554
C-----------------------------------------------------------------------EILI8555
C     ZN IS IN THE RIGHT HALF PLANE AFTER ROTATION BY CI OR -CI         EILI8556
C-----------------------------------------------------------------------EILI8557
      ZNR = ZI                                                          EILI8558
      ZNI = -ZR                                                         EILI8559
      ZBR = ZR                                                          EILI8560
      ZBI = ZI                                                          EILI8561
      CIDI = -CONER                                                     EILI8562
      INU = INT(SNGL(FNU))                                              EILI8563
      ANG = HPI*(FNU-DBLE(FLOAT(INU)))                                  EILI8564
      C2R = DCOS(ANG)                                                   EILI8565
      C2I = DSIN(ANG)                                                   EILI8566
      CAR = C2R                                                         EILI8567
      SAR = C2I                                                         EILI8568
      IN = INU + N - 1                                                  EILI8569
      IN = MOD(IN,4) + 1                                                EILI8570
      STR = C2R*CIPR(IN) - C2I*CIPI(IN)                                 EILI8571
      C2I = C2R*CIPI(IN) + C2I*CIPR(IN)                                 EILI8572
      C2R = STR                                                         EILI8573
      IF (ZI.GT.0.0D0) GO TO 10                                         EILI8574
      ZNR = -ZNR                                                        EILI8575
      ZBI = -ZBI                                                        EILI8576
      CIDI = -CIDI                                                      EILI8577
      C2I = -C2I                                                        EILI8578
   10 CONTINUE                                                          EILI8579
C-----------------------------------------------------------------------EILI8580
C     CHECK FOR UNDERFLOW AND OVERFLOW ON FIRST MEMBER                  EILI8581
C-----------------------------------------------------------------------EILI8582
      FN = DMAX1(FNU,1.0D0)                                             EILI8583
      CALL ZUNHJ(ZNR, ZNI, FN, 1, TOL, PHIR, PHII, ARGR, ARGI, ZETA1R,  EILI8584
     * ZETA1I, ZETA2R, ZETA2I, ASUMR, ASUMI, BSUMR, BSUMI)              EILI8585
      IF (KODE.EQ.1) GO TO 20                                           EILI8586
      STR = ZBR + ZETA2R                                                EILI8587
      STI = ZBI + ZETA2I                                                EILI8588
      RAST = FN/ZABS(STR,STI)                                           EILI8589
      STR = STR*RAST*RAST                                               EILI8590
      STI = -STI*RAST*RAST                                              EILI8591
      S1R = -ZETA1R + STR                                               EILI8592
      S1I = -ZETA1I + STI                                               EILI8593
      GO TO 30                                                          EILI8594
   20 CONTINUE                                                          EILI8595
      S1R = -ZETA1R + ZETA2R                                            EILI8596
      S1I = -ZETA1I + ZETA2I                                            EILI8597
   30 CONTINUE                                                          EILI8598
      RS1 = S1R                                                         EILI8599
      IF (DABS(RS1).GT.ELIM) GO TO 150                                  EILI8600
   40 CONTINUE                                                          EILI8601
      NN = MIN0(2,ND)                                                   EILI8602
      DO 90 I=1,NN                                                      EILI8603
        FN = FNU + DBLE(FLOAT(ND-I))                                    EILI8604
        CALL ZUNHJ(ZNR, ZNI, FN, 0, TOL, PHIR, PHII, ARGR, ARGI,        EILI8605
     *   ZETA1R, ZETA1I, ZETA2R, ZETA2I, ASUMR, ASUMI, BSUMR, BSUMI)    EILI8606
        IF (KODE.EQ.1) GO TO 50                                         EILI8607
        STR = ZBR + ZETA2R                                              EILI8608
        STI = ZBI + ZETA2I                                              EILI8609
        RAST = FN/ZABS(STR,STI)                                         EILI8610
        STR = STR*RAST*RAST                                             EILI8611
        STI = -STI*RAST*RAST                                            EILI8612
        S1R = -ZETA1R + STR                                             EILI8613
        S1I = -ZETA1I + STI + DABS(ZI)                                  EILI8614
        GO TO 60                                                        EILI8615
   50   CONTINUE                                                        EILI8616
        S1R = -ZETA1R + ZETA2R                                          EILI8617
        S1I = -ZETA1I + ZETA2I                                          EILI8618
   60   CONTINUE                                                        EILI8619
C-----------------------------------------------------------------------EILI8620
C     TEST FOR UNDERFLOW AND OVERFLOW                                   EILI8621
C-----------------------------------------------------------------------EILI8622
        RS1 = S1R                                                       EILI8623
        IF (DABS(RS1).GT.ELIM) GO TO 120                                EILI8624
        IF (I.EQ.1) IFLAG = 2                                           EILI8625
        IF (DABS(RS1).LT.ALIM) GO TO 70                                 EILI8626
C-----------------------------------------------------------------------EILI8627
C     REFINE  TEST AND SCALE                                            EILI8628
C-----------------------------------------------------------------------EILI8629
C-----------------------------------------------------------------------EILI8630
        APHI = ZABS(PHIR,PHII)                                          EILI8631
        AARG = ZABS(ARGR,ARGI)                                          EILI8632
        RS1 = RS1 + DLOG(APHI) - 0.25D0*DLOG(AARG) - AIC                EILI8633
        IF (DABS(RS1).GT.ELIM) GO TO 120                                EILI8634
        IF (I.EQ.1) IFLAG = 1                                           EILI8635
        IF (RS1.LT.0.0D0) GO TO 70                                      EILI8636
        IF (I.EQ.1) IFLAG = 3                                           EILI8637
   70   CONTINUE                                                        EILI8638
C-----------------------------------------------------------------------EILI8639
C     SCALE S1 TO KEEP INTERMEDIATE ARITHMETIC ON SCALE NEAR            EILI8640
C     EXPONENT EXTREMES                                                 EILI8641
C-----------------------------------------------------------------------EILI8642
        CALL ZAIRY(ARGR, ARGI, 0, 2, AIR, AII, NAI, IDUM)               EILI8643
        CALL ZAIRY(ARGR, ARGI, 1, 2, DAIR, DAII, NDAI, IDUM)            EILI8644
        STR = DAIR*BSUMR - DAII*BSUMI                                   EILI8645
        STI = DAIR*BSUMI + DAII*BSUMR                                   EILI8646
        STR = STR + (AIR*ASUMR-AII*ASUMI)                               EILI8647
        STI = STI + (AIR*ASUMI+AII*ASUMR)                               EILI8648
        S2R = PHIR*STR - PHII*STI                                       EILI8649
        S2I = PHIR*STI + PHII*STR                                       EILI8650
        STR = DEXP(S1R)*CSSR(IFLAG)                                     EILI8651
        S1R = STR*DCOS(S1I)                                             EILI8652
        S1I = STR*DSIN(S1I)                                             EILI8653
        STR = S2R*S1R - S2I*S1I                                         EILI8654
        S2I = S2R*S1I + S2I*S1R                                         EILI8655
        S2R = STR                                                       EILI8656
        IF (IFLAG.NE.1) GO TO 80                                        EILI8657
        CALL ZUCHK(S2R, S2I, NW, BRY(1), TOL)                           EILI8658
        IF (NW.NE.0) GO TO 120                                          EILI8659
   80   CONTINUE                                                        EILI8660
        IF (ZI.LE.0.0D0) S2I = -S2I                                     EILI8661
        STR = S2R*C2R - S2I*C2I                                         EILI8662
        S2I = S2R*C2I + S2I*C2R                                         EILI8663
        S2R = STR                                                       EILI8664
        CYR(I) = S2R                                                    EILI8665
        CYI(I) = S2I                                                    EILI8666
        J = ND - I + 1                                                  EILI8667
        YR(J) = S2R*CSRR(IFLAG)                                         EILI8668
        YI(J) = S2I*CSRR(IFLAG)                                         EILI8669
        STR = -C2I*CIDI                                                 EILI8670
        C2I = C2R*CIDI                                                  EILI8671
        C2R = STR                                                       EILI8672
   90 CONTINUE                                                          EILI8673
      IF (ND.LE.2) GO TO 110                                            EILI8674
      RAZ = 1.0D0/ZABS(ZR,ZI)                                           EILI8675
      STR = ZR*RAZ                                                      EILI8676
      STI = -ZI*RAZ                                                     EILI8677
      RZR = (STR+STR)*RAZ                                               EILI8678
      RZI = (STI+STI)*RAZ                                               EILI8679
      BRY(2) = 1.0D0/BRY(1)                                             EILI8680
      BRY(3) = D1MACH(2)                                                EILI8681
      S1R = CYR(1)                                                      EILI8682
      S1I = CYI(1)                                                      EILI8683
      S2R = CYR(2)                                                      EILI8684
      S2I = CYI(2)                                                      EILI8685
      C1R = CSRR(IFLAG)                                                 EILI8686
      ASCLE = BRY(IFLAG)                                                EILI8687
      K = ND - 2                                                        EILI8688
      FN = DBLE(FLOAT(K))                                               EILI8689
      DO 100 I=3,ND                                                     EILI8690
        C2R = S2R                                                       EILI8691
        C2I = S2I                                                       EILI8692
        S2R = S1R + (FNU+FN)*(RZR*C2R-RZI*C2I)                          EILI8693
        S2I = S1I + (FNU+FN)*(RZR*C2I+RZI*C2R)                          EILI8694
        S1R = C2R                                                       EILI8695
        S1I = C2I                                                       EILI8696
        C2R = S2R*C1R                                                   EILI8697
        C2I = S2I*C1R                                                   EILI8698
        YR(K) = C2R                                                     EILI8699
        YI(K) = C2I                                                     EILI8700
        K = K - 1                                                       EILI8701
        FN = FN - 1.0D0                                                 EILI8702
        IF (IFLAG.GE.3) GO TO 100                                       EILI8703
        STR = DABS(C2R)                                                 EILI8704
        STI = DABS(C2I)                                                 EILI8705
        C2M = DMAX1(STR,STI)                                            EILI8706
        IF (C2M.LE.ASCLE) GO TO 100                                     EILI8707
        IFLAG = IFLAG + 1                                               EILI8708
        ASCLE = BRY(IFLAG)                                              EILI8709
        S1R = S1R*C1R                                                   EILI8710
        S1I = S1I*C1R                                                   EILI8711
        S2R = C2R                                                       EILI8712
        S2I = C2I                                                       EILI8713
        S1R = S1R*CSSR(IFLAG)                                           EILI8714
        S1I = S1I*CSSR(IFLAG)                                           EILI8715
        S2R = S2R*CSSR(IFLAG)                                           EILI8716
        S2I = S2I*CSSR(IFLAG)                                           EILI8717
        C1R = CSRR(IFLAG)                                               EILI8718
  100 CONTINUE                                                          EILI8719
  110 CONTINUE                                                          EILI8720
      RETURN                                                            EILI8721
  120 CONTINUE                                                          EILI8722
      IF (RS1.GT.0.0D0) GO TO 140                                       EILI8723
C-----------------------------------------------------------------------EILI8724
C     SET UNDERFLOW AND UPDATE PARAMETERS                               EILI8725
C-----------------------------------------------------------------------EILI8726
      YR(ND) = ZEROR                                                    EILI8727
      YI(ND) = ZEROI                                                    EILI8728
      NZ = NZ + 1                                                       EILI8729
      ND = ND - 1                                                       EILI8730
      IF (ND.EQ.0) GO TO 110                                            EILI8731
      CALL ZUOIK(ZR, ZI, FNU, KODE, 1, ND, YR, YI, NUF, TOL, ELIM, ALIM)EILI8732
      IF (NUF.LT.0) GO TO 140                                           EILI8733
      ND = ND - NUF                                                     EILI8734
      NZ = NZ + NUF                                                     EILI8735
      IF (ND.EQ.0) GO TO 110                                            EILI8736
      FN = FNU + DBLE(FLOAT(ND-1))                                      EILI8737
      IF (FN.LT.FNUL) GO TO 130                                         EILI8738
C      FN = CIDI                                                        EILI8739
C      J = NUF + 1                                                      EILI8740
C      K = MOD(J,4) + 1                                                 EILI8741
C      S1R = CIPR(K)                                                    EILI8742
C      S1I = CIPI(K)                                                    EILI8743
C      IF (FN.LT.0.0D0) S1I = -S1I                                      EILI8744
C      STR = C2R*S1R - C2I*S1I                                          EILI8745
C      C2I = C2R*S1I + C2I*S1R                                          EILI8746
C      C2R = STR                                                        EILI8747
      IN = INU + ND - 1                                                 EILI8748
      IN = MOD(IN,4) + 1                                                EILI8749
      C2R = CAR*CIPR(IN) - SAR*CIPI(IN)                                 EILI8750
      C2I = CAR*CIPI(IN) + SAR*CIPR(IN)                                 EILI8751
      IF (ZI.LE.0.0D0) C2I = -C2I                                       EILI8752
      GO TO 40                                                          EILI8753
  130 CONTINUE                                                          EILI8754
      NLAST = ND                                                        EILI8755
      RETURN                                                            EILI8756
  140 CONTINUE                                                          EILI8757
      NZ = -1                                                           EILI8758
      RETURN                                                            EILI8759
  150 CONTINUE                                                          EILI8760
      IF (RS1.GT.0.0D0) GO TO 140                                       EILI8761
      NZ = N                                                            EILI8762
      DO 160 I=1,N                                                      EILI8763
        YR(I) = ZEROR                                                   EILI8764
        YI(I) = ZEROI                                                   EILI8765
  160 CONTINUE                                                          EILI8766
      RETURN                                                            EILI8767
      END       