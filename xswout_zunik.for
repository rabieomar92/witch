!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:12:44 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZUNIK(ZRR, ZRI, FNU, IKFLG, IPMTR, TOL, INIT, PHIR,    EILI9371
     * PHII, ZETA1R, ZETA1I, ZETA2R, ZETA2I, SUMR, SUMI, CWRKR, CWRKI)  EILI9372
C***BEGIN PROLOGUE  ZUNIK                                               EILI9373
C***REFER TO  ZBESI,ZBESK                                               EILI9374
C                                                                       EILI9375
C        ZUNIK COMPUTES PARAMETERS FOR THE UNIFORM ASYMPTOTIC           EILI9376
C        EXPANSIONS OF THE I AND K FUNCTIONS ON IKFLG= 1 OR 2           EILI9377
C        RESPECTIVELY BY                                                EILI9378
C                                                                       EILI9379
C        W(FNU,ZR) = PHI*EXP(ZETA)*SUM                                  EILI9380
C                                                                       EILI9381
C        WHERE       ZETA=-ZETA1 + ZETA2       OR                       EILI9382
C                          ZETA1 - ZETA2                                EILI9383
C                                                                       EILI9384
C        THE FIRST CALL MUST HAVE INIT=0. SUBSEQUENT CALLS WITH THE     EILI9385
C        SAME ZR AND FNU WILL RETURN THE I OR K FUNCTION ON IKFLG=      EILI9386
C        1 OR 2 WITH NO CHANGE IN INIT. CWRK IS A COMPLEX WORK          EILI9387
C        ARRAY. IPMTR=0 COMPUTES ALL PARAMETERS. IPMTR=1 COMPUTES PHI,  EILI9388
C        ZETA1,ZETA2.                                                   EILI9389
C                                                                       EILI9390
C***ROUTINES CALLED  ZDIV,ZLOG,ZSQRT                                    EILI9391
C***END PROLOGUE  ZUNIK                                                 EILI9392
C     COMPLEX CFN,CON,CONE,CRFN,CWRK,CZERO,PHI,S,SR,SUM,T,T2,ZETA1,     EILI9393
C    *ZETA2,ZN,ZR                                                       EILI9394
      DOUBLE PRECISION AC, C, CON, CONEI, CONER, CRFNI, CRFNR, CWRKI,   EILI9395
     * CWRKR, FNU, PHII, PHIR, RFN, SI, SR, SRI, SRR, STI, STR, SUMI,   EILI9396
     * SUMR, TEST, TI, TOL, TR, T2I, T2R, ZEROI, ZEROR, ZETA1I, ZETA1R, EILI9397
     * ZETA2I, ZETA2R, ZNI, ZNR, ZRI, ZRR                               EILI9398
      double precision d1mach                                           EILI9399
      INTEGER I, IDUM, IKFLG, INIT, IPMTR, J, K, L                      EILI9400
CNEA                                                                    EILI9401
      EXTERNAL ZLOG                                                     EILI9402
CNEA                                                                    EILI9403
      DIMENSION C(120), CWRKR(16), CWRKI(16), CON(2)                    EILI9404
      DATA ZEROR,ZEROI,CONER,CONEI / 0.0D0, 0.0D0, 1.0D0, 0.0D0 /       EILI9405
      DATA CON(1), CON(2)  /                                            EILI9406
     1 3.98942280401432678D-01,  1.25331413731550025D+00 /              EILI9407
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10), EILI9408
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),      EILI9409
     2     C(19), C(20), C(21), C(22), C(23), C(24)/                    EILI9410
     3     1.00000000000000000D+00,    -2.08333333333333333D-01,        EILI9411
     4     1.25000000000000000D-01,     3.34201388888888889D-01,        EILI9412
     5    -4.01041666666666667D-01,     7.03125000000000000D-02,        EILI9413
     6    -1.02581259645061728D+00,     1.84646267361111111D+00,        EILI9414
     7    -8.91210937500000000D-01,     7.32421875000000000D-02,        EILI9415
     8     4.66958442342624743D+00,    -1.12070026162229938D+01,        EILI9416
     9     8.78912353515625000D+00,    -2.36408691406250000D+00,        EILI9417
     A     1.12152099609375000D-01,    -2.82120725582002449D+01,        EILI9418
     B     8.46362176746007346D+01,    -9.18182415432400174D+01,        EILI9419
     C     4.25349987453884549D+01,    -7.36879435947963170D+00,        EILI9420
     D     2.27108001708984375D-01,     2.12570130039217123D+02,        EILI9421
     E    -7.65252468141181642D+02,     1.05999045252799988D+03/        EILI9422
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),      EILI9423
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),      EILI9424
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/      EILI9425
     3    -6.99579627376132541D+02,     2.18190511744211590D+02,        EILI9426
     4    -2.64914304869515555D+01,     5.72501420974731445D-01,        EILI9427
     5    -1.91945766231840700D+03,     8.06172218173730938D+03,        EILI9428
     6    -1.35865500064341374D+04,     1.16553933368645332D+04,        EILI9429
     7    -5.30564697861340311D+03,     1.20090291321635246D+03,        EILI9430
     8    -1.08090919788394656D+02,     1.72772750258445740D+00,        EILI9431
     9     2.02042913309661486D+04,    -9.69805983886375135D+04,        EILI9432
     A     1.92547001232531532D+05,    -2.03400177280415534D+05,        EILI9433
     B     1.22200464983017460D+05,    -4.11926549688975513D+04,        EILI9434
     C     7.10951430248936372D+03,    -4.93915304773088012D+02,        EILI9435
     D     6.07404200127348304D+00,    -2.42919187900551333D+05,        EILI9436
     E     1.31176361466297720D+06,    -2.99801591853810675D+06/        EILI9437
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),      EILI9438
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),      EILI9439
     2     C(65), C(66), C(67), C(68), C(69), C(70), C(71), C(72)/      EILI9440
     3     3.76327129765640400D+06,    -2.81356322658653411D+06,        EILI9441
     4     1.26836527332162478D+06,    -3.31645172484563578D+05,        EILI9442
     5     4.52187689813627263D+04,    -2.49983048181120962D+03,        EILI9443
     6     2.43805296995560639D+01,     3.28446985307203782D+06,        EILI9444
     7    -1.97068191184322269D+07,     5.09526024926646422D+07,        EILI9445
     8    -7.41051482115326577D+07,     6.63445122747290267D+07,        EILI9446
     9    -3.75671766607633513D+07,     1.32887671664218183D+07,        EILI9447
     A    -2.78561812808645469D+06,     3.08186404612662398D+05,        EILI9448
     B    -1.38860897537170405D+04,     1.10017140269246738D+02,        EILI9449
     C    -4.93292536645099620D+07,     3.25573074185765749D+08,        EILI9450
     D    -9.39462359681578403D+08,     1.55359689957058006D+09,        EILI9451
     E    -1.62108055210833708D+09,     1.10684281682301447D+09/        EILI9452
      DATA C(73), C(74), C(75), C(76), C(77), C(78), C(79), C(80),      EILI9453
     1     C(81), C(82), C(83), C(84), C(85), C(86), C(87), C(88),      EILI9454
     2     C(89), C(90), C(91), C(92), C(93), C(94), C(95), C(96)/      EILI9455
     3    -4.95889784275030309D+08,     1.42062907797533095D+08,        EILI9456
     4    -2.44740627257387285D+07,     2.24376817792244943D+06,        EILI9457
     5    -8.40054336030240853D+04,     5.51335896122020586D+02,        EILI9458
     6     8.14789096118312115D+08,    -5.86648149205184723D+09,        EILI9459
     7     1.86882075092958249D+10,    -3.46320433881587779D+10,        EILI9460
     8     4.12801855797539740D+10,    -3.30265997498007231D+10,        EILI9461
     9     1.79542137311556001D+10,    -6.56329379261928433D+09,        EILI9462
     A     1.55927986487925751D+09,    -2.25105661889415278D+08,        EILI9463
     B     1.73951075539781645D+07,    -5.49842327572288687D+05,        EILI9464
     C     3.03809051092238427D+03,    -1.46792612476956167D+10,        EILI9465
     D     1.14498237732025810D+11,    -3.99096175224466498D+11,        EILI9466
     E     8.19218669548577329D+11,    -1.09837515608122331D+12/        EILI9467
      DATA C(97), C(98), C(99), C(100), C(101), C(102), C(103), C(104), EILI9468
     1     C(105), C(106), C(107), C(108), C(109), C(110), C(111),      EILI9469
     2     C(112), C(113), C(114), C(115), C(116), C(117), C(118)/      EILI9470
     3     1.00815810686538209D+12,    -6.45364869245376503D+11,        EILI9471
     4     2.87900649906150589D+11,    -8.78670721780232657D+10,        EILI9472
     5     1.76347306068349694D+10,    -2.16716498322379509D+09,        EILI9473
     6     1.43157876718888981D+08,    -3.87183344257261262D+06,        EILI9474
     7     1.82577554742931747D+04,     2.86464035717679043D+11,        EILI9475
     8    -2.40629790002850396D+12,     9.10934118523989896D+12,        EILI9476
     9    -2.05168994109344374D+13,     3.05651255199353206D+13,        EILI9477
     A    -3.16670885847851584D+13,     2.33483640445818409D+13,        EILI9478
     B    -1.23204913055982872D+13,     4.61272578084913197D+12,        EILI9479
     C    -1.19655288019618160D+12,     2.05914503232410016D+11,        EILI9480
     D    -2.18229277575292237D+10,     1.24700929351271032D+09/        EILI9481
      DATA C(119), C(120)/                                              EILI9482
     1    -2.91883881222208134D+07,     1.18838426256783253D+05/        EILI9483
C                                                                       EILI9484
      IF (INIT.NE.0) GO TO 40                                           EILI9485
C-----------------------------------------------------------------------EILI9486
C     INITIALIZE ALL VARIABLES                                          EILI9487
C-----------------------------------------------------------------------EILI9488
      RFN = 1.0D0/FNU                                                   EILI9489
C-----------------------------------------------------------------------EILI9490
C     OVERFLOW TEST (ZR/FNU TOO SMALL)                                  EILI9491
C-----------------------------------------------------------------------EILI9492
      TEST = D1MACH(1)*1.0D+3                                           EILI9493
      AC = FNU*TEST                                                     EILI9494
      IF (DABS(ZRR).GT.AC .OR. DABS(ZRI).GT.AC) GO TO 15                EILI9495
      ZETA1R = 2.0D0*DABS(DLOG(TEST))+FNU                               EILI9496
      ZETA1I = 0.0D0                                                    EILI9497
      ZETA2R = FNU                                                      EILI9498
      ZETA2I = 0.0D0                                                    EILI9499
      PHIR = 1.0D0                                                      EILI9500
      PHII = 0.0D0                                                      EILI9501
      RETURN                                                            EILI9502
   15 CONTINUE                                                          EILI9503
      TR = ZRR*RFN                                                      EILI9504
      TI = ZRI*RFN                                                      EILI9505
      SR = CONER + (TR*TR-TI*TI)                                        EILI9506
      SI = CONEI + (TR*TI+TI*TR)                                        EILI9507
      CALL RRZSQRT(SR, SI, SRR, SRI)                                    EILI9508
      STR = CONER + SRR                                                 EILI9509
      STI = CONEI + SRI                                                 EILI9510
      CALL ZDIV(STR, STI, TR, TI, ZNR, ZNI)                             EILI9511
      CALL ZLOG(ZNR, ZNI, STR, STI, IDUM)                               EILI9512
      ZETA1R = FNU*STR                                                  EILI9513
      ZETA1I = FNU*STI                                                  EILI9514
      ZETA2R = FNU*SRR                                                  EILI9515
      ZETA2I = FNU*SRI                                                  EILI9516
      CALL ZDIV(CONER, CONEI, SRR, SRI, TR, TI)                         EILI9517
      SRR = TR*RFN                                                      EILI9518
      SRI = TI*RFN                                                      EILI9519
      CALL RRZSQRT(SRR, SRI, CWRKR(16), CWRKI(16))                      EILI9520
      PHIR = CWRKR(16)*CON(IKFLG)                                       EILI9521
      PHII = CWRKI(16)*CON(IKFLG)                                       EILI9522
      IF (IPMTR.NE.0) RETURN                                            EILI9523
      CALL ZDIV(CONER, CONEI, SR, SI, T2R, T2I)                         EILI9524
      CWRKR(1) = CONER                                                  EILI9525
      CWRKI(1) = CONEI                                                  EILI9526
      CRFNR = CONER                                                     EILI9527
      CRFNI = CONEI                                                     EILI9528
      AC = 1.0D0                                                        EILI9529
      L = 1                                                             EILI9530
      DO 20 K=2,15                                                      EILI9531
        SR = ZEROR                                                      EILI9532
        SI = ZEROI                                                      EILI9533
        DO 10 J=1,K                                                     EILI9534
          L = L + 1                                                     EILI9535
          STR = SR*T2R - SI*T2I + C(L)                                  EILI9536
          SI = SR*T2I + SI*T2R                                          EILI9537
          SR = STR                                                      EILI9538
   10   CONTINUE                                                        EILI9539
        STR = CRFNR*SRR - CRFNI*SRI                                     EILI9540
        CRFNI = CRFNR*SRI + CRFNI*SRR                                   EILI9541
        CRFNR = STR                                                     EILI9542
        CWRKR(K) = CRFNR*SR - CRFNI*SI                                  EILI9543
        CWRKI(K) = CRFNR*SI + CRFNI*SR                                  EILI9544
        AC = AC*RFN                                                     EILI9545
        TEST = DABS(CWRKR(K)) + DABS(CWRKI(K))                          EILI9546
        IF (AC.LT.TOL .AND. TEST.LT.TOL) GO TO 30                       EILI9547
   20 CONTINUE                                                          EILI9548
      K = 15                                                            EILI9549
   30 CONTINUE                                                          EILI9550
      INIT = K                                                          EILI9551
   40 CONTINUE                                                          EILI9552
      IF (IKFLG.EQ.2) GO TO 60                                          EILI9553
C-----------------------------------------------------------------------EILI9554
C     COMPUTE SUM FOR THE I FUNCTION                                    EILI9555
C-----------------------------------------------------------------------EILI9556
      SR = ZEROR                                                        EILI9557
      SI = ZEROI                                                        EILI9558
      DO 50 I=1,INIT                                                    EILI9559
        SR = SR + CWRKR(I)                                              EILI9560
        SI = SI + CWRKI(I)                                              EILI9561
   50 CONTINUE                                                          EILI9562
      SUMR = SR                                                         EILI9563
      SUMI = SI                                                         EILI9564
      PHIR = CWRKR(16)*CON(1)                                           EILI9565
      PHII = CWRKI(16)*CON(1)                                           EILI9566
      RETURN                                                            EILI9567
   60 CONTINUE                                                          EILI9568
C-----------------------------------------------------------------------EILI9569
C     COMPUTE SUM FOR THE K FUNCTION                                    EILI9570
C-----------------------------------------------------------------------EILI9571
      SR = ZEROR                                                        EILI9572
      SI = ZEROI                                                        EILI9573
      TR = CONER                                                        EILI9574
      DO 70 I=1,INIT                                                    EILI9575
        SR = SR + TR*CWRKR(I)                                           EILI9576
        SI = SI + TR*CWRKI(I)                                           EILI9577
        TR = -TR                                                        EILI9578
   70 CONTINUE                                                          EILI9579
      SUMR = SR                                                         EILI9580
      SUMI = SI                                                         EILI9581
      PHIR = CWRKR(16)*CON(2)                                           EILI9582
      PHII = CWRKI(16)*CON(2)                                           EILI9583
      RETURN                                                            EILI9584
      END      