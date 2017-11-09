!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:49:46 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ELMHES(NM,N,LOW,IGH,A,INT)                             EILI 373
C                                                                       EILI 374
      INTEGER I,J,M,N,LA,NM,IGH,KP1,LOW,MM1,MP1                         EILI 375
      DOUBLE PRECISION A(NM,N)                                          EILI 376
      DOUBLE PRECISION X,Y                                              EILI 377
      INTEGER INT(IGH)                                                  EILI 378
C                                                                       EILI 379
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ELMHES,   EILI 380
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             EILI 381
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   EILI 382
C                                                                       EILI 383
C     GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE                      EILI 384
C     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS                  EILI 385
C     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY                       EILI 386
C     STABILIZED ELEMENTARY SIMILARITY TRANSFORMATIONS.                 EILI 387
C                                                                       EILI 388
C     ON INPUT                                                          EILI 389
C                                                                       EILI 390
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         EILI 391
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          EILI 392
C          DIMENSION STATEMENT.                                         EILI 393
C                                                                       EILI 394
C        N IS THE ORDER OF THE MATRIX.                                  EILI 395
C                                                                       EILI 396
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING           EILI 397
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,          EILI 398
C          SET LOW=1, IGH=N.                                            EILI 399
C                                                                       EILI 400
C        A CONTAINS THE INPUT MATRIX.                                   EILI 401
C                                                                       EILI 402
C     ON OUTPUT                                                         EILI 403
C                                                                       EILI 404
C        A CONTAINS THE HESSENBERG MATRIX.  THE MULTIPLIERS             EILI 405
C          WHICH WERE USED IN THE REDUCTION ARE STORED IN THE           EILI 406
C          REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX.              EILI 407
C                                                                       EILI 408
C        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS               EILI 409
C          INTERCHANGED IN THE REDUCTION.                               EILI 410
C          ONLY ELEMENTS LOW THROUGH IGH ARE USED.                      EILI 411
C                                                                       EILI 412
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,    EILI 413
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY EILI 414
C                                                                       EILI 415
C     THIS VERSION DATED AUGUST 1983.                                   EILI 416
C                                                                       EILI 417
C     ------------------------------------------------------------------EILI 418
C                                                                       EILI 419
      LA = IGH - 1                                                      EILI 420
      KP1 = LOW + 1                                                     EILI 421
      IF (LA .LT. KP1) GO TO 200                                        EILI 422
C                                                                       EILI 423
      DO 180 M = KP1, LA                                                EILI 424
         MM1 = M - 1                                                    EILI 425
         X = 0.0D0                                                      EILI 426
         I = M                                                          EILI 427
C                                                                       EILI 428
         DO 100 J = M, IGH                                              EILI 429
            IF (DABS(A(J,MM1)) .LE. DABS(X)) GO TO 100                  EILI 430
            X = A(J,MM1)                                                EILI 431
            I = J                                                       EILI 432
  100    CONTINUE                                                       EILI 433
C                                                                       EILI 434
         INT(M) = I                                                     EILI 435
         IF (I .EQ. M) GO TO 130                                        EILI 436
C     .......... INTERCHANGE ROWS AND COLUMNS OF A ..........           EILI 437
         DO 110 J = MM1, N                                              EILI 438
            Y = A(I,J)                                                  EILI 439
            A(I,J) = A(M,J)                                             EILI 440
            A(M,J) = Y                                                  EILI 441
  110    CONTINUE                                                       EILI 442
C                                                                       EILI 443
         DO 120 J = 1, IGH                                              EILI 444
            Y = A(J,I)                                                  EILI 445
            A(J,I) = A(J,M)                                             EILI 446
            A(J,M) = Y                                                  EILI 447
  120    CONTINUE                                                       EILI 448
C     .......... END INTERCHANGE ..........                             EILI 449
  130    IF (X .EQ. 0.0D0) GO TO 180                                    EILI 450
         MP1 = M + 1                                                    EILI 451
C                                                                       EILI 452
         DO 160 I = MP1, IGH                                            EILI 453
            Y = A(I,MM1)                                                EILI 454
            IF (Y .EQ. 0.0D0) GO TO 160                                 EILI 455
            Y = Y / X                                                   EILI 456
            A(I,MM1) = Y                                                EILI 457
C                                                                       EILI 458
            DO 140 J = M, N                                             EILI 459
  140       A(I,J) = A(I,J) - Y * A(M,J)                                EILI 460
C                                                                       EILI 461
            DO 150 J = 1, IGH                                           EILI 462
  150       A(J,M) = A(J,M) + Y * A(J,I)                                EILI 463
C                                                                       EILI 464
  160    CONTINUE                                                       EILI 465
C                                                                       EILI 466
  180 CONTINUE                                                          EILI 467
C                                                                       EILI 468
  200 RETURN                                                            EILI 469
      END                        