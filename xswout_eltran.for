!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:50:55 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ELTRAN(NM,N,LOW,IGH,A,INT,Z)                           EILI 471
C                                                                       EILI 472
      INTEGER I,J,N,KL,MM,MP,NM,IGH,LOW,MP1                             EILI 473
      DOUBLE PRECISION A(NM,IGH),Z(NM,N)                                EILI 474
      INTEGER INT(IGH)                                                  EILI 475
C                                                                       EILI 476
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ELMTRANS, EILI 477
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.             EILI 478
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   EILI 479
C                                                                       EILI 480
C     THIS SUBROUTINE ACCUMULATES THE STABILIZED ELEMENTARY             EILI 481
C     SIMILARITY TRANSFORMATIONS USED IN THE REDUCTION OF A             EILI 482
C     REAL GENERAL MATRIX TO UPPER HESSENBERG FORM BY  ELMHES.          EILI 483
C                                                                       EILI 484
C     ON INPUT                                                          EILI 485
C                                                                       EILI 486
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         EILI 487
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          EILI 488
C          DIMENSION STATEMENT.                                         EILI 489
C                                                                       EILI 490
C        N IS THE ORDER OF THE MATRIX.                                  EILI 491
C                                                                       EILI 492
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING           EILI 493
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,          EILI 494
C          SET LOW=1, IGH=N.                                            EILI 495
C                                                                       EILI 496
C        A CONTAINS THE MULTIPLIERS WHICH WERE USED IN THE              EILI 497
C          REDUCTION BY  ELMHES  IN ITS LOWER TRIANGLE                  EILI 498
C          BELOW THE SUBDIAGONAL.                                       EILI 499
C                                                                       EILI 500
C        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS               EILI 501
C          INTERCHANGED IN THE REDUCTION BY  ELMHES.                    EILI 502
C          ONLY ELEMENTS LOW THROUGH IGH ARE USED.                      EILI 503
C                                                                       EILI 504
C     ON OUTPUT                                                         EILI 505
C                                                                       EILI 506
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE           EILI 507
C          REDUCTION BY  ELMHES.                                        EILI 508
C                                                                       EILI 509
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,    EILI 510
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY EILI 511
C                                                                       EILI 512
C     THIS VERSION DATED AUGUST 1983.                                   EILI 513
C                                                                       EILI 514
C     ------------------------------------------------------------------EILI 515
C                                                                       EILI 516
C     .......... INITIALIZE Z TO IDENTITY MATRIX ..........             EILI 517
      DO 80 J = 1, N                                                    EILI 518
C                                                                       EILI 519
         DO 60 I = 1, N                                                 EILI 520
   60    Z(I,J) = 0.0D0                                                 EILI 521
C                                                                       EILI 522
         Z(J,J) = 1.0D0                                                 EILI 523
   80 CONTINUE                                                          EILI 524
C                                                                       EILI 525
      KL = IGH - LOW - 1                                                EILI 526
      IF (KL .LT. 1) GO TO 200                                          EILI 527
C     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- ..........      EILI 528
      DO 140 MM = 1, KL                                                 EILI 529
         MP = IGH - MM                                                  EILI 530
         MP1 = MP + 1                                                   EILI 531
C                                                                       EILI 532
         DO 100 I = MP1, IGH                                            EILI 533
  100    Z(I,MP) = A(I,MP-1)                                            EILI 534
C                                                                       EILI 535
         I = INT(MP)                                                    EILI 536
         IF (I .EQ. MP) GO TO 140                                       EILI 537
C                                                                       EILI 538
         DO 130 J = MP, IGH                                             EILI 539
            Z(MP,J) = Z(I,J)                                            EILI 540
            Z(I,J) = 0.0D0                                              EILI 541
  130    CONTINUE                                                       EILI 542
C                                                                       EILI 543
         Z(I,MP) = 1.0D0                                                EILI 544
  140 CONTINUE                                                          EILI 545
C                                                                       EILI 546
  200 RETURN                                                            EILI 547
      END      