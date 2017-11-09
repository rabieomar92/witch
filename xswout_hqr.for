!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:50:27 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE HQR(NM,N,LOW,IGH,H,WR,WI,IERR)                         EILI 549
C  RESTORED CORRECT INDICES OF LOOPS (200,210,230,240). (9/29/89 BSG)   EILI 550
C                                                                       EILI 551
      INTEGER I,J,K,L,M,N,EN,LL,MM,NA,NM,IGH,ITN,ITS,LOW,MP2,ENM2,IERR  EILI 552
      DOUBLE PRECISION H(NM,N),WR(N),WI(N)                              EILI 553
      DOUBLE PRECISION P,Q,R,S,T,W,X,Y,ZZ,NORM,TST1,TST2                EILI 554
      LOGICAL NOTLAS                                                    EILI 555
C                                                                       EILI 556
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR,      EILI 557
C     NUM. MATH. 14, 219-231(1970) BY MARTIN, PETERS, AND WILKINSON.    EILI 558
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 359-371(1971).   EILI 559
C                                                                       EILI 560
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A REAL                   EILI 561
C     UPPER HESSENBERG MATRIX BY THE QR METHOD.                         EILI 562
C                                                                       EILI 563
C     ON INPUT                                                          EILI 564
C                                                                       EILI 565
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         EILI 566
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          EILI 567
C          DIMENSION STATEMENT.                                         EILI 568
C                                                                       EILI 569
C        N IS THE ORDER OF THE MATRIX.                                  EILI 570
C                                                                       EILI 571
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING           EILI 572
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,          EILI 573
C          SET LOW=1, IGH=N.                                            EILI 574
C                                                                       EILI 575
C        H CONTAINS THE UPPER HESSENBERG MATRIX.  INFORMATION ABOUT     EILI 576
C          THE TRANSFORMATIONS USED IN THE REDUCTION TO HESSENBERG      EILI 577
C          FORM BY  ELMHES  OR  ORTHES, IF PERFORMED, IS STORED         EILI 578
C          IN THE REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX.       EILI 579
C                                                                       EILI 580
C     ON OUTPUT                                                         EILI 581
C                                                                       EILI 582
C        H HAS BEEN DESTROYED.  THEREFORE, IT MUST BE SAVED             EILI 583
C          BEFORE CALLING  HQR  IF SUBSEQUENT CALCULATION AND           EILI 584
C          BACK TRANSFORMATION OF EIGENVECTORS IS TO BE PERFORMED.      EILI 585
C                                                                       EILI 586
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,                EILI 587
C          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES           EILI 588
C          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS            EILI 589
C          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE           EILI 590
C          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN             EILI 591
C          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT        EILI 592
C          FOR INDICES IERR+1,...,N.                                    EILI 593
C                                                                       EILI 594
C        IERR IS SET TO                                                 EILI 595
C          ZERO       FOR NORMAL RETURN,                                EILI 596
C          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED      EILI 597
C                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.        EILI 598
C                                                                       EILI 599
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,    EILI 600
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY EILI 601
C                                                                       EILI 602
C     THIS VERSION DATED SEPTEMBER 1989.                                EILI 603
C                                                                       EILI 604
C     ------------------------------------------------------------------EILI 605
C                                                                       EILI 606
      IERR = 0                                                          EILI 607
      NORM = 0.0D0                                                      EILI 608
      K = 1                                                             EILI 609
C     .......... STORE ROOTS ISOLATED BY BALANC                         EILI 610
C                AND COMPUTE MATRIX NORM ..........                     EILI 611
      DO 50 I = 1, N                                                    EILI 612
C                                                                       EILI 613
         DO 40 J = K, N                                                 EILI 614
   40    NORM = NORM + DABS(H(I,J))                                     EILI 615
C                                                                       EILI 616
         K = I                                                          EILI 617
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 50                      EILI 618
         WR(I) = H(I,I)                                                 EILI 619
         WI(I) = 0.0D0                                                  EILI 620
   50 CONTINUE                                                          EILI 621
C                                                                       EILI 622
      EN = IGH                                                          EILI 623
      T = 0.0D0                                                         EILI 624
      ITN = 30*N                                                        EILI 625
C     .......... SEARCH FOR NEXT EIGENVALUES ..........                 EILI 626
   60 IF (EN .LT. LOW) GO TO 1001                                       EILI 627
      ITS = 0                                                           EILI 628
      NA = EN - 1                                                       EILI 629
      ENM2 = NA - 1                                                     EILI 630
C     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT             EILI 631
C                FOR L=EN STEP -1 UNTIL LOW DO -- ..........            EILI 632
   70 DO 80 LL = LOW, EN                                                EILI 633
         L = EN + LOW - LL                                              EILI 634
         IF (L .EQ. LOW) GO TO 100                                      EILI 635
         S = DABS(H(L-1,L-1)) + DABS(H(L,L))                            EILI 636
         IF (S .EQ. 0.0D0) S = NORM                                     EILI 637
         TST1 = S                                                       EILI 638
         TST2 = TST1 + DABS(H(L,L-1))                                   EILI 639
         IF (TST2 .EQ. TST1) GO TO 100                                  EILI 640
   80 CONTINUE                                                          EILI 641
C     .......... FORM SHIFT ..........                                  EILI 642
  100 X = H(EN,EN)                                                      EILI 643
      IF (L .EQ. EN) GO TO 270                                          EILI 644
      Y = H(NA,NA)                                                      EILI 645
      W = H(EN,NA) * H(NA,EN)                                           EILI 646
      IF (L .EQ. NA) GO TO 280                                          EILI 647
      IF (ITN .EQ. 0) GO TO 1000                                        EILI 648
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130                      EILI 649
C     .......... FORM EXCEPTIONAL SHIFT ..........                      EILI 650
      T = T + X                                                         EILI 651
C                                                                       EILI 652
      DO 120 I = LOW, EN                                                EILI 653
  120 H(I,I) = H(I,I) - X                                               EILI 654
C                                                                       EILI 655
      S = DABS(H(EN,NA)) + DABS(H(NA,ENM2))                             EILI 656
      X = 0.75D0 * S                                                    EILI 657
      Y = X                                                             EILI 658
      W = -0.4375D0 * S * S                                             EILI 659
  130 ITS = ITS + 1                                                     EILI 660
      ITN = ITN - 1                                                     EILI 661
C     .......... LOOK FOR TWO CONSECUTIVE SMALL                         EILI 662
C                SUB-DIAGONAL ELEMENTS.                                 EILI 663
C                FOR M=EN-2 STEP -1 UNTIL L DO -- ..........            EILI 664
      DO 140 MM = L, ENM2                                               EILI 665
         M = ENM2 + L - MM                                              EILI 666
         ZZ = H(M,M)                                                    EILI 667
         R = X - ZZ                                                     EILI 668
         S = Y - ZZ                                                     EILI 669
         P = (R * S - W) / H(M+1,M) + H(M,M+1)                          EILI 670
         Q = H(M+1,M+1) - ZZ - R - S                                    EILI 671
         R = H(M+2,M+1)                                                 EILI 672
         S = DABS(P) + DABS(Q) + DABS(R)                                EILI 673
         P = P / S                                                      EILI 674
         Q = Q / S                                                      EILI 675
         R = R / S                                                      EILI 676
         IF (M .EQ. L) GO TO 150                                        EILI 677
         TST1 = DABS(P)*(DABS(H(M-1,M-1)) + DABS(ZZ) + DABS(H(M+1,M+1)))EILI 678
         TST2 = TST1 + DABS(H(M,M-1))*(DABS(Q) + DABS(R))               EILI 679
         IF (TST2 .EQ. TST1) GO TO 150                                  EILI 680
  140 CONTINUE                                                          EILI 681
C                                                                       EILI 682
  150 MP2 = M + 2                                                       EILI 683
C                                                                       EILI 684
      DO 160 I = MP2, EN                                                EILI 685
         H(I,I-2) = 0.0D0                                               EILI 686
         IF (I .EQ. MP2) GO TO 160                                      EILI 687
         H(I,I-3) = 0.0D0                                               EILI 688
  160 CONTINUE                                                          EILI 689
C     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND              EILI 690
C                COLUMNS M TO EN ..........                             EILI 691
      DO 260 K = M, NA                                                  EILI 692
         NOTLAS = K .NE. NA                                             EILI 693
         IF (K .EQ. M) GO TO 170                                        EILI 694
         P = H(K,K-1)                                                   EILI 695
         Q = H(K+1,K-1)                                                 EILI 696
         R = 0.0D0                                                      EILI 697
         IF (NOTLAS) R = H(K+2,K-1)                                     EILI 698
         X = DABS(P) + DABS(Q) + DABS(R)                                EILI 699
         IF (X .EQ. 0.0D0) GO TO 260                                    EILI 700
         P = P / X                                                      EILI 701
         Q = Q / X                                                      EILI 702
         R = R / X                                                      EILI 703
  170    S = DSIGN(DSQRT(P*P+Q*Q+R*R),P)                                EILI 704
         IF (K .EQ. M) GO TO 180                                        EILI 705
         H(K,K-1) = -S * X                                              EILI 706
         GO TO 190                                                      EILI 707
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)                             EILI 708
  190    P = P + S                                                      EILI 709
         X = P / S                                                      EILI 710
         Y = Q / S                                                      EILI 711
         ZZ = R / S                                                     EILI 712
         Q = Q / P                                                      EILI 713
         R = R / P                                                      EILI 714
         IF (NOTLAS) GO TO 225                                          EILI 715
C     .......... ROW MODIFICATION ..........                            EILI 716
         DO 200 J = K, EN                                               EILI 717
            P = H(K,J) + Q * H(K+1,J)                                   EILI 718
            H(K,J) = H(K,J) - P * X                                     EILI 719
            H(K+1,J) = H(K+1,J) - P * Y                                 EILI 720
  200    CONTINUE                                                       EILI 721
C                                                                       EILI 722
         J = MIN0(EN,K+3)                                               EILI 723
C     .......... COLUMN MODIFICATION ..........                         EILI 724
         DO 210 I = L, J                                                EILI 725
            P = X * H(I,K) + Y * H(I,K+1)                               EILI 726
            H(I,K) = H(I,K) - P                                         EILI 727
            H(I,K+1) = H(I,K+1) - P * Q                                 EILI 728
  210    CONTINUE                                                       EILI 729
         GO TO 255                                                      EILI 730
  225    CONTINUE                                                       EILI 731
C     .......... ROW MODIFICATION ..........                            EILI 732
         DO 230 J = K, EN                                               EILI 733
            P = H(K,J) + Q * H(K+1,J) + R * H(K+2,J)                    EILI 734
            H(K,J) = H(K,J) - P * X                                     EILI 735
            H(K+1,J) = H(K+1,J) - P * Y                                 EILI 736
            H(K+2,J) = H(K+2,J) - P * ZZ                                EILI 737
  230    CONTINUE                                                       EILI 738
C                                                                       EILI 739
         J = MIN0(EN,K+3)                                               EILI 740
C     .......... COLUMN MODIFICATION ..........                         EILI 741
         DO 240 I = L, J                                                EILI 742
            P = X * H(I,K) + Y * H(I,K+1) + ZZ * H(I,K+2)               EILI 743
            H(I,K) = H(I,K) - P                                         EILI 744
            H(I,K+1) = H(I,K+1) - P * Q                                 EILI 745
            H(I,K+2) = H(I,K+2) - P * R                                 EILI 746
  240    CONTINUE                                                       EILI 747
  255    CONTINUE                                                       EILI 748
C                                                                       EILI 749
  260 CONTINUE                                                          EILI 750
C                                                                       EILI 751
      GO TO 70                                                          EILI 752
C     .......... ONE ROOT FOUND ..........                              EILI 753
  270 WR(EN) = X + T                                                    EILI 754
      WI(EN) = 0.0D0                                                    EILI 755
      EN = NA                                                           EILI 756
      GO TO 60                                                          EILI 757
C     .......... TWO ROOTS FOUND ..........                             EILI 758
  280 P = (Y - X) / 2.0D0                                               EILI 759
      Q = P * P + W                                                     EILI 760
      ZZ = DSQRT(DABS(Q))                                               EILI 761
      X = X + T                                                         EILI 762
      IF (Q .LT. 0.0D0) GO TO 320                                       EILI 763
C     .......... REAL PAIR ..........                                   EILI 764
      ZZ = P + DSIGN(ZZ,P)                                              EILI 765
      WR(NA) = X + ZZ                                                   EILI 766
      WR(EN) = WR(NA)                                                   EILI 767
      IF (ZZ .NE. 0.0D0) WR(EN) = X - W / ZZ                            EILI 768
      WI(NA) = 0.0D0                                                    EILI 769
      WI(EN) = 0.0D0                                                    EILI 770
      GO TO 330                                                         EILI 771
C     .......... COMPLEX PAIR ..........                                EILI 772
  320 WR(NA) = X + P                                                    EILI 773
      WR(EN) = X + P                                                    EILI 774
      WI(NA) = ZZ                                                       EILI 775
      WI(EN) = -ZZ                                                      EILI 776
  330 EN = ENM2                                                         EILI 777
      GO TO 60                                                          EILI 778
C     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT                  EILI 779
C                CONVERGED AFTER 30*N ITERATIONS ..........             EILI 780
 1000 IERR = EN                                                         EILI 781
 1001 RETURN                                                            EILI 782
      END      