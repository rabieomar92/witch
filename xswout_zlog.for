!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:10:13 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZLOG(AR, AI, BR, BI, IERR)                             EILI5629
C***BEGIN PROLOGUE  ZLOG                                                EILI5630
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY                 EILI5631
C                                                                       EILI5632
C     DOUBLE PRECISION COMPLEX LOGARITHM B=CLOG(A)                      EILI5633
C     IERR=0,NORMAL RETURN      IERR=1, Z=CMPLX(0.0,0.0)                EILI5634
C***ROUTINES CALLED  ZABS                                               EILI5635
C***END PROLOGUE  ZLOG                                                  EILI5636
      DOUBLE PRECISION AR, AI, BR, BI, ZM, DTHETA, DPI, DHPI            EILI5637
      DOUBLE PRECISION ZABS                                             EILI5638
      EXTERNAL ZABS                                                     EILI5639
      DATA DPI , DHPI  / 3.141592653589793238462643383D+0,              EILI5640
     1                   1.570796326794896619231321696D+0/              EILI5641
C                                                                       EILI5642
      IERR=0                                                            EILI5643
      IF (AR.EQ.0.0D+0) GO TO 10                                        EILI5644
      IF (AI.EQ.0.0D+0) GO TO 20                                        EILI5645
      DTHETA = DATAN(AI/AR)                                             EILI5646
      IF (DTHETA.LE.0.0D+0) GO TO 40                                    EILI5647
      IF (AR.LT.0.0D+0) DTHETA = DTHETA - DPI                           EILI5648
      GO TO 50                                                          EILI5649
   10 IF (AI.EQ.0.0D+0) GO TO 60                                        EILI5650
      BI = DHPI                                                         EILI5651
      BR = DLOG(DABS(AI))                                               EILI5652
      IF (AI.LT.0.0D+0) BI = -BI                                        EILI5653
      RETURN                                                            EILI5654
   20 IF (AR.GT.0.0D+0) GO TO 30                                        EILI5655
      BR = DLOG(DABS(AR))                                               EILI5656
      BI = DPI                                                          EILI5657
      RETURN                                                            EILI5658
   30 BR = DLOG(AR)                                                     EILI5659
      BI = 0.0D+0                                                       EILI5660
      RETURN                                                            EILI5661
   40 IF (AR.LT.0.0D+0) DTHETA = DTHETA + DPI                           EILI5662
   50 ZM = ZABS(AR,AI)                                                  EILI5663
      BR = DLOG(ZM)                                                     EILI5664
      BI = DTHETA                                                       EILI5665
      RETURN                                                            EILI5666
   60 CONTINUE                                                          EILI5667
      IERR=1                                                            EILI5668
      RETURN                                                            EILI5669
      END      