!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:11:23 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZDIV(AR, AI, BR, BI, CR, CI)                           EILI5468
C***BEGIN PROLOGUE  ZDIV                                                EILI5469
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY                 EILI5470
C                                                                       EILI5471
C     DOUBLE PRECISION COMPLEX DIVIDE C=A/B.                            EILI5472
C                                                                       EILI5473
C***ROUTINES CALLED  ZABS                                               EILI5474
C***END PROLOGUE  ZDIV                                                  EILI5475
      DOUBLE PRECISION AR, AI, BR, BI, CR, CI, BM, CA, CB, CC, CD       EILI5476
      DOUBLE PRECISION ZABS                                             EILI5477
      EXTERNAL ZABS                                                     EILI5478
      BM = 1.0D0/ZABS(BR,BI)                                            EILI5479
      CC = BR*BM                                                        EILI5480
      CD = BI*BM                                                        EILI5481
      CA = (AR*CC+AI*CD)*BM                                             EILI5482
      CB = (AI*CC-AR*CD)*BM                                             EILI5483
      CR = CA                                                           EILI5484
      CI = CB                                                           EILI5485
      RETURN                                                            EILI5486
      END      