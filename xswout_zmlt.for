!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:09:36 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZMLT(AR, AI, BR, BI, CR, CI)                           EILI5453
C***BEGIN PROLOGUE  ZMLT                                                EILI5454
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY                 EILI5455
C                                                                       EILI5456
C     DOUBLE PRECISION COMPLEX MULTIPLY, C=A*B.                         EILI5457
C                                                                       EILI5458
C***ROUTINES CALLED  (NONE)                                             EILI5459
C***END PROLOGUE  ZMLT                                                  EILI5460
      DOUBLE PRECISION AR, AI, BR, BI, CR, CI, CA, CB                   EILI5461
      CA = AR*BR - AI*BI                                                EILI5462
      CB = AR*BI + AI*BR                                                EILI5463
      CR = CA                                                           EILI5464
      CI = CB                                                           EILI5465
      RETURN                                                            EILI5466
      END        