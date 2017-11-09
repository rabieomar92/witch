!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:01:42 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE RRZEXP(AR, AI, BR, BI)                                 EILI5613
C***BEGIN PROLOGUE  ZEXP                                                EILI5614
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY                 EILI5615
C                                                                       EILI5616
C     DOUBLE PRECISION COMPLEX EXPONENTIAL FUNCTION B=EXP(A)            EILI5617
C                                                                       EILI5618
C***ROUTINES CALLED  (NONE)                                             EILI5619
C***END PROLOGUE  ZEXP                                                  EILI5620
      DOUBLE PRECISION AR, AI, BR, BI, ZM, CA, CB                       EILI5621
      ZM = DEXP(AR)                                                     EILI5622
      CA = ZM*DCOS(AI)                                                  EILI5623
      CB = ZM*DSIN(AI)                                                  EILI5624
      BR = CA                                                           EILI5625
      BI = CB                                                           EILI5626
      RETURN                                                            EILI5627
      END      