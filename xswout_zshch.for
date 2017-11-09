!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:19:10 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZSHCH(ZR, ZI, CSHR, CSHI, CCHR, CCHI)                  EILI6376
C***BEGIN PROLOGUE  ZSHCH                                               EILI6377
C***REFER TO  ZBESK,ZBESH                                               EILI6378
C                                                                       EILI6379
C     ZSHCH COMPUTES THE COMPLEX HYPERBOLIC FUNCTIONS CSH=SINH(X+I*Y)   EILI6380
C     AND CCH=COSH(X+I*Y), WHERE I**2=-1.                               EILI6381
C                                                                       EILI6382
C***ROUTINES CALLED  (NONE)                                             EILI6383
C***END PROLOGUE  ZSHCH                                                 EILI6384
C                                                                       EILI6385
      DOUBLE PRECISION CCHI, CCHR, CH, CN, CSHI, CSHR, SH, SN, ZI, ZR,  EILI6386
     * DCOSH, DSINH                                                     EILI6387
      SH = DSINH(ZR)                                                    EILI6388
      CH = DCOSH(ZR)                                                    EILI6389
      SN = DSIN(ZI)                                                     EILI6390
      CN = DCOS(ZI)                                                     EILI6391
      CSHR = SH*CN                                                      EILI6392
      CSHI = CH*SN                                                      EILI6393
      CCHR = CH*CN                                                      EILI6394
      CCHI = SH*SN                                                      EILI6395
      RETURN                                                            EILI6396
      END       