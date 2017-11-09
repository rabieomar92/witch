!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:00:45 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE CDIV(AR,AI,BR,BI,CR,CI)                                EILI 357
      DOUBLE PRECISION AR,AI,BR,BI,CR,CI                                EILI 358
C                                                                       EILI 359
C     COMPLEX DIVISION, (CR,CI) = (AR,AI)/(BR,BI)                       EILI 360
C                                                                       EILI 361
      DOUBLE PRECISION S,ARS,AIS,BRS,BIS                                EILI 362
      S = DABS(BR) + DABS(BI)                                           EILI 363
      ARS = AR/S                                                        EILI 364
      AIS = AI/S                                                        EILI 365
      BRS = BR/S                                                        EILI 366
      BIS = BI/S                                                        EILI 367
      S = BRS**2 + BIS**2                                               EILI 368
      CR = (ARS*BRS + AIS*BIS)/S                                        EILI 369
      CI = (AIS*BRS - ARS*BIS)/S                                        EILI 370
      RETURN                                                            EILI 371
      END      