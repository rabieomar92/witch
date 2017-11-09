!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:47:35 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      DOUBLE PRECISION FUNCTION ZABS(ZR, ZI)                            EILI3018
C***BEGIN PROLOGUE  ZABS                                                EILI3019
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY                 EILI3020
C                                                                       EILI3021
C     ZABS COMPUTES THE ABSOLUTE VALUE OR MAGNITUDE OF A DOUBLE         EILI3022
C     PRECISION COMPLEX VARIABLE CMPLX(ZR,ZI)                           EILI3023
C                                                                       EILI3024
C***ROUTINES CALLED  (NONE)                                             EILI3025
C***END PROLOGUE  ZABS                                                  EILI3026
      DOUBLE PRECISION ZR, ZI, U, V, Q, S                               EILI3027
      U = DABS(ZR)                                                      EILI3028
      V = DABS(ZI)                                                      EILI3029
      S = U + V                                                         EILI3030
C-----------------------------------------------------------------------EILI3031
C     S*1.0D0 MAKES AN UNNORMALIZED UNDERFLOW ON CDC MACHINES INTO A    EILI3032
C     TRUE FLOATING ZERO                                                EILI3033
C-----------------------------------------------------------------------EILI3034
      S = S*1.0D+0                                                      EILI3035
      IF (S.EQ.0.0D+0) GO TO 20                                         EILI3036
      IF (U.GT.V) GO TO 10                                              EILI3037
      Q = U/V                                                           EILI3038
      ZABS = V*DSQRT(1.D+0+Q*Q)                                         EILI3039
      RETURN                                                            EILI3040
   10 Q = V/U                                                           EILI3041
      ZABS = U*DSQRT(1.D+0+Q*Q)                                         EILI3042
      RETURN                                                            EILI3043
   20 ZABS = 0.0D+0                                                     EILI3044
      RETURN                                                            EILI3045
      END                