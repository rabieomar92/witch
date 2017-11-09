!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:11:59 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZUCHK(YR, YI, NZ, ASCLE, TOL)                          EILI8268
C***BEGIN PROLOGUE  ZUCHK                                               EILI8269
C***REFER TO ZSERI,ZUOIK,ZUNK1,ZUNK2,ZUNI1,ZUNI2,ZKSCL                  EILI8270
C                                                                       EILI8271
C      Y ENTERS AS A SCALED QUANTITY WHOSE MAGNITUDE IS GREATER THAN    EILI8272
C      EXP(-ALIM)=ASCLE=1.0E+3*D1MACH(1)/TOL. THE TEST IS MADE TO SEE   EILI8273
C      IF THE MAGNITUDE OF THE REAL OR IMAGINARY PART WOULD UNDERFLOW   EILI8274
C      WHEN Y IS SCALED (BY TOL) TO ITS PROPER VALUE. Y IS ACCEPTED     EILI8275
C      IF THE UNDERFLOW IS AT LEAST ONE PRECISION BELOW THE MAGNITUDE   EILI8276
C      OF THE LARGEST COMPONENT; OTHERWISE THE PHASE ANGLE DOES NOT HAVEEILI8277
C      ABSOLUTE ACCURACY AND AN UNDERFLOW IS ASSUMED.                   EILI8278
C                                                                       EILI8279
C***ROUTINES CALLED  (NONE)                                             EILI8280
C***END PROLOGUE  ZUCHK                                                 EILI8281
C                                                                       EILI8282
C     COMPLEX Y                                                         EILI8283
      DOUBLE PRECISION ASCLE, SS, ST, TOL, WR, WI, YR, YI               EILI8284
      INTEGER NZ                                                        EILI8285
      NZ = 0                                                            EILI8286
      WR = DABS(YR)                                                     EILI8287
      WI = DABS(YI)                                                     EILI8288
      ST = DMIN1(WR,WI)                                                 EILI8289
      IF (ST.GT.ASCLE) RETURN                                           EILI8290
      SS = DMAX1(WR,WI)                                                 EILI8291
      ST = ST/TOL                                                       EILI8292
      IF (SS.LT.ST) NZ = 1                                              EILI8293
      RETURN                                                            EILI8294
      END      