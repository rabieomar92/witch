!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:55:12 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      DOUBLE COMPLEX FUNCTION ZDOTC(N,ZX,INCX,ZY,INCY)                  EILI1498
C                                                                       EILI1499
C     FORMS THE DOT PRODUCT OF A VECTOR.                                EILI1500
C     JACK DONGARRA, 3/11/78.                                           EILI1501
C                                                                       EILI1502
      DOUBLE COMPLEX ZX(1),ZY(1),ZTEMP                                  EILI1503
      ZTEMP = (0.0D0,0.0D0)                                             EILI1504
      ZDOTC = (0.0D0,0.0D0)                                             EILI1505
      IF(N.LE.0)RETURN                                                  EILI1506
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               EILI1507
C                                                                       EILI1508
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                EILI1509
C          NOT EQUAL TO 1                                               EILI1510
C                                                                       EILI1511
      IX = 1                                                            EILI1512
      IY = 1                                                            EILI1513
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 EILI1514
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 EILI1515
      DO 10 I = 1,N                                                     EILI1516
        ZTEMP = ZTEMP + DCONJG(ZX(IX))*ZY(IY)                           EILI1517
        IX = IX + INCX                                                  EILI1518
        IY = IY + INCY                                                  EILI1519
   10 CONTINUE                                                          EILI1520
      ZDOTC = ZTEMP                                                     EILI1521
      RETURN                                                            EILI1522
C                                                                       EILI1523
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            EILI1524
C                                                                       EILI1525
   20 DO 30 I = 1,N                                                     EILI1526
        ZTEMP = ZTEMP + DCONJG(ZX(I))*ZY(I)                             EILI1527
   30 CONTINUE                                                          EILI1528
      ZDOTC = ZTEMP                                                     EILI1529
      RETURN                                                            EILI1530
      END      