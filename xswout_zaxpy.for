!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:54:32 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZAXPY(N,ZA,ZX,INCX,ZY,INCY)                            EILI1466
C                                                                       EILI1467
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.                            EILI1468
C     JACK DONGARRA, 3/11/78.                                           EILI1469
C                                                                       EILI1470
      DOUBLE COMPLEX ZX(1),ZY(1),ZA                                     EILI1471
      DOUBLE PRECISION DCABS1                                           EILI1472
      IF(N.LE.0)RETURN                                                  EILI1473
      IF (DCABS1(ZA) .EQ. 0.0D0) RETURN                                 EILI1474
      IF (INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                              EILI1475
C                                                                       EILI1476
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                EILI1477
C          NOT EQUAL TO 1                                               EILI1478
C                                                                       EILI1479
      IX = 1                                                            EILI1480
      IY = 1                                                            EILI1481
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 EILI1482
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 EILI1483
      DO 10 I = 1,N                                                     EILI1484
        ZY(IY) = ZY(IY) + ZA*ZX(IX)                                     EILI1485
        IX = IX + INCX                                                  EILI1486
        IY = IY + INCY                                                  EILI1487
   10 CONTINUE                                                          EILI1488
      RETURN                                                            EILI1489
C                                                                       EILI1490
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            EILI1491
C                                                                       EILI1492
   20 DO 30 I = 1,N                                                     EILI1493
        ZY(I) = ZY(I) + ZA*ZX(I)                                        EILI1494
   30 CONTINUE                                                          EILI1495
      RETURN                                                            EILI1496
      END      