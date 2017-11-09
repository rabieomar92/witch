!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:53:18 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      INTEGER FUNCTION IZAMAX(N,ZX,INCX)                                EILI1561
C                                                                       EILI1562
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.            EILI1563
C     JACK DONGARRA, 1/15/85.                                           EILI1564
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.                           EILI1565
C                                                                       EILI1566
      DOUBLE COMPLEX ZX(1)                                              EILI1567
      DOUBLE PRECISION SMAX                                             EILI1568
      INTEGER I,INCX,IX,N                                               EILI1569
      DOUBLE PRECISION DCABS1                                           EILI1570
C                                                                       EILI1571
      IZAMAX = 0                                                        EILI1572
      IF( N.LT.1 .OR. INCX.LE.0 )RETURN                                 EILI1573
      IZAMAX = 1                                                        EILI1574
      IF(N.EQ.1)RETURN                                                  EILI1575
      IF(INCX.EQ.1)GO TO 20                                             EILI1576
C                                                                       EILI1577
C        CODE FOR INCREMENT NOT EQUAL TO 1                              EILI1578
C                                                                       EILI1579
      IX = 1                                                            EILI1580
      SMAX = DCABS1(ZX(1))                                              EILI1581
      IX = IX + INCX                                                    EILI1582
      DO 10 I = 2,N                                                     EILI1583
         IF(DCABS1(ZX(IX)).LE.SMAX) GO TO 5                             EILI1584
         IZAMAX = I                                                     EILI1585
         SMAX = DCABS1(ZX(IX))                                          EILI1586
    5    IX = IX + INCX                                                 EILI1587
   10 CONTINUE                                                          EILI1588
      RETURN                                                            EILI1589
C                                                                       EILI1590
C        CODE FOR INCREMENT EQUAL TO 1                                  EILI1591
C                                                                       EILI1592
   20 SMAX = DCABS1(ZX(1))                                              EILI1593
      DO 30 I = 2,N                                                     EILI1594
         IF(DCABS1(ZX(I)).LE.SMAX) GO TO 30                             EILI1595
         IZAMAX = I                                                     EILI1596
         SMAX = DCABS1(ZX(I))                                           EILI1597
   30 CONTINUE                                                          EILI1598
      RETURN                                                            EILI1599
      END      