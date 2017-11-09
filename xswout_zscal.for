!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:53:56 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE  ZSCAL(N,ZA,ZX,INCX)                                   EILI1601
C                                                                       EILI1602
C     SCALES A VECTOR BY A CONSTANT.                                    EILI1603
C     JACK DONGARRA, 3/11/78.                                           EILI1604
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.                           EILI1605
C                                                                       EILI1606
      DOUBLE COMPLEX ZA,ZX(1)                                           EILI1607
      INTEGER I,INCX,IX,N                                               EILI1608
C                                                                       EILI1609
      IF( N.LE.0 .OR. INCX.LE.0 )RETURN                                 EILI1610
      IF(INCX.EQ.1)GO TO 20                                             EILI1611
C                                                                       EILI1612
C        CODE FOR INCREMENT NOT EQUAL TO 1                              EILI1613
C                                                                       EILI1614
      IX = 1                                                            EILI1615
      DO 10 I = 1,N                                                     EILI1616
        ZX(IX) = ZA*ZX(IX)                                              EILI1617
        IX = IX + INCX                                                  EILI1618
   10 CONTINUE                                                          EILI1619
      RETURN                                                            EILI1620
C                                                                       EILI1621
C        CODE FOR INCREMENT EQUAL TO 1                                  EILI1622
C                                                                       EILI1623
   20 DO 30 I = 1,N                                                     EILI1624
        ZX(I) = ZA*ZX(I)                                                EILI1625
   30 CONTINUE                                                          EILI1626
      RETURN                                                            EILI1627
      END      