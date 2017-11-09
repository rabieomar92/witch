!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:09:06 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      DOUBLE PRECISION FUNCTION DCABS1(Z)                               EILI1629
      DOUBLE COMPLEX Z,ZZ                                               EILI1630
      DOUBLE PRECISION T(2)                                             EILI1631
      EQUIVALENCE (ZZ,T(1))                                             EILI1632
      ZZ = Z                                                            EILI1633
      DCABS1 = DABS(T(1)) + DABS(T(2))                                  EILI1634
      RETURN                                                            EILI1635
      END                                                               EILI1636      