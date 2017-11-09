!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:55:53 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE XERROR(MESS,NMESS,L1,L2)                               EILI 305
C                                                                       EILI 306
C     THIS IS A DUMMY XERROR ROUTINE TO PRINT ERROR MESSAGES WITH NMESS EILI 307
C     CHARACTERS. L1 AND L2 ARE DUMMY PARAMETERS TO MAKE THIS CALL      EILI 308
C     COMPATIBLE WITH THE SLATEC XERROR ROUTINE. THIS IS A FORTRAN 77   EILI 309
C     ROUTINE.                                                          EILI 310
C                                                                       EILI 311
      CHARACTER*(*) MESS                                                EILI 312
      NN=NMESS/70                                                       EILI 313
      NR=NMESS-70*NN                                                    EILI 314
      IF(NR.NE.0) NN=NN+1                                               EILI 315
      K=1                                                               EILI 316
      PRINT 900                                                         EILI 317
  900 FORMAT(/)                                                         EILI 318
      DO 10 I=1,NN                                                      EILI 319
        KMIN=MIN0(K+69,NMESS)                                           EILI 320
        PRINT *, MESS(K:KMIN)                                           EILI 321
        K=K+70                                                          EILI 322
   10 CONTINUE                                                          EILI 323
      PRINT 900                                                         EILI 324
      RETURN                                                            EILI 325
      END       