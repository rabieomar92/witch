!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 1:37:06 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE RG(NM,N,A,WR,WI,MATZ,Z,IV1,FV1,IERR)                   EILI  45
C                                                                       EILI  46
      INTEGER N,NM,IS1,IS2,IERR,MATZ                                    EILI  47
      DOUBLE PRECISION A(NM,N),WR(N),WI(N),Z(NM,N),FV1(N)               EILI  48
      INTEGER IV1(N)                                                    EILI  49
C                                                                       EILI  50
C     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF                 EILI  51
C     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK)     EILI  52
C     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED)             EILI  53
C     OF A REAL GENERAL MATRIX.                                         EILI  54
C                                                                       EILI  55
C     ON INPUT                                                          EILI  56
C                                                                       EILI  57
C        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL    EILI  58
C        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM            EILI  59
C        DIMENSION STATEMENT.                                           EILI  60
C                                                                       EILI  61
C        N  IS THE ORDER OF THE MATRIX  A.                              EILI  62
C                                                                       EILI  63
C        A  CONTAINS THE REAL GENERAL MATRIX.                           EILI  64
C                                                                       EILI  65
C        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF              EILI  66
C        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO          EILI  67
C        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS.    EILI  68
C                                                                       EILI  69
C     ON OUTPUT                                                         EILI  70
C                                                                       EILI  71
C        WR  AND  WI  CONTAIN THE REAL AND IMAGINARY PARTS,             EILI  72
C        RESPECTIVELY, OF THE EIGENVALUES.  COMPLEX CONJUGATE           EILI  73
C        PAIRS OF EIGENVALUES APPEAR CONSECUTIVELY WITH THE             EILI  74
C        EIGENVALUE HAVING THE POSITIVE IMAGINARY PART FIRST.           EILI  75
C                                                                       EILI  76
C        Z  CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS   EILI  77
C        IF MATZ IS NOT ZERO.  IF THE J-TH EIGENVALUE IS REAL, THE      EILI  78
C        J-TH COLUMN OF  Z  CONTAINS ITS EIGENVECTOR.  IF THE J-TH      EILI  79
C        EIGENVALUE IS COMPLEX WITH POSITIVE IMAGINARY PART, THE        EILI  80
C        J-TH AND (J+1)-TH COLUMNS OF  Z  CONTAIN THE REAL AND          EILI  81
C        IMAGINARY PARTS OF ITS EIGENVECTOR.  THE CONJUGATE OF THIS     EILI  82
C        VECTOR IS THE EIGENVECTOR FOR THE CONJUGATE EIGENVALUE.        EILI  83
C                                                                       EILI  84
C        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR      EILI  85
C           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR HQR      EILI  86
C           AND HQR2.  THE NORMAL COMPLETION CODE IS ZERO.              EILI  87
C                                                                       EILI  88
C        IV1  AND  FV1  ARE TEMPORARY STORAGE ARRAYS.                   EILI  89
C                                                                       EILI  90
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,    EILI  91
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY EILI  92
C                                                                       EILI  93
C     THIS VERSION DATED AUGUST 1983.                                   EILI  94
C                                                                       EILI  95
C     ------------------------------------------------------------------EILI  96
C                                                                       EILI  97
      IF (N .LE. NM) GO TO 10                                           EILI  98
      IERR = 10 * N                                                     EILI  99
      GO TO 50                                                          EILI 100
C                                                                       EILI 101
   10 CALL  BALANC(NM,N,A,IS1,IS2,FV1)                                  EILI 102
      CALL  ELMHES(NM,N,IS1,IS2,A,IV1)                                  EILI 103
      IF (MATZ .NE. 0) GO TO 20                                         EILI 104
C     .......... FIND EIGENVALUES ONLY ..........                       EILI 105
      CALL  HQR(NM,N,IS1,IS2,A,WR,WI,IERR)                              EILI 106
      GO TO 50                                                          EILI 107
C     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS ..........      EILI 108
   20 CALL  ELTRAN(NM,N,IS1,IS2,A,IV1,Z)                                EILI 109
      CALL  HQR2(NM,N,IS1,IS2,A,WR,WI,Z,IERR)                           EILI 110
      IF (IERR .NE. 0) GO TO 50                                         EILI 111
      CALL  BALBAK(NM,N,IS1,IS2,FV1,N,Z)                                EILI 112
   50 RETURN                                                            EILI 113
      END                              