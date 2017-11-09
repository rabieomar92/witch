!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 9:52:39 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE BALBAK(NM,N,LOW,IGH,SCALE,M,Z)                         EILI 282
C                                                                       EILI 283
      INTEGER I,J,K,M,N,II,NM,IGH,LOW                                   EILI 284
      DOUBLE PRECISION SCALE(N),Z(NM,M)                                 EILI 285
      DOUBLE PRECISION S                                                EILI 286
C                                                                       EILI 287
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALBAK,   EILI 288
C     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH.              EILI 289
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).   EILI 290
C                                                                       EILI 291
C     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL GENERAL          EILI 292
C     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING            EILI 293
C     BALANCED MATRIX DETERMINED BY  BALANC.                            EILI 294
C                                                                       EILI 295
C     ON INPUT                                                          EILI 296
C                                                                       EILI 297
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         EILI 298
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          EILI 299
C          DIMENSION STATEMENT.                                         EILI 300
C                                                                       EILI 301
C        N IS THE ORDER OF THE MATRIX.                                  EILI 302
C                                                                       EILI 303
C        LOW AND IGH ARE INTEGERS DETERMINED BY  BALANC.                EILI 304
C                                                                       EILI 305
C        SCALE CONTAINS INFORMATION DETERMINING THE PERMUTATIONS        EILI 306
C          AND SCALING FACTORS USED BY  BALANC.                         EILI 307
C                                                                       EILI 308
C        M IS THE NUMBER OF COLUMNS OF Z TO BE BACK TRANSFORMED.        EILI 309
C                                                                       EILI 310
C        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGEN-          EILI 311
C          VECTORS TO BE BACK TRANSFORMED IN ITS FIRST M COLUMNS.       EILI 312
C                                                                       EILI 313
C     ON OUTPUT                                                         EILI 314
C                                                                       EILI 315
C        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE                 EILI 316
C          TRANSFORMED EIGENVECTORS IN ITS FIRST M COLUMNS.             EILI 317
C                                                                       EILI 318
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,    EILI 319
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY EILI 320
C                                                                       EILI 321
C     THIS VERSION DATED AUGUST 1983.                                   EILI 322
C                                                                       EILI 323
C     ------------------------------------------------------------------EILI 324
C                                                                       EILI 325
      IF (M .EQ. 0) GO TO 200                                           EILI 326
      IF (IGH .EQ. LOW) GO TO 120                                       EILI 327
C                                                                       EILI 328
      DO 110 I = LOW, IGH                                               EILI 329
         S = SCALE(I)                                                   EILI 330
C     .......... LEFT HAND EIGENVECTORS ARE BACK TRANSFORMED            EILI 331
C                IF THE FOREGOING STATEMENT IS REPLACED BY              EILI 332
C                S=1.0D0/SCALE(I). ..........                           EILI 333
         DO 100 J = 1, M                                                EILI 334
  100    Z(I,J) = Z(I,J) * S                                            EILI 335
C                                                                       EILI 336
  110 CONTINUE                                                          EILI 337
C     ......... FOR I=LOW-1 STEP -1 UNTIL 1,                            EILI 338
C               IGH+1 STEP 1 UNTIL N DO -- ..........                   EILI 339
  120 DO 140 II = 1, N                                                  EILI 340
         I = II                                                         EILI 341
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 140                     EILI 342
         IF (I .LT. LOW) I = LOW - II                                   EILI 343
         K = SCALE(I)                                                   EILI 344
         IF (K .EQ. I) GO TO 140                                        EILI 345
C                                                                       EILI 346
         DO 130 J = 1, M                                                EILI 347
            S = Z(I,J)                                                  EILI 348
            Z(I,J) = Z(K,J)                                             EILI 349
            Z(K,J) = S                                                  EILI 350
  130    CONTINUE                                                       EILI 351
C                                                                       EILI 352
  140 CONTINUE                                                          EILI 353
C                                                                       EILI 354
  200 RETURN                                                            EILI 355
      END      