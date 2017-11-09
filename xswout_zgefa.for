!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 1:38:16 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZGEFA(A,LDA,N,IPVT,INFO)                               EILI1355
      INTEGER LDA,N,IPVT(1),INFO                                        EILI1356
      COMPLEX*16 A(LDA,1)                                               EILI1357
C                                                                       EILI1358
C     ZGEFA FACTORS A COMPLEX*16 MATRIX BY GAUSSIAN ELIMINATION.        EILI1359
C                                                                       EILI1360
C     ZGEFA IS USUALLY CALLED BY ZGECO, BUT IT CAN BE CALLED            EILI1361
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.          EILI1362
C     (TIME FOR ZGECO) = (1 + 9/N)*(TIME FOR ZGEFA) .                   EILI1363
C                                                                       EILI1364
C     ON ENTRY                                                          EILI1365
C                                                                       EILI1366
C        A       COMPLEX*16(LDA, N)                                     EILI1367
C                THE MATRIX TO BE FACTORED.                             EILI1368
C                                                                       EILI1369
C        LDA     INTEGER                                                EILI1370
C                THE LEADING DIMENSION OF THE ARRAY  A .                EILI1371
C                                                                       EILI1372
C        N       INTEGER                                                EILI1373
C                THE ORDER OF THE MATRIX  A .                           EILI1374
C                                                                       EILI1375
C     ON RETURN                                                         EILI1376
C                                                                       EILI1377
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS         EILI1378
C                WHICH WERE USED TO OBTAIN IT.                          EILI1379
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE       EILI1380
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER          EILI1381
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.       EILI1382
C                                                                       EILI1383
C        IPVT    INTEGER(N)                                             EILI1384
C                AN INTEGER VECTOR OF PIVOT INDICES.                    EILI1385
C                                                                       EILI1386
C        INFO    INTEGER                                                EILI1387
C                = 0  NORMAL VALUE.                                     EILI1388
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR       EILI1389
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES        EILI1390
C                     INDICATE THAT ZGESL OR ZGEDI WILL DIVIDE BY ZERO  EILI1391
C                     IF CALLED.  USE  RCOND  IN ZGECO FOR A RELIABLE   EILI1392
C                     INDICATION OF SINGULARITY.                        EILI1393
C                                                                       EILI1394
C     LINPACK. THIS VERSION DATED 08/14/78 .                            EILI1395
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      EILI1396
C                                                                       EILI1397
C     SUBROUTINES AND FUNCTIONS                                         EILI1398
C                                                                       EILI1399
C     BLAS ZAXPY,ZSCAL,IZAMAX                                           EILI1400
C     FORTRAN DABS                                                      EILI1401
C                                                                       EILI1402
C     INTERNAL VARIABLES                                                EILI1403
C                                                                       EILI1404
      COMPLEX*16 T                                                      EILI1405
      INTEGER IZAMAX,J,K,KP1,L,NM1                                      EILI1406
C                                                                       EILI1407
      COMPLEX*16 ZDUM                                                   EILI1408
      DOUBLE PRECISION CABS1                                            EILI1409
      DOUBLE PRECISION DREAL,DIMAG                                      EILI1410
      COMPLEX*16 ZDUMR,ZDUMI                                            EILI1411
      DREAL(ZDUMR) = ZDUMR                                              EILI1412
      DIMAG(ZDUMI) = (0.0D0,-1.0D0)*ZDUMI                               EILI1413
      CABS1(ZDUM) = DABS(DREAL(ZDUM)) + DABS(DIMAG(ZDUM))               EILI1414
C                                                                       EILI1415
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING                        EILI1416
C                                                                       EILI1417
      INFO = 0                                                          EILI1418
      NM1 = N - 1                                                       EILI1419
      IF (NM1 .LT. 1) GO TO 70                                          EILI1420
      DO 60 K = 1, NM1                                                  EILI1421
         KP1 = K + 1                                                    EILI1422
C                                                                       EILI1423
C        FIND L = PIVOT INDEX                                           EILI1424
C                                                                       EILI1425
         L = IZAMAX(N-K+1,A(K,K),1) + K - 1                             EILI1426
         IPVT(K) = L                                                    EILI1427
C                                                                       EILI1428
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED          EILI1429
C                                                                       EILI1430
         IF (CABS1(A(L,K)) .EQ. 0.0D0) GO TO 40                         EILI1431
C                                                                       EILI1432
C           INTERCHANGE IF NECESSARY                                    EILI1433
C                                                                       EILI1434
            IF (L .EQ. K) GO TO 10                                      EILI1435
               T = A(L,K)                                               EILI1436
               A(L,K) = A(K,K)                                          EILI1437
               A(K,K) = T                                               EILI1438
   10       CONTINUE                                                    EILI1439
C                                                                       EILI1440
C           COMPUTE MULTIPLIERS                                         EILI1441
C                                                                       EILI1442
            T = -(1.0D0,0.0D0)/A(K,K)                                   EILI1443
            CALL ZSCAL(N-K,T,A(K+1,K),1)                                EILI1444
C                                                                       EILI1445
C           ROW ELIMINATION WITH COLUMN INDEXING                        EILI1446
C                                                                       EILI1447
            DO 30 J = KP1, N                                            EILI1448
               T = A(L,J)                                               EILI1449
               IF (L .EQ. K) GO TO 20                                   EILI1450
                  A(L,J) = A(K,J)                                       EILI1451
                  A(K,J) = T                                            EILI1452
   20          CONTINUE                                                 EILI1453
               CALL ZAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)                  EILI1454
   30       CONTINUE                                                    EILI1455
         GO TO 50                                                       EILI1456
   40    CONTINUE                                                       EILI1457
            INFO = K                                                    EILI1458
   50    CONTINUE                                                       EILI1459
   60 CONTINUE                                                          EILI1460
   70 CONTINUE                                                          EILI1461
      IPVT(N) = N                                                       EILI1462
      IF (CABS1(A(N,N)) .EQ. 0.0D0) INFO = N                            EILI1463
      RETURN                                                            EILI1464
      END                  