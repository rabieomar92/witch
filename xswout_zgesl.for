!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 1:38:45 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZGESL(A,LDA,N,IPVT,B,JOB)                              EILI1233
      INTEGER LDA,N,IPVT(1),JOB                                         EILI1234
      COMPLEX*16 A(LDA,1),B(1)                                          EILI1235
C                                                                       EILI1236
C     ZGESL SOLVES THE COMPLEX*16 SYSTEM                                EILI1237
C     A * X = B  OR  CTRANS(A) * X = B                                  EILI1238
C     USING THE FACTORS COMPUTED BY ZGECO OR ZGEFA.                     EILI1239
C                                                                       EILI1240
C     ON ENTRY                                                          EILI1241
C                                                                       EILI1242
C        A       COMPLEX*16(LDA, N)                                     EILI1243
C                THE OUTPUT FROM ZGECO OR ZGEFA.                        EILI1244
C                                                                       EILI1245
C        LDA     INTEGER                                                EILI1246
C                THE LEADING DIMENSION OF THE ARRAY  A .                EILI1247
C                                                                       EILI1248
C        N       INTEGER                                                EILI1249
C                THE ORDER OF THE MATRIX  A .                           EILI1250
C                                                                       EILI1251
C        IPVT    INTEGER(N)                                             EILI1252
C                THE PIVOT VECTOR FROM ZGECO OR ZGEFA.                  EILI1253
C                                                                       EILI1254
C        B       COMPLEX*16(N)                                          EILI1255
C                THE RIGHT HAND SIDE VECTOR.                            EILI1256
C                                                                       EILI1257
C        JOB     INTEGER                                                EILI1258
C                = 0         TO SOLVE  A*X = B ,                        EILI1259
C                = NONZERO   TO SOLVE  CTRANS(A)*X = B  WHERE           EILI1260
C                            CTRANS(A)  IS THE CONJUGATE TRANSPOSE.     EILI1261
C                                                                       EILI1262
C     ON RETURN                                                         EILI1263
C                                                                       EILI1264
C        B       THE SOLUTION VECTOR  X .                               EILI1265
C                                                                       EILI1266
C     ERROR CONDITION                                                   EILI1267
C                                                                       EILI1268
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A   EILI1269
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY  EILI1270
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER       EILI1271
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE     EILI1272
C        CALLED CORRECTLY AND IF ZGECO HAS SET RCOND .GT. 0.0           EILI1273
C        OR ZGEFA HAS SET INFO .EQ. 0 .                                 EILI1274
C                                                                       EILI1275
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX                 EILI1276
C     WITH  P  COLUMNS                                                  EILI1277
C           CALL ZGECO(A,LDA,N,IPVT,RCOND,Z)                            EILI1278
C           IF (RCOND IS TOO SMALL) GO TO ...                           EILI1279
C           DO 10 J = 1, P                                              EILI1280
C              CALL ZGESL(A,LDA,N,IPVT,C(1,J),0)                        EILI1281
C        10 CONTINUE                                                    EILI1282
C                                                                       EILI1283
C     LINPACK. THIS VERSION DATED 08/14/78 .                            EILI1284
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      EILI1285
C                                                                       EILI1286
C     SUBROUTINES AND FUNCTIONS                                         EILI1287
C                                                                       EILI1288
C     BLAS ZAXPY,ZDOTC                                                  EILI1289
C     FORTRAN DCONJG                                                    EILI1290
C                                                                       EILI1291
C     INTERNAL VARIABLES                                                EILI1292
C                                                                       EILI1293
      COMPLEX*16 ZDOTC,T                                                EILI1294
      INTEGER K,KB,L,NM1                                                EILI1295
C     DOUBLE PRECISION DREAL,DIMAG                                      EILI1296
      COMPLEX*16 ZDUMR,ZDUMI                                            EILI1297
C     DREAL(ZDUMR) = ZDUMR                                              EILI1298
C     DIMAG(ZDUMI) = (0.0D0,-1.0D0)*ZDUMI                               EILI1299
C                                                                       EILI1300
      NM1 = N - 1                                                       EILI1301
      IF (JOB .NE. 0) GO TO 50                                          EILI1302
C                                                                       EILI1303
C        JOB = 0 , SOLVE  A * X = B                                     EILI1304
C        FIRST SOLVE  L*Y = B                                           EILI1305
C                                                                       EILI1306
         IF (NM1 .LT. 1) GO TO 30                                       EILI1307
         DO 20 K = 1, NM1                                               EILI1308
            L = IPVT(K)                                                 EILI1309
            T = B(L)                                                    EILI1310
            IF (L .EQ. K) GO TO 10                                      EILI1311
               B(L) = B(K)                                              EILI1312
               B(K) = T                                                 EILI1313
   10       CONTINUE                                                    EILI1314
            CALL ZAXPY(N-K,T,A(K+1,K),1,B(K+1),1)                       EILI1315
   20    CONTINUE                                                       EILI1316
   30    CONTINUE                                                       EILI1317
C                                                                       EILI1318
C        NOW SOLVE  U*X = Y                                             EILI1319
C                                                                       EILI1320
         DO 40 KB = 1, N                                                EILI1321
            K = N + 1 - KB                                              EILI1322
            B(K) = B(K)/A(K,K)                                          EILI1323
            T = -B(K)                                                   EILI1324
            CALL ZAXPY(K-1,T,A(1,K),1,B(1),1)                           EILI1325
   40    CONTINUE                                                       EILI1326
      GO TO 100                                                         EILI1327
   50 CONTINUE                                                          EILI1328
C                                                                       EILI1329
C        JOB = NONZERO, SOLVE  CTRANS(A) * X = B                        EILI1330
C        FIRST SOLVE  CTRANS(U)*Y = B                                   EILI1331
C                                                                       EILI1332
         DO 60 K = 1, N                                                 EILI1333
            T = ZDOTC(K-1,A(1,K),1,B(1),1)                              EILI1334
            B(K) = (B(K) - T)/DCONJG(A(K,K))                            EILI1335
   60    CONTINUE                                                       EILI1336
C                                                                       EILI1337
C        NOW SOLVE CTRANS(L)*X = Y                                      EILI1338
C                                                                       EILI1339
         IF (NM1 .LT. 1) GO TO 90                                       EILI1340
         DO 80 KB = 1, NM1                                              EILI1341
            K = N - KB                                                  EILI1342
            B(K) = B(K) + ZDOTC(N-K,A(K+1,K),1,B(K+1),1)                EILI1343
            L = IPVT(K)                                                 EILI1344
            IF (L .EQ. K) GO TO 70                                      EILI1345
               T = B(L)                                                 EILI1346
               B(L) = B(K)                                              EILI1347
               B(K) = T                                                 EILI1348
   70       CONTINUE                                                    EILI1349
   80    CONTINUE                                                       EILI1350
   90    CONTINUE                                                       EILI1351
  100 CONTINUE                                                          EILI1352
      RETURN                                                            EILI1353
      END                   