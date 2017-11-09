!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:17:34 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE ZRATI(ZR, ZI, FNU, N, CYR, CYI, TOL)                   EILI6243
C***BEGIN PROLOGUE  ZRATI                                               EILI6244
C***REFER TO  ZBESI,ZBESK,ZBESH                                         EILI6245
C                                                                       EILI6246
C     ZRATI COMPUTES RATIOS OF I BESSEL FUNCTIONS BY BACKWARD           EILI6247
C     RECURRENCE.  THE STARTING INDEX IS DETERMINED BY FORWARD          EILI6248
C     RECURRENCE AS DESCRIBED IN J. RES. OF NAT. BUR. OF STANDARDS-B,   EILI6249
C     MATHEMATICAL SCIENCES, VOL 77B, P111-114, SEPTEMBER, 1973,        EILI6250
C     BESSEL FUNCTIONS I AND J OF COMPLEX ARGUMENT AND INTEGER ORDER,   EILI6251
C     BY D. J. SOOKNE.                                                  EILI6252
C                                                                       EILI6253
C***ROUTINES CALLED  ZABS,ZDIV                                          EILI6254
C***END PROLOGUE  ZRATI                                                 EILI6255
C     COMPLEX Z,CY(1),CONE,CZERO,P1,P2,T1,RZ,PT,CDFNU                   EILI6256
      DOUBLE PRECISION AK, AMAGZ, AP1, AP2, ARG, AZ, CDFNUI, CDFNUR,    EILI6257
     * CONEI, CONER, CYI, CYR, CZEROI, CZEROR, DFNU, FDNU, FLAM, FNU,   EILI6258
     * FNUP, PTI, PTR, P1I, P1R, P2I, P2R, RAK, RAP1, RHO, RT2, RZI,    EILI6259
     * RZR, TEST, TEST1, TOL, TTI, TTR, T1I, T1R, ZI, ZR, ZABS          EILI6260
      INTEGER I, ID, IDNU, INU, ITIME, K, KK, MAGZ, N                   EILI6261
      DIMENSION CYR(N), CYI(N)                                          EILI6262
      EXTERNAL ZABS                                                     EILI6263
      DATA CZEROR,CZEROI,CONER,CONEI,RT2/                               EILI6264
     1 0.0D0, 0.0D0, 1.0D0, 0.0D0, 1.41421356237309505D0 /              EILI6265
      AZ = ZABS(ZR,ZI)                                                  EILI6266
      INU = INT(SNGL(FNU))                                              EILI6267
      IDNU = INU + N - 1                                                EILI6268
      MAGZ = INT(SNGL(AZ))                                              EILI6269
      AMAGZ = DBLE(FLOAT(MAGZ+1))                                       EILI6270
      FDNU = DBLE(FLOAT(IDNU))                                          EILI6271
      FNUP = DMAX1(AMAGZ,FDNU)                                          EILI6272
      ID = IDNU - MAGZ - 1                                              EILI6273
      ITIME = 1                                                         EILI6274
      K = 1                                                             EILI6275
      PTR = 1.0D0/AZ                                                    EILI6276
      RZR = PTR*(ZR+ZR)*PTR                                             EILI6277
      RZI = -PTR*(ZI+ZI)*PTR                                            EILI6278
      T1R = RZR*FNUP                                                    EILI6279
      T1I = RZI*FNUP                                                    EILI6280
      P2R = -T1R                                                        EILI6281
      P2I = -T1I                                                        EILI6282
      P1R = CONER                                                       EILI6283
      P1I = CONEI                                                       EILI6284
      T1R = T1R + RZR                                                   EILI6285
      T1I = T1I + RZI                                                   EILI6286
      IF (ID.GT.0) ID = 0                                               EILI6287
      AP2 = ZABS(P2R,P2I)                                               EILI6288
      AP1 = ZABS(P1R,P1I)                                               EILI6289
C-----------------------------------------------------------------------EILI6290
C     THE OVERFLOW TEST ON K(FNU+I-1,Z) BEFORE THE CALL TO CBKNU        EILI6291
C     GUARANTEES THAT P2 IS ON SCALE. SCALE TEST1 AND ALL SUBSEQUENT    EILI6292
C     P2 VALUES BY AP1 TO ENSURE THAT AN OVERFLOW DOES NOT OCCUR        EILI6293
C     PREMATURELY.                                                      EILI6294
C-----------------------------------------------------------------------EILI6295
      ARG = (AP2+AP2)/(AP1*TOL)                                         EILI6296
      TEST1 = DSQRT(ARG)                                                EILI6297
      TEST = TEST1                                                      EILI6298
      RAP1 = 1.0D0/AP1                                                  EILI6299
      P1R = P1R*RAP1                                                    EILI6300
      P1I = P1I*RAP1                                                    EILI6301
      P2R = P2R*RAP1                                                    EILI6302
      P2I = P2I*RAP1                                                    EILI6303
      AP2 = AP2*RAP1                                                    EILI6304
   10 CONTINUE                                                          EILI6305
      K = K + 1                                                         EILI6306
      AP1 = AP2                                                         EILI6307
      PTR = P2R                                                         EILI6308
      PTI = P2I                                                         EILI6309
      P2R = P1R - (T1R*PTR-T1I*PTI)                                     EILI6310
      P2I = P1I - (T1R*PTI+T1I*PTR)                                     EILI6311
      P1R = PTR                                                         EILI6312
      P1I = PTI                                                         EILI6313
      T1R = T1R + RZR                                                   EILI6314
      T1I = T1I + RZI                                                   EILI6315
      AP2 = ZABS(P2R,P2I)                                               EILI6316
      IF (AP1.LE.TEST) GO TO 10                                         EILI6317
      IF (ITIME.EQ.2) GO TO 20                                          EILI6318
      AK = ZABS(T1R,T1I)*0.5D0                                          EILI6319
      FLAM = AK + DSQRT(AK*AK-1.0D0)                                    EILI6320
      RHO = DMIN1(AP2/AP1,FLAM)                                         EILI6321
      TEST = TEST1*DSQRT(RHO/(RHO*RHO-1.0D0))                           EILI6322
      ITIME = 2                                                         EILI6323
      GO TO 10                                                          EILI6324
   20 CONTINUE                                                          EILI6325
      KK = K + 1 - ID                                                   EILI6326
      AK = DBLE(FLOAT(KK))                                              EILI6327
      T1R = AK                                                          EILI6328
      T1I = CZEROI                                                      EILI6329
      DFNU = FNU + DBLE(FLOAT(N-1))                                     EILI6330
      P1R = 1.0D0/AP2                                                   EILI6331
      P1I = CZEROI                                                      EILI6332
      P2R = CZEROR                                                      EILI6333
      P2I = CZEROI                                                      EILI6334
      DO 30 I=1,KK                                                      EILI6335
        PTR = P1R                                                       EILI6336
        PTI = P1I                                                       EILI6337
        RAP1 = DFNU + T1R                                               EILI6338
        TTR = RZR*RAP1                                                  EILI6339
        TTI = RZI*RAP1                                                  EILI6340
        P1R = (PTR*TTR-PTI*TTI) + P2R                                   EILI6341
        P1I = (PTR*TTI+PTI*TTR) + P2I                                   EILI6342
        P2R = PTR                                                       EILI6343
        P2I = PTI                                                       EILI6344
        T1R = T1R - CONER                                               EILI6345
   30 CONTINUE                                                          EILI6346
      IF (P1R.NE.CZEROR .OR. P1I.NE.CZEROI) GO TO 40                    EILI6347
      P1R = TOL                                                         EILI6348
      P1I = TOL                                                         EILI6349
   40 CONTINUE                                                          EILI6350
      CALL ZDIV(P2R, P2I, P1R, P1I, CYR(N), CYI(N))                     EILI6351
      IF (N.EQ.1) RETURN                                                EILI6352
      K = N - 1                                                         EILI6353
      AK = DBLE(FLOAT(K))                                               EILI6354
      T1R = AK                                                          EILI6355
      T1I = CZEROI                                                      EILI6356
      CDFNUR = FNU*RZR                                                  EILI6357
      CDFNUI = FNU*RZI                                                  EILI6358
      DO 60 I=2,N                                                       EILI6359
        PTR = CDFNUR + (T1R*RZR-T1I*RZI) + CYR(K+1)                     EILI6360
        PTI = CDFNUI + (T1R*RZI+T1I*RZR) + CYI(K+1)                     EILI6361
        AK = ZABS(PTR,PTI)                                              EILI6362
        IF (AK.NE.CZEROR) GO TO 50                                      EILI6363
        PTR = TOL                                                       EILI6364
        PTI = TOL                                                       EILI6365
        AK = TOL*RT2                                                    EILI6366
   50   CONTINUE                                                        EILI6367
        RAK = CONER/AK                                                  EILI6368
        CYR(K) = RAK*PTR*RAK                                            EILI6369
        CYI(K) = -RAK*PTI*RAK                                           EILI6370
        T1R = T1R - CONER                                               EILI6371
        K = K - 1                                                       EILI6372
   60 CONTINUE                                                          EILI6373
      RETURN                                                            EILI6374
      END      