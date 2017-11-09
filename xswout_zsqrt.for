!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:02:16 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE RRZSQRT(AR, AI, BR, BI)                                EILI3237
C***BEGIN PROLOGUE  ZSQRT                                               EILI3238
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY                 EILI3239
C                                                                       EILI3240
C     DOUBLE PRECISION COMPLEX SQUARE ROOT, B=CSQRT(A)                  EILI3241
C                                                                       EILI3242
C***ROUTINES CALLED  ZABS                                               EILI3243
C***END PROLOGUE  ZSQRT                                                 EILI3244
      DOUBLE PRECISION AR, AI, BR, BI, ZM, DTHETA, DPI, DRT             EILI3245
      DOUBLE PRECISION ZABS                                             EILI3246
      EXTERNAL ZABS                                                     EILI3247
      DATA DRT , DPI / 7.071067811865475244008443621D-1,                EILI3248
     1                 3.141592653589793238462643383D+0/                EILI3249
      ZM = ZABS(AR,AI)                                                  EILI3250
      ZM = DSQRT(ZM)                                                    EILI3251
      IF (AR.EQ.0.0D+0) GO TO 10                                        EILI3252
      IF (AI.EQ.0.0D+0) GO TO 20                                        EILI3253
      DTHETA = DATAN(AI/AR)                                             EILI3254
      IF (DTHETA.LE.0.0D+0) GO TO 40                                    EILI3255
      IF (AR.LT.0.0D+0) DTHETA = DTHETA - DPI                           EILI3256
      GO TO 50                                                          EILI3257
   10 IF (AI.GT.0.0D+0) GO TO 60                                        EILI3258
      IF (AI.LT.0.0D+0) GO TO 70                                        EILI3259
      BR = 0.0D+0                                                       EILI3260
      BI = 0.0D+0                                                       EILI3261
      RETURN                                                            EILI3262
   20 IF (AR.GT.0.0D+0) GO TO 30                                        EILI3263
      BR = 0.0D+0                                                       EILI3264
      BI = DSQRT(DABS(AR))                                              EILI3265
      RETURN                                                            EILI3266
   30 BR = DSQRT(AR)                                                    EILI3267
      BI = 0.0D+0                                                       EILI3268
      RETURN                                                            EILI3269
   40 IF (AR.LT.0.0D+0) DTHETA = DTHETA + DPI                           EILI3270
   50 DTHETA = DTHETA*0.5D+0                                            EILI3271
      BR = ZM*DCOS(DTHETA)                                              EILI3272
      BI = ZM*DSIN(DTHETA)                                              EILI3273
      RETURN                                                            EILI3274
   60 BR = ZM*DRT                                                       EILI3275
      BI = ZM*DRT                                                       EILI3276
      RETURN                                                            EILI3277
   70 BR = ZM*DRT                                                       EILI3278
      BI = -ZM*DRT                                                      EILI3279
      RETURN                                                            EILI3280
      END       