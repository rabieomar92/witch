!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 10:10:53 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      DOUBLE PRECISION FUNCTION DGAMLN(Z,IERR)                          EILI3047
C***BEGIN PROLOGUE  DGAMLN                                              EILI3048
C***DATE WRITTEN   830501   (YYMMDD)                                    EILI3049
C***REVISION DATE  830501   (YYMMDD)                                    EILI3050
C***CATEGORY NO.  B5F                                                   EILI3051
C***KEYWORDS  GAMMA FUNCTION,LOGARITHM OF GAMMA FUNCTION                EILI3052
C***AUTHOR  AMOS, DONALD E., SANDIA NATIONAL LABORATORIES               EILI3053
C***PURPOSE  TO COMPUTE THE LOGARITHM OF THE GAMMA FUNCTION             EILI3054
C***DESCRIPTION                                                         EILI3055
C                                                                       EILI3056
C               **** A DOUBLE PRECISION ROUTINE ****                    EILI3057
C         DGAMLN COMPUTES THE NATURAL LOG OF THE GAMMA FUNCTION FOR     EILI3058
C         Z.GT.0.  THE ASYMPTOTIC EXPANSION IS USED TO GENERATE VALUES  EILI3059
C         GREATER THAN ZMIN WHICH ARE ADJUSTED BY THE RECURSION         EILI3060
C         G(Z+1)=Z*G(Z) FOR Z.LE.ZMIN.  THE FUNCTION WAS MADE AS        EILI3061
C         PORTABLE AS POSSIBLE BY COMPUTIMG ZMIN FROM THE NUMBER OF BASEEILI3062
C         10 DIGITS IN A WORD, RLN=AMAX1(-ALOG10(R1MACH(4)),0.5E-18)    EILI3063
C         LIMITED TO 18 DIGITS OF (RELATIVE) ACCURACY.                  EILI3064
C                                                                       EILI3065
C         SINCE INTEGER ARGUMENTS ARE COMMON, A TABLE LOOK UP ON 100    EILI3066
C         VALUES IS USED FOR SPEED OF EXECUTION.                        EILI3067
C                                                                       EILI3068
C     DESCRIPTION OF ARGUMENTS                                          EILI3069
C                                                                       EILI3070
C         INPUT      Z IS D0UBLE PRECISION                              EILI3071
C           Z      - ARGUMENT, Z.GT.0.0D0                               EILI3072
C                                                                       EILI3073
C         OUTPUT      DGAMLN IS DOUBLE PRECISION                        EILI3074
C           DGAMLN  - NATURAL LOG OF THE GAMMA FUNCTION AT Z.NE.0.0D0   EILI3075
C           IERR    - ERROR FLAG                                        EILI3076
C                     IERR=0, NORMAL RETURN, COMPUTATION COMPLETED      EILI3077
C                     IERR=1, Z.LE.0.0D0,    NO COMPUTATION             EILI3078
C                                                                       EILI3079
C                                                                       EILI3080
C***REFERENCES  COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT     EILI3081
C                 BY D. E. AMOS, SAND83-0083, MAY, 1983.                EILI3082
C***ROUTINES CALLED  I1MACH,D1MACH                                      EILI3083
C***END PROLOGUE  DGAMLN                                                EILI3084
      DOUBLE PRECISION CF, CON, FLN, FZ, GLN, RLN, S, TLG, TRM, TST,    EILI3085
     * T1, WDTOL, Z, ZDMY, ZINC, ZM, ZMIN, ZP, ZSQ, D1MACH              EILI3086
      INTEGER I, IERR, I1M, K, MZ, NZ, I1MACH                           EILI3087
      DIMENSION CF(22), GLN(100)                                        EILI3088
C           LNGAMMA(N), N=1,100                                         EILI3089
      DATA GLN(1), GLN(2), GLN(3), GLN(4), GLN(5), GLN(6), GLN(7),      EILI3090
     1     GLN(8), GLN(9), GLN(10), GLN(11), GLN(12), GLN(13), GLN(14), EILI3091
     2     GLN(15), GLN(16), GLN(17), GLN(18), GLN(19), GLN(20),        EILI3092
     3     GLN(21), GLN(22)/                                            EILI3093
     4     0.00000000000000000D+00,     0.00000000000000000D+00,        EILI3094
     5     6.93147180559945309D-01,     1.79175946922805500D+00,        EILI3095
     6     3.17805383034794562D+00,     4.78749174278204599D+00,        EILI3096
     7     6.57925121201010100D+00,     8.52516136106541430D+00,        EILI3097
     8     1.06046029027452502D+01,     1.28018274800814696D+01,        EILI3098
     9     1.51044125730755153D+01,     1.75023078458738858D+01,        EILI3099
     A     1.99872144956618861D+01,     2.25521638531234229D+01,        EILI3100
     B     2.51912211827386815D+01,     2.78992713838408916D+01,        EILI3101
     C     3.06718601060806728D+01,     3.35050734501368889D+01,        EILI3102
     D     3.63954452080330536D+01,     3.93398841871994940D+01,        EILI3103
     E     4.23356164607534850D+01,     4.53801388984769080D+01/        EILI3104
      DATA GLN(23), GLN(24), GLN(25), GLN(26), GLN(27), GLN(28),        EILI3105
     1     GLN(29), GLN(30), GLN(31), GLN(32), GLN(33), GLN(34),        EILI3106
     2     GLN(35), GLN(36), GLN(37), GLN(38), GLN(39), GLN(40),        EILI3107
     3     GLN(41), GLN(42), GLN(43), GLN(44)/                          EILI3108
     4     4.84711813518352239D+01,     5.16066755677643736D+01,        EILI3109
     5     5.47847293981123192D+01,     5.80036052229805199D+01,        EILI3110
     6     6.12617017610020020D+01,     6.45575386270063311D+01,        EILI3111
     7     6.78897431371815350D+01,     7.12570389671680090D+01,        EILI3112
     8     7.46582363488301644D+01,     7.80922235533153106D+01,        EILI3113
     9     8.15579594561150372D+01,     8.50544670175815174D+01,        EILI3114
     A     8.85808275421976788D+01,     9.21361756036870925D+01,        EILI3115
     B     9.57196945421432025D+01,     9.93306124547874269D+01,        EILI3116
     C     1.02968198614513813D+02,     1.06631760260643459D+02,        EILI3117
     D     1.10320639714757395D+02,     1.14034211781461703D+02,        EILI3118
     E     1.17771881399745072D+02,     1.21533081515438634D+02/        EILI3119
      DATA GLN(45), GLN(46), GLN(47), GLN(48), GLN(49), GLN(50),        EILI3120
     1     GLN(51), GLN(52), GLN(53), GLN(54), GLN(55), GLN(56),        EILI3121
     2     GLN(57), GLN(58), GLN(59), GLN(60), GLN(61), GLN(62),        EILI3122
     3     GLN(63), GLN(64), GLN(65), GLN(66)/                          EILI3123
     4     1.25317271149356895D+02,     1.29123933639127215D+02,        EILI3124
     5     1.32952575035616310D+02,     1.36802722637326368D+02,        EILI3125
     6     1.40673923648234259D+02,     1.44565743946344886D+02,        EILI3126
     7     1.48477766951773032D+02,     1.52409592584497358D+02,        EILI3127
     8     1.56360836303078785D+02,     1.60331128216630907D+02,        EILI3128
     9     1.64320112263195181D+02,     1.68327445448427652D+02,        EILI3129
     A     1.72352797139162802D+02,     1.76395848406997352D+02,        EILI3130
     B     1.80456291417543771D+02,     1.84533828861449491D+02,        EILI3131
     C     1.88628173423671591D+02,     1.92739047287844902D+02,        EILI3132
     D     1.96866181672889994D+02,     2.01009316399281527D+02,        EILI3133
     E     2.05168199482641199D+02,     2.09342586752536836D+02/        EILI3134
      DATA GLN(67), GLN(68), GLN(69), GLN(70), GLN(71), GLN(72),        EILI3135
     1     GLN(73), GLN(74), GLN(75), GLN(76), GLN(77), GLN(78),        EILI3136
     2     GLN(79), GLN(80), GLN(81), GLN(82), GLN(83), GLN(84),        EILI3137
     3     GLN(85), GLN(86), GLN(87), GLN(88)/                          EILI3138
     4     2.13532241494563261D+02,     2.17736934113954227D+02,        EILI3139
     5     2.21956441819130334D+02,     2.26190548323727593D+02,        EILI3140
     6     2.30439043565776952D+02,     2.34701723442818268D+02,        EILI3141
     7     2.38978389561834323D+02,     2.43268849002982714D+02,        EILI3142
     8     2.47572914096186884D+02,     2.51890402209723194D+02,        EILI3143
     9     2.56221135550009525D+02,     2.60564940971863209D+02,        EILI3144
     A     2.64921649798552801D+02,     2.69291097651019823D+02,        EILI3145
     B     2.73673124285693704D+02,     2.78067573440366143D+02,        EILI3146
     C     2.82474292687630396D+02,     2.86893133295426994D+02,        EILI3147
     D     2.91323950094270308D+02,     2.95766601350760624D+02,        EILI3148
     E     3.00220948647014132D+02,     3.04686856765668715D+02/        EILI3149
      DATA GLN(89), GLN(90), GLN(91), GLN(92), GLN(93), GLN(94),        EILI3150
     1     GLN(95), GLN(96), GLN(97), GLN(98), GLN(99), GLN(100)/       EILI3151
     2     3.09164193580146922D+02,     3.13652829949879062D+02,        EILI3152
     3     3.18152639620209327D+02,     3.22663499126726177D+02,        EILI3153
     4     3.27185287703775217D+02,     3.31717887196928473D+02,        EILI3154
     5     3.36261181979198477D+02,     3.40815058870799018D+02,        EILI3155
     6     3.45379407062266854D+02,     3.49954118040770237D+02,        EILI3156
     7     3.54539085519440809D+02,     3.59134205369575399D+02/        EILI3157
C             COEFFICIENTS OF ASYMPTOTIC EXPANSION                      EILI3158
      DATA CF(1), CF(2), CF(3), CF(4), CF(5), CF(6), CF(7), CF(8),      EILI3159
     1     CF(9), CF(10), CF(11), CF(12), CF(13), CF(14), CF(15),       EILI3160
     2     CF(16), CF(17), CF(18), CF(19), CF(20), CF(21), CF(22)/      EILI3161
     3     8.33333333333333333D-02,    -2.77777777777777778D-03,        EILI3162
     4     7.93650793650793651D-04,    -5.95238095238095238D-04,        EILI3163
     5     8.41750841750841751D-04,    -1.91752691752691753D-03,        EILI3164
     6     6.41025641025641026D-03,    -2.95506535947712418D-02,        EILI3165
     7     1.79644372368830573D-01,    -1.39243221690590112D+00,        EILI3166
     8     1.34028640441683920D+01,    -1.56848284626002017D+02,        EILI3167
     9     2.19310333333333333D+03,    -3.61087712537249894D+04,        EILI3168
     A     6.91472268851313067D+05,    -1.52382215394074162D+07,        EILI3169
     B     3.82900751391414141D+08,    -1.08822660357843911D+10,        EILI3170
     C     3.47320283765002252D+11,    -1.23696021422692745D+13,        EILI3171
     D     4.88788064793079335D+14,    -2.13203339609193739D+16/        EILI3172
C                                                                       EILI3173
C             LN(2*PI)                                                  EILI3174
      DATA CON                    /     1.83787706640934548D+00/        EILI3175
C                                                                       EILI3176
C***FIRST EXECUTABLE STATEMENT  DGAMLN                                  EILI3177
      DGAMLN=0.D0                                                       EILI3178
      IERR=0                                                            EILI3179
      IF (Z.LE.0.0D0) GO TO 70                                          EILI3180
      IF (Z.GT.101.0D0) GO TO 10                                        EILI3181
      NZ = INT(SNGL(Z))                                                 EILI3182
      FZ = Z - FLOAT(NZ)                                                EILI3183
      IF (FZ.GT.0.0D0) GO TO 10                                         EILI3184
      IF (NZ.GT.100) GO TO 10                                           EILI3185
      DGAMLN = GLN(NZ)                                                  EILI3186
      RETURN                                                            EILI3187
   10 CONTINUE                                                          EILI3188
      WDTOL = D1MACH(4)                                                 EILI3189
      WDTOL = DMAX1(WDTOL,0.5D-18)                                      EILI3190
      I1M = I1MACH(14)                                                  EILI3191
      RLN = D1MACH(5)*FLOAT(I1M)                                        EILI3192
      FLN = DMIN1(RLN,20.0D0)                                           EILI3193
      FLN = DMAX1(FLN,3.0D0)                                            EILI3194
      FLN = FLN - 3.0D0                                                 EILI3195
      ZM = 1.8000D0 + 0.3875D0*FLN                                      EILI3196
      MZ = INT(SNGL(ZM)) + 1                                            EILI3197
      ZMIN = FLOAT(MZ)                                                  EILI3198
      ZDMY = Z                                                          EILI3199
      ZINC = 0.0D0                                                      EILI3200
      IF (Z.GE.ZMIN) GO TO 20                                           EILI3201
      ZINC = ZMIN - FLOAT(NZ)                                           EILI3202
      ZDMY = Z + ZINC                                                   EILI3203
   20 CONTINUE                                                          EILI3204
      ZP = 1.0D0/ZDMY                                                   EILI3205
      T1 = CF(1)*ZP                                                     EILI3206
      S = T1                                                            EILI3207
      IF (ZP.LT.WDTOL) GO TO 40                                         EILI3208
      ZSQ = ZP*ZP                                                       EILI3209
      TST = T1*WDTOL                                                    EILI3210
      DO 30 K=2,22                                                      EILI3211
        ZP = ZP*ZSQ                                                     EILI3212
        TRM = CF(K)*ZP                                                  EILI3213
        IF (DABS(TRM).LT.TST) GO TO 40                                  EILI3214
        S = S + TRM                                                     EILI3215
   30 CONTINUE                                                          EILI3216
   40 CONTINUE                                                          EILI3217
      IF (ZINC.NE.0.0D0) GO TO 50                                       EILI3218
      TLG = DLOG(Z)                                                     EILI3219
      DGAMLN = Z*(TLG-1.0D0) + 0.5D0*(CON-TLG) + S                      EILI3220
      RETURN                                                            EILI3221
   50 CONTINUE                                                          EILI3222
      ZP = 1.0D0                                                        EILI3223
      NZ = INT(SNGL(ZINC))                                              EILI3224
      DO 60 I=1,NZ                                                      EILI3225
        ZP = ZP*(Z+FLOAT(I-1))                                          EILI3226
   60 CONTINUE                                                          EILI3227
      TLG = DLOG(ZDMY)                                                  EILI3228
      DGAMLN = ZDMY*(TLG-1.0D0) - DLOG(ZP) + 0.5D0*(CON-TLG) + S        EILI3229
      RETURN                                                            EILI3230
C                                                                       EILI3231
C                                                                       EILI3232
   70 CONTINUE                                                          EILI3233
      IERR=1                                                            EILI3234
      RETURN                                                            EILI3235
      END       