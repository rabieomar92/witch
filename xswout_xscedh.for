!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 1:30:33 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : XSWOUT.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
      SUBROUTINE XSCEDH(NG,CRD,FG,FB,CB,DC,ZA,VF,HI,ZF,ZS,AKE,BSQ       XSCE   2
     1                 ,IRFL,NIT,IER)                                   XSCE   3
C-Title  : XSCEDH Subroutine                                            XSCE   4
C-Purpose: Correction to homogenized x-sect by EDH method               XSCE   5
C-Description:                                                          XSCE   6
C-D  In a homogenized zone the simple flux and volume averaged cross    XSCE   7
C-D  sections conserve the reaction rates (and net leakage) but not     XSCE   8
C-D  the partial currents. As a consequence, a homogenized zone respondsXSCE   9
C-D  differently to the surrounding than the correspnding heterogeneous XSCE  10
C-D  zone. To correct for this deficiency, the Effective Diffusion      XSCE  11
C-D  Homogeneization (EDH) correction is applied: the cross sections areXSCE  12
C-D  scaled by the heterogeneity factor.                                XSCE  13
C-D    The coding assumes cylindircal geometry for normal cells (IRFL=0)XSCE  14
C-D  or slab geometry for reflector cells (IRFL=1) where CRD is the cellXSCE  15
C-D  radius or the reflector HALF-thickness, respectively.              XSCE  16
C-D    The cross sections in NG groups are assumed. The following       XSCE  17
C-D  parameters are specified: the averaged flux FG, the boundary flux  XSCE  18
C-D  FB (outer boundary for normal cells, inner boundary for the        XSCE  19
C-D  reflector), the outer boundary current (for reflector slabs only). XSCE  20
C-D  The buckling-squared BSQ may be specified to account for axial     XSCE  21
C-D  leakage. For cells with fissile material the K-eff AKE must        XSCE  22
C-D  also be given. The diffusion equation is solved analytically for   XSCE  23
C-D  the homogenized zone using boundary flux as the boundary condition.XSCE  24
C-D  Cross section scaling is performed iteratively (NIT is the number oXSCE  25
C-D  iterations) until reaction rates based on the calculated average   XSCE  26
C-D  flux match the reference values. On exit the flux discontinuity    XSCE  27
C-D  factors are stored in FG.                                          XSCE  28
C-D                                                                     XSCE  29
C-D  Error conditions:                                                  XSCE  30
C-D    The flag IER may take the following values on exit:              XSCE  31
C-D           0     - normal termination                                XSCE  32
C-D    100*MXIT+JER - no convergence on EDH iterations                  XSCE  33
C-D                                                                     XSCE  34
C-Version: 95/07 Rename TL to CB, describe CB,IRFL,NIT.                 XSCE  35
C-                                                                      XSCE  36
C-Extern.: Either: EISPACK/LINPACK/BLAS routines: RG,ZGEFA,ZGESL,ZBESI  XSCE  37
C-E        Or    : IMSL library routines: EIGRF, MMBZIN, LEQ2C          XSCE  38
C-E                                                                     XSCE  39
C-E  NOTE: Some statements in the routine routine are enclosed in labelsXSCE  40
C-E        to allow automatic code conversion, depending on the version XSCE  41
C-E        of the mathematical routines library. Currently available    XSCE  42
C-E        options include:                                             XSCE  43
C-E          "C***** IMSL"   to activate IMSL routines,                 XSCE  44
C-E          "C***** EILIBE" to activate EISPACK/LINPACK/BLAS routines. XSCE  45
C-E        The IMSL routines are shorter because they provide Bessel    XSCE  46
C-E        function routines of integer order, but the library is       XSCE  47
C-E        commercial and is not part of the package. On the other hand,XSCE  48
C-E        the Bessel function routines from the BLAS library allow     XSCE  49
C-E        selection of arbitrary order. There should be no difference  XSCE  50
C-E        in performance, regardless of which library routines are     XSCE  51
C-E        selected. The EISPACK/LINPACK/BLAS library routines are      XSCE  52
C-E        public domain software available through NETLIB and are      XSCE  53
C-E        provided with the CORD-2 package.                            XSCE  54
C-                                                                      XSCE  55
C-Author : A.Trkov, Institute "J.Stefan", Ljubljana, Slovenia, (1993)   XSCE  56
C-                                                                      XSCE  57
C* Scratch arrays                                                       XSCE  58
      PARAMETER       (MXI=20, MXR=100, MXD=2000)                       XSCE  59
      DIMENSION        IWO(MXI),RWO(MXR)                                XSCE  60
      DOUBLE PRECISION DWO(MXD)                                         XSCE  61
     1                ,CFR,CFI,FNR,FNI,FFR,FFI                          XSCE  62
     1                ,A1,A2,A3,A4,GR,GI,AR,AI, BIR(4),BII(4)           XSCE  63
      DIMENSION        DC(1),ZA(1),VF(1),HI(1),ZF(1),ZS(NG,1)           XSCE  64
     1                ,FB(1),FG(1),CB(1)                                XSCE  65
C* Max.No.of iter.for heter.fact.and D and convergence crit.            XSCE  66
      DATA MXIT,ERHT,ERDC/ 40, 1.E-5, 5.E-5/                            XSCE  67
C* Reserve space in double precision work-field                         XSCE  68
C*  LEM - equation matrix (size: ng*ng)                                 XSCE  69
C*  LEG - eigenvalue array - may be complex (size: ng*2)                XSCE  70
C*  LEV - eigenvector array - may be complex (size: ng*ng*2)            XSCE  71
C*  LSC - scratch array (size: (ng+2)*n)                                XSCE  72
C*  LEF - eigenfunction values on the boundary,                         XSCE  73
C*  LEI - integral of the eigenfunctions on the boundary,               XSCE  74
C*  LED - derivative of the eigenfunctions on the boundary.             XSCE  75
C*  LFB - boundary flux (boundary condition for diffusion solution),    XSCE  76
C*  LES - solution matrix array (size: ng*ng*2)                         XSCE  77
      LEM=1                                                             XSCE  78
      LEG=LEM+NG*NG                                                     XSCE  79
      LEV=LEG+NG*2                                                      XSCE  80
C***** IMSL                                                             XSCE  81
C     LSC=LEV+NG*NG*2                                                   XSCE  82
C     LEF=LSC+NG*(NG+2) *2                                              XSCE  83
C***** IMSL                                                             XSCE  84
C***** EILIBE                                                           XSCE  85
      LSC=LEV+NG*NG                                                     XSCE  86
      LEF=LSC+NG                                                        XSCE  87
C***** EILIBE                                                           XSCE  88
      LEI=LEF+NG*2                                                      XSCE  89
      LED=LEI+NG*2                                                      XSCE  90
      LFB=LED+NG*2                                                      XSCE  91
      LES=LFB+NG*2                                                      XSCE  92
      LBD=LES+NG*NG*2                                                   XSCE  93
      IF(LBD.GT.MXD) STOP 'Fatal Error: (XSCEDH) DWO capacity exceeded.'XSCE  94
C* Reserve space in single precission work-field                        XSCE  95
C*  LFG - initial average flux,                                         XSCE  96
C*  LHF - heterogeneity factors from previous iteration,                XSCE  97
      LFG=1                                                             XSCE  98
      LHF=LFG+NG                                                        XSCE  99
      LCU=LHF+NG                                                        XSCE 100
      LBR=LCU+NG                                                        XSCE 101
C* Check scratch array dimensions                                       XSCE 102
      IF(LBR.GT.MXR) STOP 'Fatal Error: (XSCEDH) RWO capacity exceeded.'XSCE 103
C        WRITE(6,694) '  Fg        Fb        Ja        Jb  CRD=',CRD     XSCE 104
      DO 14 IG=1,NG                                                     XSCE 105
      TLK=( -ZA(IG) - BSQ*DC(IG) ) * FG(IG)                             XSCE 106
      DO 12 JG=1,NG                                                     XSCE 107
      IF(AKE.GT.0.1) TLK=TLK + HI(IG)*VF(JG)*FG(JG)/AKE                 XSCE 108
      IF(JG .NE. IG) TLK=TLK - ZS(JG,IG)*FG(IG) + ZS(IG,JG)*FG(JG)      XSCE 109
   12 CONTINUE                                                          XSCE 110
C* Cell     : Boundary current = total leakage cur.                     XSCE 111
      IF(IRFL.NE.1) RWO(LCU-1+IG)=         TLK*CRD*0.5                  XSCE 112
C* Reflector: Cur.on inner face= Cur.on outer face - total leakage cur. XSCE 113
      IF(IRFL.EQ.1) RWO(LCU-1+IG)=CB(IG) - TLK*CRD*2.0                  XSCE 114
   14 CONTINUE                                                          XSCE 115
      DO 16 IG=1,NG                                                     XSCE 116
C        WRITE(6,691) FG(IG),FB(IG),RWO(LCU-1+IG),CB(IG)                 XSCE 117
C* Save initial flux                                                    XSCE 118
      RWO(LFG-1+IG) = FG(IG)                                            XSCE 119
      RWO(LHF-1+IG) = 1.                                                XSCE 120
      FG (      IG) = 1.                                                XSCE 121
   16 CONTINUE                                                          XSCE 122
C*                                                                      XSCE 123
C* Iterate for the flux heterogeneity factor                            XSCE 124
      MIT= 0                                                            XSCE 125
      NIT= 0                                                            XSCE 126
      JIT= 0                                                            XSCE 127
   20 JIT= JIT+1                                                        XSCE 128
      NIT= NIT+1                                                        XSCE 129
C* Generate the diffusion equation matrix system                        XSCE 130
   30 DO 34 IG=1,NG                                                     XSCE 131
      FS     = 0.                                                       XSCE 132
      IF(AKE.GT.0.1) FS = HI(IG)*VF(IG)/AKE                             XSCE 133
C* Diagonal term (absorption, buckling - group fission)                 XSCE 134
      IA     = LEM-1 + (IG-1)*NG + IG                                   XSCE 135
      DWO(IA)=          DBLE((ZA(IG)   -FS) / DC(IG) + BSQ)             XSCE 136
C* Diagonal term (group inscattering)                                   XSCE 137
C* Off-diagonal terms (- outscattering - group fission)                 XSCE 138
      DO 32 JG=1,NG                                                     XSCE 139
      IF(JG.EQ.IG) GO TO 32                                             XSCE 140
      JA = LEM-1 + (JG-1)*NG + IG                                       XSCE 141
      IF(AKE.GT.0.1) FS = HI(IG)*VF(JG)/AKE                             XSCE 142
      DWO(JA)=        - DBLE((ZS(IG,JG)+FS) / DC(IG)      )             XSCE 143
      DWO(IA)=DWO(IA) + DBLE( ZS(JG,IG)     / DC(IG)      )             XSCE 144
   32 CONTINUE                                                          XSCE 145
   34 CONTINUE                                                          XSCE 146
C* Calculate eigenvalues (at LEG) and eigenvectors (at LEV)             XSCE 147
      IJOB=1                                                            XSCE 148
C***** IMSL                                                             XSCE 149
C     CALL EIGRF(DWO(LEM),NG,NG,IJOB,DWO(LEG),DWO(LEV),NG,DWO(LSC),IER) XSCE 150
C     IF(IER.NE.0)                                                      XSCE 151
C    1WRITE(6,693) ' Warning: No convergence in EDH calculation.'       XSCE 152
C***** IMSL                                                             XSCE 153
C***** EILIBE                                                           XSCE 154
      IF( NG.GT.MXI) STOP 'Fatal Error: (XSCEDH) IWO capacity exceeded.'XSCE 155
      CALL RG(NG,NG,DWO(LEM),DWO(LEG),DWO(LEG+NG)                       XSCE 156
     1       ,IJOB,DWO(LEV),IWO,DWO(LSC),IER)                           XSCE 157
      IF(IER.NE.0)                                                      XSCE 158
     1WRITE(6,*) 'Warning: No convergence in EDH calculation.'          XSCE 159
C***** EILIBE                                                           XSCE 160
      IF(IER.NE.0) GO TO 92                                             XSCE 161
C* Green's functions of the diffusion operator, derivative and integral XSCE 162
      IF(IRFL.EQ.1) GO TO 50                                            XSCE 163
C*                                                                      XSCE 164
C* CASE: cylindrical geometry                                           XSCE 165
      DO 44 IG=1,NG                                                     XSCE 166
C***** IMSL                                                             XSCE 167
C     A1 = DWO(LEG+2*IG-2)                                              XSCE 168
C     A2 = DWO(LEG+2*IG-1)                                              XSCE 169
C***** IMSL                                                             XSCE 170
C***** EILIBE                                                           XSCE 171
      A1 = DWO(LEG-1+IG)                                                XSCE 172
      A2 = DWO(LEG-1+IG+NG)                                             XSCE 173
C***** EILIBE                                                           XSCE 174
      A3 = SQRT( A1*A1 + A2*A2 )                                        XSCE 175
      GR = SQRT( 0.5D0* ( A1 + A3) )                                    XSCE 176
      GI = SQRT( 0.5D0* (-A1 + A3) )                                    XSCE 177
      IF(A2.LT.0) GI=-GI                                                XSCE 178
      AR = GR * DBLE(CRD)                                               XSCE 179
      AI = GI * DBLE(CRD)                                               XSCE 180
C***** IMSL                                                             XSCE 181
C     CALL MMBZIN (AR,AI,2,BIR,BII,IER)                                 XSCE 182
C     IF(IER.NE.0)                                                      XSCE 183
C    1WRITE(6,693) ' XSCEDH - MMBZIN WARNING            IER=',IER       XSCE 184
C***** IMSL                                                             XSCE 185
C***** EILIBE                                                           XSCE 186
      CALL ZBESI  (AR,AI, 0.D0, 1, 2,BIR,BII,NZER,IER)                  XSCE 187
      IF(IER.NE.0)                                                      XSCE 188
     1WRITE(6,693) ' XSCEDH - ZBESI  WARNING            IER=',IER       XSCE 189
C***** EILIBE                                                           XSCE 190
C     IF(IER.NE.0) STOP 'XSCEDH ERROR computing bessel functions'       XSCE 191
C*       boundary function value                                        XSCE 192
      DWO(LEF-2+IG*2) = BIR(1)                                          XSCE 193
      DWO(LEF-1+IG*2) = BII(1)                                          XSCE 194
C*       boundary derivative value                                      XSCE 195
      DWO(LED-2+IG*2) = BIR(2)*GR - BII(2)*GI                           XSCE 196
      DWO(LED-1+IG*2) = BII(2)*GR + BIR(2)*GI                           XSCE 197
C*       boundary integral value                                        XSCE 198
      DWO(LEI-2+IG*2) = (BIR(2)*GR + BII(2)*GI) / (GR*GR + GI*GI)       XSCE 199
      DWO(LEI-1+IG*2) = (BII(2)*GR - BIR(2)*GI) / (GR*GR + GI*GI)       XSCE 200
   44 CONTINUE                                                          XSCE 201
      GO TO 60                                                          XSCE 202
C*                                                                      XSCE 203
C* CASE: slab geometry with zero-flux b.c. on rhs.                      XSCE 204
   50 DO 54 IG=1,NG                                                     XSCE 205
      DWO(LEF-1+IG*2) = 0.                                              XSCE 206
      DWO(LED-1+IG*2) = 0.                                              XSCE 207
      DWO(LEI-1+IG*2) = 0.                                              XSCE 208
C***** IMSL                                                             XSCE 209
C     EJG     = DWO(LEG+2*IG-2)                                         XSCE 210
C***** IMSL                                                             XSCE 211
C***** EILIBE                                                           XSCE 212
      EJG     = DWO(LEG+  IG-1)                                         XSCE 213
C***** EILIBE                                                           XSCE 214
      IF(EJG.LT.0) GO TO 52                                             XSCE 215
        EJG     = SQRT( EJG)                                            XSCE 216
        ALM     = EJG*CRD                                               XSCE 217
        DWO(LEF-2+IG*2)=  COSH(ALM) * 2.0                               XSCE 218
        DWO(LED-2+IG*2)=(-SINH(ALM)                                     XSCE 219
     1                  - COSH(ALM)*COSH(ALM)/SINH(ALM) )* EJG          XSCE 220
        DWO(LEI-2+IG*2)=  SINH(ALM) / EJG                               XSCE 221
      GO TO 54                                                          XSCE 222
   52   EJG     = SQRT(-EJG)                                            XSCE 223
        ALM     = EJG*CRD                                               XSCE 224
        DWO(LEF-2+IG*2)=  COS (ALM) * 2.0                               XSCE 225
        DWO(LED-2+IG*2)=( SIN (ALM)                                     XSCE 226
     1                  - COS (ALM)* COS (ALM)/SIN (ALM))* EJG          XSCE 227
        DWO(LEI-2+IG*2)=  SIN (ALM) / EJG                               XSCE 228
   54 CONTINUE                                                          XSCE 229
C* Apply boundary conditions on boundary fluxes (solve a system of eq.) XSCE 230
   60 DO 65 IG=1,NG                                                     XSCE 231
      DWO(LFB-2+IG*2) = DBLE(FB(IG))                                    XSCE 232
      DWO(LFB-1+IG*2) = 0.D0                                            XSCE 233
      DO 65 JG=1,NG                                                     XSCE 234
C***** IMSL                                                             XSCE 235
C     JEV = LEV+2*(JG-1)*NG+2*IG-2                                      XSCE 236
C     CFR = DWO(JEV  )                                                  XSCE 237
C     CFI = DWO(JEV+1)                                                  XSCE 238
C***** IMSL                                                             XSCE 239
C***** EILIBE                                                           XSCE 240
      JEV = LEV+  (JG-1)*NG+  IG-1                                      XSCE 241
      CFR = DWO(JEV  )                                                  XSCE 242
      CFI = 0.D0                                                        XSCE 243
      IF(DWO(LEG-1+JG+NG)) 62,64,63                                     XSCE 244
   62 CFI =-CFR                                                         XSCE 245
      CFR = DWO(JEV-NG)                                                 XSCE 246
      GO TO 64                                                          XSCE 247
   63 CFI = DWO(JEV+NG)                                                 XSCE 248
   64 CONTINUE                                                          XSCE 249
C***** EILIBE                                                           XSCE 250
      JES = LES+2*(JG-1)*NG+2*IG-2                                      XSCE 251
      JEF = LEF-2+JG*2                                                  XSCE 252
      FNR = DWO(JEF  )                                                  XSCE 253
      FNI = DWO(JEF+1)                                                  XSCE 254
      DWO(JES  ) = CFR * FNR - CFI * FNI                                XSCE 255
      DWO(JES+1) = CFI * FNR + CFR * FNI                                XSCE 256
   65 CONTINUE                                                          XSCE 257
C***** IMSL                                                             XSCE 258
C     CALL LEQ2C(DWO(LES),NG,NG,DWO(LFB),1,NG,0,DWO(LSC),DWO(LEM),IER)  XSCE 259
C     IF(IER.NE.0)                                                      XSCE 260
C    1WRITE(6,693) ' XSCEDH - LEQ2C  ERROR              IER=',IER       XSCE 261
C     IF(IER.NE.0) GO TO 92                                             XSCE 262
C***** IMSL                                                             XSCE 263
C***** EILIBE                                                           XSCE 264
      CALL ZGEFA(DWO(LES),NG,NG,IWO,IER)                                XSCE 265
      IF(IER.NE.0)                                                      XSCE 266
     1WRITE(6,693) ' XSCEDH - ZGEFA  ERROR              IER=',IER       XSCE 267
      IF(IER.NE.0) GO TO 92                                             XSCE 268
      CALL ZGESL(DWO(LES),NG,NG,IWO,DWO(LFB), 0)                        XSCE 269
C***** EILIBE                                                           XSCE 270
C* Calculate average group flux and boundary current                    XSCE 271
      DO 78 IG=1,NG                                                     XSCE 272
      A1=0.D0                                                           XSCE 273
      A2=0.D0                                                           XSCE 274
      DO 76 JG=1,NG                                                     XSCE 275
      JFB = LFB-2+JG*2                                                  XSCE 276
      JEI = LEI-2+JG*2                                                  XSCE 277
      JED = LED-2+JG*2                                                  XSCE 278
C***** IMSL                                                             XSCE 279
C     JEV = LEV+2*(JG-1)*NG+2*IG-2                                      XSCE 280
C     CFR = DWO(JEV  )                                                  XSCE 281
C     CFI = DWO(JEV+1)                                                  XSCE 282
C***** IMSL                                                             XSCE 283
C***** EILIBE                                                           XSCE 284
      JEV = LEV+  (JG-1)*NG+  IG-1                                      XSCE 285
      CFR = DWO(JEV  )                                                  XSCE 286
      CFI = 0.D0                                                        XSCE 287
      IF(DWO(LEG-1+JG+NG)) 72,74,73                                     XSCE 288
   72 CFI =-CFR                                                         XSCE 289
      CFR = DWO(JEV-NG)                                                 XSCE 290
      GO TO 74                                                          XSCE 291
   73 CFI = DWO(JEV+NG)                                                 XSCE 292
   74 CONTINUE                                                          XSCE 293
C***** EILIBE                                                           XSCE 294
      FFR = DWO(JFB  )                                                  XSCE 295
      FFI = DWO(JFB+1)                                                  XSCE 296
C* Determine average flux                                               XSCE 297
      FNR= DWO(JEI  )                                                   XSCE 298
      FNI= DWO(JEI+1)                                                   XSCE 299
      A3 = CFR * FNR - CFI * FNI                                        XSCE 300
      A4 = CFI * FNR + CFR * FNI                                        XSCE 301
      A1 = A1  +(FFR*A3 - FFI*A4)                                       XSCE 302
C* Determine boundary current                                           XSCE 303
      FNR= DWO(JED  )                                                   XSCE 304
      FNI= DWO(JED+1)                                                   XSCE 305
      A3 = CFR * FNR - CFI * FNI                                        XSCE 306
      A4 = CFI * FNR + CFR * FNI                                        XSCE 307
      A2 = A2  -(FFR*A3 - FFI*A4)                                       XSCE 308
   76 CONTINUE                                                          XSCE 309
      RV =1.0/CRD                                                       XSCE 310
      IF(IRFL.NE.1) RV=2.0*RV                                           XSCE 311
      FG(IG)=A1*RV                                                      XSCE 312
      CB(IG)=A2*DC(IG)                                                  XSCE 313
   78 CONTINUE                                                          XSCE 314
C* Update the cross sections to match the reaction rates                XSCE 315
      ICON     = 0                                                      XSCE 316
      ICUR     = 0                                                      XSCE 317
      DO 80 IG=1,NG                                                     XSCE 318
      CURF     =        CB(IG) / RWO(LCU-1+IG)                          XSCE 319
      HETF     = RWO(LFG-1+IG) / FG(IG)                                 XSCE 320
      FACT     = HETF/RWO(LHF-1+IG)                                     XSCE 321
      IF(ABS(FACT-1.).GT.ERHT) ICON=1                                   XSCE 322
      IF(ABS(CURF-1.).GT.ERDC) ICUR=1                                   XSCE 323
      RWO(LHF-1+IG) = HETF                                              XSCE 324
      FG(IG)   = HETF                                                   XSCE 325
      CB(IG)   = CURF                                                   XSCE 326
      IF(IRFL.NE.1)                                                     XSCE 327
     1DC(IG)   = DC(IG)   * FACT                                        XSCE 328
C    1DC(IG)   = DC(IG)   / FACT                                        XSCE 329
      ZA(IG)   = ZA(IG)   * FACT                                        XSCE 330
      VF(IG)   = VF(IG)   * FACT                                        XSCE 331
      ZF(IG)   = ZF(IG)   * FACT                                        XSCE 332
      DO 80 JG=1,NG                                                     XSCE 333
      ZS(JG,IG)= ZS(JG,IG)* FACT                                        XSCE 334
   80 CONTINUE                                                          XSCE 335
C* Re-iterate on heterogeneity factor if necessary                      XSCE 336
      IF(ICON.EQ.0) GO TO 82                                            XSCE 337
      IF(JIT.LT.MXIT) GO TO 20                                          XSCE 338
      IER=-NIT                                                          XSCE 339
C* If slab, iterate for diffusion constants                             XSCE 340
   82 IF(IRFL.NE.1) RETURN                                              XSCE 341
      MIT=MIT+1                                                         XSCE 342
      IF(ICUR.EQ.0) RETURN                                              XSCE 343
      DO 91 IG=1,NG                                                     XSCE 344
      FF=CB(IG)                                                         XSCE 345
C* Guard against unconverged current ratio                              XSCE 346
      IF(FF.LT. 0.9) FF=0.9                                             XSCE 347
      IF(FF.GT. 1.1) FF=1.1                                             XSCE 348
C...Try different optins to accelerate convergence on Diffusion const.  XSCE 349
C     FF=1.+ 3.*(FF-1.)                                                 XSCE 350
      OMG=1.5                                                           XSCE 351
      IF(RWO(LCU-1+IG) .GT. 0 .AND. CB(IG).LT.0.0) OMG=-1.              XSCE 352
      IF(RWO(LCU-1+IG) .GT. 0 .AND. CB(IG).LT.0.5) OMG= 1.              XSCE 353
      IF(RWO(LCU-1+IG) .LT. 0) OMG=-OMG                                 XSCE 354
      FF=FF**OMG                                                        XSCE 355
      DC(IG)=DC(IG)/FF                                                  XSCE 356
   91 CONTINUE                                                          XSCE 357
      JIT=0                                                             XSCE 358
      IF(MIT.LT.MXIT) GO TO 20                                          XSCE 359
C* On convergence problems set IER to -MIT                              XSCE 360
      IER=IER-MIT*100                                                   XSCE 361
      RETURN                                                            XSCE 362
C* No analytic solution to the homogenized diffusion equation           XSCE 363
   92 IER=-4999-NIT                                                     XSCE 364
      RETURN                                                            XSCE 365
C... Formats for scratch printout                                       XSCE 366
  691 FORMAT(2F10.7,1P,6E10.3)                                          XSCE 367
  693 FORMAT(A40,I6)                                                    XSCE 368
  694 FORMAT(A40,1P,E10.3)                                              XSCE 369
      END                 
  