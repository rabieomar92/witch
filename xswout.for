*DECK XSWOUT                                                            XSWO   1
C      PROGRAM XSWOUT                                                   XSWO   2
       SUBROUTINE XSWOUT(PING, LGR, PIJH, pcWITCHInp, LAYERID)
       CHARACTER*50 PCLG
       CHARACTER*(*) pcWITCHInp
       INTEGER      PING, PIJH, LAYERID

C-Title  : XSWOUT driver program for XSCWOU                             XSWO   3
C-Purpose: Extract cross sections from WIMS output .OUT                 XSWO   4
C-M                                                                     XSWO   5
C-M  Manual for Program XSWOUT                                          XSWO   6
C-M  =========================                                          XSWO   7
C-M  Wims output is read and cell-averaged cross sections are           XSWO   8
C-M  calculated. Homogenization can be done using the Effective         XSWO   9
C-M  Diffusion Homogenization method (EDH), or else the simple flux     XSWO  10
C-M  and volume weighting can be applied.                               XSWO  11
C-M    The leakage edit of WIMS is applicable only for fuel-bearing     XSWO  12
C-M  regions and is incorrect if one tries to obtain homogenized cross  XSWO  13
C-M  sections for non-fuelled cells, such as water channels or burnable XSWO  14
C-M  poison cells. For this reason the Cell-edit in WIMS is processed,  XSWO  15
C-M  in which the cross sections are homogenized by flux and volume     XSWO  16
C-M  weighting.                                                         XSWO  17
C-M                                                                     XSWO  18
C-M  Definitions:                                                       XSWO  19
C-M    TRANSPORT CROSS SECTION is reconstructed from the absorption     XSWO  20
C-M  cross section and the scattering matrix. The diffusion constant    XSWO  21
C-M  is defined (D = 1 / 3.SigmaTransport).                             XSWO  22
C-M    ABSORPTION CROSS SECTION is defined as the sum of the capture    XSWO  23
C-M  and fission cross sections.                                        XSWO  24
C-M    FISSION YIELD is the product of the fission cross section and    XSWO  25
C-M  the number of neutrons per fission.                                XSWO  26
C-M    NEUTRON SPECTRUM GROUP FRACTION is the fraction of the neutrons  XSWO  27
C-M  born in a particular group in fission.                             XSWO  28
C-M    FISSION ENERGY PER NEUTRON FLUX is the fission cross section     XSWO  29
C-M  scaled with the energy released per fission, such that the         XSWO  30
C-M  product with the neutron flux gives directly the power.            XSWO  31
C-M    SCATTERING MATRIX is given in terms of the cross sections        XSWO  32
C-M  for scattering from a particular group into all other groups.      XSWO  33
C-M    CONDENSATION is performed using the cell-average flux from the   XSWO  34
C-M  Cell-edit of WIMS.                                                 XSWO  35
C-M    EDH method is applied by default to obtain effective diffusion   XSWO  36
C-M  few-group cross sections. Cylindrical 1D geometry is assumed for   XSWO  37
C-M  the EDH correction. The cell boundary flux value is approximated   XSWO  38
C-M  by the average flux in the outermost region of the cell. The       XSWO  39
C-M  exception is the case when a FREE command is encountered on WIMS   XSWO  40
C-M  input. It is assumed that reflector constants are to be generated. XSWO  41
C-M  Boundary flux is approximated by the innermost region of the cell  XSWO  42
C-M  and a zero-flux boundary condition is taken on the outer boundary  XSWO  43
C-M  of the equivalent homogenized problem.                             XSWO  44
C-M                                                                     XSWO  45
C-M  Instructions:                                                      XSWO  46
C-M  The following input parameters are read from input:                XSWO  47
C-M    FLNX  - output cross sections filename                           XSWO  48
C-M    NG,NP - number of macrogroups NG and the number of additional    XSWO  49
C-M            cross section parameters NP. The parameters are read     XSWO  50
C-M            from 10-column fields, but blanks are ignored, so the    XSWO  51
C-M            entries need not be right-justified.                     XSWO  52
C-M              By default NP=1 to imply the EDH correction to the     XSWO  53
C-M            cross sections and the printout of the EDH flux          XSWO  54
C-M            scaling factor. NP>1 may be entered to reserve space     XSWO  55
C-M            for the P1 cross section components (for consistency     XSWO  56
C-M            with cross sections from other sources), although        XSWO  57
C-M            these are all zero in XSWOUT.                            XSWO  58
C-M              To force suppression of the EDH correction,            XSWO  59
C-M            enter negative NG.                                       XSWO  60
C-M              To apply the EDH method on the cross sections          XSWO  61
C-M            but to suppress the printout of the EDH factor           XSWO  62
C-M            set NP<0.                                                XSWO  63
C-M    NR(g) - the list containing last groups of the transport         XSWO  64
C-M            group structure, belonging to the few-group set          XSWO  65
C-M            (i.e.same type of specification as for the FEWGROUP      XSWO  66
C-M            command in WIMS). If blank is entered for the previous   XSWO  67
C-M            input request (NG, NP), a 10-group set is assumed and    XSWO  68
C-M            no request for the group boundaries is issued. By        XSWO  69
C-M            default, groups (3,5,6,10,14,21,25,32) of the WIMS       XSWO  70
C-M            transport groups are used.                               XSWO  71
C-M    FLNO -  WIMS output filename to be processed. Requests for       XSWO  72
C-M            FLNO are repeated so that several WIMS files can be      XSWO  73
C-M            processed in sequence and homogenized cross sections     XSWO  74
C-M            written on the same output cross sections file.          XSWO  75
C-M            Processing is terminated when a blank filename is        XSWO  76
C-M            encountered.                                             XSWO  77
C-M                                                                     XSWO  78
C-M  Output cross section format:                                       XSWO  79
C-M    The cross sections are printed in 10-column format which is      XSWO  80
C-M  compatible with the GNOMER and BINODE codes. The cross sections    XSWO  81
C-M  are sorted in following order for each group:                      XSWO  82
C-M   - Diffusion constant [cm],                                        XSWO  83
C-M   - Absorption cross section [1/cm],                                XSWO  84
C-M   - Fission yield [1/cm],                                           XSWO  85
C-M   - Neutron spectrum group fraction,                                XSWO  86
C-M   - Fission cross section [1/cm] (or fission energy per unit        XSWO  87
C-M     neutron flux [pJ/cm], when burnup printout is available),       XSWO  88
C-M   - Scattering matrix elements [1/cm],                              XSWO  89
C-M   - EDH heterogeneity factor (only when EDH correction to           XSWO  90
C-M     the cross sections is specified and the number of groups        XSWO  91
C-M     is not 3 or 11).                                                XSWO  92
C-M  The output record is limited to 80-columns. If more than three     XSWO  93
C-M  groups are requested, the cross sections for each group occupy     XSWO  94
C-M  more than one record. Similarly, more than two records are         XSWO  95
C-M  needed if the number of groups is greater than 11.                 XSWO  96
C-M                                                                     XSWO  97
C-M  Error conditions:                                                  XSWO  98
C-M  The EDH iterations may terminate with an error flag, which can be  XSWO  99
C-M  interpreted as follows:                                            XSWO 100
C-M           0 - normal termination.                                   XSWO 101
C-M   -100*MXIT - no convergence on the diffusion constant in EDH       XSWO 102
C-M               iterations (MXIT=40 at present).                      XSWO 103
C-M  -(5000+NIT)- solution is singular at EDH iteration NIT.            XSWO 104
C-M                                                                     XSWO 105
C-M  References:                                                        XSWO 106
C-M  [1] A.Trkov, M.Ravnik: Effective Diffusion Homogenization of Cross XSWO 107
C-M      Sections for Pressurized Water Reactor Core Calculations,      XSWO 108
C-M      Nucl.Sci.Eng. Vol.116, No.2, pp.86-95, Feb.1994.               XSWO 109
C-M                                                                     XSWO 110
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (1993)  XSWO 111
C-                                                                      XSWO 112
      CHARACTER*40  FLNX,FLNM                                           XSWO 113
      INTEGER       NFI, IFLAG, NWIMSINPUT, RDLENF, RW
      CHARACTER*40  CAWIMSINPUT(1000)
      CHARACTER*132 CTEXT
      CHARACTER*10  CFILEWIMSHEADER
      INTEGER       LGR(PING)
      DIMENSION     SCR(12000)                                          XSWO 114
     1             ,DC(80), ZA(80),VF(80),HI(80),ZF(80),ZS(6400)        XSWO 115
C* Logical file unit numbers                                            XSWO 116
      DATA  LOU,LKB,LTT/ 1, 5, 6 /                                      XSWO 117
C      DATA  NG1,LGR/ 10, 3, 5, 6, 10, 14, 21, 25, 32, 72*0/             XSWO 118

!     Set the triga core layer ID 
      print*, ' Running ROUTINE-D.'      
      NWIMSINPUT=0
      OPEN(UNIT=50, FILE='WIMS-OUT.$$$',STATUS='UNKNOWN',ERR=911)
      GOTO 811
911   PRINT*, ' Fatal Error: Header file WITCH-OUT.$$$ is not found.'
      CLOSE(UNIT=50)
      STOP
811   CONTINUE

      call RDSEARCH(50, 0, '   WIFLNM ',10, CTEXT, IFLAG)
      IF (IFLAG .ne. 0) THEN
         GOTO 101
      ENDIF
      NWIMSINPUT = NWIMSINPUT + 1
      CAWIMSINPUT(NWIMSINPUT) = CTEXT(11:RDLENF(CTEXT))
      GOTO 811
101   CONTINUE
      NM  = 1                                                           XSWO 119
      IH  = 1                                                           XSWO 120
      IF(LAYERID .EQ. 0) THEN
      FLNX = trim(pcWITCHInp) // '-XSOUT.TXS'
      ELSE
      WRITE(FLNX,'(I10.10,A4)') LAYERID, '.TXS'
      ENDIF
      NG1 = PING
      JH = PIJH
      IF(JH .GT.0) IH=JH                                                XSWO 128
      IF(NG1.LT.0) IH=0                                                 XSWO 129
      NG1=ABS(NG1)                                                      XSWO 130
C     READ (PCLG,*) (LGR(J),J=1,NG1)                                       XSWO 132
   12 NG  = LGR(NG1)                                                    XSWO 133
C***** VAX                                                              XSWO 134
C     OPEN(UNIT=LOU,FILE=FLNX,STATUS='UNKNOWN',CARRIAGECONTROL='LIST')  XSWO 135
C***** VAX                                                              XSWO 136
C***** STANDARD                                                         XSWO 137
      OPEN(UNIT=LOU,FILE=FLNX,STATUS='UNKNOWN')                         XSWO 138
C***** STANDARD                                                         XSWO 139
      RW = 1
      print*,' Preparing TRIGA cross section file (TXS). Please wait...'
      WRITE(LOU,'(6A10,2I10.9)') (' 000000000',I=1,6), NWIMSINPUT,
     &    LAYERID
   20 IF (RW .GT. NWIMSINPUT) GOTO 90
      WRITE(LOU,'(A10,I10.9,I10.9)') ' 111111111',RW,NG1
      FLNM = CAWIMSINPUT(RW)
c      PRINT'(3A,$)', 'Processing ', TRIM(FLNM), '. '
      BACKSPACE 6
      IF(FLNM.EQ.'                                        ') GO TO 90   XSWO 142
      CALL XSCWOU(FLNM,NM,NG,NG1,LGR,DC,ZA,VF,HI,ZF,ZS,SCR,IH,LTT)      XSWO 143
      DO 40 I=1,NG1                                                     XSWO 144
      JG=(I-1)*NG1*NM                                                   XSWO 145
      IF(IH .LE.0) GO TO 22                                             XSWO 146
C* Print x-sect with EDH factor                                         XSWO 147
      WRITE(LOU,691)                                                    XSWO 148
     1   DC(I),ZA(I),VF(I),HI(I),ZF(I),(ZS(J+JG),J=1,NG1),SCR(I)        XSWO 149
     2 ,(0.,J=2,IH)                                                     XSWO 150
      GO TO 40                                                          XSWO 151
C* Print x-sect without EDH factor                                      XSWO 152
   22 WRITE(LOU,691)                                                    XSWO 153
     1   DC(I),ZA(I),VF(I),HI(I),ZF(I),(ZS(J+JG),J=1,NG1)               XSWO 154
   40 CONTINUE                                                          XSWO 155
      RW = RW + 1
      GO TO 20                                                          XSWO 156
      print*, ' ROUTINE-D completed.'
   90 WRITE(LOU,'(6A10,2I10.9)') (' 999999999',I=1,6), NWIMSINPUT,
     & LAYERID
      CLOSE(unit=50)
      RETURN                                                            XSWO 157
  690 FORMAT(2A40)                                                      XSWO 158
  691 FORMAT(8F10.7)                                                    XSWO 159
  692 FORMAT(BN,2I10.10)                                                XSWO 160
      CLOSE(UNIT=50)
      END                                                               XSWO 163
      SUBROUTINE XSCWOU(FLNM,NM,NG,NG1,LGR,DC,ZA,VF,HI,ZF,ZS,SCR,IH,LTT)XSWO 164
C-Title  : XSCWOU Subroutine                                            XSWO 165
C-Purpose: Read and condense cross sections from WIMS output file       XSWO 166
C-Description:                                                          XSWO 167
C-D  Open the WIMS output file FLNM, read cross sections (NG groups),   XSWO 168
C-D  perform condensation into NG1 macro-groups where LGR contains      XSWO 169
C-D  the number of the last WIMS group within each macrogroup.          XSWO 170
C-D    Cross sections and the neutron spectrum are extracted from the   XSWO 171
C-D  cell edit (rather then the leakage edit which is inappropriate     XSWO 172
C-D  in the case of non-fuel cells). The relevant FORTRAN statements    XSWO 173
C-D  which enable cross section retrieval from the leakage edit are     XSWO 174
C-D  coded but inactive.                                                XSWO 175
C-D    The multiplication factor and buckling are read from the         XSWO 176
C-D  leakage edit.                                                      XSWO 177
C-D    The output cross sections on coarse macrogroups are: diffusion   XSWO 178
C-D  constant (DC), absorption cross section (ZA=capture+fission),      XSWO 179
C-D  fission yield (VF=nu*fission),fission spectrum fraction (HI),      XSWO 180
C-D  fission cross section (ZF), scattering matrix (ZS). When fission   XSWO 181
C-D  cross section is not directly present on WIMS output, it is        XSWO 182
C-D  reconstructed from the fission yield, by assuming some VN value    XSWO 183
C-D  (No.of neutrons per fission), interpolated between 2.55 and 2.44   XSWO 184
C-D  by group number (ref. to VN(I) Inline function).                   XSWO 185
C-D    If cross sections on output need to be sorted by material        XSWO 186
C-D  index, then NM is the number of materials (ref. to DIMENSION       XSWO 187
C-D  statement), otherwise set NM to 1                                  XSWO 188
C-D    File FLNM on unit 2 is open and closed internally each           XSWO 189
C-D  time the XSCWOU routine is called.                                 XSWO 190
C-D    Error messages or warnings are printed on unit-LTT.              XSWO 191
C-D                                                                     XSWO 192
C-Author : A.Trkov, Institute "J.Stefan", Ljubljana (1991)              XSWO 193
C-Version: 93/3 - Implementation of multigroup EDH homogenization.      XSWO 194
C-                                                                      XSWO 195
      CHARACTER*40  FLNM                                                XSWO 196
      CHARACTER*132 RECI                                                XSWO 197
      DIMENSION     LGR(1),   DC(NM,1),ZA(NM,1),VF(NM,1),HI(NM,1)       XSWO 198
     1             ,SCR(NG,7),ZF(NM,1),ZS(NM,NG1,1)                     XSWO 199
C* Test arrays to extract "nue" from burnup edit                        XSWO 200
      DIMENSION     NFG(4) ,ZFI(4)                                      XSWO 201
C* Initialize constants                                                 XSWO 202
      DATA PI / 3.1415926 /                                             XSWO 203
C* Inline function for the number of neutrons per fission               XSWO 204
      VN(II,NN) = 2.55 - 0.11 *FLOAT(II-1)/FLOAT(MAX(1,NN-1))           XSWO 205
C* Initialize variables                                                 XSWO 206
      NZF =0                                                            XSWO 207
      IRFL=0                                                            XSWO 208
      J2  =0                                                            XSWO 209
      RTH =0.                                                           XSWO 210
C* Open the WIMS optput file                                            XSWO 211
      OPEN(UNIT=2,FILE=FLNM,STATUS='OLD')                               XSWO 212
C* Skip the initial printout                                            XSWO 213
   10 READ (2,191,END=94,ERR=94) RECI                                   XSWO 214

C* Check for reflector data                                             XSWO 215
      IF( (RECI(1: 5).EQ.' FREE') .OR.                                  XSWO 216
     1    (RECI(6:10).EQ.' FREE')      ) IRFL=1                         XSWO 217
C     IF(RECI(1:9).EQ.' * REFTHI') READ (RECI,195) RTH                  XSWO 218
C* Process the region and cell edit                                     XSWO 219
      IF(RECI(1:20).NE.'1FEW-GROUP REGIONAL ') GO TO 10                 XSWO 220
      IRG=0                                                             XSWO 221
      DX0=0                                                             XSWO 222
   20 READ (2,191,END=94) RECI                                          XSWO 223
      IF(RECI(1:20).EQ.' CELL             VO') GO TO 25                 XSWO 224
      IF(RECI(1:7) .NE.'0REGION')              GO TO 20                 XSWO 225
C* Region edit - read the boundary flux and current                     XSWO 226
      IRG=IRG+1                                                         XSWO 227
      READ (2,191,END=94)                                               XSWO 228
      READ (2,191,END=94)                                               XSWO 229
      DO 22 JG=1,NG                                                     XSWO 230
      READ (2,197,ERR=92) DFI,ABI,FYI,RIF,RAF                           XSWO 231
C* Save neutron fission yield to calculate nue                          XSWO 232
      IF(IRG.EQ.1) SCR(JG,7) =FYI                                       XSWO 233
C* Boundary flux - slab=first reg., cell=last reg.                      XSWO 234
      IF(IRG.EQ.1 .OR. IRFL.NE.1) SCR(JG,2) =RAF                        XSWO 235
C* Outer boundary current (slab case: Jin = Flux/4 - Curr/2 = 0 )       XSWO 236
      SCR(JG,4)=0.                                                      XSWO 237
      IF(IRFL.EQ.1) SCR(JG,4)=RAF*0.5                                   XSWO 238
   22 CONTINUE                                                          XSWO 239
      GO TO 20                                                          XSWO 240
C* Cell edit - read volume, cross sections and flux spectrum            XSWO 241
   25 READ (RECI,196) VOL                                               XSWO 242
      IF(IRFL.EQ.0) CRD = SQRT(VOL/PI)                                  XSWO 243
      IF(IRFL.EQ.1) CRD = 0.5*VOL                                       XSWO 244
      READ (2,191,END=94)                                               XSWO 245
      READ (2,191,END=94)                                               XSWO 246
      DO 26 JG=1,NG                                                     XSWO 247
      READ (2,197,ERR=92) DFI,ABI,FYI,RIF,RAF                           XSWO 248
      SCR(JG,3) =RAF                                                    XSWO 249
      SCR(JG,5) =ABI                                                    XSWO 250
      SCR(JG,6) =FYI                                                    XSWO 251
      IF(SCR(JG,7).LT.1.E-6) SCR(JG,7)=VN(JG,NG-1)                      XSWO 252
   26 CONTINUE                                                          XSWO 253
C* Read the scattering matrix                                           XSWO 254
   30 READ (2,191,END=94) RECI                                          XSWO 255
      IF(RECI(1:20).NE.'0CELL AVERAGE SCATTE') GO TO 30                 XSWO 256
      J2 =0                                                             XSWO 257
   32 DO 34 I=1,5                                                       XSWO 258
   34 READ (2,191)                                                      XSWO 259
      J1=J2+1                                                           XSWO 260
      J2=MIN( J1+9, NG)                                                 XSWO 261
      DO 38 JG=1,NG                                                     XSWO 262
      READ (2,192,ERR=92) (SCR(J,8+JG),J=J1,J2)                         XSWO 263
   38 CONTINUE                                                          XSWO 264
      IF(J2.LT.NG) GO TO 32                                             XSWO 265
C* Process the fission spectrum printout                                XSWO 266
   40 READ (2,191,END=94) RECI                                          XSWO 267
      IF(RECI(1:20).NE.'           GROUP    ') GO TO 40                 XSWO 268
C* Read and condense the fission spectrum                               XSWO 269
      SPT= 0.                                                           XSWO 270
      J2 = 0                                                            XSWO 271
      DO 48 IG=1,NG1                                                    XSWO 272
      SP = 0.                                                           XSWO 273
      J1 = J2+1                                                         XSWO 274
      J2 = LGR(IG)                                                      XSWO 275
      DO 46 JG=J1,J2                                                    XSWO 276
      READ (2,194,ERR=92) SA,SPI                                        XSWO 277
      SP = SP+SPI                                                       XSWO 278
   46 CONTINUE                                                          XSWO 279
      HI(1,IG)=SP                                                       XSWO 280
      SPT     =SPT+SP                                                   XSWO 281
   48 CONTINUE                                                          XSWO 282
C* Process the leakage edit - read bucklings and K-eff                  XSWO 283
      BSQ = 0.                                                          XSWO 284
      AKE = 0.                                                          XSWO 285
   50 READ (2,191,END=70) RECI                                          XSWO 286
      IF(RECI(1:10).EQ.'1LEAKAGE E'          ) GO TO 51                 XSWO 287
      IF(RECI(1:20).EQ.' MACROSCOPIC FISSION') GO TO 61                 XSWO 288
      GO TO 50                                                          XSWO 289
C* Leakage eidt - read bucklings                                        XSWO 290
   51 READ (2,191,END=94)                                               XSWO 291
      READ (2,191,END=94)                                               XSWO 292
      READ (2,191,END=94) RECI                                          XSWO 293
      IF(RECI(1:10).EQ.' RADIAL BU') READ (RECI,198) BSR,BSX            XSWO 294
      BSQ = BSR+BSX                                                     XSWO 295
C* Read the K-eff                                                       XSWO 296
   52 READ (2,191,END=94) RECI                                          XSWO 297
      IF(RECI(13:22).NE.'GROUPS....') GO TO 52                          XSWO 298
      READ (RECI,199) AKE                                               XSWO 299
C...Read cross sections from the leakage edit                           XSWO 300
C. 54 READ (2,191,END=94) RECI                                          XSWO 301
C.    IF(RECI(1:20).NE.' GROUP     DIFFUSION') GO TO 54                 XSWO 302
C...This option is mutually exclusive with reaction rates processing    XSWO 303
      GO TO 50                                                          XSWO 304
C* Burnup output - read the fission cross section                       XSWO 305
   61 READ (2,201) (NFG(J),J=1,4)                                       XSWO 306
C* Use last burnable material to extract the fission cross section      XSWO 307
C     DO 62 I=1,4                                                       XSWO 308
C     NZF=5-I                                                           XSWO 309
C     IF(NFG(NZF).NE.0) GO TO 63                                        XSWO 310
C  62 CONTINUE                                                          XSWO 311
      NZF=1                                                             XSWO 312
   63 DO 64 JG=1,NG                                                     XSWO 313
      READ (2,204) (ZFI(J),J=1,4)                                       XSWO 314
      IF(ZFI(NZF).GT.1E-6) SCR(JG,7)=SCR(JG,7)/ZFI(NZF)                 XSWO 315
   64 CONTINUE                                                          XSWO 316
C* Read the energy released per fission (MeV)                           XSWO 317
   66 READ (2,191,END=70) RECI                                          XSWO 318
      IF(RECI(1:15).NE.' FISSION ENERGY') GO TO 66                      XSWO 319
      READ (RECI,208) EFS                                               XSWO 320
C* Convert to picoJoules                                                XSWO 321
      EFS=EFS * 0.16                                                    XSWO 322
C* Normalize nue by energy released per fission                         XSWO 323
      DO 68 JG=1,NG                                                     XSWO 324
      SCR(JG,7)=SCR(JG,7)/EFS                                           XSWO 325
   68 CONTINUE                                                          XSWO 326
C* Begin cross section condensation                                     XSWO 327
   70 J2 = 0                                                            XSWO 328
      DO 76 IG=1,NG1                                                    XSWO 329
      DF = 0.                                                           XSWO 330
      AB = 0.                                                           XSWO 331
      FY = 0.                                                           XSWO 332
      FS = 0.                                                           XSWO 333
      FL = 0.                                                           XSWO 334
      FB = 0.                                                           XSWO 335
      TL = 0.                                                           XSWO 336
      J1 = J2+1                                                         XSWO 337
      J2 = LGR(IG)                                                      XSWO 338
      DO 72 JG=J1,J2                                                    XSWO 339
C...Read cross sections from the leakage edit                           XSWO 340
C     READ (2,193,ERR=92) DFJ,DXJ,ABJ,REJ,FYJ,FLJ                       XSWO 341
      FLB= SCR(JG,2)                                                    XSWO 342
      FLI= SCR(JG,3)                                                    XSWO 343
      TLI= SCR(JG,4)                                                    XSWO 344
      ABI= SCR(JG,5)                                                    XSWO 345
      FYI= SCR(JG,6)                                                    XSWO 346
      VNI= SCR(JG,7)                                                    XSWO 347
      IF(NZF.EQ.0) VNI= VN(JG,NG1)                                      XSWO 348
      FSI= FYI/VNI                                                      XSWO 349
      DFI= ABI                                                          XSWO 350
      DO 71 KG=1,NG                                                     XSWO 351
      DFI= DFI+SCR(KG,8+JG)                                             XSWO 352
   71 CONTINUE                                                          XSWO 353
      DFI= 1./(3.*DFI)                                                  XSWO 354
      DF = DF + DFI*FLI                                                 XSWO 355
      AB = AB + ABI*FLI                                                 XSWO 356
      FY = FY + FYI*FLI                                                 XSWO 357
      FS = FS + FSI*FLI                                                 XSWO 358
      TL = TL +     TLI                                                 XSWO 359
      FL = FL +     FLI                                                 XSWO 360
      FB = FB +     FLB                                                 XSWO 361
   72 CONTINUE                                                          XSWO 362
      SCR(IG,1)= FL                                                     XSWO 363
      SCR(IG,2)= FB                                                     XSWO 364
      SCR(IG,4)= TL                                                     XSWO 365
      DC(1,IG) = DF/FL                                                  XSWO 366
      ZA(1,IG) = AB/FL                                                  XSWO 367
      VF(1,IG) = FY/FL                                                  XSWO 368
      ZF(1,IG) = FS/FL                                                  XSWO 369
   76 CONTINUE                                                          XSWO 370
C* Condense the scattering matrix (and normalize the fission spectrum)  XSWO 371
      J2 = 0                                                            XSWO 372
      DO 88 LG=1,NG1                                                    XSWO 373
      HI(1,LG)=HI(1,LG)/SPT                                             XSWO 374
C* Preset coarse group elements IG of row LG to zero                    XSWO 375
      DO 81 IG=1,NG1                                                    XSWO 376
      ZS(1,IG,LG)=0.                                                    XSWO 377
   81 CONTINUE                                                          XSWO 378
C* Loop over all fine group rows JG belonging to coarse row LG          XSWO 379
      J1 = J2+1                                                         XSWO 380
      J2 = LGR(LG)                                                      XSWO 381
      DO 84 JG=J1,J2                                                    XSWO 382
      K2 = 0                                                            XSWO 383
C* Loop to calculate coarse group elements IG of row LG                 XSWO 384
      DO 84 IG=1,NG1                                                    XSWO 385
      SC = 0.                                                           XSWO 386
      K1 = K2+1                                                         XSWO 387
      K2 = LGR(IG)                                                      XSWO 388
C* Sum over a row of fine group elements KG belonging to coarse group IGXSWO 389
      DO 82 KG=K1,K2                                                    XSWO 390
      SC = SC + SCR(KG,8+JG)                                            XSWO 391
   82 CONTINUE                                                          XSWO 392
      ZS(1,IG,LG)=ZS(1,IG,LG) + SC*SCR(JG,3)                            XSWO 393
   84 CONTINUE                                                          XSWO 394
      FL = SCR(LG,1)                                                    XSWO 395
      DO 88 IG=1,NG1                                                    XSWO 396
      ZS(1,IG,LG)=ZS(1,IG,LG) / FL                                      XSWO 397
   88 CONTINUE                                                          XSWO 398
      GO TO 10                                                          XSWO 399
C* Error conditions (EOF=-1, Read-error=-2)                             XSWO 400
   92 WRITE(LTT,691) '  Fatal Error: Corrupted WIMS output ', TRIM(FLNM)XSWO 401
      CLOSE(UNIT=50)
      STOP
      J2=0                                                              XSWO 402
   94 IF(J2.LE.1)  continue                                             XSWO 403
      CLOSE(UNIT=2)                                                     XSWO 405
      IF(J2.LE.1) RETURN                                                XSWO 406
      IF(IH.EQ.0) RETURN                                                XSWO 407
C* Perform homog.x-sect correction according to EDH method              XSWO 408
      IF(RTH.NE.0) CRD=RTH*0.5                                          XSWO 409
C* Precondition the scattering matrix                                   XSWO 410
      CALL ZSCOND(NM,NG1,ZS)                                            XSWO 411
C... XSCEDH not coded for NM>1                                          XSWO 412
      IF(NM.NE.1) STOP 'Fatal Error: Invalid NM call to XSCEDH.'        XSWO 413
      CALL XSCEDH(NG1,CRD,SCR(1,1),SCR(1,2),SCR(1,4)                    XSWO 414
     1           ,DC,ZA,VF,HI,ZF,ZS,AKE,BSQ,IRFL,NIT,IER)               XSWO 415
c      WRITE(LTT,692) NIT                                               XSWO 416
      IF(IER.NE.0) WRITE(LTT,693)                                       XSWO 417
      RETURN                                                            XSWO 418
  191 FORMAT(A132)                                                      XSWO 419
  192 FORMAT(3X,10F12.0)                                                XSWO 420
  193 FORMAT(7X,7F14.0)                                                 XSWO 421
  194 FORMAT(16X,2F14.0)                                                XSWO 422
  195 FORMAT(11X,F10.0)                                                 XSWO 423
  196 FORMAT(24X,F14.0)                                                 XSWO 424
  197 FORMAT(7X,3F13.0,5X,2F13.0)                                       XSWO 425
  198 FORMAT(16X,F15.0,24X,F15.0)                                       XSWO 426
  199 FORMAT(63X,F14.0)                                                 XSWO 427
  201 FORMAT(5I14)                                                      XSWO 428
  204 FORMAT(7X,4F14.0)                                                 XSWO 429
  208 FORMAT(15X,F12.0)                                                 XSWO 430
  691 FORMAT(A,A)                                                       XSWO 431
  692 FORMAT(I3,' EDH iterations performed.')                           XSWO 432
  693 FORMAT(' Warning: EDH iteration converges. ')                     XSWO 433
      END                                                               XSWO 434
      SUBROUTINE ZSCOND(NM,NG,ZS)                                       XSWO 435
C-Title  : Subroutine ZSCOND                                            XSWO 436
C-Purpose: Precondition the scattering matrix                           XSWO 437
C-Description:                                                          XSWO 438
C-D  The EDH methos is sensitive to small but non-zero off-diagonal     XSWO 439
C-D  scattering matrix elements, which sometimes cause numerical        XSWO 440
C-D  instabilities. The outscattering elements, which are smaller       XSWO 441
C-D  than a fraction SMALL of the total outscattering cross section,    XSWO 442
C-D  are identified. They are set to zero, their contribution being     XSWO 443
C-D  added to the self-scattering term.                                 XSWO 444
C-                                                                      XSWO 445
      DIMENSION ZS(NM,NG,NG)                                            XSWO 446
      DATA SMALL/1.E-9/                                                 XSWO 447
      DO 60 IM=1,NM                                                     XSWO 448
        DO 40 IG=1,NG                                                   XSWO 449
C* Sum off-diagonal terms to get the outscattering cross section        XSWO 450
        SG=0.                                                           XSWO 451
        DO 20 JG=1,NG                                                   XSWO 452
        IF(JG.NE.IG) SG=SG+ZS(IM,JG,IG)                                 XSWO 453
   20   CONTINUE                                                        XSWO 454
        DO 22 JG=1,NG                                                   XSWO 455
        IF(JG.EQ.IG) GO TO 22                                           XSWO 456
        IF(ABS(ZS(IM,JG,IG)/SG).GE.SMALL) GO TO 22                      XSWO 457
C* Suppress small outscattering matrix elements                         XSWO 458
        ZS(IM,IG,IG)=ZS(IM,IG,IG)+ZS(IM,JG,IG)                          XSWO 459
        ZS(IM,JG,IG)=0.                                                 XSWO 460
   22   CONTINUE                                                        XSWO 461
   40   CONTINUE                                                        XSWO 462
   60 CONTINUE                                                          XSWO 463
      RETURN                                                            XSWO 464
      END                                                               XSWO 465

      subroutine RDSEARCH(pNF, pnSkip, pcText, pnTextLen, 
     &                    pcTextRV, piFlag)
         
         character*(*) pcText
         character*(*) pcTextRV ! Text return value (RV)
         integer pNF, pnSkip, pnTextLen
         integer piFlag
         integer i
         
         piFlag = 0
         i = pnSkip
1        continue
         read(pNF,'(A,A)', end=999) pcTextRV
         if(pcText(1:pnTextLen) .ne. pcTextRV(i+1:i+pnTextLen)) goto 1
         return
999      piFlag = 1
         return
         
      end subroutine
      
      integer function RDLENF(pcString)
         integer mlen, i
         character*(*) pcString
         rclenf = 1
         mlen=LEN(pcString)
         do i=mlen, 1, -1
            if(pcString(i:i) .ne. ' ') then
               rclenf = i
               return       
            endif
         enddo   
      end function 