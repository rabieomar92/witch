*DECK XSWOUT                                                            
C      PROGRAM XSWOUT                                                   
       SUBROUTINE XSWOUT(PING, LGR, PIJH, PCWITCHINP, LAYERID, TYPE)
       CHARACTER*50 PCLG
       CHARACTER*4  TYPE(128)
       CHARACTER*(*) PCWITCHINP
       INTEGER      PING, PIJH, LAYERID

C-TITLE  : XSWOUT DRIVER PROGRAM FOR XSCWOU                             
C-PURPOSE: EXTRACT CROSS SECTIONS FROM WIMS OUTPUT .OUT                 
C-M                                                                     
C-M  MANUAL FOR PROGRAM XSWOUT                                          
C-M  =========================                                          
C-M  WIMS OUTPUT IS READ AND CELL-AVERAGED CROSS SECTIONS ARE           
C-M  CALCULATED. HOMOGENIZATION CAN BE DONE USING THE EFFECTIVE         
C-M  DIFFUSION HOMOGENIZATION METHOD (EDH), OR ELSE THE SIMPLE FLUX     
C-M  AND VOLUME WEIGHTING CAN BE APPLIED.                               
C-M    THE LEAKAGE EDIT OF WIMS IS APPLICABLE ONLY FOR FUEL-BEARING     
C-M  REGIONS AND IS INCORRECT IF ONE TRIES TO OBTAIN HOMOGENIZED CROSS  
C-M  SECTIONS FOR NON-FUELLED CELLS, SUCH AS WATER CHANNELS OR BURNABLE 
C-M  POISON CELLS. FOR THIS REASON THE CELL-EDIT IN WIMS IS PROCESSED,  
C-M  IN WHICH THE CROSS SECTIONS ARE HOMOGENIZED BY FLUX AND VOLUME     
C-M  WEIGHTING.                                                         
C-M                                                                     
C-M  DEFINITIONS:                                                       
C-M    TRANSPORT CROSS SECTION IS RECONSTRUCTED FROM THE ABSORPTION     
C-M  CROSS SECTION AND THE SCATTERING MATRIX. THE DIFFUSION CONSTANT    
C-M  IS DEFINED (D = 1 / 3.SIGMATRANSPORT).                             
C-M    ABSORPTION CROSS SECTION IS DEFINED AS THE SUM OF THE CAPTURE    
C-M  AND FISSION CROSS SECTIONS.                                        
C-M    FISSION YIELD IS THE PRODUCT OF THE FISSION CROSS SECTION AND    
C-M  THE NUMBER OF NEUTRONS PER FISSION.                                
C-M    NEUTRON SPECTRUM GROUP FRACTION IS THE FRACTION OF THE NEUTRONS  
C-M  BORN IN A PARTICULAR GROUP IN FISSION.                             
C-M    FISSION ENERGY PER NEUTRON FLUX IS THE FISSION CROSS SECTION     
C-M  SCALED WITH THE ENERGY RELEASED PER FISSION, SUCH THAT THE         
C-M  PRODUCT WITH THE NEUTRON FLUX GIVES DIRECTLY THE POWER.            
C-M    SCATTERING MATRIX IS GIVEN IN TERMS OF THE CROSS SECTIONS        
C-M  FOR SCATTERING FROM A PARTICULAR GROUP INTO ALL OTHER GROUPS.      
C-M    CONDENSATION IS PERFORMED USING THE CELL-AVERAGE FLUX FROM THE   
C-M  CELL-EDIT OF WIMS.                                                 
C-M    EDH METHOD IS APPLIED BY DEFAULT TO OBTAIN EFFECTIVE DIFFUSION   
C-M  FEW-GROUP CROSS SECTIONS. CYLINDRICAL 1D GEOMETRY IS ASSUMED FOR   
C-M  THE EDH CORRECTION. THE CELL BOUNDARY FLUX VALUE IS APPROXIMATED   
C-M  BY THE AVERAGE FLUX IN THE OUTERMOST REGION OF THE CELL. THE       
C-M  EXCEPTION IS THE CASE WHEN A FREE COMMAND IS ENCOUNTERED ON WIMS   
C-M  INPUT. IT IS ASSUMED THAT REFLECTOR CONSTANTS ARE TO BE GENERATED. 
C-M  BOUNDARY FLUX IS APPROXIMATED BY THE INNERMOST REGION OF THE CELL  
C-M  AND A ZERO-FLUX BOUNDARY CONDITION IS TAKEN ON THE OUTER BOUNDARY  
C-M  OF THE EQUIVALENT HOMOGENIZED PROBLEM.                             
C-M                                                                     
C-M  INSTRUCTIONS:                                                      
C-M  THE FOLLOWING INPUT PARAMETERS ARE READ FROM INPUT:                
C-M    FLNX  - OUTPUT CROSS SECTIONS FILENAME                           
C-M    NG,NP - NUMBER OF MACROGROUPS NG AND THE NUMBER OF ADDITIONAL    
C-M            CROSS SECTION PARAMETERS NP. THE PARAMETERS ARE READ     
C-M            FROM 10-COLUMN FIELDS, BUT BLANKS ARE IGNORED, SO THE    
C-M            ENTRIES NEED NOT BE RIGHT-JUSTIFIED.                     
C-M              BY DEFAULT NP=1 TO IMPLY THE EDH CORRECTION TO THE     
C-M            CROSS SECTIONS AND THE PRINTOUT OF THE EDH FLUX          
C-M            SCALING FACTOR. NP>1 MAY BE ENTERED TO RESERVE SPACE     
C-M            FOR THE P1 CROSS SECTION COMPONENTS (FOR CONSISTENCY     
C-M            WITH CROSS SECTIONS FROM OTHER SOURCES), ALTHOUGH        
C-M            THESE ARE ALL ZERO IN XSWOUT.                            
C-M              TO FORCE SUPPRESSION OF THE EDH CORRECTION,            
C-M            ENTER NEGATIVE NG.                                       
C-M              TO APPLY THE EDH METHOD ON THE CROSS SECTIONS          
C-M            BUT TO SUPPRESS THE PRINTOUT OF THE EDH FACTOR           
C-M            SET NP<0.                                                
C-M    NR(G) - THE LIST CONTAINING LAST GROUPS OF THE TRANSPORT         
C-M            GROUP STRUCTURE, BELONGING TO THE FEW-GROUP SET          
C-M            (I.E.SAME TYPE OF SPECIFICATION AS FOR THE FEWGROUP      
C-M            COMMAND IN WIMS). IF BLANK IS ENTERED FOR THE PREVIOUS   
C-M            INPUT REQUEST (NG, NP), A 10-GROUP SET IS ASSUMED AND    
C-M            NO REQUEST FOR THE GROUP BOUNDARIES IS ISSUED. BY        
C-M            DEFAULT, GROUPS (3,5,6,10,14,21,25,32) OF THE WIMS       
C-M            TRANSPORT GROUPS ARE USED.                               
C-M    FLNO -  WIMS OUTPUT FILENAME TO BE PROCESSED. REQUESTS FOR       
C-M            FLNO ARE REPEATED SO THAT SEVERAL WIMS FILES CAN BE      
C-M            PROCESSED IN SEQUENCE AND HOMOGENIZED CROSS SECTIONS     
C-M            WRITTEN ON THE SAME OUTPUT CROSS SECTIONS FILE.          
C-M            PROCESSING IS TERMINATED WHEN A BLANK FILENAME IS        
C-M            ENCOUNTERED.                                             
C-M                                                                     
C-M  OUTPUT CROSS SECTION FORMAT:                                       
C-M    THE CROSS SECTIONS ARE PRINTED IN 10-COLUMN FORMAT WHICH IS      
C-M  COMPATIBLE WITH THE GNOMER AND BINODE CODES. THE CROSS SECTIONS    
C-M  ARE SORTED IN FOLLOWING ORDER FOR EACH GROUP:                      
C-M   - DIFFUSION CONSTANT [CM],                                        
C-M   - ABSORPTION CROSS SECTION [1/CM],                                
C-M   - FISSION YIELD [1/CM],                                           
C-M   - NEUTRON SPECTRUM GROUP FRACTION,                                
C-M   - FISSION CROSS SECTION [1/CM] (OR FISSION ENERGY PER UNIT        
C-M     NEUTRON FLUX [PJ/CM], WHEN BURNUP PRINTOUT IS AVAILABLE),       
C-M   - SCATTERING MATRIX ELEMENTS [1/CM],                              
C-M   - EDH HETEROGENEITY FACTOR (ONLY WHEN EDH CORRECTION TO           
C-M     THE CROSS SECTIONS IS SPECIFIED AND THE NUMBER OF GROUPS        
C-M     IS NOT 3 OR 11).                                                
C-M  THE OUTPUT RECORD IS LIMITED TO 80-COLUMNS. IF MORE THAN THREE     
C-M  GROUPS ARE REQUESTED, THE CROSS SECTIONS FOR EACH GROUP OCCUPY     
C-M  MORE THAN ONE RECORD. SIMILARLY, MORE THAN TWO RECORDS ARE         
C-M  NEEDED IF THE NUMBER OF GROUPS IS GREATER THAN 11.                 
C-M                                                                     
C-M  ERROR CONDITIONS:                                                  
C-M  THE EDH ITERATIONS MAY TERMINATE WITH AN ERROR FLAG, WHICH CAN BE  
C-M  INTERPRETED AS FOLLOWS:                                            
C-M           0 - NORMAL TERMINATION.                                   
C-M   -100*MXIT - NO CONVERGENCE ON THE DIFFUSION CONSTANT IN EDH       
C-M               ITERATIONS (MXIT=40 AT PRESENT).                      
C-M  -(5000+NIT)- SOLUTION IS SINGULAR AT EDH ITERATION NIT.            
C-M                                                                     
C-M  REFERENCES:                                                        
C-M  [1] A.TRKOV, M.RAVNIK: EFFECTIVE DIFFUSION HOMOGENIZATION OF CROSS 
C-M      SECTIONS FOR PRESSURIZED WATER REACTOR CORE CALCULATIONS,      
C-M      NUCL.SCI.ENG. VOL.116, NO.2, PP.86-95, FEB.1994.               
C-M                                                                     
C-AUTHOR : A.TRKOV, INSTITUTE JOZEF STEFAN, LJUBLJANA, SLOVENIA (1993)  
C-                                                                      
      CHARACTER*40  FLNX,FLNM                                           
      INTEGER       NFI, IFLAG, NWIMSINPUT, RDLENF, RW
      CHARACTER*40  CAWIMSINPUT(1000)
      CHARACTER*132 CTEXT
      CHARACTER*10  CFILEWIMSHEADER
      INTEGER       LGR(PING)
      DIMENSION     SCR(12000)                                          
     1             ,DC(80), ZA(80),VF(80),HI(80),ZF(80),ZS(6400)
     
C* LOGICAL FILE UNIT NUMBERS                                            
      DATA  LOU,LKB,LTT/ 1, 5, 6 /
      
!     SET THE TRIGA CORE LAYER ID 
c      PRINT*, ' Running ROUTINE-D.'      
      NWIMSINPUT=0
      OPEN(UNIT=50, FILE='WIMS-OUT.$$$',STATUS='UNKNOWN',ERR=911)
      GOTO 811
911   PRINT*, ' Fatal Error: The header file WITCH-OUT.$$$',
     &        ' is not found.'
      CLOSE(UNIT=50)
      STOP
811   CONTINUE

      CALL RDSEARCH(50, 0, '   WIFLNM ',10, CTEXT, IFLAG)
      IF (IFLAG .NE. 0) THEN
         GOTO 101
      ENDIF
      NWIMSINPUT = NWIMSINPUT + 1
      CAWIMSINPUT(NWIMSINPUT) = CTEXT(11:RDLENF(CTEXT))
      GOTO 811
101   CONTINUE
      NM  = 1                                                           
      IH  = 1                                                           
      IF(LAYERID .EQ. 0) THEN
      FLNX = TRIM(PCWITCHINP) // '-XSOUT.TXS'
      ELSE
      WRITE(FLNX,'(I10.10,A4)') LAYERID, '.TXS'
      ENDIF
      NG1 = PING
      JH = PIJH
      IF(JH .GT.0) IH=JH                                                
      IF(NG1.LT.0) IH=0                                                 
      NG1=ABS(NG1)                                                      
   12 NG  = LGR(NG1)                                                    
      OPEN(UNIT=LOU,FILE=FLNX,STATUS='UNKNOWN')                         
      RW = 1
c      PRINT*,' Preparing TRIGA cross section file (TXS).',
c     &       ' Please wait...'
      WRITE(LOU,'(6A10,2I10.9)') (' 000000000',I=1,6), NWIMSINPUT,
     &    LAYERID
   20 IF (RW .GT. NWIMSINPUT) GOTO 90
      WRITE(LOU,'(A10,I10.9,I10.9,A10)') 
     &   ' 111111111',RW,NG1,ADJUSTR(TYPE(RW))
      FLNM = CAWIMSINPUT(RW)
      BACKSPACE 6
      IF(FLNM.EQ.'                                        ') GO TO 90   
      CALL XSCWOU(FLNM,NM,NG,NG1,LGR,DC,ZA,VF,HI,ZF,ZS,SCR,IH,LTT)      
      DO 40 I=1,NG1                                                     
      JG=(I-1)*NG1*NM                                                   
      IF(IH .LE.0) GO TO 22                                             
C* PRINT X-SECT WITH EDH FACTOR                                         
      WRITE(LOU,691)                                               
     1   DC(I),ZA(I),VF(I),HI(I),ZF(I),(ZS(J+JG),J=1,NG1),SCR(I)  
     2 ,(0.,J=2,IH)                                                   
      GO TO 40                                                          
C* PRINT X-SECT WITHOUT EDH FACTOR
   22 WRITE(LOU,691)                                       
     1   DC(I),ZA(I),VF(I),HI(I),ZF(I),(ZS(J+JG),J=1,NG1)      
   40 CONTINUE                                                          
      RW = RW + 1
      GO TO 20
c      PRINT*, ' ROUTINE-D completed.'
   90 WRITE(LOU,'(6A10,2I10.9)') (' 999999999',I=1,6), NWIMSINPUT,
     & LAYERID
      CLOSE(UNIT=50)
      CLOSE(UNIT=LOU)
      RETURN                                                            
  690 FORMAT(2A40)                                                      
  691 FORMAT(8F10.7)                                                    
  692 FORMAT(BN,2I10.10)                                                
      CLOSE(UNIT=50)
      CLOSE(UNIT=LOU)
      END                                                               
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

      END                                                               
      SUBROUTINE ZSCOND(NM,NG,ZS)                                       
C-TITLE  : SUBROUTINE ZSCOND                                            
C-PURPOSE: PRECONDITION THE SCATTERING MATRIX                           
C-DESCRIPTION:                                                          
C-D  THE EDH METHOS IS SENSITIVE TO SMALL BUT NON-ZERO OFF-DIAGONAL     
C-D  SCATTERING MATRIX ELEMENTS, WHICH SOMETIMES CAUSE NUMERICAL        
C-D  INSTABILITIES. THE OUTSCATTERING ELEMENTS, WHICH ARE SMALLER       
C-D  THAN A FRACTION SMALL OF THE TOTAL OUTSCATTERING CROSS SECTION,    
C-D  ARE IDENTIFIED. THEY ARE SET TO ZERO, THEIR CONTRIBUTION BEING     
C-D  ADDED TO THE SELF-SCATTERING TERM.                                 
C-                                                                      
      DIMENSION ZS(NM,NG,NG)                                            
      DATA SMALL/1.E-9/                                                 
      DO 60 IM=1,NM                                                     
        DO 40 IG=1,NG                                                   
C* SUM OFF-DIAGONAL TERMS TO GET THE OUTSCATTERING CROSS SECTION        
        SG=0.                                                           
        DO 20 JG=1,NG                                                   
        IF(JG.NE.IG) SG=SG+ZS(IM,JG,IG)                                 
   20   CONTINUE                                                        
        DO 22 JG=1,NG                                                   
        IF(JG.EQ.IG) GO TO 22                                           
        IF(ABS(ZS(IM,JG,IG)/SG).GE.SMALL) GO TO 22                      
C* SUPPRESS SMALL OUTSCATTERING MATRIX ELEMENTS                         
        ZS(IM,IG,IG)=ZS(IM,IG,IG)+ZS(IM,JG,IG)                          
        ZS(IM,JG,IG)=0.                                                 
   22   CONTINUE                                                        
   40   CONTINUE                                                        
   60 CONTINUE                                                          
      RETURN                                                            
      END                                                               

      SUBROUTINE RDSEARCH(PNF, PNSKIP, PCTEXT, PNTEXTLEN, 
     &                    PCTEXTRV, PIFLAG)
         
         CHARACTER*(*) PCTEXT
         CHARACTER*(*) PCTEXTRV ! TEXT RETURN VALUE (RV)
         INTEGER PNF, PNSKIP, PNTEXTLEN
         INTEGER PIFLAG
         INTEGER I
         
         PIFLAG = 0
         I = PNSKIP
1        CONTINUE
         READ(PNF,'(A,A)', END=999) PCTEXTRV
         IF(PCTEXT(1:PNTEXTLEN) .NE. PCTEXTRV(I+1:I+PNTEXTLEN)) GOTO 1
         RETURN
999      PIFLAG = 1
         RETURN
         
      END SUBROUTINE
      
      INTEGER FUNCTION RDLENF(PCSTRING)
         INTEGER MLEN, I
         CHARACTER*(*) PCSTRING
         RCLENF = 1
         MLEN=LEN(PCSTRING)
         DO I=MLEN, 1, -1
            IF(PCSTRING(I:I) .NE. ' ') THEN
               RCLENF = I
               RETURN       
            ENDIF
         ENDDO   
      END FUNCTION 