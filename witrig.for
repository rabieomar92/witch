!     CLASSIFIED AND NOT TO BE DISCLOSED.
!     Declaration of WITRIG for F90 compatible. 
!     This code is written by M. R. Omar for Malaysian Nuclear Agency.
!     Purpose: Prepare WIMS input files (WIMInnn.WIN) for each unit cells.
!              This subroutine also prepares the header file WiTrig.$$$.
!
!
!
!     raPel()   - field, power by element (PE)
!     iaRing() - the number of fuel elements in the ring.
!     PthRe    - power of the reactor (TRIGLAV.INP)
!     PavEl    - average power per element (samo BU)
!     caAux    - field of site location tags (A-01,B-01,B-02...)
!     caElementTags   - field of tag elements

c      program witrig
      subroutine witrig(piLayerID, pcWITCHInp, pcELEMInp)
      implicit none
      
      character(len=50), intent(in) :: pcWITCHInp, pcELEMInp
      integer, intent(in) :: piLayerID

c     DECRARE DENSITY OF WATER FUNCTION HEADER. WATER DENSITY IS A FUNCTION OF
c     LOCAL TEMPERATURE.
      real :: fRoWater

c     DECLARING THE NUMBER OF CORE LAYERS. THIS IS IMPORTANT BECAUSE WE DIVIDE
c     A FUEL ELEMENT INTO SEVERAL LAYERS.
      integer :: nCoreLayers
      
c     MULTI-PURPOSE INTEGER VARS FOR ITERATION LOOPS.
      integer :: i, j, n, k, l, m
      
c     DECLARING INTEGER VALUE FOR RING INDEX RANGING FROM 1 TO 7.
      integer :: iRing
      
c     DECLARING CONSTANT FOR MAXIMUM NUMBER OF ELEMENTS.
      integer, parameter :: nMaxEl = 128
      
c     MAXIMUM NUMBER OF CORE RING IN MARK-II MESH = 7
      integer, parameter :: nMaxRing = 7
      
c     DECLARING THE NUMBER CORE RING SET BY THE USER IN MAIN.INP.
      integer           :: nRing
      
c     DECLARING AN ARRAY STORING NUMBER OF ELEMENTS IN A RING. IE.
c     RING-A: iaRing(1) = 1
c     RING-B: iaRing(2) = 6
c     RING-C: iaRing(3) = 12 AND SO ON..
      integer, allocatable :: iaRing(:)
      
c     DECLARING RING RADIUS ARRAY IN CENTIMETER (CM)
      real :: raRadius(7)
      
c     DECLARING UNIT CELL RADIUS AND AXIAL REFLECTOR THICKNESS.
      real :: rUnitCellRadius, rRefThick
      
    
      ! Declaring enum for cladding type (SS304, Fe, Al)
      integer :: nCladType = 1
      ! Declaring the array of power of each fuel element in the TRIGA reactor.
      real   , allocatable :: raPel(:)
      !  Default input file name...
      character(len=50), parameter :: WITCHINP = 'MAIN.INP'
      character(len=50), parameter :: ELEMINP  = 'FUEL_INVENTORY.INP'
      ! File unit numbers.
      integer :: NFIN, NFP, NFEL, NFW, NFX, NFER, NFY, NFZB, NFZP
      ! Code input parameters (from TRIGLAV.INP)
      integer :: nIsXe
      ! Declaring a variable that stores the thermal power of the reactor, i.e. 1000kWt?
      real    :: rPthRe
      ! Declaring burnup profile factor along fuel length
      real    :: rBUShapeFactor
      ! Declaring a variable that stores average power of each fuel element.
      real    :: rPavEl
      ! Declaring a variable that stores the power of fuel element.
      real    :: rPel
      ! Declaring a variable for storing IO file names.
      character(len=40) :: cFilInp, cFilEle, cFilPow, cFileWIMS
      ! Declaring density of light water (Temperature dependent)
      real :: rDensityLW
      ! Declaring light water (coolant) temperature
      real :: rTempLW
      ! Declaring vars that stores the date and time.
      character(len=10) :: DTE, TME
      ! Declaring dummy variable for TRIGLAV input card searching.
      character(len=80) :: cLineA
      
      ! Declaring TRIMON input card searching status. 
      !     0-Search found
      !     1-Search failed
      integer           :: iFlag      
      ! Declaring variable for fuel element index for fuel data array retrieval.
      integer :: iEl      
      ! Declaring the number of fuel elements that still have the U-235 in it (not fully burned U-235)
      integer :: nElBu
      ! Declaring the number of fuel elements inside the reactor
      integer :: nElem
      character(len=7) :: cCellID
      character(len=4) :: caElementTags(nMaxEl), caAux(nMaxEl)
      character(len=4) :: cCellType
      ! Sum power of fuel elements. 
      real :: rSumPEl      
      
c     DECLARING MASS OF URANIUM IN UZrH, DENSITY OF UZrH, FUEL ENRICHMENT AND WEIGHT
c     PERCENT OF U-235.
      real :: rMassUZrH, rDensityUZrH, rEnrichment, rWtU235, rMassUZrHt
      ! rBuMW - Burnup of the fuel element in MW
      ! rBUPercent - Burnup of U-235 in the fuel element in MW.
      real :: rMassEr166, rMassEr167
      real :: rMassU235, rPPFRing, fPPF, rPPF
      
c     DECLARING WEIGHT PERCENT FOR Er-166 & Er-167 IN FLIP & LEU FUELS. THIS IS NOT 
c     APPLICABLE FOR PUSPATI TRIGA REACTOR.
      real :: rWtEr166, rWtEr167
      
c     FUEL AVERAGE TEMPERATURE
      real :: rTempFuel

c     DECLARING THE AXIAL BUCKLING FREE PARAMETER (IF REQUIRED IN SOME CORE CALCULATION)
c     HERE THE DEFAULT VALUE IS 0.0 SINCE CORE BUCKLING BEHAVIOUR IS ALREADY EXHIBITED
c     IN THE MONTE CARLO SIMULATION.
      real :: rDBSquared = 0.0
      
c     DECLARING VARS FOR FUEL SPEC. CORRECTION
      real :: rDensityUZrHM, rWtU235M, rMassUZrHM, rEnrichmentM, 
     &        rWtEr166M, rWtEr167M, rMassEr166M, rMassEr167M     
      character(len=2) :: cRing, cLayer
      real, allocatable :: raAxialPPF(:), raRadialPPF(:)
      logical :: lShowHighPowerFuelMessage = .false.
      
c     DECLARING VARS FOR TEMP. RELATIONS (PREDICTING TEMPERATURE DIST IN CORE)
      real :: raTempRel(100,100)
      integer :: nPiecewise, nCoefs, nCell
      integer :: nPiecewiseST08, nPiecewiseST12, nPiecewiseST20
      logical :: lIsPDISTExists = .false.
      
c     DECLARING VARS FOR PREVIOUS FUEL BURNUP SPECIFICATION. NOTE rPSpec is 
c     IMPORTANT TO ACCOUNT FUEL BURNUP.
      real :: rBuMW, rBUPercent 
      logical :: lIsZBURNExists = .false.
      character(len=2) :: cLayerID
      real :: rPCell
      real :: rBuCell
      real :: raBuCell(127)
      real :: raPSpec(5) ! rPSpec(1) for ST08, rPSpec(2) for ST12, ... etc...
      real :: raMSpec(5)
      real :: raDSpec(5)
      real :: raESpec(5)
      real :: raEr166Spec(5)
      real :: raEr167Spec(5)
      real :: raUr235Spec(5)
      real :: raEr166MSpec(5)
      real :: raEr167MSpec(5)
      real :: rPrevTau
      
c     HERE WE SET THE PRESET VALUES OF FUEL SPECS. SINCE MALAYSIA OWN THIS,
c     THE SPEC VALUES WiLl BE BASED ON THE FUELS USED BY MALAYSIAN NUCLEAR
c     AGENCY.

      raPSpec(1) = 10.0
      raPSpec(2) = 10.0
      raPSpec(3) = 10.0
      raPSpec(4) = 10.0
      raPSpec(5) = 10.0
      
      raMSpec(1) = 190.450
      raMSpec(2) = 278.54712
      raMSpec(3) = 495.000
      raMSpec(4) = 191.801
      raMSpec(5) = 494.921
      
      raDSpec(1) = 5.81070
      raDSpec(2) = 6.00000
      raDSpec(3) = 7.77008
      raDSpec(4) = 5.85955
      raDSpec(5) = 6.38695
      
      raESpec(1) = 19.9000
      raESpec(2) = 19.9000
      raESpec(3) = 19.9000
      raESpec(4) = 69.9618
      raESpec(5) = 19.7676
      
      raEr166Spec(1) = 0.0000000
      raEr166Spec(2) = 0.0000000
      raEr166Spec(3) = 0.0000000
      raEr166Spec(4) = 0.0051105
      raEr166Spec(5) = 0.0014630

      raEr167Spec(1) = 0.0000000
      raEr167Spec(2) = 0.0000000
      raEr167Spec(3) = 0.0000000
      raEr167Spec(4) = 0.0035039
      raEr167Spec(5) = 0.0010000
      
      raEr166MSpec(1) = 0.0000000
      raEr166MSpec(2) = 0.0000000
      raEr166MSpec(3) = 0.0000000
      raEr166MSpec(4) = 11.543240
      raEr166MSpec(5) = 3.6019600

      raEr167MSpec(1) = 0.0000000
      raEr167MSpec(2) = 0.0000000
      raEr167MSpec(3) = 0.0000000
      raEr167MSpec(4) = 7.9144400
      raEr167MSpec(5) = 2.4620300     

      raUr235Spec(1) = 0.01689786
      raUr235Spec(2) = 0.02332300
      raUr235Spec(3) = 0.03333333
      raUr235Spec(4) = 0.05941050
      raUr235Spec(5) = 0.03973700   
      
      allocate (iaRing(nMaxRing))
      allocate (raPel(nMaxEl))
     
      do i=1, 100, 1
      do j=1, 100, 1
         raTempRel(i,j) = 0.0
      enddo
      enddo
! -----------------------------------------------------------------------------
!                         PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------
!     ** NFIN is the file ID for reading TRILAV.OUT file.
!        NFP  is the file ID for reading the power distribution file.
!        NFEL is the file ID for reading the ELEM.INP file.
!        NFW  is the file ID for writing the WIMS input file.
!        NFX  is the file ID for writing the WIMS input header file (WiTrig.$$$)
!        NFER is the file ID for writing the error log file.
! -----------------------------------------------------------------------------
      data NFIN, NFP, NFEL, NFW, NFX, NFY / 11, 12, 13, 21, 22, 30 /
      data NFER / 9 /
      data NFZB / 31 /
      data NFZP / 32 /
      
!     HERE WE COLLECT PREVIOUS BURNUP DATA FROM ZBURN.OUT
!     FIRST WE CHECK WHETHER ZBURN.OUT FILE EXISTS.
      open(unit=NFZB, file='ZBURN.OUT', status='OLD', err=292)
      lIsZBURNExists = .true.
      goto 293
292   lIsZBURNExists = .false.
      goto 294
293   continue ! CASE IF ZBURN.OUT EXISTS.
      read(NFZB,*,end=294,err=292) rPrevTau
      print*, ' WITCH: Reading axial burnup' //
     &   ' data from ZBURN.OUT. '
      write(*,'(A,G0.3)') 'WITCH: Previous-Tau (days) = ', rPrevTau
      write(cLayerID,'(I0.2)') piLayerID
      m = 1
      
      do i=1, 6400, 1
         read(NFZB,*,end=294) cCellID, rBUCell
         if(cCellID(6:7) .eq. cLayerID) then
            raBuCell(m) = rBUCell / 1000.0
            m = m + 1
            if (m .eq. 127) then
               goto 294
            endif
         endif
      enddo
294   continue ! CONTINUE IF ZBURN DOES NOT EXISTS.
 

!     Initialize the total sum of power element to zero for later sum iteration
!     calculation.      
      rSumPEl = 0.0
      
!     Get the system date and time to label WIMS input
      call rbdatetime(DTE, TME)

!     Set the input file names to the default defined param values.

       cFilInp = trim(pcWITCHInp) // '.INP'
       cFilEle = trim(pcELEMInp) // '.INP'
       
!     Open file stream for the two input files (TRIGLAV.INP & ELEM.INP)      
      open(unit=NFIN, file=cFilInp, status='OLD', err=911)
      open(unit=NFEL, file=cFilEle, status='OLD', err=919)
      goto 915
911   print*, ' WITCH: Fatal Error. ' //
     &       'The main input file (MAIN.INP) is not found!'
      stop
915   continue
      goto 920
919   print*, ' WITCH: Fatal Error.' //
     &   ' FUEL_INVENTORY.INP is not found!'
      stop
920   continue

!     OPEN THE TEMPORARY FILES SPECIFYING THE NAMES OF THE WIMS INPUT OUTPUT
!     FILES.
      open(unit=NFX,    file='WIMS-IN.$$$', status='UNKNOWN')
      open(unit=NFY,    file='WIMS-OUT.$$$', status='UNKNOWN')

!     SEARCH FOR XENON CORRECTION CARD.
      call rbsearch(NFIN ,0, '#XENON', 6, cLineA, iFlag)
      if(iFlag .eq. 1) then
         nIsXe = 0
      else
         read(NFIN,*) nIsXe
      endif
      
      rewind NFIN
      
!     SEARCH FOR NOMINAL REACTOR POWER.
      call rbsearch(NFIN, 0, '#POWER', 6, cLineA, iFlag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: POWER card is not found!'
         stop
      else
         read(NFIN,*) rPthRe
      endif
      
      rewind NFIN
      
      ! Search for the number of core layers from the input file.
      call rbsearch(NFIN, 0, '#NLAYERS', 8, cLineA, iFlag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: NLAYERS card is not found!'
         stop
      else
         read(NFIN,*) nCoreLayers
      endif
      if(piLayerID .lt. 1) goto 37
      if(piLayerID .gt. nCoreLayers) goto 37
      goto 38
37    print*, ' The specified Layer ID ', piLayerID, ' is not valid!'
38    continue

      rewind NFIN

      call rbsearch(NFIN, 0, '#BZSQUARED', 7, cLineA, iFlag)
      if(iFlag .eq. 1) then
         goto 912
      else
         read(NFIN,*) rDBSquared
      endif
      
912   rewind NFIN
      
      call rbsearch(NFIN, 0, '#FUELSPEC', 13, cLineA, iFlag)
      if(iFlag .eq. 1) then
         goto 124
      else
         read(NFIN,*,end=123,err=123) raMSpec(1), 
     &               raMSpec(2), raMSpec(3),
     &                raMSpec(4), raMSpec(5)
         read(NFIN,*,end=123,err=123) raDSpec(1), 
     &               raDSpec(2), raDSpec(3),
     &                raDSpec(4), raDSpec(5)
         read(NFIN,*,end=123,err=123) raESpec(1), 
     &               raESpec(2), raESpec(3),
     &                raESpec(4), raESpec(5)
         read(NFIN,*,end=123,err=123) raEr166Spec(1), 
     &               raEr166Spec(2), raEr166Spec(3),
     &                raEr166Spec(4), raEr166Spec(5)
         read(NFIN,*,end=123,err=123) raEr167Spec(1), 
     &               raEr167Spec(2), raEr167Spec(3),
     &                raEr167Spec(4), raEr167Spec(5)
         read(NFIN,*,end=123,err=123) raEr167MSpec(1), 
     &               raEr167MSpec(2), raEr167MSpec(3),
     &                raEr167MSpec(4), raEr167MSpec(5)
         read(NFIN,*,end=123,err=123) raEr167MSpec(1), 
     &               raEr167MSpec(2), raEr167MSpec(3),
     &                raEr167MSpec(4), raEr167MSpec(5)
         read(NFIN,*,end=123,err=123) raUr235Spec(1), 
     &               raUr235Spec(2), raUr235Spec(3),
     &                raUr235Spec(4), raUr235Spec(5)
     
      endif
      goto 124
123   print*, ' WITCH: Fatal error. Bad FUELSPEC input.'
      write(*,'(A,I0)')'  Line : ', abs(iFlag-1)
      stop
      
124   rewind NFIN
      
!     Search for the number of core rings.
      call rbsearch(NFIN, 0, '#NRINGS', 7, cLineA, iFlag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: NRINGS card is not found!'
         stop
      else
         read(NFIN,*) nRing
      endif
      if(nRing .eq. 6) then
         iaRing(1) = 1
         iaRing(2) = 6
         iaRing(3) = 12
         iaRing(4) = 18
         iaRing(5) = 24
         iaRing(6) = 30
      elseif(nRing .eq. 7) then
         iaRing(1) = 1
         iaRing(2) = 6
         iaRing(3) = 12
         iaRing(4) = 18
         iaRing(5) = 24
         iaRing(6) = 30
         iaRing(7) = 36
      endif
      
      nElem=0
      nElBu=0
      do i=1, nRing, 1
         nElem = nElem + iaRing(i)
      enddo
      if(nElem .gt. nMaxEl) then
         print*, ' WITCH: Fatal error. Too many '//
     &      'fuel elements!'
         stop
      endif
      
      rewind NFIN

      ! Here we calculate the unit cell radius for the appropriate core size.
      call rbsearch(NFIN, 0, '#DIMENSIONS', 11, cLineA, iFlag)
      
      if(iFlag .eq. 1) then
         print*, ' WITCH: Fatal error. DIMENSIONS '//
     &         'card is not found!'
         stop
      else
         read(NFIN,*,err=1001) (raRadius(i),i=1,nRing)
         read(NFIN,*,err=1001) rRefThick
         read(NFIN,*,err=1001) rRefThick
      endif
      goto 1002
1001  print*, ' WITCH: Fatal error. DIMENSIONS card '//
     &   'format error!'
      write(*,'(A,I0)')'  Line : ', abs(iFlag-1)
      stop
1002  rewind NFIN

!     HERE WE CALCULATE THE APPROPRIATE 2D CELL RADIUS, ACCORDING TO THE SPECIFIED
!     CORE DIMENSIONS.
      if(nRing .eq. 7) then
      rUnitCellRadius = sqrt((raRadius(nRing)**2)/127.0)

      elseif(nRing .eq. 6) then
      rUnitCellRadius = sqrt((raRadius(nRing)**2)/91.0)
      endif
      ! rUnitCellRadius = 2.31317
!     READING THE $* LOADING CARD ---------------------------------------------

      call rbsearch(NFIN, 0, '#CORECONFIG', 11, cLineA, iFlag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: CORECONFIG card is not found!'
         stop
      endif
      
      n = 1
!     Read and write the loading of the first ring, A-1.
      read (NFIN,'(A4,1x,A4)') caAux(n), caElementTags(n)

!     Read and write fuel elements loading for row 2 - 6
      do i=1, 15, 1
         read (NFIN,'(A4,1x,A4,5(1x,A4,1x,A4))')
     &          (caAux(n+j), caElementTags(n+j), j=1, 6)
         n = n + 6
      enddo
      if(nRing .eq. 6) then
         n = n + 1
        ! read  (NFIN,'(A4,1x,A4)') caAux(n), caElementTags(n)  
         caAux(n) = '   R'
         caElementTags(n) = 'G   '
      endif
!     Read and write row 7 (G ring) for reactors with 7 fuel rings.
      if (nRing .eq. 7) then
         do j=1, 6, 1
            read (NFIN,'(A4,1x,A4,5(1x,A4,1x,A4))') 
     &            (caAux(n+i), caElementTags(n+i),i=1, 6)
            n = n + 6
         enddo
         n = n + 1
         !read  (NFIN,'(A4,1x,A4)') caAux(n), caElementTags(n) 
         caAux(n) = '   R'
         caElementTags(n) = 'G   '
      endif
  
      goto 14
13    print*, ' Fatal Error: Incorrect core configuration pattern.'
14    continue
!     END READING THE $* LOADING CARD -----------------------------------------

      rewind NFIN
      
!     Water temperature: rTempLW, rDensityLW
      call rbsearch(NFIN,0,'#TCOOL',6,cLineA,iflag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: TCOOL card is not found!'
         stop
      endif
      read(NFIN,'(F10.0)') rTempLW
      rDensityLW = fRoWater(rTempLW)

      rewind NFIN
      
!     READING #TEMPREL CARD ------------------------------------------------
      call rbsearch(NFIN,0,'#TEMPREL',8,cLineA,iFlag)
      if(iFlag .eq. 1) then
         print*, ' WITCH: Fatal error. TEMPREL '//
     &      'card is missing!'
         stop
      endif
      
      
      read(NFIN,*,err=6485) nPiecewise
      do i=1, nPiecewise, 1
         read(NFIN,*,err=6485) nCoefs, 
     &     (raTempRel(i,j),j=1,nCoefs+3)
      enddo 
      
      goto 6487
6485  print*, ' WITCH: Fatal error. Incorrect TEMPREL'//
     &   ' card format!'
      write(*,'(A,I0)')'  Line : ', abs(iFlag-1)
      stop
6487  continue
 
!     END READING #TEMPREL CARD --------------------------------------------      

!     Loop over several data sets for multiple WIMS runs
      call CheckShema(caElementTags, nElem)
      
c ----------- Power factor calculation and normalization (begin) -----------
c ------- Also: This is the initial guess of the power distribution --------

!        P_el = α_P(R) * (m_el / m_avg) * (1 - BU% / 100) * P_avg
!        
!           * P_el = raPel(iEl) = Power of the specified fuel element
!           * α_P(R) = rPFFRing = Power form factor of ring R.
!
!        Note please refer formula (3) in TRIGLAV manual.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            

!     HERE WE CALCULATE THE TOTAL NUMBER OF FUEL CHANNEL (BURNABLE ELEM.)
      
      nElBu = 0
      
      do iEl=1, nElem, 1
         call ReadEle(NFEL, caElementTags(iEl), cCellType,
     &                rMassUZrH, rEnrichment, rMassEr166, rMassEr167, 
     &                rBuMW, rBUPercent)  
         if(rMassUZrH .gt. 0.0) nElBu = nElBu + 1 
      enddo
      
      rewind NFEL
      
      do iEl=1, nElem, 1
!     Reading fuel data with ELEM.INP:cCellType,rM235,Enr,WEr,BuM,BuU
         call ReadEle(NFEL, caElementTags(iEl), cCellType,
     &                rMassUZrH, rEnrichment, rMassEr166, rMassEr167, 
     &                rBuMW, rBUPercent)

     
!        Calculating the mass of U-235
         rMassU235 = rMassUZrH * rEnrichment / 100.0
       
!        Calculating the power of fuel element using the power form factor (PFF).
            rPPF = fPPF(NFIN, iEl, piLayerID, 
     &               iaRing, nRing, nCoreLayers)
     
         raPEl(iEl) = rPPF * rMassU235 * (1.0 - (rBUPercent / 100.0))
         rSumPEl    = rSumPEl + raPEl(iEl)

      enddo
c ----------- Power factor calculation and normalization (end) -------------

!     Calculating the average power of each fuel elements. 
      rPavEl = rPPF * rPthRe / nElBu
    
!     Calculate UZrH temp for non-fuel cell.
         do i=1, nPiecewise, 1
            if( (rPavEl .gt. raTempRel(i,1)) .and.
     &          (rPavEl .le. raTempRel(i,2))  ) then
               
               rTempFuel = 0.0
               ! Here the power series is up to N=10.
               do j=0, 10, 1
                  rTempFuel = rTempFuel + raTempRel(i,j+3)*rPavEl**j
               enddo
               rTempFuel = rTempFuel + rTempLW
               goto 6481
            else
               if(i .eq. nPiecewise) then
                  rTempFuel = rTempLW
               endif
            endif    
         enddo
6481     continue
c ------------- Preparing WIMS inputs for all elements (begin) -------------

      do iEl=1, nElem+1, 1
      
!        FIRST WE FILTER OUT ALL NON FUEL CELLS.
         if(caElementTags(iEl) .eq. 'G   ') then
            cCellType = 'G   '
            goto 36
         endif
         if(caElementTags(iEl) .eq. 'CHN1') then
            cCellType = 'CHN1'
            goto 36
         endif    
         if(caElementTags(iEl) .eq. 'CHN2') then
            cCellType = 'CHN2'
            goto 36
         endif 
         if(caElementTags(iEl) .eq. 'CHN3') then
            cCellType = 'CHN3'
            goto 36
         endif      
         if(caElementTags(iEl) .eq. 'CHN4') then
            cCellType = 'CHN4'
            goto 36
         endif      
         if(caElementTags(iEl) .eq. 'TRCR') then
            cCellType = 'TRCR'
            goto 36
         endif
         if(caElementTags(iEl) .eq. 'BERY') then
            cCellType = 'BERY'
            goto 36
         endif   
         if(caElementTags(iEl) .eq. 'COOL') then
            cCellType = 'COOL'
            goto 36
         endif
         if(caElementTags(iEl) .eq. 'GRAP') then
            cCellType = 'GRAP'
            goto 36
         endif
         if(caElementTags(iEl) .eq. 'W   ') then
            cCellType = 'W   '
            goto 36
         endif
!        Reading fuel data with ELEM.INP:cCellType,rM235,Enr,WEr,BuM,BuU
         call ReadEle(NFEL, caElementTags(iEl), cCellType,
     &                rMassUZrH, rEnrichment, rMassEr166, rMassEr167,
     &                rBuMW, rBUPercent)

!        Correction of data according to the original fuel spec. This is 
!        for after fuel burnup.
         call FuelSpec(cCellType, rDensityUZrHM, rWtU235M, rMassUZrHM, 
     &                rEnrichmentM, rWtEr166M, rWtEr167M,
     &                rMassEr166M, rMassEr167M)
     
!        Correction of data according to the original spec. given by user.
         if(cCellType .eq. 'FE08') then
            rDensityUZrHM =      raDSpec(1)
            rWtU235M      =  raUr235Spec(1)
            rMassUZrHM    =      raMSpec(1)
            rEnrichmentM  =      raESpec(1)
            rWtEr166M     =  raEr166Spec(1)
            rWtEr167M     =  raEr167Spec(1)
            rMassEr166M   = raEr166MSpec(1)
            rMassEr167M   = raEr167MSpec(1)
            
            
       
         elseif(cCellType .eq. 'FE12') then
            rDensityUZrHM =      raDSpec(2)
            rWtU235M      =  raUr235Spec(2)
            rMassUZrHM    =      raMSpec(2)
            rEnrichmentM  =      raESpec(2)
            rWtEr166M     =  raEr166Spec(2)
            rWtEr167M     =  raEr167Spec(2)
            rMassEr166M   = raEr166MSpec(2)
            rMassEr167M   = raEr167MSpec(2)      

         elseif(cCellType .eq. 'FE20') then
            rDensityUZrHM =      raDSpec(3)
            rWtU235M      =  raUr235Spec(3)
            rMassUZrHM    =      raMSpec(3)
            rEnrichmentM  =      raESpec(3)
            rWtEr166M     =  raEr166Spec(3)
            rWtEr167M     =  raEr167Spec(3)
            rMassEr166M   = raEr166MSpec(3)
            rMassEr167M   = raEr167MSpec(3)         
         elseif(cCellType .eq. 'FLIP') then
            rDensityUZrHM =      raDSpec(4)
            rWtU235M      =  raUr235Spec(4)
            rMassUZrHM    =      raMSpec(4)
            rEnrichmentM  =      raESpec(4)
            rWtEr166M     =  raEr166Spec(4)
            rWtEr167M     =  raEr167Spec(4)
            rMassEr166M   = raEr166MSpec(4)
            rMassEr167M   = raEr167MSpec(4)         
         elseif(cCellType .eq. 'LEUF') then
            rDensityUZrHM =      raDSpec(5)
            rWtU235M      =  raUr235Spec(5)
            rMassUZrHM    =      raMSpec(5)
            rEnrichmentM  =      raESpec(5)
            rWtEr166M     =  raEr166Spec(5)
            rWtEr167M     =  raEr167Spec(5)
            rMassEr166M   = raEr166MSpec(5)
            rMassEr167M   = raEr167MSpec(5)         
         endif
         
         rBuMW = 0.0
         rMassU235 = rMassUZrH * rEnrichment / 100.0
         rMassUZrHt = rMassUZrH - (rMassU235 * 
     &         (((rBUPercent) / 100.0)))
         
         rEnrichment = 100.0 * (1.0-0.01*rBUPercent) * 
     &      rMassU235 / rMassUZrHt
     
         if(rMassUZrHM .gt. 0.0)  rDensityUZrH = rDensityUZrHM * 
     &      rMassUZrHt / rMassUZrHM
         if(rEnrichmentM .gt. 0.0)  rWtU235 = rWtU235M * 
     &      rEnrichment / rEnrichmentM
         if(rMassEr166M .gt. 0.0) rWtEr166 = rWtEr166M * 
     &      rMassEr166 / rMassEr166M
         if(rMassEr167M .gt. 0.0) rWtEr167 = rWtEr167M * 
     &      rMassEr167 / rMassEr167M
      
!        Power of fuel element calculation..
         rPEl = raPEl(iEl) * rPthRe / rSumPEl

c     ------------------------------------------------------------------------
c        FUEL TEMPERATURE-POWER RELATION CALCULATION BASED ON EMPIRICAL 
c        FORMULA GIVEN BY OMAR ET. AL.
c     ------------------------------------------------------------------------
         ! Here we check if fuel element power is in valid range
         if(rPel .gt. 20.0) then
            print*, ' Warning: High power element is detected! ',
     &              ' Fuel Tag = ', caElementTags(iEl)
         elseif(rPel .le. 0.0) then
            print*, ' Warning: Zero power element is detected!',
     &              ' Fuel Tag = ', caElementTags(iEl)
         endif
         
         do i=1, nPiecewise, 1
            if( (rPel .gt. raTempRel(i,1)) .and.
     &          (rPel .le. raTempRel(i,2))  ) then
               
               rTempFuel = 0.0
               ! Here the power series is up to N=10.
               do j=0, 10, 1
                  rTempFuel = rTempFuel + raTempRel(i,j+3)*rPel**j
               enddo
               rTempFuel = rTempFuel + rTempLW
               goto 6488
            endif    
         enddo
6488     continue

36       continue
         
         if (lIsZBURNExists .eqv. .true.) then
            rBuMW = raBuCell(iEl)
         endif
         
!        Generates WIMS input, WIMInnn.WIN for each fuel cell index iEl.
         call WimInp(NFW, NFX, NFY, cFileWIMS, iEl, 
     &               caAux, cCellType,
     &               rDensityUZrH, rWtU235, rWtEr166, rWtEr167, rPEl,
     &               rTempFuel, rTempLW, rDensityLW, rMassUZrHt, rBuMW,
     &               nIsXe, nCladType, TME, DTE, rUnitCellRadius,
     &               raRadius(nRing),rRefThick,rDBSquared,nCoreLayers,
     &               raPSpec, rPrevTau)
      enddo

      close(unit=NFX)
      close(unit=NFW)
      close(unit=NFY)
      close(unit=NFIN)
      close(unit=NFZB)
c      close(unit=NFZP)
1000  continue

      end subroutine


! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------  
!                      S U B R O U T I N E ------ WimInp
!
!        This subroutine allows the programmer write a WIMS input file for a
!        unit cell. The parameters are given as follows:
!        
!        * pNFO - Witrig.pri file identifier.
!        * pNFW - WIMS input file identifier -- WimiNNN.win
!        * pNFX - Witrig.$$$ WIMS header file identifier.
!        * pcFileWIMS - WIMS input file name, i.e. WimiNNN.win.
!        * piEl - Unit cell index number.
!        * pcaElementTags - An array of characters containing the Fuel Site Tags.
!          If the unit cell does not contains a fuel element, the element of this
!          array will be disregarded.
!        * pcCellType - The type of the unit cell, i.e. G, GR, W, LW, ST8, ST12...
!        * prDensityUZrH - Density of UZrH in the fuel element (= in cell)
!        * prWtU235 - Percent by weight of U-235 in the unit cell.
!        * prWtEr166 - Percent by weight of Er-166 in the unit cell.
!        * prWtEr167 - Percent by weight of Er-167 in the unit cell.
!        * prPEl - Fuel power of the fuel element inside the unit cell.
!          prPEl = 0.0 if the specified unit cell is not a fuel cell.
!        * prFuelTemp - Temperature of the fuel element inside the unit cell.
!        * prTempLW - Temperature of coolant (Light Water, LW) in the unit cell.
!        * prDensityLW - Density of coolang (Light Water, LW)
!        * prMassUZrH - Mass of UZrH in the fuel cell (if it is a fuel cell)
!        * prBuMW - Fuel burnup in MWd if the cell is a fuel cell.
!        * pnIsXe - A flag indicating whether this code is monitoring Xe
!          buildup.
!        * pTME, pDTE - characters storing the date and time string from
!          rbdatetime function.
!
!  Written by M. R. Omar, 2017.
! ----------------------------------------------------------------------------- 
      subroutine WimInp(pNFW, pNFX, pNFY, pcFileWIMS, piEl, 
     &                  pcaElementTags, pcCellType,
     &                  prDensityUZrH , prWtU235, prWtEr166,
     &                  prWtEr167, prPEl, prFuelTemp, prTempLW,
     &                  prDensityLW, prMassUZrH, prBuMW,
     &                  pnIsXe, pnCladType, pTME, pDTE, prCellRadius,
     &                  prCoreRadius, prReflectorThickness, prBuckling,
     &                  pnCoreLayers, praPSpec, prPrevTau)
         implicit none
         integer, parameter :: nMaxEl = 290
         character(len=*), intent(inout) :: pcFileWIMS
         character(len=*), intent(in) :: pcaElementTags(nMaxEl)
         character(len=*), intent(in) :: pcCellType, pTME, pDTE
         integer, intent(in) :: pNFW, pNFX, pNFY, piEl, pnIsXe
         integer, intent(in) :: pnCladType, pnCoreLayers
         real   , intent(in) :: prDensityUZrH, prWtU235, prWtEr166
         real   , intent(in) :: prWtEr167, prPEl, prFuelTemp
         real   , intent(in) :: prTempLW, prDensityLW, prMassUZrH
         real   , intent(in) :: prBuMW, prCellRadius, prBuckling
         real   , intent(in) :: prCoreRadius, prReflectorThickness
         real   , intent(in) :: praPSpec(5), prPrevTau
         character(len=3) :: cIel ! Dummy variable to for int -> str conversion.
         character(len=11) :: cFileWIMSOUT
!        We begin warmup the idea of naming the WIMS input file.
         pcFileWIMS = 'WIMI000.WIN'
         cFileWIMSOUT = 'WIMI000.WOU'
!        Converting and setting cIel as the character string representation of iEl.
         write(cIel,'(I3)') piEl
!        Formatting the string representation of iEl by adding preceeding 0.
         call rbfint(cIel)
!        Replacing the 3 zeros 0 in cFileWIMS into cIel.
!        iEl = 3 --> cIel = 3 (str) --> Wimi003.win
         pcFileWIMS(5:7) = cIel
         cFileWIMSOUT(5:7) = cIel
 !       Begin write to Witrig.$$$ header file        
         write(pNFX,'(''   WIFLNM '',A)') pcFileWIMS
         write(pNFY,'(''   WIFLNM '',A)') cFileWIMSOUT
         open(unit=pNFW, file=pcFileWIMS, status='UNKNOWN')

         write(pNFW,*)
     &      '*WIMS INPUT @', pTME, pDTE, ' (i, Tag, Type)',
     &      piEl, pcaElementTags(piEl), pcCellType
         print*, piEl, pcaElementTags(piEl), ' ', pcCellType, ' ',
     &          prMassUZrH, prDensityUZrH
         write(pNFW,'(''*  '',A,G0.5)') 
     &      'UZrH Density (g cm-3)   : ', prDensityUZrH
         write(pNFW,'(''*  '',A,G0.5)') 
     &      'Wt % U-235              : ', prWtU235
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Wt % Er-166             : ', prWtEr166
         write(pNFW,'(''*  '',A,G0.5)') 
     &      'Wt % Er-167             : ', prWtEr167
         write(pNFW,'(''*  '',A,G0.10)') 
     &      'Fuel Power, PEl (kW)    : ', prPEl

         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Fuel Temp. (K)          : ', prFuelTemp
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Coolant Temp. (K)       : ', prTempLW
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Coolant Density (g cm-3): ', prDensityLW
         write(pNFW,'(''*  '',A,G0.5)') 
     &      'UZrH Mass (g)           : ', prMassUZrH
         write(pNFW,'(''*  '',A,G0.5)') 
     &      'Fuel Burnup (MWd)       : ', prBuMW

         call WimCom(pNFW, pcCellType, prDensityUZrH,
     &               prWtU235, prWtEr166, prWtEr167, prPEl,
     &               prFuelTemp, prTempLW, prDensityLW,
     &               prMassUZrH, prBuMW, pnIsXe, pnCladType,
     &               prCellRadius, prCoreRadius,
     &               prReflectorThickness, prBuckling, pnCoreLayers,
     &               praPSpec, prPrevTau)
     
     
         close(pNFW)
         return
      end subroutine

! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------  
!                      S U B R O U T I N E ------ CheckShema
!
!        This subroutine allows the programmer to check the fuel scheme.
!        This subroutine checks for fuel name duplicates and also fuel
!        element number duplicates.
!        
!        * pcaElementTags - An array of characters containing the Fule Site Tags.
!        * pnElem - Number of fuel elements in the reactor.
!
! ----------------------------------------------------------------------------- 
      subroutine CheckShema(pcaElementTags, pnElem)

      integer, intent(in) :: pnElem
      character(len=4), intent(in) :: pcaElementTags(pnElem)
      integer :: i, j, k
         do i=1, nElem, 1
            do j=1, nElem, 1
               if(j .ne. i) then
                  if(pcaElementTags(i) .eq. pcaElementTags(j)) then
                     write(*, 900) pcaElementTags(i), i, j
                     k = ichar(pcaElementTags(i)(1:1))
                     if((k .ge. 48) .and. (k .le. 57)) then
                        write(*, 901) pcaElementTags(i)
                     endif
                  endif
               endif
            enddo
         enddo
         return
900      format(' Warning: Same element names: ',a,i4,i4)
901      format(' Warning: Same fuel element numbers: ', A) 
      end subroutine

      
! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------  
!                      S U B R O U T I N E ------ ReadEle
!
!     This subroutine enables us to read the fuel data from ELEM.INP.
!     This subroutine allows the programmer to search for the data of
!     the desired fuel element (searching its Fuel Site Tag, pcFuelTag).
!     Data includes the following:
!
!        * pNFEL - The file index specifying the ELEM.INP file.
!        * pcaElementTags - Characters of fuel element tags.
!        * pcElementType - Fuel element type (ST8, ST12, LEU, ...)
!        * prMassU - Mass of Uranium in the fuel element.
!        * prEnrichment - Fuel enrichment of the fuel element.
!        * prMassEr166 - Mass of Er-166 in the fuel.
!        * prMassEr167 - Mass of Er-167 in the fuel.
!        * prBuMW - Fuel element burnup in MW.
!        * prBuU235 - FUel element burnup of U-235.
!
! -----------------------------------------------------------------------------  
      subroutine ReadEle(pNFEL, pcFuelTag, pcCellType, prMassU, 
     &                   prEnrichment, prMassEr166, prMassEr167,
     &                   prBuMW, prBuU235)


         integer, intent(in) :: pNFEL
         character(len=4), intent(in)  :: pcFuelTag
         real   , intent(out) :: prMassU, prEnrichment, prMassEr166, 
     &                           prMassEr167, prBuMW, prBuU235

         character(len=4), intent(out) :: pcCellType
      
!        cFuelTagRead is just a character variable for reading fuel tag (6574 , 6945 , ...)
         character(len=4) :: cFuelTagRead
         integer :: NFER
         integer :: iX
      
!        Initialise all parameter values.
         NFER         = 9
         prMassU      = 0.0
         prEnrichment = 0.0
         prMassEr166  = 0.0
         prMassEr167  = 0.0
         prBuMW       = 0.0
         prBuU235     = 0.0
         iX           = 0
      
!        Check whether the 
         if(trim(pcFuelTag) .eq. 'W' .or. 
     &      trim(pcFuelTag) .eq. 'G' .or. 
     &      trim(pcFuelTag) .eq. 'COOL' .or.
     &      trim(pcFuelTag) .eq. 'GRAP' .or.
     &      trim(pcFuelTag) .eq. 'BERY' .or.
     &      trim(pcFuelTag) .eq. 'CHN1' .or.
     &      trim(pcFuelTag) .eq. 'CHN2' .or.
     &      trim(pcFuelTag) .eq. 'CHN3' .or.
     &      trim(pcFuelTag) .eq. 'CHN4' .or.
     &      trim(pcFuelTag) .eq. 'TRCR') then  
            pcCellType = trim(pcFuelTag)
            goto 999
         endif      
         rewind pNFEL      

         call rbskip(pNFEL,1)

1        continue
         iX = 0
         read(pNFEL,100,end=999,err=998) 
     &      cFuelTagRead, pcCellType, prMassU,
     &      prEnrichment, prMassEr166, prMassEr167,
     &      prBuMW, prBuU235
         if(cFuelTagRead .ne. pcFuelTag) goto 1
2        read(pNFEL,100,end=999) cFuelTagRead
         if(cFuelTagRead .eq. pcFuelTag) then
            iX = iX + 1
         endif

         goto 2
998      continue

         write(*,*) 'Fatal Error: An error has occured while',
     &              ' reading FUEL_INVENTORY.INP.'
         stop
999      return
100      format(A4,1X,A4,1X,6F10.0)

900      format(' Fatal Error: Missing fuel element... ', A)
901      format(' Warning    : Multiple fuel elements... ', A, I4)
      end subroutine

      
      
      
! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------      
!                     F U N C T I O N ------ fRoWater(rTempLW)
! 
!     Defines the function that calculates the density of water (rDensityLW) 
!     as a unction of water/coolant temperature. The calculation is based 
!     on empirical formula > rDensityLW = [A] + [B]*Twater
!
!     Cooling water density decreases with increasing temperature (the effect 
!     is small and its influence on the neutron behavior is neglected, changing 
!     the water density by 1% changes the multiplication factor by 6E-04 [3])
!
! -----------------------------------------------------------------------------    
      real function fRoWater(rTempLW)
         real, intent(in) :: rTempLW
         fRoWater = 1.131021 - 0.00045 * rTempLW
         return
      end function

! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------  
!                      S U B R O U T I N E ------ fFuelSpec
!
!     Sets all of the fuel element parameter values according to the fuel 
!     element type (ST8, ST12, ...). Please  note the following parameter 
!     names:
!
!        * prDensityU   - Density of U-ZrH in the fuel element.
!        * prWtU235     - Percent weight of U-235 in %.
!        * prMassU      - Mass of uranium in the whole fuel element.
!        * prEnrichment - Fuel enrichment.
!        * Wt166I, Wt167I - Percent weight of Er-166/Er-167.
!        * rM166I, rM167I - Mass of Er-166/Er-167.
!
!     This subroutine will identify the fuel element type where the element 
!     type is acquired from cCellType.
!
! -----------------------------------------------------------------------------  

      subroutine FuelSpec(pcCellType, prDensityU, prWtU235, prMassU, 
     &                   prEnrichment, prWtEr166, prWtEr167,
     &                   prMassEr166, prMassEr167)
         character(len=*), intent(in) :: pcCellType
         real, intent(out) :: prDensityU, prWtU235, prMassU,
     &      prEnrichment, prWtEr166, prWtEr167, prMassEr166, 
     &      prMassEr167
     
         if(pcCellType .eq. 'FE08') then
            prDensityU   = 5.8107    !5.784889
            prWtU235     = 0.016897855
            prMassU      = 190.45
            prEnrichment = 19.9
            prWtEr166    = 0.0
            prWtEr167    = 0.0
            prMassEr166  = 0.0
            prMassEr167  = 0.0
         endif 
         if(pcCellType .eq. 'FE12') then
            prDensityU   = 6.22438  !6.714887659   !6.12226       
            prWtU235     = 0.023323 !0.021428571   !0.023323  
            prMassU      = 278.0    !276.54712     !275.0000036
            prEnrichment = 19.90341
            prWtEr166    = 0.0
            prWtEr167    = 0.0
            prMassEr166  = 0.0
            prMassEr167  = 0.0
         endif 
         if(pcCellType .eq. 'FE20') then
            prDensityU   = 7.770084290
            prWtU235     = 0.033333333
            prMassU      = 494.9802000
            prEnrichment = 19.90000000
            prWtEr166    = 0.0
            prWtEr167    = 0.0
            prMassEr166  = 0.0
            prMassEr167  = 0.0
         endif       
         if(pcCellType .eq. 'FLIP') then
            prDensityU   = 5.85955
            prWtU235     = 0.0594105
            prMassU      = 191.80805
            prEnrichment = 69.96179
            prWtEr166    = 0.0051105
            prWtEr167    = 0.0035039
            prMassEr166  = 11.54324
            prMassEr167  = 7.91444
         endif
         if(pcCellType .eq. 'LEU ') then
            prDensityU   = 6.38695
            prWtU235     = 0.039737
            prMassU      = 494.92055
            prEnrichment = 19.76759
            prWtEr166    = 0.001463
            prWtEr167    = 0.001
            prMassEr166  = 3.60196
            prMassEr167  = 2.46203
         endif
         return
      end subroutine

      
! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------  
!                F U N C T I O N ------ fPFFRing(iel,iaRing,nRing)
!
!     This function returns the  power  form factors of  the specified element
!     number (iel). If the element iel belongs to a ring (i.e. Ring-A, Ring-B, 
!     Ring-C ..), then the power form factor is set to the corresponding value 
!     for the ring:
!
!        Ring-A = 1.0
!        Ring-B = 0.9
!        Ring-C = 0.8
!        Ring-D = 0.7
!        Ring-E = 0.6
!        Ring-F = 0.5
!        Ring-G = 0.5
!
!     ---------------------------------------------------------------------

      real function fPPF(pNFIN, iEl,iLayer,iaRing,nRing,nLayers)
         integer, intent(in) :: iEl, nRing, pNFIN
         integer, intent(in) :: iLayer, nLayers
         integer, intent(in) :: iaRing(7)
         real, allocatable :: raAxialPPF(:), raRadialPPF(:)
         integer iElSum, iRing
         character(len=80) :: cLineA
         character(len=2) :: cRing, cLayer
         integer :: iFlag
         integer :: i
         
         rewind pNFIN
      !  Set Power Form Factor Values (PFF)
         allocate(raAxialPPF(nLayers), raRadialPPF(nRing))
         call rbsearch(pNFIN, 0, '#PFF', 4, cLineA, iFlag)
         if(iFlag .eq. 1) then
            fPPF = 1.0
            return
         else
            read(pNFIN, *, end=232,err=232)
     &          (raRadialPPF(i), i=1, nRing)
            read(pNFIN, *, end=234, err=234) 
     &          (raAxialPPF(i), i=1, nLayers)         
         endif
       
         goto 235
232      print*, ' WITCH: Fatal error. Bad radial PFF card input.'
         stop
234      print*, ' WITCH: Fatal error. Bad axial PFF'//
     &         ' card input.'
         stop
235      continue
         iElSum = 0
         do iRing=1, nRing, 1
            iElSum = iElSum + iaRing(iRing)
            if(iElSum .ge. iEl) goto 1 
         enddo
 1       continue
         
         if(iRing .eq. 1) fPPF = raRadialPPF(1)*raAxialPPF(iLayer)
         if(iRing .eq. 2) fPPF = raRadialPPF(2)*raAxialPPF(iLayer)
         if(iRing .eq. 3) fPPF = raRadialPPF(3)*raAxialPPF(iLayer)
         if(iRing .eq. 4) fPPF = raRadialPPF(4)*raAxialPPF(iLayer)
         if(iRing .eq. 5) fPPF = raRadialPPF(5)*raAxialPPF(iLayer)
         if(iRing .eq. 6) fPPF = raRadialPPF(6)*raAxialPPF(iLayer)
         if(iRing .eq. 7) fPPF = raRadialPPF(7)*raAxialPPF(iLayer)
         return 
      end function


      ! --------------------------------------------------------------- !
      ! Title  : rbsearch Subroutine
      ! Purpose: Scan a file for a specified string
      !
      ! Text   - The text to be searched for.
      ! NF     - File number identifier.
      ! NSkip  - Number of characters to be skipped from search.
      ! TxtLen - Character length of the string to be searched.
      ! --------------------------------------------------------------- !
      
      subroutine rbsearch(pNF, pnSkip, pcText, pnTextLen, 
     &                    pcTextRV, piFlag)
         
         character(len=*), intent(in)  :: pcText
         character(len=*), intent(out) :: pcTextRV ! Text return value (RV)
         integer,          intent(in)  :: pNF, pnSkip, pnTextLen
         integer,          intent(out) :: piFlag
         integer                         :: i
         
         piFlag = 0
         i = pnSkip
         rewind pNF
1        continue
         read(pNF,'(A,A)', end=999) pcTextRV
         if(pcText(1:pnTextLen) .ne. pcTextRV(i+1:i+pnTextLen)) goto 1
         return
999      piFlag = 1
         return
         
      end subroutine      

      
      ! --------------------------------------------------------------- !
      ! Title  : rbskip Subroutine
      ! Purpose: A subroutine to skip line rows.
      !
      ! pNF - File identifier index of the desired file.
      ! pnSkip - Number of line(s) to skip.
      !
      ! --------------------------------------------------------------- !
      
      subroutine rbskip(pNF, pnSkip)
         implicit none
         integer, intent(in) :: pNF, pnSkip
         character(len=1) :: ch
         integer :: i
         if(pnSkip .le. 0) return
         do i=1,pnSkip
            read(pNF,'(A80)') ch
         enddo
         return
      end subroutine
      
      ! --------------------------------------------------------------- !
      ! Title  : rbdatetime Subroutine
      ! Purpose: A subroutine to return date and time as strings.
      !
      ! --------------------------------------------------------------- !     
      subroutine rbdatetime(ParamDate, ParamTime)
            implicit none
            character(len=*), intent(out) :: ParamDate, ParamTime
            character(len=100) :: Dummy(3)
            integer            :: Value(8)
            
            call date_and_time(Dummy(1), Dummy(2), Dummy(3), Value)
            
            write(ParamDate, '(I2,A,I2,A,I4)') 
     &            Value(3), '/', Value(2), '/', Value(1)
     
            write(ParamTime, '(I2,A,I2,A,I2)')
     &            Value(5), ':', Value(6), ':', Value(7)
            
            return
      end subroutine
      
      ! --------------------------------------------------------------- !
      ! Title  : rbfint Subroutine
      ! Purpose: A subroutine to format string representation of integer
      !          by replacing whitespace with preceeding 0.
      !          i.e. 10 --> 010, 2 --> 002, 12 --> 012
      !
      ! --------------------------------------------------------------- !        
      subroutine rbfint(cNiz)
      implicit none
         character(len=*), intent(inout) :: cNiz
         integer :: i, rblenf
         do i=1, rblenf(cNiz)  
            IF(cNiz(i:i) .eq. ' ') cNiz(i:i) = '0'
         enddo
         return
      end subroutine

      integer function rblenf(pcString)
         implicit none
         integer :: mlen, i
         character(len=*), intent(in) :: pcString
         rblenf = 1
         mlen=LEN(pcString)
         do i=mlen, 1, -1
            if(pcString(i:i) .ne. ' ') then
               rblenf = i
               return       
            endif
         enddo   
      end function
      