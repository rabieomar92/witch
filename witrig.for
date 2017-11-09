!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 27/8/2017 9:55:27 PM
!     Author         : RABIEMSI\RABIE
!     Device Name    : RABIEMSI
!     Project Name   : TRIGLAV.fip
!     Source File    : witrig.for
!     --------------------------------------------------------

!     CLASSIFIED AND NOT TO BE DISCLOSED.
!     Declaration of WITRIG for F90 compatible. 
!     This code is written by M. R. Omar for Malaysian Nuclear Agency.
!     Purpose: Prepare WIMS input files (WIMInnn.WIN) for each unit cells.
!              This subroutine also prepares the header file WiTrig.$$$.
!
!
!
!     rrrraPel()   - field, power by element (PE)
!     iaRing() - the number of fuel elements in the ring.
!     PthRe    - power of the reactor (TRIGLAV.INP)
!     PavEl    - average power per element (samo BU)
!     caAux    - field of site location tags (A-01,B-01,B-02...)
!     caElementTags   - field of tag elements

c      program witrig
      subroutine witrig(pcWITCHInp, pcELEMInp)
      implicit none
      
      character(len=50), intent(in) :: pcWITCHInp, pcELEMInp
      
      ! Declaration of function header(s)
      real :: fRoWater ! returns the density of water at the specified temperature.
      real :: fPFFRing ! return the power form factor of the specified fuel ring.
      
      
      ! Integer for iteration loop purpose. Could be used everywhere in this code.
      integer :: i
      ! Declaring integer for fuel ring index, value varies from 1 to 10.
      integer :: iRing
      ! Maximum allowable number of fuel elements = 290.
      integer, parameter :: nMaxEl = 290
      ! Maximum number of TRIGA core fuel elements ring = 10
      integer, parameter :: nMaxRing = 10
      !  Declaring an integer for storing the number of fuel ring in the TRIGLAV.INP setup.
      integer           :: nRing
      ! Declaring the number of fuel elements in the ring.
      integer, allocatable :: iaRing(:)
      ! Declaring the array of power of each fuel element in the TRIGA reactor.
      real   , allocatable :: raPel(:)
      !  Default input file name...
      character(len=50), parameter :: WITCHINP = 'WITCH.INP'
      character(len=50), parameter :: ELEMINP    = 'ELEM.INP'
      character(len=50), parameter :: TRIGLAVOUT = 'TRSTART.OUT'
      character(len=50), parameter :: WIMSINPUTDIR = 'wims_input'
      ! File unit numbers.
      integer :: NFIN, NFP, NFEL, NFW, NFX, NFO, NFER, NFY
      ! Code input parameters (from TRIGLAV.INP)
      integer :: nIsXe
      ! Declaring a variable that stores the thermal power of the reactor, i.e. 1000kWt?
      real    :: rPthRe
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
      ! Declaring TRIGLAV input card searching status. 
      !     0-Search found
      !     1-Search failed
      integer           :: iFlag      
      ! Declaring variable for fuel element index for fuel data array retrieval.
      integer :: iEl      
      ! Declaring the number of fuel elements that still have the U-235 in it (not fully burned U-235)
      integer :: nElBu
      ! Declaring the number of fuel elements inside the reactor
      integer :: nElem
      
      character(len=4) :: caElementTags(nMaxEl), caAux(nMaxEl)
      character(len=4) :: cCellType
      ! Sum power of fuel elements. 
      real :: rSumPEl      
      ! Declaring mass of uranium, fuel enrichment, mass of Erbium 166/167, Burnup in MW, Burnup of U-235.
      real :: rMassUZrH, rDensityUZrH, rEnrichment, rMassEr166, rWtU235
      ! rBuMW - Burnup of the fuel element in MW
      ! rBUPercent - Burnup of U-235 in the fuel element in MW.
      real :: rMassEr167, rBuMW, rBUPercent
      real :: rMassU235, rPFFRing
      ! Declaring weight by percent of Er-166 and Er-167 for FLIP fuel and LEU fuel.
      real :: rWtEr166, rWtEr167
      ! Declaring fuel temperature
      real :: rTempFuel
      ! Dummy fuel element variables for fuel element data correction according to default spec.
      real :: rDensityUZrHM, rWtU235M, rMassUZrHM, rEnrichmentM, 
     &        rWtEr166M, rWtEr167M, rMassEr166M, rMassEr167M     
      

      
      allocate (iaRing(nMaxRing))
      allocate (raPel(nMaxEl))

! -----------------------------------------------------------------------------
!                         PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------
!     ** NFIN is the file ID for reading TRILAV.OUT file.
!        NFP  is the file ID for reading the power distribution file.
!        NFEL is the file ID for reading the ELEM.INP file.
!        NFW  is the file ID for writing the WIMS input file.
!        NFO  is the file ID for writing WiTrig.pri file.
!        NFX  is the file ID for writing the WIMS input header file (WiTrig.$$$)
!        NFER is the file ID for writing the error log file.
! -----------------------------------------------------------------------------
      data NFIN, NFP, NFEL, NFW, NFX, NFY / 11, 12, 13, 21, 22, 30 /
      data NFO, NFER / 23, 9 /
      
!     Initialize the total sum of power element to zero for later sum iteration calculation.      
      rSumPEl = 0.0
      
!     Get the system date and time to label WIMS input
      call rbdatetime(DTE, TME)

      print*, ' Running ROUTINE-B.'
!     Set the input file names to the default defined param values.
c      cFilInp = 'WITCH.TMP'
c      cFilEle = 'ELEM.INP'
       cFilInp = trim(pcWITCHInp) // '.TMP'
       cFilEle = trim(pcELEMInp) // '.INP'
       
!     Open file stream for the two input files (TRIGLAV.INP & ELEM.INP)      
      open(unit=NFIN, file=cFilInp, status='OLD', err=911)
      open(unit=NFEL, file=cFilEle, status='OLD', err=911)
      goto 915
911   write(*,*) ' Fatal Error: Could not load input file(s)!'
      stop
915   continue
!     Open file stream for the two output files (Witrig.pri & WiTrig.$$$)
      open(unit=NFO,    file='WITCH-OUT.OUT', status='UNKNOWN')
      open(unit=NFX,    file='WIMS-IN.$$$', status='UNKNOWN')
      open(unit=NFY,    file='WIMS-OUT.$$$', status='UNKNOWN')



      
!     Search for No. of elements

!     Search for xenon correction flag (0-Xe free or 1-Xe in equilibrium)
      call rbsearch(NFIN ,0, '$* XENON', 8, cLineA, iFlag)

      if(iFlag .eq. 1) then
         print*, ' Warning: XENON card is not found! Equilibrium Xe'
         print*, ' concentration monitor is turned off by default.'
         nIsXe = 0
      else
         read(NFIN,*) nIsXe
      endif
      rewind NFIN
!     Search for thermal reactor power in kW
      call rbsearch(NFIN, 0, '$* POWER', 8, cLineA, iFlag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: POWER card is not found!'
         goto 1000
      else
         read(NFIN,*) rPthRe
      endif
      
!     Search for SHEMA1 card.
      call rbsearch(NFIN, 0, '$* SHEMA1', 9, cLineA, iFlag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: SHEMA1 card is not found!'
         goto 1000
      endif
      read(NFIN,*,err=912) nRing
      read(NFIN,*,err=912) (iaRing(i), i=1, nRing)
      goto 913
912   print*, ' Fatal Error: Cannot read the fuel element scheme',
     &        ' from the'
      print*, ' input file.'
      stop
913   continue
      nElem=0
      nElBu=0
      do i=1, nRing, 1
         nElem = nElem + iaRing(i)
      enddo
      if(nElem .gt. nMaxEl) then
         print*, ' Fatal Error: Too many fuel elements!'
      endif
      
!     Seach for SHEMA2 card.
      call rbsearch(NFIN, 0, '$* SHEMA2', 9, cLineA, iFlag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: SHEMA2 card is not found!'
         goto 1000
      endif
      
!     Read fuel elements tag by ring.
      i = 0
      do iRing = 1, nRing, 1
         read(NFIN,'(6(A5,1X,A4))') (caAux(i+iEl), caElementTags(i+iEl),
     &                               iEl=1, iaRing(iRing))
         i = i + iaRing(iRing)
      enddo
      
!     Water temperature: rTempLW, rDensityLW
      rewind NFIN
      call rbsearch(NFIN,0,'$* TWATER',9,cLineA,iflag)
      if(iFlag .eq. 1) then
         print*, ' Fatal Error: TWATER card is not found!'
         goto 1000
      endif
      read(NFIN,'(F10.0)') rTempLW
      rDensityLW = fRoWater(rTempLW)

!     We will see that we need to write WIMS outputs and copy this information to WITRIG.$$$
      rewind NFIN
      call rbsearch(NFIN,0,'$* WIMSOUTPUT',13, cLineA,iFlag)
      if(iFlag .eq. 0) write(NFX,'(A)') '   WIMSOUT '
      if(iFlag .eq. 1) write(NFX,'(A)') '   WIMS-NO '
      
!     Control printouts

      write(23,*) ' WIMS INPUT GENERATOR FOR TRIGA MARK',
     &            ' II REACTOR'
      write(23,*) ' for Malaysian Nuclear Agency'
      write(23,*) ' Author: M. R. Omar, Universiti Sains Malaysia'
      write(23,*) ' Time  : ', DTE, ' ', TME
      write(23,*) ''
      print*
      write(*,*)  '                      S U M M A R Y'
      write(*,*) 
      write(*,'(A,I17)') '     Number of core cells        : ', nElem
      write(*,'(A,I17)') '     Number of core ring         : ', nRing
      write(*,'(A,ES17.6)') '     Coolant Temperature (K)     : ', 
     &    rTempLW
      write(*,'(A,ES17.6)') '     Coolant Density (g cm-3)    : ',
     &    rDensityLW
      write(23,*) ' S U M M A R Y'
      write(23,'(A,I17)') ' Number of core cells        : ', nElem
      write(23,'(A,I17)') ' Number of core ring         : ', nRing
      write(23,'(A,F17.6)') ' Coolant Temperature (K)     : ', rTempLW
      write(23,'(A,F17.6)')  ' Coolant Density (g cm-3)    : ',
     & rDensityLW   
      write(23,'(A,I17)') ' Number of element(s) that   : ',
     &             nElBu
      write(23,*)         '  contains U-ZrH fuel'
      write(23,'(A,ES17.6)') ' Reactor power (kWT)         : ',
     &             rPthRe
      write(23,'(A,ES17.6)') ' Total fuel power (kW)       : ',
     &    rSumPel
      write(23,'(A,ES17.6)') ' Average power of each fuel  : ',
     &    rPavEl
      write(23,*) 
      write(23,*) ' C O R E   C E L L   S C H E M E'
      write(23,'(6(I4,1X,A5,1X,A4))') 
     &           (iEl, caAux(iEl), caElementTags(iEL), iEl=1, nElem)
      write(23,*)
      write(23,*)
      
!     Loop over several data sets for multiple WIMS runs
      call CheckShema(NFO, caElementTags, nElem)
      
c ----------- Power factor calculation and normalization (begin) -----------
c ------- Also: This is the initial guess of the power distribution --------

!        P_el = α_P(R) * (m_el / m_avg) * (1 - BU% / 100) * P_avg
!        
!           * P_el = rrrraPel(iEl) = Power of the specified fuel element
!           * α_P(R) = rPFFRing = Power form factor of ring R.
!
!        Note please refer formula (3) in TRIGLAV manual.

      do iEl=1, nElem, 1
!     Reading fuel data with ELEM.INP:cCellType,rM235,Enr,WEr,BuM,BuU
         call ReadEle(NFEL, NFO, caElementTags(iEl), cCellType,
     &                rMassUZrH, rEnrichment, rMassEr166, rMassEr167, 
     &                rBuMW, rBUPercent)
     
!        The number of fuel elements in the scheme
         if(rMassUZrH .gt. 0.0) nElBu = nElBu + 1 
!        Calculating the mass of U-235
         rMassU235 = rMassUZrH * rEnrichment / 100.0
!        Calculating the power of fuel element using the power form factor (PFF).
         rPFFRing  = fPFFRing(iEl, iaRing, nRing)
         raPEl(iEl) = rPFFRing * rMassU235 * (1 - (rBUPercent / 100.0))
         rSumPEl   = rSumPEl + raPEl(iEl)
      enddo
c ----------- Power factor calculation and normalization (end) -------------

!     Calculating the average power of each fuel elements. 
      rPavEl = rPthRe / nElBu
      write(*,'(A,I17)') '     Number of element(s) that   : ',
     &             nElBu
      write(*,*)         '     contains U-ZrH fuel'
      write(*,'(A,ES17.6)') '     Reactor power (kWT)         : ',
     &             rPthRe
      write(*,'(A,ES17.6)') '     Total fuel power (kW)       : ',
     &    rSumPel
      write(*,'(A,ES17.6)') '     Average power of each fuel  : ',
     &    rPavEl
      
c ------------- Preparing WIMS inputs for all elements (begin) -------------

!     Printing table headers...
      print*
      write(*,*) ' Processing cell input. Please wait...'
c      write(*,'(A59)') 
c     &   ' __________________________________________________________'
c      write(*,'(A4,2X, 2A6, 1X, 4A10)')
c     &    'i', 'TAG', 'TYPE', 'm-UZrH(g)', 'e %', 
c     &                'BU (MWd)', 'BU %'
c           write(*,'(A59)') 
c     &   ' __________________________________________________________'
      do iel=1, nElem, 1
!        Reading fuel data with ELEM.INP:cCellType,rM235,Enr,WEr,BuM,BuU
         call ReadEle(NFEL, NFO, caElementTags(iEl), cCellType,
     &                rMassUZrH, rEnrichment, rMassEr166, rMassEr167,
     &                rBuMW, rBUPercent)
     
!        Correction of data according to the baseline: RoUr, W235, W166I, W167I
         call fIzhPod(cCellType, rDensityUZrHM, rWtU235M, rMassUZrHM, 
     &                rEnrichmentM, rWtEr166M, rWtEr167M,
     &                rMassEr166M, rMassEr167M)
     
         rMassU235 = rDensityUZrHM * rEnrichment / 100.0
      
         if(rMassUZrHM .gt. 0.0)  rDensityUZrH = rDensityUZrHM * 
     &      rMassUZrH / rMassUZrHM
         if(rEnrichmentM .gt. 0.0)  rWtU235 = rWtU235M * 
     &      rEnrichment / rEnrichmentM
         if(rMassEr166M .gt. 0.0) rWtEr166 = rWtEr166M * 
     &      rMassEr166 / rMassEr166M
         if(rMassEr167M .gt. 0.0) rWtEr167 = rWtEr167M * 
     &      rMassEr167 / rMassEr167M
      
!        Power of fuel element calculation..
         rPEl = raPEl(iEl) * rPthRe / rSumPEl
         
!        Fuel temperature calculation... This is empirical formula!
         if(rPEl .le. 8.0) rTempFuel 
     &   = rTempLW + 67.18 * rPEl - 8.381 * rPEl**2 + 0.3843 * rPEl**3
     
         if(rPEl .gt. 8.0) rTempFuel = rTempLW + 157.8 + 5.0 * rPEl
         if(rPEl .gt. 20.0) print*,
     &      'Warning: High power element is detected! Fuel Tag = ',
     &      caElementTags(iEl)
         
         write(23,'(A4,2X,2A6,1X,4A10)')
     &       'i', 'TAG', 'TYPE', 'm-UZrH(g)', 'e %', 
     &                     'BU (MWd)', 'BU %'
         write(23,'(I4,A2,2A6,1X,4F10.5)') iEl , ' ',
     &      caElementTags(iEl), 
     &      cCellType, rMassUZrHM, rEnrichment, rBuMW,
     &      rBuPercent
c         write(*,'(I4,A2,2A6,1X,6F10.5)') iEl ,' ', caElementTags(iEl), 
c     &      cCellType, rMassUZrHM, rEnrichment, rBuMW, rBuPercent
         write(23,*) '  | '
         write(23,*) '  | '
         
         if(cCellType .eq. 'ST8 ' .or. cCellType .eq. 'ST12') then
            write(23,*) '  |____  F U E L   S P E C I F I C A T I O N S'
            write(23,*) '  |      m-UZrH (g)            : ',
     &          rMassUZrHM
            write(23,*) '  |      Enrich. (%)           : ', 
     &          rEnrichmentM
            write(23,*) '  |      Density UZrH (g cm-3) : ',
     &          rDensityUZrHM
            write(23,*) '  | '
            write(23,*) '  |____  F U E L   C O N S T A N T S'
            write(23,*) '  |      Density of UZrH (g cm-3)      : ',
     &              rDensityUZrH
            write(23,*) '  |      Fuel Element Power (kW)       : ',
     &         rPEl
            write(23,*) '  |      Fuel Temperature (K)          : ',
     &              rTempFuel
         endif
         
         write(23,*) '  |      Local Coolant Temperature (K) : ',
     &       rTempLW
         write(23,*) '  |      Local Coolant Density (g cm-3): ',
     &              rDensityLW
         write(23,*) '  |      Xe Concentration Monitor      : ', nIsXe

!        Generates WIMS input, WIMInnn.WIN for each fuel cell index iEl.
         call WimInp(NFW, NFX, NFY, NFO, cFileWIMS, iEl, 
     &               caElementTags(iEl), cCellType,
     &               rDensityUZrH, rWtU235, rWtEr166, rWtEr167, rPEl,
     &               rTempFuel, rTempLW, rDensityLW, rMassUZrH, rBuMW,
     &               nIsXe, TME, DTE)
      enddo
      print*, ' ROUTINE-B completed.'
      close(unit=NFX)
      close(unit=NFW)
      close(unit=NFY)
      close(unit=NFO)
      close(unit=NFIN)

!     Managing WIMS input file directory. All of the WIMS input file
!     will be stored in WIMSINPUTDIR folder. To change the directory
!     name, please edit the WIMSINPUTDIR parameter at the declaration
!     section of this program.


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
      subroutine WimInp(pNFW, pNFX, pNFY, pNFO, pcFileWIMS, piEl, 
     &                  pcaElementTags, pcCellType,
     &                  prDensityUZrH , prWtU235, prWtEr166,
     &                  prWtEr167, prPEl, prFuelTemp, prTempLW,
     &                  prDensityLW, prMassUZrH, prBuMW,
     &                  pnIsXe, pTME, pDTE)
         implicit none
         integer, parameter :: nMaxEl = 270
         character(len=*), intent(inout) :: pcFileWIMS
         character(len=*), intent(in) :: pcaElementTags(nMaxEl)
         character(len=*), intent(in) :: pcCellType, pTME, pDTE
         integer, intent(in) :: pNFW, pNFX, pNFY, pNFO, piEl, pnIsXe
         real   , intent(in) :: prDensityUZrH, prWtU235, prWtEr166
         real   , intent(in) :: prWtEr167, prPEl, prFuelTemp
         real   , intent(in) :: prTempLW, prDensityLW, prMassUZrH
         real   , intent(in) :: prBuMW
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

         write(pNFW,'(''*  '',A,F10.5)') 
     &      'UZrH Density (g cm-3)   : ', prDensityUZrH
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Wt % U-235              : ', prWtU235
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Wt % Er-166             : ', prWtEr166
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Wt % Er-167             : ', prWtEr167
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Fuel Power, PEl (kW)    : ', prPEl
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Fuel Temp. (K)          : ', prFuelTemp
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Coolant Temp. (K)       : ', prTempLW
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Coolant Density (g cm-3): ', prDensityLW
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'UZrH Mass (g)           : ', prMassUZrH
         write(pNFW,'(''*  '',A,F10.5)') 
     &      'Fuel Burnup (MWd)       : ', prBuMW

         call WimCom(pNFW, pNFO, pcCellType, prDensityUZrH,
     &               prWtU235, prWtEr166, prWtEr167, prPEl,
     &               prFuelTemp, prTempLW, prDensityLW,
     &               prMassUZrH, prBuMW, pnIsXe)
     
     
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
!        * pNFO - Witrig.pri file identifier.
!        * pcaElementTags - An array of characters containing the Fule Site Tags.
!        * pnElem - Number of fuel elements in the reactor.
!
! ----------------------------------------------------------------------------- 
      subroutine CheckShema(pNFO, pcaElementTags, pnElem)

      integer, intent(in) :: pNFO, pnElem
      character(len=4), intent(in) :: pcaElementTags(pnElem)
      integer :: i, j, k
         do i=1, nElem, 1
            do j=1, nElem, 1
               if(j .ne. i) then
                  if(pcaElementTags(i) .eq. pcaElementTags(j)) then
                     write(pNFO, 900) pcaElementTags(i), i, j
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
!        * pNFO  - The file index specifying the Witrig.pri file.
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
      subroutine ReadEle(pNFEL, pNFO, pcFuelTag, pcCellType, prMassU, 
     &                   prEnrichment, prMassEr166, prMassEr167,
     &                   prBuMW, prBuU235)


         integer, intent(in) :: pNFEL, pNFO
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
         if(pcFuelTag .eq. 'W   ' .or. 
     &      pcFuelTag .eq. 'G   ' .or. 
     &      pcFuelTag .eq. 'LW  ' .or.
     &      pcFuelTag .eq. 'GR  ' .or.
     &      pcFuelTag .eq. 'BE  ' .or.
     &      pcFuelTag .eq. 'IC1 ' .or.
     &      pcFuelTag .eq. 'IC2 ' .or.
     &      pcFuelTag .eq. 'IC3 ' .or.
     &      pcFuelTag .eq. 'IC4 ' .or.
     &      pcFuelTag .eq. 'TRCR') then  
            pcCellType = pcFuelTag
            goto 999
         endif      
         rewind pNFEL      
         call rbskip(pNFEL,2)
1        continue
         iX = 0
         read(pNFEL,100,end=998) cFuelTagRead, pcCellType, prMassU,
     &                           prEnrichment, prMassEr166, prMassEr167,
     &                           prBuMW, prBuU235
         if(cFuelTagRead .ne. pcFuelTag) goto 1
2        read(pNFEL,100,end=999) cFuelTagRead
         if(cFuelTagRead .eq. pcFuelTag) then
            iX = iX + 1
            write(pNFO,901) cFuelTagRead, iX
         endif
         goto 2
998      write(pNFO,900) pcFuelTag
         write(*,*) 'Fatal Error: An error has occured while',
     &              ' reading ELEM.INP.'
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
!                      S U B R O U T I N E ------ fIzhPod
!
!     Sets all of the fuel element parameter values according to the fuel 
!     element type (ST8, ST12, ...). Please  note the following parameter 
!     names:
!
!        * RoUrI - Density of U-ZrH in the fuel element.
!        * W235I - Percent weight of U-235 in %.
!        * rMUrI - Mass of uranium in the whole fuel element.
!        * EnrOI - Fuel enrichment.
!        * Wt166I, Wt167I - Percent weight of Er-166/Er-167.
!        * rM166I, rM167I - Mass of Er-166/Er-167.
!
!     This subroutine will identify the fuel element type where the element 
!     type is acquired from cCellType.
!
! -----------------------------------------------------------------------------  

      subroutine fIzhPod(pcCellType, prDensityU, prWtU235, prMassU, 
     &                   prEnrichment, prWtEr166, prWtEr167,
     &                   prMassEr166, prMassEr167)
         character(len=*), intent(in) :: pcCellType
         real, intent(inout) :: prDensityU, prWtU235, prMassU,
     &      prEnrichment, prWtEr166, prWtEr167, prMassEr166, 
     &      prMassEr167
     
         if(pcCellType .eq. 'ST8 ') then
            prDensityU   = 5.8107
            prWtU235     = 0.016897855
            prMassU      = 190.1762
            prEnrichment = 19.90235
            prWtEr166    = 0.0
            prWtEr167    = 0.0
            prMassEr166  = 0.0
            prMassEr167  = 0.0
         endif 
         if(pcCellType .eq. 'ST12') then
            prDensityU   = 6.12226
            prWtU235     = 0.023323
            prMassU      = 276.54712
            prEnrichment = 19.90341
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

      real function fPFFRing(iel,iaRing,nRing)
         integer, parameter :: nMaxRing = 10
         integer, intent(in) :: iel, nRing
         integer, intent(in) :: iaRing(nMaxRing)
         integer iElSum, iRing
         
         iElSum = 0
         do iRing=1, nRing, 1
            iElSum = iElSum + iaRing(iRing)
            if(iElSum .ge. iel) goto 1 
         enddo
 1       continue
         fPodRin=0.5
         if(iRing.EQ.1) fPFFRing = 1.0
         if(iRing.EQ.2) fPFFRing = 0.9
         if(iRing.EQ.3) fPFFRing = 0.8
         if(iRing.EQ.4) fPFFRing = 0.7
         if(iRing.EQ.5) fPFFRing = 0.6
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
            read(pNF,'(A)') ch
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
      