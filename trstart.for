!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 27/8/2017 9:55:27 PM
!     Author         : RABIEMSI\RABIE
!     Device Name    : RABIEMSI
!     Project Name   : TRIGLAV.fip
!     Source File    : trstart.for

      subroutine trstart(pcWITCHInp, pcELEMInp, pcLoading, piLG, piNG)
      
      implicit none
      
      character(len=50), intent(in) :: pcWITCHInp, pcELEMInp
      character(len=4) , intent(out) :: pcLoading(128)
     
      integer, intent(out) :: piLG(35), piNG
!     Declaring strings that store the input file name, output file name, 
!     comments in the input file and a dummy variable that helps to search
!     for input cards.
      character(len=80) :: cFileIn, cFileOut, cComment, cTextTrv
      
!     Declaring strings that store the core site location (A-1, B-1, B-2 ...)
!     and the fuel element tags (1153, 1143, ...)
      character(len=4)  :: cIden(6), cElem(6)
      
!     Declaring file unit number. NFI is for the input file, NFO is for the
!     output file.
      integer :: NFI, NFO
     
!     Declaring the number of reactor core layers.
      integer :: nCoreLayers
      
!     Declaring a control flag that tells whether an input card is found.
      integer :: iFlag
      
!     Declaring an integer that stores the TRIGA core ring ID.
      integer :: iRing
      
!     Declaring an integer that stores the finite difference mesh type when
!     solving the multi-group diffusion equations of the core.
      integer :: iMesh
      
!     Declaring simulation flags. 
!        - iFlag1: Print the cell cross sections in the simulation output file.
!        - iFlag2: Print the inner iteration in the simulation output file.
!        - iFlag3: Print the core flux in the simulation output file.
!        - iFlag4: Flux data print out in the plot file (.flu)
      integer :: iFlag1, iFlag2, iFlag3, iFlag4
!     Mesh network
      integer :: iaMeshNetwork(102)
!     Convergence criterion.
      real :: rInnerIterLimit, rOuterIterLimit, rkeffLimit
!     Buckling
      real :: rBuckling
!     Reactor power
      real :: rPthRe
!     Coolant temperature, light water (LW)
      real :: rTempLW
!     Declaring flag indicating whether monitor Xe or not.
      integer :: iIsXe
!     Declaing burnup interval in days.
      real :: rBuTau
      integer :: i, j, k, iCellID

      data NFI, NFO / 5, 10 /

!     Setting the file names to their default values.

      cFileIn = trim(pcWITCHInp) // '.INP'
      cFileOut = trim(pcWITCHInp) // '.TMP'

!     Open files...
      open (unit=NFI, file=cFileIn, status='OLD', err=14)

! --------------------- MACROGROUP ------------------
      call rasearch(NFI,0,'#MACROGROUP',11,cTextTrv,iFlag)
      if (iFlag .gt. 0) goto 765
      read (NFI,*,err=767) piNG
      read (NFI,*,err=761) (piLG(i),i=1,piNG)
      goto 766
765   print*, 'WITCH: MACROGROUP card is not found in '//
     &        ' the input file.'
      print*, 'WITCH: Default option, G = 4.'
      piNG = 4
      piLG(1) = 5
      piLG(2) = 10
      piLG(3) = 21
      piLG(4) = 32    
766   continue
      goto 768
767   print*
      print*, 'WITCH: Fatal error. Bad integer value'//
     &      ' for macrogroup count.'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1)
      stop
761   print*, 'WITCH: Fatal error. An integer '//
     &    'list is expected'//
     &    ' for the last microgroup number.'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1) + 1
      stop
768   continue

! --------------------- POWER --------------------------      
      call rasearch(NFI,0,'#POWER',6, cTextTrv, iFlag)
      if (iFlag .gt. 0) then
         print*, 'WITCH: POWER card is not found in '//
     &         ' the main input file (MAIN.INP).'
         print*, 'WITCH: Core power will be set to '//
     &         'zero (~0.01kW).'
         rPthRe = 0.01
      else
         read (NFI,*,err=201) rPthRe
      endif
      
      goto 301
201   print*, 'WITCH: Fatal error.'//
     &      ' Invalid POWER card parameter!'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1)
      stop
      goto 100
      
301   rewind NFI
! --------------------- TCOOL -------------------------- 

      call rasearch(NFI,0,'#TCOOL',6, cTextTrv, iFlag)
      if (iFlag .gt. 0) then
         print*, 'WITCH: TCOOL card is not found in '//
     &         ' the main input file (MAIN.INP).'
         print*, 'WITCH: Coolant temperature will '//
     &         'be set at 300K.'
         rTempLW = 300.0
      else
         read (NFI,*,end=202,err=202) rTempLW
      endif
      goto 302
202   print*, 'WITCH: Fatal error. Invalid TCOOL '//
     &         'card parameter.'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1)
      stop
      
302   rewind NFI

! --------------------- NLAYERS -------------------------- 
      call rasearch(NFI,0,'#NLAYERS',8, cTextTrv, iFlag)
      if (iFlag .gt. 0) then
         print*, 'WITCH: Fatal error. NLAYERS '//
     &      'card is not found the main input file (MAIN.INP).'
         stop
      endif
      read (NFI,*,end=203,err=203) nCoreLayers
      goto 303
203   print*, 'WITCH: Fatal error. Invalid NLAYERS'//
     &   ' card parameter.'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1)
      stop
      
303   rewind NFI
      
! --------------------- XENON -------------------------- 
      call rasearch(NFI,0,'#XENON',6, cTextTrv, iFlag)
      if (iFlag .gt. 0) then
         print*, 'WITCH: XENON card is not found'//
     &         ' in the main input file (MAIN.INP). '//
     &         'Xenon monitor is off.'
         iIsXe = 0
      else
         read (NFI,*,end=204,err=204) iIsXe
      endif
      
      goto 304
204   print*, 'WITCH: Fatal error. Bad XENON card'//
     &      ' parameter input!'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1)
      goto 100
304   rewind NFI

! --------------------- BURNUP -------------------------- 
      call rasearch(NFI,0,'#BURNUP',7, cTextTrv, iFlag)
      if (iFlag .gt. 0) then
         print*, 'WITCH: Fatal error. BURNUP'//
     &      ' card is not found in'//
     &      ' the main input file (MAIN.INP).'
         stop
      endif
      read (NFI,*,end=205,err=205) rBuTau
      goto 305
205   print*, 'WITCH: Fatal error. Invalid BURNUP'//
     &      ' card parameter!'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1) 
      stop
305   rewind NFI

! --------------------- NRINGS -------------------------- 
      call rasearch(NFI,0,'#NRINGS',7, cTextTrv, iFlag)
      if (iFlag .gt. 0) then
         print*, 'WITCH: Fatal error. NRINGS '//
     &      'card is not found!'
         stop
      endif
      read (NFI,*,end=206,err=206) iRing
      goto 306
206   print*, 'WITCH: Fatal error. Invalid NRINGS'//
     &     ' card parameter!'
      write(*,'(A,I0)') ' Line : ', abs(iFlag-1)   
      stop
306   rewind NFI
      
! --------------------- LOADING -------------------------- 
      call rasearch(NFI,0,'#CORECONFIG',11, cTextTrv, iFlag)
      if (iFlag .gt. 0) then
         print*, 'WITCH: Fatal error. CORECONFIG '//
     &         'card is not found!'
         stop
      endif
!     Read and write the loading of the first ring, A-1.
      read (NFI,'(A4,1x,A4)') cIden(1), cElem(1)
!     Write to the output parameter. This is for TXS file cell type identifying purpose.
      pcLoading(1) = cElem(1)
      iCellID = 2
!     Read and write fuel elements loading for row 2 - 6
      do i=1, 15, 1
         read (NFI,'(A4,1x,A4,5(1x,A4,1x,A4))', end=13, err=13) 
     &         (cIden(j), cElem(j), j=1, 6)
         do k=1, 6, 1
            pcLoading(iCellID) = cElem(k)
            iCellID = iCellID + 1
         enddo
      enddo
!     Read and write row 7 (G ring) for reactors with 7 fuel rings.
      if (iRing .eq. 7) then
         do j=1, 6, 1
            read (NFI,'(A4,1x,A4,5(1x,A4,1x,A4))', end=13, err=13) 
     &            (cIden(i), cElem(i),i=1, 6)
            do k=1, 6, 1
               pcLoading(iCellID) = cElem(k)
               iCellID = iCellID + 1
            enddo
         enddo
      endif

!     Read and write reflector type
!      read  (NFI,'(A4,1x,A4)', end=13, err=13) cIden(1), cElem(1)
      pcLoading(iCellID) = '   R'

      rewind NFI

      goto 99

C-D  error end of program
11    print *,' Fatal Error: Input file parameters are incorect!'
      goto 100
12    print *,' Fatal Error: Invalid fuel rings count!'
      goto 100    
13    print *,' Fatal Error: Incorrect core configuration pattern!'
      goto 100
14    print *,' Fatal Error: MAIN.INP is not found.'
      goto 100
15    print *, 'Fatal Error: Cannot create the output file.'
      goto 100
      
C-D  normal end of program
99    continue
      goto 111
100   close (NFI)
      stop
111   close (NFI)
      return
      
      end subroutine
      
      
      
      
      
      
      
      
      
      

      subroutine rasearch(pNF, pnSkip, pcText, pnTextLen, 
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
         piFlag = piFlag - 1
         read(pNF,'(A,A)', end=999) pcTextRV
         if(pcText(1:pnTextLen) .ne. pcTextRV(i+1:i+pnTextLen)) goto 1
         return
999      piFlag = abs(piFlag)
         return
      end subroutine
