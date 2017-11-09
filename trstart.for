!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 27/8/2017 9:55:27 PM
!     Author         : RABIEMSI\RABIE
!     Device Name    : RABIEMSI
!     Project Name   : TRIGLAV.fip
!     Source File    : trstart.for

      subroutine trstart(pcWITCHInp, pcELEMInp)
      
      implicit none
      
      character(len=50), intent(in) :: pcWITCHInp, pcELEMInp
      
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
      integer :: i, j, k
C-D  mreza: azimuthal mesh

      data NFI, NFO / 5, 10 /

      print*, ' Running ROUTINE-A.'
!     Setting the file names to their default values.
c      cFileIn  = 'WITCH.INP'
c      cFileOut = 'WITCH.TMP'

      cFileIn = trim(pcWITCHInp) // '.INP'
      cFileOut = trim(pcWITCHInp) // '.TMP'

!     Open files...
      open (unit=NFI, file=cFileIn, status='OLD', err=14)
      open (unit=NFO, file=cFileOut, status='UNKNOWN', err=15)

      call rasearch(NFI, 0, '$* WITCH', 8, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      write (NFO,'(A)') '$* WITCH'
      read  (NFI,'(A)') cComment
      print*, ' ', cComment ! Display comment line 1 to console.
      write (NFO,'(A)') cComment
      read  (NFI,'(A)') cComment
      print*, ' ', cComment ! Display comment line 2 to console.
      write (NFO,'(A)') cComment

      call rasearch(NFI, 0, '$* FLAGS', 8, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) iFlag1
      read (NFI,*) iFlag2
      read (NFI,*) iFlag3
      read (NFI,*) iFlag4
      
      write (NFO,'(4I10,A)') iFlag1, iFlag2, iFlag3, iFlag4, 
     &   ' ! print (xs.,inn.,flu.,fluf.)'

      call rasearch(NFI, 0, '$* ITERATIONS', 13, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) iFlag1
      read (NFI,*) iFlag2
      write (NFO,'(2I10,20X,A)') i,j,' ! iterations (inn.,out.)'

      call rasearch(NFI, 0, '$* CONVERGENCE', 14, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) rInnerIterLimit
      read (NFI,*) rOuterIterLimit
      read (NFI,*) rkeffLimit
      write (NFO,'(3E10.3,10X,A)') rInnerIterLimit, rOuterIterLimit,
     &       rkeffLimit,' ! e(inn.),e(out.),e(k)'
      write (NFO,'(2I5,30X,A)') 4, 4,' ! no. groups'
      write (NFO,'(4F10.8,A)') 1.0, 0.0, 0.0, 0.0,' ! hi'
      
      call rasearch(NFI,0,'$* BUCKLING', 11, cTextTrv, iFlag)
      if (iFlag.GT.0) goto 11
      read (NFI,*) rBuckling
      write (NFO,'(4F10.8,A)') rBuckling, rBuckling, rBuckling,
     &      rBuckling,' ! buckling'

      call rasearch(NFI,0,'$* POWER',8, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read  (NFI,*) rPthRe
      write (NFO,'(A)') '$* POWER'
      write (NFO,'(F10.4)') rPthRe

      call rasearch(NFI,0,'$* TWATER',9, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) rTempLW
      write (NFO,'(A)') '$* TWATER'
      write (NFO,'(F10.4)') rTempLW

      call rasearch(NFI,0,'$* XENON',8, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) iIsXe
      write (NFO,'(A)') '$* XENON'
      write (NFO,*) iIsXe

      call rasearch(NFI,0,'$* BURNUP',9, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) rBuTau
      write (NFO,'(A)') '$* BURNUPD'
      write (NFO,*) rBuTau

      call rasearch(NFI,0,'$* RINGS',8, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) iRing
      if ((iRing .gt. 7) .or.(iRing .lt. 6)) goto 12

      call rasearch(NFI,0,'$* MESH',7, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      read (NFI,*) iMesh
      if (iMesh .lt. 10) then
         do i=1, 102, 1
            iaMeshNetwork(i) = 1
         enddo
      endif
      
      if (iMesh .ge. 10) then
         do i=0, 5, 1
            do j=1, 3, 1
               iaMeshNetwork(17*i+j) = 1
            enddo
            iaMeshNetwork(17*i+4)  = 2 
            iaMeshNetwork(17*i+5)  = 1 
            iaMeshNetwork(17*i+6)  = 2 
            iaMeshNetwork(17*i+7)  = 1 
            iaMeshNetwork(17*i+8)  = 2 
            iaMeshNetwork(17*i+9)  = 2 
            iaMeshNetwork(17*i+10) = 1
            iaMeshNetwork(17*i+11) = 2 
            iaMeshNetwork(17*i+12) = 1 
            iaMeshNetwork(17*i+13) = 2 
            do j=14, 16, 1
               iaMeshNetwork(17*i+j) = 1
            enddo
            iaMeshNetwork(17*i+17) = 4
         enddo 
      endif

      write (NFO,'(A)') '$* SHEMA1'
      i=102
      write (NFO,'(16I5)') iRing + 1, i
      if (iRing .eq. 6) THEN
         write (NFO,'(16I5)') 1,6,12,18,24,30,1
!        Dimensions of 6 rings in the core (Radius).
         write (NFO,'(8F10.4)')
     &   2.027, 6.0175, 9.9595, 13.927, 17.902, 22.06, 54.5
!        Number of meshes in each fuel ring.
         if (iMesh .eq. 1)  write (NFO,'(16I5)') 1,1,1,1,1,1,2
         if (iMesh .eq. 2)  write (NFO,'(16I5)') 6,6,6,6,6,6,18
         if (iMesh .eq. 3)  write (NFO,'(16I5)') 10,10,10,10,10,10,30
         if (iMesh .eq. 11) write (NFO,'(16I5)') 1,2,2,2,2,2,16
         if (iMesh .eq. 12) write (NFO,'(16I5)') 3,6,6,6,6,6,48
         if (iMesh .eq. 13) write (NFO,'(16I5)') 6,12,12,12,12,12,24
      else
         write (NFO,'(16I5)') 1,6,12,18,24,30,36,1
!        Dimensions of 7 rings in the core (Radius)
         write (NFO,'(8F10.4)')
     &   2.3125,6.12,10.08,14.07,18.06,22.06,26.06,58.5
!        Number of meshes in each fuel ring.
         if (iMesh .eq. 1)  write (NFO,'(16I5)') 1,1,1,1,1,1,1,2
         if (iMesh .eq. 2)  write (NFO,'(16I5)') 6,6,6,6,6,6,6,18
         if (iMesh .eq. 3)  write (NFO,'(16I5)') 10,10,10,10,10,10,10,30
         if (iMesh .eq. 11) write (NFO,'(16I5)') 1,2,2,2,2,2,2,16
         if (iMesh .eq. 12) write (NFO,'(16I5)') 3,6,6,6,6,6,6,48
         if (iMesh .eq. 13) write (NFO,'(16I5)') 6,12,12,12,12,12,12,22
      endif
      write (NFO,'(16I5)') (iaMeshNetwork(i), i=1, 102)

      call rasearch(NFI,0,'$* LOADING',10, cTextTrv, iFlag)
      if (iFlag .gt. 0) goto 11
      write (NFO,'(A)') '$* SHEMA2'
!     Read and write the loading of the first ring, A-1.
      read (NFI,'(2x,1x,A4,1x,A4)') cIden(1), cElem(1)
      write (NFO,'(1x,A4,1x,A4)') cIden(1), cElem(1)
      
!     Read and write fuel elements loading for row 2 - 6
      do i=1, 15, 1
         read (NFI,'(2x,6(1x,A4,1x,A4))') (cIden(j), cElem(j), j=1, 6)
         write (NFO,'(6(1x,A4,1x,A4))')   (cIden(j), cElem(j), j=1, 6)         
      enddo
!     Read and write row 7 (G ring) for reactors with 7 fuel rings.
      if (iRing .eq. 7) then
         do j=1, 6, 1
            read (NFI,'(2x,6(1x,A4,1x,A4))', end=13) 
     &            (cIden(i), cElem(i),i=1, 6)
            write (NFO,'(6(1x,A4,1x,A4))') (cIden(i), cElem(i), i=1, 6)
         enddo
      endif

!     Read  and write reflector type
      read  (NFI,'(2x,1x,A4,1x,A4)') cIden(1), cElem(1)
      write (NFO,'(1x,A4,1x,A4)')    cIden(1), cElem(1)

!     rewrite $* WIMSOUTPUT
      rewind NFI
      call rasearch(NFI,0,'$* WIMSOUTPUT',13, cTextTrv, iFlag)
      if (iFlag.EQ.0) write (NFO,'(A)') '$* WIMSOUTPUT'

      goto 99

C-D  error end of program
11    print *,' Fatal Error: Input file parameters are incorect!'
      goto 100
12    print *,' Fatal Error: Number of fuel rings is incorect!'
      goto 100    
13    print *,' Fatal Error: Core loading pattern is incorect!'
      goto 100
14    print *,' Fatal Error: Cannot find the input file.'
      goto 100
15    print *, 'Fata; Error: Cannot create the output file.'
      goto 100
      
C-D  normal end of program
99    print *,' ROUTINE-A completed.'
100   close (NFI)
      close (NFO)
      
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
1        continue
         read(pNF,'(A,A)', end=999) pcTextRV
         if(pcText(1:pnTextLen) .ne. pcTextRV(i+1:i+pnTextLen)) goto 1
         return
999      piFlag = 1
         return
      end subroutine
