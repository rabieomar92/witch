!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 10/10/2017 5:24:59 PM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : TRIGLAV_Project.fip
!     --------------------------------------------------------
      
      
      program MAIN
         implicit none
         integer :: LG(35), NG, TMP, i, NFI, iFlag, iLayerID, l, iError
         integer nCount, iDummy
         integer :: nLayers = 20
         character(len=80) :: cText, cWITCHInp, cELEMInp, cJoinedOutput
         character(len=50), allocatable :: caJoinFileList(:)
         character(len=4) :: caLoading(128)
         character(len=30) :: cNuclearData = 'WD4TRIC.BIN'
         integer, parameter :: NFIN = 29
!     LayerID stores the ID of the TRIGA core z-axis cell layer that is
!     is going to be processed.

      ! For command line argument's purpose.
      character(len=30) :: cArg, cForm
      character(len=4)  :: cLoading

      NFI = 20
      iLayerID = 0
         cWITCHInp = 'MAIN'
         cELEMInp = 'FUEL_INVENTORY'  
         cJoinedOutput = 'xsdata'
      if (iargc() .gt. 0) then
         
         do i=1, iargc(), 1
            call getarg(i, cArg)
            if (trim(cArg) .eq. '-in') then
               if((i+1) .gt. iargc()) then
                  print*, ' Fatal Error: Invalid argument input!'
                  stop
               endif
               call getarg(i+1, cArg)
               cWITCHInp = trim(cArg)
            elseif (trim(cArg) .eq. '-ndata') then
               if((i+1) .gt. iargc()) then
                  print*, ' Fatal Error: Invalid argument input!'
                  stop
               endif
               call getarg(i+1, cArg)
               cNuclearData = trim(cArg)
            elseif (trim(cArg) .eq. '-elem-in') then
               if((i+1) .gt. iargc()) then
                  print*, ' Fatal Error: Invalid argument input!'
                  stop
               endif
               call getarg(i+1, cArg)
               cELEMInp = trim(cArg)
            elseif (trim(cArg) .eq. '-out') then
               if((i+1) .gt. iargc()) then
                  print*, ' Fatal Error: Invalid -out input!'
                  stop
               endif
               call getarg(i+1, cArg)
               cJoinedOutput = trim(cArg)               
            elseif (trim(cArg) .eq. '-clean') then
               goto 51
            elseif (trim(cArg) .eq. '-join-tapes') then
               if((i+1) .gt. iargc()) then
                  print*, ' Fatal Error: Incomplete argument input!'
                  stop
               endif
               call getarg(i+1, cArg)
               read(cArg,'(I2)',iostat=iError) nCount
               if(iError .ne. 0) then
                  print*, ' Fatal Error: Invalid argument input!'
                  stop
               endif
               allocate(caJoinFileList(nCount))
               if((i+2) .gt. iargc()) then
                  print*, ' Fatal Error: Incomplete argument input!'
                  stop
               endif
               call getarg(i+2, cArg)
               read(cArg,'(A)',iostat=iError) cJoinedOutput
               if(iError .ne. 0) then
                  print*, ' Fatal Error: Invalid argument input!'
                  stop
               endif
               do l=1, nCount, 1
                  call getarg(i+2+l,cArg)
                  read(cArg,'(I10)',err=19) iDummy
                  goto 99
19             print*, ' Fatal Error: Core layer ID list should ',
     &                  'be a list of integers.'
                  stop
99                continue
                  write(cArg,'(I10.10)') iDummy
                  caJoinFileList(l) = trim(cArg)
               enddo
               call TXSJOINER(nCount,caJoinFileList,cJoinedOutput)
               goto 60
            elseif (trim(cArg) .eq. '-layer-id') then
               if((i+1) .gt. iargc()) then
                  print*, ' Fatal Error: Invalid argument input!'
                  stop
               endif
               call getarg(i+1,cArg)
               read (cArg,'(I2)',err=16) iLayerID
               goto 17
16             print*, ' Fatal Error: Core layer ID should ',
     &                  'be an integer.'
               stop
17             continue
            elseif (trim(cArg) .eq. '-info' .or.
     &              trim(cArg) .eq. '-help' .or.
     &              trim(cArg) .eq. '/?') then
               print*
               print*, ' W I T C H '
               print*, ' WIMS-INTEGRATED TRIGA CORE',
     &                 ' HOMOGENISATION CODE'
               print*, ' A code for TRIGA core homogenisation.',
     &                 ' See code '
               print*, ' manual for more details.'
               print*
               print*, ' Universiti Sains Malaysia'
               print*, ' Malaysian Nuclear Agency'
               print*, ' October, 2017.'
               print*
               print*, ' Usage: witch [-in input_name]',
     &                 ' [-elem-in elem_name] [-clean]',
     &                 ' [-layer-id'
               print*, '              layerid][-join-tapes fcount',
     &                 ' txsout layeridlist]'
               print*
               print*, ' OPTIONS'
               print*
               print*,  ' -clean        Removes  junk/un-used  file(s)',
     &                  ' generated by the previous'
               print*,  '               WITCH code run.'
               print*,  ' -in           Specifies the input file',
     &                  ' name.'
               print*,  ' -elem-in      Specifies the fuel element ',
     &                  'input file name.'
               print*,  ' -layer-id     Specifies the layer ID of ',
     &                  'current reactor core layer.'
               print*,  ' -join-tapes   Join all core layer tape',
     &                  ' into a single tape.'
               print*
               print*, ' VARIABLES'
               print*
               print*, ' input_name    The code input file name',
     &                 ' without .INP extension.'
               print*, ' elem_name     The  UZrH  fuel  element  input',
     &                 ' file  name  without .INP'
               print*, '               extension.'
           print*, ' layerid       An  integer  value specifying the',
     &                 ' ID of the reactor core'
               print*, '               layer.'
               print*, ' fcount        An  integer  value ',
     &                 'specifying  the number',
     &                 ' of tapes to be'
               print*, '               joined.'
               print*, ' txsout        A string value specifying',
     &                 ' the name of the joined tape.'
               print*, ' layeridlist   A  list  of integers  that',
     &                 ' represents the  list  of core'
               print*, '               layer tapes',
     &                 ' to be joined.  The integer list count ',
     &                 'should'
               print*, '               not exceed fcount.'
               print*
               print*,  ' **If the user does not specify',
     &                  ' input_name or ',
     &                  'elem_name, their'
               print*,  '   default value  will be used  instead;  ',
     &                  'input_name=WITCH and'
               print*,  '   elem_name=ELEM.'
               print*
               stop
            endif
         enddo
      elseif(iargc() .eq. 0) then
         cWITCHInp = 'MAIN'
         cELEMInp = 'FUEL_INVENTORY'      
      endif

      open(unit=NFIN, file=trim(cWITCHInp) // '.INP',
     &     status='OLD', err=1111)
      goto 1000
1111  print*, ' Fatal Error: MAIN.INP is not found!'
      stop
      
1000  call rasearch(NFIN, 0, '#NLAYERS', 8, cText, iFlag)
      
      if(iFlag .eq. 1) then
         print*, 'WITCH: Fatal Error. NLAYERS card is not found!'
         write(*,'(A,I0)') '  Line : ', abs(iFlag-1)
         stop
      else
         read(NFIN,*,err=1001) nLayers
      endif
      goto 1002
1001  print*, ' Fatal Error: NLAYERS card format error!'
      stop      
1002  continue
      close(unit=NFIN)

      do i=1, nLayers, 1  !--------------------------------------!
            call system("copy database\WD4TRIC.BIN WD4TRIC.BIN")
      call TRSTART(cWITCHInp, cELEMInp, caLoading,LG,NG)   
      call WITRIG(i, cWITCHInp, cELEMInp)           
      call LOOXSB(trim(cNuclearData),i)            
      call XSWOUT(NG, LG, 1, cWITCHInp, i, caLoading)  
      call system('del /q Wimi* WITCH-OUT.OUT'
     &          //        ' WIMS-IN.$$$ WIMS-OUT.$$$ *.TMP')
      call system('del /q FORT.*')
      
      enddo
      
      call sleep(5)
      allocate(caJoinFileList(nLayers))
      do l=1, nLayers, 1
         write(cArg,'(I10.10)') l
         caJoinFileList(l) = trim(cArg)
      enddo
      call TXSJOINER(nLayers,caJoinFileList,trim(cJoinedOutput))
      goto 60
51    continue
      call system('del /q Wimi* WITCH-OUT.OUT'
     &          //        ' WIMS-IN.$$$ WIMS-OUT.$$$ *.TMP')
      call system('del /q FORT.*')
      
60    continue

      end program


