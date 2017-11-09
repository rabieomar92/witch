!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 27/8/2017 9:55:27 PM
!     Author         : RABIEMSI\RABIE
!     Device Name    : RABIEMSI
!     Project Name   : TRIGLAV.fip
!     Source File    : looxsa.for
!     --------------------------------------------------------


!     Declaration of LOOXSA for F90 compatible.
!     M. R. Omar edit version.
!     Purpose: Prepare DOS procedure for WIMSD4 runs for assembly cells
!
!           input    : LI1 - wicord input
!           output   : LO1 - wimsd4 procedure
!           output   : LO2 - wimsd4 procedure - (.COM)
!           extern.  : NARGS,GETARG (system routines)


      program looxsa
      
      implicit none
      
      character(len=132) :: TextRV                   ! Stores the rcsearch return value.
      character(len=70)  :: FileN(6), FileCom
      integer*2          :: iFils(6)
      integer            :: iFlag, NPIKA, rclenf, NArguments
      integer            :: LI1, LO1, LO2, i, iWIMSOut, NOIN
! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------
!     LI1 is the file identifier for the WiTrig.$$$ file.
!     ** WiTrig.&&& file contains a set of WIMSD4 code input names in the form 
!        of WIMInnn.WIN for each unit cell of TRIGRA core.
!
!     LO1 is the file indentifier for the WIMSXS.BAT file.
!     LO2 is the file identifier for the WIMSXS.COM file.
!     ** Both WIMSXS.BAT and WIMSXS.COM  files contains  a set of instructions 
!        that  executes  WIMS  code  (WIMSD4.EXE)  to  do  the  homogenisation 
!        calculation of the unit cell. 
! -----------------------------------------------------------------------------
      data LI1, LO1, LO2 / 11, 20, 21 /

      
! -----------------------------------------------------------------------------
!                         PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------
!     If the command line arguments are not specified, this program will auto-
!     matically rcsearch for the default input filename parameters listed below.
! -----------------------------------------------------------------------------
      character(len=70), parameter :: LI1Name     = 'WiTrig.$$$'
      character(len=70), parameter :: LO1Name     = 'WIMSBATCH.BAT'
      character(len=70), parameter :: WIMSLibName = 'WD4TRIC.BIN'
      
      
      if (iargc() .gt. 0) then
            NArguments = iargc()
            do i=1, NArguments, 1
                  call getarg(i,FileN(i))
            enddo
      elseif (iargc() .eq. 0) then
            do i=1, 6, 1
                  FileN(i) = ' '
            enddo        
            FileN(1) = LI1Name
            FileN(2) = LO1Name
            FileN(3) = WIMSLibName
            
            ! This code will validate the existence of the input file.
            open(unit=LI1, file=FileN(1), status='OLD', err=71)
            goto 74
71          print*, '   Warning: The specified WIMS input header ',
     &              'file is not found! File = ', FileN(1)
            stop
74          continue
      endif

      print*, '   ----------------------------------------------'
      print*, '   Program LOOXSA (Rabie''s Revision August 2017)'
      print*, '   ----------------------------------------------'
      print*, '   ** This  sub-program  prepares  the  necessary'
      print*, '      batch files for  WIMS code cell homogenisa-'
      print*, '      ion.'
      print*, '   ** Edited by M. R. Omar, August 2017.'
      print*, '   ----------------------------------------------'
       

       
!     This section renames WIMSD4 .BAT extension into .COM
      NPIKA = 0
      do while (FileN(2)(NPIKA:NPIKA) .ne. '.')
            NPIKA = NPIKA + 1
      enddo     
      FileCom = FileN(2)(1:NPIKA-1) // '.COM'
       
!     This section recalls the user regarding the input given...       
      print*, '   Wicord Input     :', FileN(1)(1:rclenf(FileN(1)))
      print*, '   WIMSD4 Batch     :', FileN(2)(1:rclenf(FileN(2)))
      print*, '   WIMSD4 Com       :', FileCom(1:rclenf(FILECOM))
      print*, '   WIMSD4 Library   :', FileN(3)(1:rclenf(FileN(3)))

      open (unit=LI1, file=FileN(1), status='OLD')
      open (unit=LO1, file=FileN(2))
      open (unit=LO2, file=FileCom)
      
!     -------------------------------------------------------------------------     
!     NOIN is the number of cell input. Each TRIGA cell will be represented by
!     a single WIMS input in the form of WIMInnn.WIN. NOIN = nnn from the file
!     name.
!     -------------------------------------------------------------------------
      NOIN=0
      
      FileN(4) = '-4'
      FileN(5) = '5 10 21 32'

!     Now we look for the WIMSOUT card inside WiTrig.$$$ file.
      rewind LI1
      call  rcsearch(LI1, 0, '   WIMSOUT ', 10, TextRV, iFLag)
      if(iFLag .eq. 0) iwimsout = 1

      
!     Now we look for the MACROGR card inside WiTrig.$$$ file.      
      rewind LI1
      call  rcsearch(LI1, 0, '$* MACROGR ', 10, TextRV, iFLag)
      if(iFLag .eq. 1) goto 998
      read(TextRV(11:20), '(A)') FileN(4)
      read(LI1, '(10X,A)')       FileN(5)
998   continue

!     Print the the used the number of macrogroups.
      write(*,'(A,A)') '    The number of macrogroups     : ',
     &                 FileN(4)(1:rclenf(FileN(4)))
      write(*,'(A,A)') '    The last fine group of macrogr: ',
     &                 FileN(5)(1:rclenf(FileN(5)))

!     -------------------------------------------------------------------------
!     We begin writing the WIMSXS.BAT file.
!     -------------------------------------------------------------------------
      rewind LI1
      write(LO1,'(''@echo off'')')
1     continue
      call rcsearch(LI1,0,'   WIFLNM ',10, TextRV, iFLag)
      if(iFlag .eq. 1) goto 999
       
      
      ! One WIFLNM card found, we add the number of WIMS input file 
      ! (WIMInnn.WIN)
      NOIN = NOIN + 1
      NPIKA = 11
      do while (TextRV(NPIKA:NPIKA) .ne. '.')
            NPIKA = NPIKA + 1
      enddo

      write(LO1, '(''echo '',I10,'': '',A,A)')
     &  NOIN, TextRV(11:22),' '
      if(NOIN .eq. 1) then
            write(LO1,'(''echo '',I10,'': '',A,A)')
     &         NOIN, TextRV(11:22), '  > wims.log'
      else
            write(LO1,'(''echo '',I10,'': '',A,A)')
     &         NOIN, TextRV(11:22), ' >> wims.log'
      endif

      write(LO1,'(''%wd4d%WIMSD4 '',A,1X,A,1X,A)')
     &      TextRV(11:22), TextRV(11:NPIKA-1) // '.out',
     &      FileN(3)(1:rclenf(FileN(3)))
     
      write(LO1,'(''echo '',A,T76,A)')
     &      TextRV(11:NPIKA-1)//'.xsc','  >runp.scr'
     
      write(LO1,'(''echo '',A,T76,A)')
     &      FileN(4)(1:rclenf(FileN(4))),' >>runp.scr'
      write(LO1,'(''echo '',A,T76,A)')
     &      FileN(5)(1:rclenf(FileN(5))),' >>runp.scr'
     
      write(LO1,'(''echo '',A,T76,A)')
     &      TextRV(11:NPIKA-1)//'.out',' >>runp.scr'
     
      write(LO1,'(A,T76,A)')
     &      '%cd2d%XSWOUT  <runp.scr     >>wims.log'
     
      write(LO1,'(A,A,A)')
     &      'call  %cd2e% %cd2d%WiRa235 ',
     &      TextRV(11:NPIKA-1)//'.out ',
     &      TextRV(11:NPIKA-1)//'.ra1 '

      write(LO1,'(''del fort.* '',A,1X,A,1X,A,1X,A)')
      
      if(iwimsout .ne. 1) then
            write(LO1,'(''del '',A,1X,A,1X,A,1X,A)')
     &            TextRV(11:NPIKA-1)//'.out'
      end if
      
      call WimsCom(LO2,NOIN,TextRV,NPIKA,FileN)
      goto 1
      
! -----------------------------------------------------------------------------
!     END OF BATCH LOOP FOR EACH WIMS INPUT FOR TRIGLAV CORE CELLS.
! -----------------------------------------------------------------------------
      
 999  continue
      write(LO1,'(A,A,A)') 'REM ----------'
      write(LO1,'(A,A,A)') ':STOP'
      close(LI1)
      close(LO1)
      close(LO2)
      stop
      
      end program looxsa
      
      
      subroutine WimsCom(pLO2, pNOIN, pTextRV, pNPIKA, pFileN)
         character(len=132), intent(in) :: pTextRV
         character(len=*)  , intent(in) :: pFileN(6)
         integer           , intent(in) :: pLO2, pNOIN, pNPIKA
         integer                        :: rclenf
         
         
         write(pLO2,'(''$! '',I10,'': '',A,A)')
     &        pNOIN, pTextRV(11:22), ' '
         write(pLO2,'(''$ ass/user '',A,1X,A,1X,A)')
     &        pTextRV(11:22),'FOR005'
         write(pLO2,'(''$ ass/user '',A,1X,A,1X,A)')
     &        pTextRV(11:pNPIKA-1)//'.out','FOR006'
         write(pLO2,'(''$ ass/user '',A,1X,A,1X,A)')
     &        pFileN(3)(1:rclenf(pFileN(3))),'FOR002'
         write(pLO2,'(''$ run wd4d:WD4C '',A,1X,A,1X,A)')
         write(pLO2,'(''$ run cd2d:XSWOUT '',A,1X,A,1X,A)')
         write(pLO2,'(1X,A,T76,A)') pTextRV(11:pNPIKA-1) // '.xsc'
         write(pLO2,'(1X,A,T76,A)') pFileN(4)(1:rclenf(pFileN(4)))
         write(pLO2,'(1X,A,T76,A)') pFileN(5)(1:rclenf(pFileN(5)))
         write(pLO2,'(1X,A,T76,A)') pTextRV(11:pNPIKA-1) // '.out'
         write(pLO2,'(A,A,A)') '$ run cd2d:WiRa235 '
         write(pLO2,'(A,A,A)') pTextRV(11:pNPIKA-1) // '.out '
         write(pLO2,'(A,A,A)') pTextRV(11:pNPIKA-1) // '.ra1 '
         write(pLO2,'(''$!del fort.* '',A,1X,A,1X,A,1X,A)')

         if(iwimsout .ne. 1) write(pLO2,'(''$ del '',A,1X,A,1X,A,1X,A)')
     &         pTextRV(11:pNPIKA-1)//'.out'//'./noco/nolog'
    
         write(pLO2,'(''$ '')')
         return
      end subroutine

      
      ! --------------------------------------------------------------- !
      ! Title  : rcsearch Subroutine
      ! Purpose: Scan a file for a specified string
      !
      ! Text   - The text to be searched for.
      ! NF     - File number identifier.
      ! NSkip  - Number of characters to be skipped from search.
      ! TxtLen - Character length of the string to be searched.
      ! --------------------------------------------------------------- !
      
      subroutine rcsearch(pNF, pNSkip, pText, pTxtLen, pTextRV, pFlag)
         
         character(len=*), intent(in)  :: pText
         character(len=*), intent(out) :: pTextRV
         integer,          intent(in)  :: pNF, pNSkip, pTxtLen
         integer,          intent(out) :: pFlag
         integer                       :: i
         
         pFlag = 0
         i = pNSkip
1        continue
         read(pNF,'(A132)', end=999) pTextRV
         if(pText .ne. pTextRV(i + 1:i + pTxtLen)) goto 1
         return
999      pFlag = 1
         return
         
      end subroutine

      ! --------------------------------------------------------------- !
      ! Title  : rclenf Subroutine
      ! Purpose: Get the number of characters in a string.
      !
      ! String - The target string.
      ! --------------------------------------------------------------- !
      
      integer function rclenf(String)

            character(len=*), intent(in) :: String
            integer                      :: MLen, i
            rclenf=0
            MLen = LEN(String)
            do i = MLen, 1, -1
                  if(String(i:i) .ne. ' ') then
                        rclenf = i
                        return
                  endif
            enddo
            return
      end function