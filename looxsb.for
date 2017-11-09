!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 8/10/2017 2:31:38 AM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : TRIGLAV_Project.fip
!     --------------------------------------------------------
      
      
c      program LOOXSB
      subroutine looxsb
      implicit none
      
         character(len=30), parameter :: cFileWIMSHeader = 'WIMS-IN.$$$'
         character(len=30), parameter :: cFileWIMSCode   = 
     &      'WIMSD4.EXE'
         character(len=30), parameter :: cFileWIMSLib    = 'WD4TRIC.BIN'
         character(len=11) :: caWIMSInput(92)
         character(len=500) :: caWIMSExecuteCmd(92)
         character(len=50) :: cText, cFileWIMSInput
         character(len=100) :: cProgressText
         integer :: NFW, NFI
         integer :: nWIMSInput
         integer :: iFlag
         integer :: rclenf
         integer :: i, j, h
         
         data NFW, NFI / 10, 70 /
         nWIMSInput = 0
      print*, ' Running ROUTINE-C.'
         open (unit=NFI, file=cFileWIMSHeader, status='old', err=911)
100      continue      
         call rcsearch(NFI, 0, '   WIFLNM ',10, cText, iFlag)
         if (iFlag .ne. 0) then
            goto 101
         endif
         nWIMSInput = nWIMSInput + 1
         caWIMSInput(nWIMSInput) = cText(11:rclenf(cText))
         goto 100
101      continue

         do i=1, nWIMSInput, 1
            write(caWIMSExecuteCmd(i), '(A,2X,A,2X,A,2X,A)')
     &   trim(cFileWIMSCode),
     &   trim(caWIMSInput(i)),
     &   trim(caWIMSInput(i)(1:7)) // '.wou', trim(cFileWIMSLib)
         enddo
          call system('cls') 
         do i=1, nWIMSInput, 1
            write(*,*)
     &         'Running WIMS calculation for cell ',
     &          trim(caWIMSInput(i)(5:7)), '.'
            print'(A5,$)', ' 0% ['
            do h=1, int(real(i)*40.0/92.0), 1
               print'(A1,$)', '>'
            enddo
            if(int(real(i)*40.0/92.0) .lt. 92) then
               do h=1, (40 - int(real(i)*40.0/92.0)), 1
                  print'(A1,$)', ' '
               enddo
            endif
            print'(A6)', '] 100%'
            print*
            print*
            call execute_command_line(' ' 
     &          // ' ' // trim(caWIMSExecuteCmd(i)),wait=.true.)
            call system('cls')   
         enddo
         
         goto 999
911      print*, ' Fatal Error: Could not open the header file.'
         stop
999      print*, ' ROUTINE-C completed.'
         close(unit=NFI)
      end subroutine

!     is subroutine format the integer strings into strings with 
!     leading zero(s).
      subroutine rcfint(cNiz)
      implicit none
         character(len=*), intent(inout) :: cNiz
         integer :: i, rclenf
         do i=1, rclenf(cNiz)  
            IF(cNiz(i:i) .eq. ' ') cNiz(i:i) = '0'
         enddo
         return
      end subroutine
      
      subroutine rcsearch(pNF, pnSkip, pcText, pnTextLen, 
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
      
      integer function rclenf(pcString)
         implicit none
         integer :: mlen, i
         character(len=*), intent(in) :: pcString
         rclenf = 1
         mlen=LEN(pcString)
         do i=mlen, 1, -1
            if(pcString(i:i) .ne. ' ') then
               rclenf = i
               return       
            endif
         enddo   
      end function