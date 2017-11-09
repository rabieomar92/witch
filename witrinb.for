!     --------------------------------------------------------
!     THIS FORTRAN SOURCE FILE WAS GENERATED USING FORTRAN-IT.
!     --------------------------------------------------------
!     Generation time: 3/10/2017 9:25:03 PM
!     Author         : 
!     Device Name    : RABIEMSI
!     Project Name   : TRIGLAV.fip
!     --------------------------------------------------------
      
      
!     Begin your code here:
      
!     ---------------------------------------------------------------------
!                     B E R Y L L I U M   E L E M E N T      
!     ---------------------------------------------------------------------
      subroutine WBEINP(pNF, prDensityLW, prTempLW, prTempBe, prTempAl)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prDensityLW, prTempLW, prTempBe,
     &                          prTempAl
         real :: rDensityUZrH, rDensityFe, rDensityZr, rDensityBe,
     &           rDensityAl, rDensityLW, rTempUZrH, rTempZr, rTempFe,
     &           rTempBe, rTempLW, rM1Wt2351, rM1Wt0238, rM1Wt3239,
     &           rM1Wt0091, rM1Wt5001, rM2Wt1056, rM3Wt2001, rTempAl,
     &           rM3Wt0016, rM4Wt0091, rM5Wt0009, rM6Wt0027,
     &           rM7Wt2001, rM7Wt0016
         write(pNF,1000)
         write(pNF,1010)
 1000    format(
     &'*Be element, T(F,Zr)=470F, T(Fe)=350F, T(Be,Al,W)=298F'/,
     &'*IDENT    BE'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH  8 11'/,
     &'NREGION 4 1 8'/,
     &'NMATERIAL 7'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 1.7905   5'/,
     &'ANNULUS 2 1.8670   6'/,
     &'ANNULUS 3 2.313177 3'/,
     &'ANNULUS 4 6.120091 7'/,
     &'ARRAY 1 (1 6 4.189053 0)'/,
     &'RODSUB 1 1 0.31750  4'/,
     &'RODSUB 1 2 1.82245  1'/,
     &'RODSUB 1 3 1.82626  0'/,
     &'RODSUB 1 4 1.87706  2'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 $'/,
     &'         40 41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 2 2 2 2'/,
     &'BUCKLING 0.0  0.00464')

1010     format(
     &'*MATERIAL 1  6.12226  470.00  1    $'/,
     &'*            235.4  0.023323  2238.4  0.0938579 $'/,
     &'*            91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     470.00  2  91   1.0'/,
     &'*MATERIAL 5  1.85     298.00  3   9   1.0'/,
     &'*MATERIAL 6  2.70     298.00  2  27   1.0'/,
     &'*MATERIAL 7  1.       298.00  3  2001 0.1119  16 0.8881')

         rDensityUZrH = 6.12226
         rDensityFe   = 7.889
         rDensityZr   = 6.49
         rDensityBe   = 1.85
         rDensityAl   = 2.70
         rDensityLW   = prDensityLW
         rTempUZrH    = 470.00
         rTempZr      = 470.00
         rTempFe      = 350.0
         rTempBe      = prTempBe
         rTempLW      = prTempLW
         rTempAl      = prTempAl
         rM1Wt2351    = 0.023323    ! U-235     MATERIAL 1
         rM1Wt0238    = 0.0938579   ! U-238     MATERIAL 1
         rM1Wt3239    = 0.0         ! Pu-239    MATERIAL 1
         rM1Wt0091    = 0.8670113   ! Zr-91     MATERIAL 1
         rM1Wt5001    = 0.0158076   ! H in ZrH  MATERIAL 1
         rM2Wt1056    = 1.0         ! Fe-56     MATERIAL 2
         rM3Wt2001    = 0.1119      ! H         MATERIAL 3
         rM3Wt0016    = 0.8881      ! O         MATERIAL 3
         rM4Wt0091    = 1.0         ! Zr        MATERIAL 4
         rM5Wt0009    = 1.0         ! Be        MATERIAL 5
         rM6Wt0027    = 1.0         ! Al        MATERIAL 6
         rM7Wt2001    = 0.1119      ! H         MATERIAL 7
         rM7Wt0016    = 0.8881      ! O         MATERIAL 7   
c-
         write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
         write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,'  91  ',
     &      rM1Wt0091 ,'$'
         write(pNF,1002) '5001  ',rM1Wt5001, '3239.1',rM1Wt3239
         write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
         write(pNF,1002) '1056  ',rM2Wt1056
         write(pNF,1001) 'MATERIAL 3 ',rDensityLW,rTempLW,3,' $'
         write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
         write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
         write(pNF,1002) '91    ',rM4Wt0091
         write(pNF,1001) 'MATERIAL 5 ',rDensityBe,rTempBe,3,' $'
         write(pNF,1002) '9     ',rM5Wt0009 
         write(pNF,1001) 'MATERIAL 6 ',rDensityAl,rTempAl,2,' $'
         write(pNF,1002) '27    ',rM6Wt0027  
         write(pNF,1001) 'MATERIAL 7 ',rDensityLW, rTempLW ,3,' $'
         write(pNF,1002) '2001  ',rM7Wt2001,'16    ',rM7Wt0016

 1001    format(A,F6.4,1X,F6.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)

         write(pNF,1003) 'POWERC 0 0 '  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 1 3'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         return
      end subroutine
      

!     ---------------------------------------------------------------------
!                        W A T E R   R E F L E C T O R      
!     ---------------------------------------------------------------------
      subroutine WWINP(pNF, prDensityLW, prTempLW, prTempAl, prTempC)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prDensityLW, prTempLW, prTempAl, prTempC
         real :: rDensity, rDensityFe, rDensityZr, rDensityBe,
     &           rDensityAl, rDensityLW, rFuelTemp, rTempZr, rTempAl,
     &           rTempFe, rTempBe, rTempLW, rM1Wt2351, rM1Wt0238,
     &           rM1Wt3239, rM1Wt0091, rM1Wt5001, rM1Wt9001,
     &           rM1Wt0016, rM1Wt1056, rM2Wt0027, rM3Wt2001,
     &           rM3Wt0016
     
      write(pNF,1000)
      write(pNF,1010)
 1000 format(
     &'*Water, free boundary condition'/,
     &'*IDENT    w'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH 21 21'/,
     &'NREGION 3'/,
     &'NMATERIAL 3'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 22.06 1'/,
     &'ANNULUS 2 22.66 2'/,
     &'ANNULUS 3 53.16 3'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 $'/,
     &'         40 41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 10 1 10'/,
     &'FREE')
c-po starem    
 1010 format(
     &'*MATERIAL 1 3.141539 296.00 1 235.4 0.01243 2238.4 0.050026 $'/
CNEA &'*MATERIAL 1  3.141539 296.00  1  235.4 0.01243 2238.4 0.050026 $'/'
     &'*         91 0.68716 5001 0.012110 3239.1 0.0 $'/,
     &'*         9001 0.019944 16 0.159554 1056 0.059179'/,
     &'*MATERIAL 2  2.70 296.00  2    27 1.0'/,
     &'*MATERIAL 3  1.00 296.00  3    2001 0.1119 16 0.8881')

         rDensity     = 3.141539
         rDensityFe   = 7.889
         rDensityZr   = 6.49
         rDensityBe   = 1.85
         rDensityAl   = 2.70
         rDensityLW   = prDensityLW
         rFuelTemp    = 0.0
         rTempLW      = prTempLW
         rTempAl      = prTempAl
         rM1Wt2351    = 0.0124300   ! U-235     MATERIAL 1
         rM1Wt0238    = 0.0500260   ! U-238     MATERIAL 1
         rM1Wt3239    = 0.0000000   ! Pu-239    MATERIAL 1
         rM1Wt0091    = 0.6871600   ! Zr-91     MATERIAL 1
         rM1Wt5001    = 0.0121100   ! H in ZrH  MATERIAL 1
         rM1Wt9001    = 0.0199440   !           MATERIAL 1
         rM1Wt0016    = 0.1595540   ! O         MATERIAL 1
         rM1Wt1056    = 0.0591790   ! Fe        MATERIAL 1
         rM2Wt0027    = 1.0000000   ! Al        MATERIAL 2
         rM3Wt2001    = 0.1119000   ! H         MATERIAL 3
         rM3Wt0016    = 0.8881000   ! O         MATERIAL 3  
         
      write(pNF,1001) 'MATERIAL 1 ',rDensity,rTempLW,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238, '  91  ',
     &                rM1Wt0091  ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239,'9001  ',
     &                rM1Wt9001,'$'
      write(pNF,1002) '16    ',rM1Wt0016,  '1056  ',rM1Wt1056
      write(pNF,1001) 'MATERIAL 2 ',rDensityAl,rTempAl,2,' $'
      write(pNF,1002) '27    ',rM2Wt0027
      write(pNF,1001) 'MATERIAL 3 ',rDensityLW,rTempLW,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
c-     
 1001    format(A,F6.4,1X,F6.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)
c-
         write(pNF,1003) 'POWERC 0 0 '  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 2 3'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'NOBUCKLING'
         write(pNF,1003) 'BEGINC'
c-
         return
      end subroutine
      

!     ---------------------------------------------------------------------
!                              W A T E R   C E L L     
!     ---------------------------------------------------------------------      
      subroutine WLWINP(pNF, prDensityLW1, prTempLW1, prDensityLW2,
     &                  prTempLW2)
         implicit none
         integer, intent(in) :: pNF
         real,    intent(in) :: prDensityLW1, prTempLW1, prDensityLW2,
     &                          prTempLW2
         real :: rDensityUZrH, rDensityFe, rDensityZr, rDensityBe,
     &           rDensityAl, rDensityLW1, rDensityLW2, rTempLW1,
     &           rTempUZrH, rTempZr, rTempFe, rTempBe, rM1Wt2351,
     &           rM1Wt0238, rM1Wt3239, rM1Wt0091, rM1Wt5001, rM2Wt1056,
     &           rTempLW2, rM3Wt2001, rM3Wt0016, rM4Wt0091, rM5Wt2001,
     &           rM5Wt0016
     
         write(pNF,1000)
         write(pNF,1010)
 1000    format(
     &'*water channel (empty p.), T(F,Zr)=450F, T(Fe)=350F, T(W)=298F'/,
     &'*IDENT    LW'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH  4 7'/,
     &'NREGION 2 1 6'/,
     &'NMATERIAL 5'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 2.313177 5'/,
     &'ANNULUS 2 6.120091 3'/,
     &'ARRAY 1 (1 6 4.189053 0)'/,
     &'RODSUB 1 1 0.31750  4'/,
     &'RODSUB 1 2 1.82245  1'/,
     &'RODSUB 1 3 1.82626  0'/,
     &'RODSUB 1 4 1.87706  2'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 40 $'/,
     &'         41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH  2 2'/,
     &'BUCKLING 0.0  0.00464')
c-po starem     
 1010    format(
     &'*MATERIAL 1  6.12226  470.00  1    $'/,
     &'*         235.4  0.023323  2238.4  0.0938579 $'/,
     &'*         91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     470.00  2  91   1.0'/,
     &'*MATERIAL 5  1.       298.00  3  2001 0.1119  16 0.8881')

         rDensityUZrH = 6.12226
         rDensityFe   = 7.889
         rDensityZr   = 6.49
         rDensityBe   = 1.85
         rDensityAl   = 2.70
         rDensityLW1  = prDensityLW1
         rDensityLW2  = prDensityLW2
         rTempLW1     = prTempLW1
         rTempLW2     = prTempLW2
         rTempUZrH    = 470.00
         rTempZr      = 470.00
         rTempFe      = 350.0
         rM1Wt2351    = 0.023323    ! U-235     MATERIAL 1
         rM1Wt0238    = 0.0938579   ! U-238     MATERIAL 1
         rM1Wt3239    = 0.0         ! Pu-239    MATERIAL 1
         rM1Wt0091    = 0.8670113   ! Zr-91     MATERIAL 1
         rM1Wt5001    = 0.0158076   ! H in ZrH  MATERIAL 1
         rM2Wt1056    = 1.0         ! Fe-56     MATERIAL 2
         rM3Wt2001    = 0.1119      ! H         MATERIAL 3
         rM3Wt0016    = 0.8881      ! O         MATERIAL 3
         rM4Wt0091    = 1.0         ! Zr        MATERIAL 4
         rM5Wt2001    = 0.11190     ! H         MATERIAL 5
         rM5Wt0016    = 0.88810     ! O         MATERIAL 5

      write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,'  91  ',
     &                rM1Wt0091  ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239
      write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
      write(pNF,1002) '1056  ',rM2Wt1056
      write(pNF,1001) 'MATERIAL 3 ',rDensityLW1,rTempLW1,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
      write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
      write(pNF,1002) '91    ',rM4Wt0091
      write(pNF,1001) 'MATERIAL 5 ',rDensityLW2,rTempLW2,3,' $'
      write(pNF,1002) '2001  ',rM5Wt2001,'16    ',rM5Wt0016
 
 1001    format(A,F6.4,1X,F6.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)

         write(pNF,1003) 'POWERC 0 0 '  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 1 1'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         return
      end subroutine

      
!     ---------------------------------------------------------------------
!                      G R A P H I T E   R E F L E C T O R     
!     ---------------------------------------------------------------------        
      subroutine WGINP(pNF, prTempUZrH, prTempAl, prTempC)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prTempUZrH, prTempAl, prTempC
         real :: rDensityUZrH, rDensityFe, rDensityZr, rDensityBe,
     &           rDensityAl, rDensityC, rTempC, rTempUZrH , rTempZr,
     &           rTempFe, rTempAl, rM1Wt2351, rM1Wt0238, rM1Wt3239,
     &           rM1Wt0091, rM1Wt5001, rM1Wt9001, rM1Wt0016, rM1Wt1056,
     &           rM2Wt0027, rM3Wt0012
         write(pNF,1000)
         write(pNF,1010)
 1000    format(
     &'*C reflector, free boundary condition'/,
     &'*IDENT    g'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH 21 21'/,
     &'NREGION 3'/,
     &'NMATERIAL 3'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 22.06 1'/,
     &'ANNULUS 2 22.66 2'/,
     &'ANNULUS 3 53.16 3'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 $'/,
     &'         40 41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 10 1 10'/,
     &'FREE')

 1010    format(
     &'*MATERIAL 1  3.141539  296.00  1    $'/,
     &'*         235.4  0.012430 2238.4  0.050026 $'/,
     &'*         91     0.68716  5001    0.012110  3239.1 0.0'/,
     &'*         9001   0.019944   16    0.159554  1056   0.059179'/,
     &'*MATERIAL 2  2.70     296.00  2    27 1.0'/,
     &'*MATERIAL 3  1.60     296.00  3    12 1.0')

         rDensityUZrH = 3.141539
         rDensityFe   = 7.889
         rDensityZr   = 6.49
         rDensityBe   = 1.85
         rDensityAl   = 2.70
         rDensityC    = 1.60
         rTempC       = prTempC
         rTempUZrH    = prTempUZrH
         rTempAl      = prTempAl
         rM1Wt2351    = 0.012430    ! U-235     MATERIAL 1
         rM1Wt0238    = 0.050026    ! U-238     MATERIAL 1
         rM1Wt3239    = 0.0         ! Pu-239    MATERIAL 1
         rM1Wt0091    = 0.687160    ! Zr-91     MATERIAL 1
         rM1Wt5001    = 0.012110    ! H in ZrH  MATERIAL 1
         rM1Wt9001    = 0.019944    ! H         MATERIAL 1
         rM1Wt0016    = 0.159554    ! O         MATERIAL 1
         rM1Wt1056    = 0.059179    ! Fe-56     MATERIAL 1
         rM2Wt0027    = 1.0         ! Fe-56     MATERIAL 2
         rM3Wt0012    = 1.0         ! C         MATERIAL 3

      write(pNF,1001) 'MATERIAL 1 ',rTempUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,
     &                '  91  ',rM1Wt0091  ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239,
     &                '9001  ',rM1Wt9001,'$'
      write(pNF,1002) '16    ',rM1Wt0016,  '1056  ',rM1Wt1056
      write(pNF,1001) 'MATERIAL 2 ',rDensityAl,rTempAl,2,' $'
      write(pNF,1002) '27    ',rM2Wt0027
      write(pNF,1001) 'MATERIAL 3 ',rDensityC, rTempC,3,' $'
      write(pNF,1002) '12    ',rM3Wt0012
    
 1001 format(A,F6.4,1X,F6.2,I3,A)
 1002 format(4(1X,A6,1X,F10.7))
 1003 format(A,F6.2,F6.2,I5)
c-
         write(pNF,1003) 'POWERC 0 0 '  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 2 3'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'NOBUCKLING'
         write(pNF,1003) 'BEGINC'
         return
      end subroutine

!     ---------------------------------------------------------------------
!                      G R A P H I T E   E L E M E N T     
!     ---------------------------------------------------------------------       
      subroutine WGRINP(pNF, prDensityLW, prTempLW, prTempC, prTempAl)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prDensityLW, prTempLW, prTEmpC, prTempAl
         real :: rDensityUZrH, rDensityFe, rDensityLW, rDensityZr,
     &           rDensityC, rDensityAl, rTempLW, rTempFe, rTempAl,
     &           rTempZr, rTempUZrH, rTempC, rM1Wt2351, rM1Wt0238,
     &           rM1Wt0091, rM1Wt5001, rM1Wt3239, rM2Wr1056, rM3Wt2001,
     &           rM3Wt0016, rM4Wt0091, rM5Wt0012, rM6Wt0027, rM7Wt2001,
     &           rM7Wt0016
         write(pNF,1000)
         write(pNF,1010)
 1000    format(
     &'*graphite element, T(F,Zr)=470F, T(Fe)=350F, T(C,Al,W)=298F'/,
     &'*IDENT    GR'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH  6 9'/,
     &'NREGION 4 1 8'/,
     &'NMATERIAL 7'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 1.7905   5'/,
     &'ANNULUS 2 1.8670   6'/,
     &'ANNULUS 3 2.313177 3'/,
     &'ANNULUS 4 6.120091 7'/,
     &'ARRAY 1 (1 6 4.189053 0)'/,
     &'RODSUB 1 1 0.31750  4'/,
     &'RODSUB 1 2 1.82245  1'/,
     &'RODSUB 1 3 1.82626  0'/,
     &'RODSUB 1 4 1.87706  2'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 $'/,
     &'         40 41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 1 1 2 2'/,
     &'BUCKLING 0.0  0.00464')

 1010     format(
     &'*MATERIAL 1  6.12226  470.00  1    $'/,
     &'*         235.4  0.023323  2238.4  0.0938579 $'/,
     &'*         91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     470.00  2  91   1.0'/,
     &'*MATERIAL 5  1.60     298.00  3  12   1.0'/,
     &'*MATERIAL 6  2.70     298.00  2  27   1.0'/,
     &'*MATERIAL 7  1.       298.00  3  2001 0.1119  16 0.8881')

      rDensityUZrH  = 6.1222600
      rDensityFe    = 7.8890000
      rDensityLW    = prDensityLW
      rDensityZr    = 6.4900000
      rDensityC     = 1.6000000
      rDensityAl    = 2.7000000
      rTempLW       = prTempLW
      rTempFe       = 350.00000
      rTempZr       = 470.00000
      rTempUZrH     = 470.00000
      rTempC        = prTempC
      rTempAl       = prTempAl
      rM1Wt2351     = 0.0233230
      rM1Wt0238     = 0.0938579
      rM1Wt0091     = 0.8670113
      rM1Wt5001     = 0.0158076
      rM1Wt3239     = 0.0000000
      rM2Wr1056     = 1.0000000
      rM3Wt2001     = 0.1119000
      rM3Wt0016     = 0.8881000
      rM4Wt0091     = 1.0000000
      rM5Wt0012     = 1.0000000
      rM6Wt0027     = 1.0000000 
      rM7Wt2001     = 0.1119000
      rM7Wt0016     = 0.8881000
c-
      write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,'  91  ',
     &                  rM1Wt0091  ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239
      write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
      write(pNF,1002) '1056  ',rM2Wr1056
      write(pNF,1001) 'MATERIAL 3 ',prDensityLW,prTempLW,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
      write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
      write(pNF,1002) '91    ',rM4Wt0091
      write(pNF,1001) 'MATERIAL 5 ',rDensityC,rTempC,3,' $'
      write(pNF,1002) '12    ',rM5Wt0012
      write(pNF,1001) 'MATERIAL 6 ',rDensityAl,rTempAl,2,' $'
      write(pNF,1002) '27    ',rM6Wt0027  
      write(pNF,1001) 'MATERIAL 7 ',rDensityLW,rTempLW,3,' $'
      write(pNF,1002) '2001  ',rM7Wt2001,'16    ',rM7Wt0016
    
 1001 format(A,F6.4,1X,F6.2,I3,A)
 1002 format(4(1X,A6,1X,F10.7))
 1003 format(A,F6.2,F6.2,I5)

      write(pNF,1003) 'POWERC 0 0 '  
      write(pNF,1003) 'BEGINC'
      write(pNF,1003) 'REGION 1 3'
      write(pNF,1003) 'LEAKAGE 5'
      write(pNF,1003) 'BUCKLING 0.0  0.00464'
      write(pNF,1003) 'BEGINC'
      write(pNF,1003) '*EOF'
         return
      end subroutine

!     ---------------------------------------------------------------------
!                   I R R A D I A T I O N   C H A N N E L   1    
!     --------------------------------------------------------------------- 
      subroutine WIC1INP(pNF, prDensityLW, prTempLW, prTempAl)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prDensityLW, prTempLW, prTempAl
         real :: rDensityUZrH , rTempUZrH, rM1Wt2351, rM1Wt0238, 
     &           rM1Wt0091, rM1Wt5001, rM1Wt3239, rDensityFe, 
     &           rTempFe, rM2Wt1056, rDensityLW, rTempLW, 
     &           rM3Wt2001, rM3Wt0016, rDensityZr, rTempZr, 
     &           rM4Wt0091, rDensityAl, rTempAl, rM5Wt0027,
     &           rM6Wt2001, rM6Wt0016
     
         write(pNF,1000)
         write(pNF,1010)
 1000    format(
     &'*IC1, void channel, T(F,Zr)=470F, T(W,Al)=298F, T(Fe)=350F'/,
     &'*IDENT    IC1'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH  8 11'/,
     &'NREGION 4 1 8'/,
     &'NMATERIAL 6 1'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 1.70000  0'/,
     &'ANNULUS 2 1.90000  5'/,
     &'ANNULUS 3 2.313177 3'/,
     &'ANNULUS 4 6.120091 6'/,
     &'ARRAY 1 (1 6 4.189053 0)'/,
     &'RODSUB 1 1 0.31750  4'/,
     &'RODSUB 1 2 1.82245  1'/,
     &'RODSUB 1 3 1.82626  0'/,
     &'RODSUB 1 4 1.87706  2'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 40 $'/,
     &'         41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 1 1 2 4'/,
     &'BUCKLING 0.0  0.00464')
c-po starem
 1010 format(
     &'*MATERIAL 1  6.12226  470.00  1    $'/,
     &'*         235.4  0.023323  2238.4  0.0938579 $'/,
     &'*         91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     470.00  2  91   1.0'/,
     &'*MATERIAL 5  2.70     298.00  2  27   1.0'/,
     &'*MATERIAL 6  1.       298.00  3  2001 0.1119  16 0.8881')

         rDensityLW   = prDensityLW
         rDensityUZrH = 6.12226
         rDensityZr   = 6.49000
         rDensityAl   = 2.70000
         rDensityFe   = 7.88900
         rTempLW      = prTempLW
         rTempAl      = prTempAl
         rTempZr      = 470.0
         rTempUZrH    = 470.0
         rTempFe      = 350.0
         rM1Wt2351    = 0.0233230
         rM1Wt0238    = 0.0938579
         rM1Wt0091    = 0.8670113
         rM1Wt5001    = 0.0158076
         rM1Wt3239    = 0.0000000
         rM2Wt1056    = 1.0000000
         rM3Wt2001    = 0.1119000
         rM3Wt0016    = 0.8881000
         rM4Wt0091    = 1.0000000
         rM5Wt0027    = 1.0000000
         rM6Wt2001    = 0.1119000
         rM6Wt0016    = 0.8881000

         write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
         write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,'  91  ',
     &                     rM1Wt0091  ,'$'
         write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239
         write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
         write(pNF,1002) '1056  ',rM2Wt1056
         write(pNF,1001) 'MATERIAL 3 ',rDensityLW,rTempLW,3,' $'
         write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
         write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
         write(pNF,1002) '91    ',rM4Wt0091
         write(pNF,1001) 'MATERIAL 5 ',rDensityAl,rTempAl,2,' $'
         write(pNF,1002) '27    ',rM5Wt0027
         write(pNF,1001) 'MATERIAL 6 ',rDensityLW,rTempLW,3,' $'
         write(pNF,1002) '2001  ',rM6Wt2001,'16    ',rM6Wt0016
      
 1001    format(A,F6.4,1X,F6.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)
   
         write(pNF,1003) 'POWERC 0 0 '  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 1 3'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) '*EOF'
         return
      end subroutine

!     ---------------------------------------------------------------------
!                   I R R A D I A T I O N   C H A N N E L   2    
!     ---------------------------------------------------------------------       
      subroutine WIC2INP(pNF, prDensityLW1, prTempLW1, 
     &                   prDensityLW2, prTempLW2, prTempAl)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prDensityLW1, prTempLW1, prTempAl, 
     &                          prDensityLW2, prTempLW2
         real :: rDensityUZrH , rTempUZrH, rM1Wt2351, rM1Wt0238, 
     &           rM1Wt0091, rM1Wt5001, rM1Wt3239, rDensityFe, 
     &           rTempFe, rM2Wt1056, rDensityLW1, rTempLW1, 
     &           rM3Wt2001, rM3Wt0016, rDensityZr, rTempZr, 
     &           rM4Wt0091, rDensityAl, rTempAl, rM5Wt0027,
     &           rDensityLW2, rTempLW2, rM6Wt2001,
     &           rM6Wt0016
     
      write(pNF,1000)
      write(pNF,1010)
 1000 format(
     &'*IC2,half void channel, T(F,Zr)=470F, T(Fe)=350F, T(W,Al)=298F'/,
CNEA &'*IC2, half void channel, T(F,Zr)=470F, T(Fe)=350F, T(W,Al)=298F'/,
     &'*IDENT    IC2'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH 7 10'/,
     &'NREGION 5 1 9'/,
     &'NMATERIAL 7 1'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 0.85000  0'/,
     &'ANNULUS 2 1.70000  6'/,
     &'ANNULUS 3 1.90000  5'/,
     &'ANNULUS 4 2.313177 3'/,
     &'ANNULUS 5 6.120091 7'/,
     &'ARRAY 1 (1 6 4.189053 0)'/,
     &'RODSUB 1 1 0.31750  4'/,
     &'RODSUB 1 2 1.82245  1'/,
     &'RODSUB 1 3 1.82626  0'/,
     &'RODSUB 1 4 1.87706  2'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 40 $'/,
     &'         41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 1 1 1 2 2'/,
     &'BUCKLING 0.0  0.00464')
c-po starem
 1010 format(
     &'*MATERIAL 1  6.12226  470.00  1    $'/,
     &'*         235.4  0.023323  2238.4  0.0938579 $'/,
     &'*         91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     470.00  2  91   1.0'/,
     &'*MATERIAL 5  2.70     298.00  2  27   1.0'/,
     &'*MATERIAL 6  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 7  1.       298.00  3  2001 0.1119  16 0.8881')

!     MATERIAL 1
         rDensityUZrH = 6.12226
         rTempUZrH    = 470.00
         rM1Wt2351    = 0.0233230
         rM1Wt0238    = 0.0938579
         rM1Wt0091    = 0.8670113
         rM1Wt5001    = 0.0158076
         rM1Wt3239    = 0.0000000
!     MATERIAL 2      
         rDensityFe   = 7.889
         rTempFe      = 350.00
         rM2Wt1056    = 1.0
!     MATERIAL 3, 7
         rDensityLW1  = prDensityLW1
         rTempLW1     = prTempLW1
         rM3Wt2001    = 0.1119
         rM3Wt0016    = 0.8881
!     MATERIAL 4
         rDensityZr   = 6.49
         rTempZr      = 470.0
         rM4Wt0091    = 1.0
!     MATERIAL 5
         rDensityAl   = 2.7
         rTempAl      = prTempAl
         rM5Wt0027    = 1.0
!     MATERIAL 6      
         rDensityLW2  = prDensityLW2
         rTempLW2     = prTempLW2
         rM6Wt2001    = 0.1119
         rM6Wt0016    = 0.8881

      write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,'  91  ',
     &                rM1Wt0091  ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239
      write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
      write(pNF,1002) '1056  ',rM2Wt1056
      write(pNF,1001) 'MATERIAL 3 ',rDensityLW1,rTempLW1,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
      write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
      write(pNF,1002) '91    ',rM4Wt0091
      write(pNF,1001) 'MATERIAL 5 ',rDensityAl,rTempAl,2,' $'
      write(pNF,1002) '27    ',rM5Wt0027
      write(pNF,1001) 'MATERIAL 6 ',prDensityLW2,prTempLW2,3,' $'
      write(pNF,1002) '2001  ',rM6Wt2001,'16    ',rM6Wt0016
      write(pNF,1001) 'MATERIAL 7 ',prDensityLW1,prTempLW1,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
    
 1001    format(A,F6.4,1X,F6.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)

         write(pNF,1003) 'POWERC 0 0 '  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 1 4'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) '*EOF'
         return
      end subroutine

!     ---------------------------------------------------------------------
!                   I R R A D I A T I O N   C H A N N E L   3    
!     ---------------------------------------------------------------------   
      subroutine WIC3INP(pNF, prDensityLW1, prTempLW1, prDensityLW2,
     &                   prTempLW2, prTempAl)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prDensityLW1, prTempLW1, prTempAl, 
     &                          prDensityLW2, prTempLW2
         real :: rDensityUZrH , rTempUZrH, rM1Wt2351, rM1Wt0238, 
     &           rM1Wt0091, rM1Wt5001, rM1Wt3239, rDensityFe, 
     &           rTempFe, rM2Wt1056, rDensityLW1, rTempLW1, 
     &           rM3Wt2001, rM3Wt0016, rDensityZr, rTempZr, 
     &           rM4Wt0091, rDensityAl, rTempAl, rM5Wt0027,
     &           rDensityLW2, rTempLW2, rM6Wt2001, rM6Wt0016
      write(pNF,1000)
      write(pNF,1010)
 1000 format(
     &'*IC3, channel filled with water, T(F,Zr)=470F,',
     &' T(Fe)=350F, T(W,Al)=298F'/,
     &'*IDENT    IC3'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH  7 10'/,
     &'NREGION 4 1 8'/,
     &'NMATERIAL 7 1'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 1.70000  5'/,
     &'ANNULUS 2 1.90000  6'/,
     &'ANNULUS 3 2.313177 3'/,
     &'ANNULUS 4 6.120091 7'/,
     &'ARRAY 1 (1 6 4.189053 0)'/,
     &'RODSUB 1 1 0.31750  4'/,
     &'RODSUB 1 2 1.82245  1'/,
     &'RODSUB 1 3 1.82626  0'/,
     &'RODSUB 1 4 1.87706  2'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 40 $'/,
     &'         41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 2 1 2 2'/,
     &'BUCKLING 0.0  0.00464')
c-po starem
 1010 format(
     &'*MATERIAL 1  6.12226  470.00  1    $'/,
     &'*         235.4  0.023323  2238.4  0.0938579 $'/,
     &'*         91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     470.00  2  91   1.0'/,
     &'*MATERIAL 5  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 6  2.7      298.00  2    27 1.0'/,
     &'*MATERIAL 7  1.       298.00  3  2001 0.1119  16 0.8881')


!     MATERIAL 1
         rDensityUZrH = 6.12226
         rTempUZrH    = 470.00
         rM1Wt2351    = 0.0233230
         rM1Wt0238    = 0.0938579
         rM1Wt0091    = 0.8670113
         rM1Wt5001    = 0.0158076
         rM1Wt3239    = 0.0000000
!     MATERIAL 2      
         rDensityFe   = 7.889
         rTempFe      = 350.00
         rM2Wt1056    = 1.0
!     MATERIAL 3, 7
         rDensityLW1  = prDensityLW1
         rTempLW1     = prTempLW1
         rM3Wt2001    = 0.1119
         rM3Wt0016    = 0.8881
!     MATERIAL 4
         rDensityZr   = 6.49
         rTempZr      = 470.0
         rM4Wt0091    = 1.0
!     MATERIAL 6
         rDensityAl   = 2.7
         rTempAl      = prTempAl
         rM5Wt0027    = 1.0
!     MATERIAL 5      
         rDensityLW2  = prDensityLW2
         rTempLW2     = prTempLW2
         rM6Wt2001    = 0.1119
         rM6Wt0016    = 0.8881

      write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,'  91  ',
     &                 rM1Wt0091 ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',0.0
      write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
      write(pNF,1002) '1056  ',rM2Wt1056
      write(pNF,1001) 'MATERIAL 3 ',rDensityLW1,rTempLW1,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
      write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
      write(pNF,1002) '91    ',rM4Wt0091
      write(pNF,1001) 'MATERIAL 5 ',rDensityLW2,rTempLW2,3,' $'
      write(pNF,1002) '2001  ',rM6Wt2001,'16     ',rM6Wt0016
      write(pNF,1001) 'MATERIAL 6 ',rDensityAl,rTempAl,2,' $'
      write(pNF,1002) '27    ',rM5Wt0027  
      write(pNF,1001) 'MATERIAL 7 ',rDensityLW1,rTempLW1,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
 
 1001    format(A,F6.4,1X,F6.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)

         write(pNF,1003) 'POWERC 0 0 '  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 1 3'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) '*EOF'
         return
      end subroutine

!     ---------------------------------------------------------------------
!                   I R R A D I A T I O N   C H A N N E L   4    
!     ---------------------------------------------------------------------   
      subroutine WIC4INP(pNF, prDensityLW, prTempLW, prTempAl)
         implicit none
         integer, intent(in) :: pNF
         real   , intent(in) :: prDensityLW, prTempLW, prTempAl

         real :: rDensityUZrH , rTempUZrH, rM1Wt2351, rM1Wt0238, 
     &           rM1Wt0091, rM1Wt5001, rM1Wt3239, rDensityFe, 
     &           rTempFe, rM2Wt1056, rDensityLW1, rTempLW1, 
     &           rM3Wt2001, rM3Wt0016, rDensityZr, rTempZr, 
     &           rM4Wt0091, rDensityAl, rTempAl, rM5Wt0027,
     &           rDensityLW2, rTempLW2, rM6Wt2001, rM6Wt0016
     
      write(pNF,1000)
      write(pNF,1010)
 1000 format(
     &'*IC4, tranzient roc, T(F,Zr)=470F, T(W,Al)=298F, T(Fe)=350F'/,
     &'*IDENT    IC4'/,
     &'CELL 7'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH  8 11'/,
     &'NREGION 4 1 8'/,
     &'NMATERIAL 6 1'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 1.51640  0'/,
     &'ANNULUS 2 1.58750  5'/,
     &'ANNULUS 3 2.313177 3'/,
     &'ANNULUS 4 6.120091 6'/,
     &'ARRAY 1 (1 6 4.189053 0)'/,
     &'RODSUB 1 1 0.31750  4'/,
     &'RODSUB 1 2 1.82245  1'/,
     &'RODSUB 1 3 1.82626  0'/,
     &'RODSUB 1 4 1.87706  2'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 40 $'/,
     &'         41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 1 1 2 4'/,
     &'BUCKLING 0.0  0.00464')
c-po starem
 1010 format(
     &'*MATERIAL 1  6.12226  470.00  1    $'/,
     &'*         235.4  0.023323  2238.4  0.0938579 $'/,
     &'*         91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     470.00  2  91   1.0'/,
     &'*MATERIAL 5  2.70     298.00  2  27   1.0'/,
     &'*MATERIAL 6  1.       298.00  3  2001 0.1119  16 0.8881')

!     MATERIAL 1
         rDensityUZrH = 6.12226
         rTempUZrH    = 470.00
         rM1Wt2351    = 0.0233230
         rM1Wt0238    = 0.0938579
         rM1Wt0091    = 0.8670113
         rM1Wt5001    = 0.0158076
         rM1Wt3239    = 0.0000000
!     MATERIAL 2      
         rDensityFe   = 7.889
         rTempFe      = 350.00
         rM2Wt1056    = 1.0
!     MATERIAL 3
         rDensityLW1  = prDensityLW
         rTempLW1     = prTempLW
         rM3Wt2001    = 0.1119
         rM3Wt0016    = 0.8881
!     MATERIAL 4
         rDensityZr   = 6.49
         rTempZr      = 470.0
         rM4Wt0091    = 1.0
!     MATERIAL 5
         rDensityAl   = 2.7
         rTempAl      = prTempAl
         rM5Wt0027    = 1.0
!     MATERIAL 5      
         rDensityLW2  = prDensityLW
         rTempLW2     = prTempLW
         rM6Wt2001    = 0.1119
         rM6Wt0016    = 0.8881
         

      write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt2351,'2238.4',rM1Wt0238,'  91  ',
     &                rM1Wt0091  ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239
      write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
      write(pNF,1002) '1056  ',rM2Wt1056
      write(pNF,1001) 'MATERIAL 3 ',rDensityLW1,rTempLW1,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
      write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
      write(pNF,1002) '91    ',rM4Wt0091
      write(pNF,1001) 'MATERIAL 5 ',rDensityAl,rTempAl,2,' $'
      write(pNF,1002) '27    ',rM5Wt0027
      write(pNF,1001) 'MATERIAL 6 ',rDensityLW2,rTempLW2,3,' $'
      write(pNF,1002) '2001  ',rM6Wt2001,'16    ',rM6Wt0016
  
 1001    format(A,F6.4,1X,F6.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)

         write(pNF,1003) 'POWERC 0 0 '
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'REGION 1 3'
         write(pNF,1003) 'LEAKAGE 5'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) '*EOF'
         return
      end subroutine

!     ---------------------------------------------------------------------
!                            F U E L   S T D 1 2 %  
!     --------------------------------------------------------------------- 
      subroutine WST12INP(pNF, prDensityUZrH, prTempUZrH, prWeightU235,
     &                    prTempFe, prDensityLW, prTempLW, prTempZr,
     &                    pnWhat)
         implicit none
         integer, intent(in) :: pNF, pnWhat
         real   , intent(in) :: prDensityUZrH, prTempUZrH, prWeightU235,
     &                          prTempFe, prDensityLW, prTempLW,
     &                          prTempZr
         real :: rDensityUZrH, rTempUZrH, rM1Wt0235, rM1Wt0238,
     &           rM1Wt0091,
     &           rM1Wt5001, rM1Wt3239, rDensityFe, rTempFe, rM2Wt1056, 
     &           rDensityLW, rTempLW, rM3Wt2001, rM3Wt0016, rDensityZr,
     &           rTempZr, rM4Wt0091
     
         if(pnWhat .eq. 2) goto 2
         write(pNF, 1000)
         write(pNF, 1010)
1000     format(
     &'*st. 12% triga fuel.el., P=10kW/el., P(spec.)=35.971kW/kgU'/,
     &'*IDENT    ST12'/,
     &'CELL 6'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH 11 11'/,
     &'NREGION 5 0 5'/,
     &'NMATERIAL 4 1'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 0.31750 4'/,
     &'ANNULUS 2 1.82245 1'/,
     &'ANNULUS 3 1.82626 0'/,
     &'ANNULUS 4 1.87706 2'/,
     &'ANNULUS 5 2.31317 3'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 $'/,
     &'         40 41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 1 4 1 1 4'/,
     &'BUCKLING 0.0  0.00464')
c-po starem0.1171809
 1010 format(
     &'*MATERIAL 1  6.12226  518.00  1    $'/,
     &'*         235.4  0.023323  2238.4  0.0938579 $'/,
     &'*         91     0.8670113 5001    0.0158076  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     513.00  2  91   1.0')

         rDensityUZrH = prDensityUZrH
         rTempUZrH    = prTempUZrH
         rM1Wt0235    = prWeightU235
         rM1Wt0238    = 0.0938579
         rM1Wt0091    = 0.8670113
         rM1Wt5001    = 0.0158076
         rDensityFe   = 7.889
         rTempFe      = prTempFe
         rM2Wt1056    = 1.0
         rDensityLW   = prDensityLW
         rTempLW      = prTempLW
         rM3Wt2001    = 0.1119
         rM3Wt0016    = 0.8881
         rDensityZr   = 6.49
         rTempZr      = prTempZr
         rM4Wt0091    = 1.0

      write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt0235,'2238.4',rM1Wt0238,'  91  ',
     &                rM1Wt0091  ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',0.0
      write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
      write(pNF,1002) '1056  ',rM2Wt1056
      write(pNF,1001) 'MATERIAL 3 ',rDensityLW,rTempLW,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
      write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
      write(pNF,1002) '91    ',rM4Wt0091
         return
 2       continue 
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         return
 1001    format(A,F6.4,1X,F7.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)
      end subroutine

!     ---------------------------------------------------------------------
!                             F U E L   S T D 8 %  
!     ---------------------------------------------------------------------        
      subroutine WST8INP(pNF, prDensityUZrH, prTempUZrH, prWeightU235,
     &                   prTempFe, prDensityLW, prTempLW, prTempZr, 
     &                   pnWhat)
         implicit none
         integer, intent(in) :: pNF, pnWhat
         real   , intent(in) :: prDensityUZrH, prTempUZrH, prWeightU235,
     &                          prTempFe, prDensityLW, prTempLW,
     &                          prTempZr
         real :: rDensityUZrH, rTempUZrH, rM1Wt0235, rM1Wt0238,
     &           rM1Wt0091,
     &           rM1Wt5001, rM1Wt3239, rDensityFe, rTempFe, rM2Wt1056, 
     &           rDensityLW, rTempLW, rM3Wt2001, rM3Wt0016, rDensityZr,
     &           rTempZr, rM4Wt0091
         if(pnWhat .eq. 2) goto 2
         write(pNF, 1000)
         write(pNF, 1010)
 1000    format(
     &'*stand. 8% triga fuel.el., P=10kW/el. or P(spec.)=35.971kW/kgU'/,
     &'*IDENT    ST8'/,
     &'CELL 6'/,
     &'SEQUENCE 1'/,
     &'NGROUP 32'/,
     &'NMESH 11 11'/,
     &'NREGION 5 0 5'/,
     &'NMATERIAL 4 1'/,
     &'PREOUT'/,
     &'INITIATE'/,
     &'ANNULUS 1 0.31750 4'/,
     &'ANNULUS 2 1.82245 1'/,
     &'ANNULUS 3 1.82626 0'/,
     &'ANNULUS 4 1.87706 2'/,
     &'ANNULUS 5 2.31317 3'/,
     &'FEWGROUP 2 4 6 11 14 21 23 25 26 27 28 29 30 32 33 35 38 $'/,
     &'         40 41 43 45 47 49 52 54 55 56 57 60 63 66 69'/,
     &'MESH 1 4 1 1 4'/,
     &'BUCKLING 0.0  0.00464')

 1010  format(
     &'*MATERIAL 1  5.8107  513.00  1    $'/,
     &'*         235.4  0.016897855  2238.4  0.068005955 $'/,
     &'*         91     0.89863319   5001    0.016463  3239.1 0.0'/,
     &'*MATERIAL 2  7.889    350.00  2  1056 1.0'/,
     &'*MATERIAL 3  1.       298.00  3  2001 0.1119  16 0.8881'/,
     &'*MATERIAL 4  6.49     513.00  2  91   1.0')

         rDensityUZrH = prDensityUZrH
         rTempUZrH    = prTempUZrH
         rM1Wt0235    = prWeightU235
         rM1Wt0238    = 0.068005955
         rM1Wt0091    = 0.89863319
         rM1Wt5001    = 0.016463
         rM1Wt3239    = 0.0
         rDensityFe   = 7.889
         rTempFe      = prTempFe
         rM2Wt1056    = 1.0
         rDensityLW   = prDensityLW
         rTempLW      = prTempLW
         rM3Wt2001    = 0.1119
         rM3Wt0016    = 0.8881
         rDensityZr   = 6.49
         rTempZr      = prTempZr
         rM4Wt0091    = 1.0
c-
      write(pNF,1001) 'MATERIAL 1 ',rDensityUZrH,rTempUZrH,1,' $'
      write(pNF,1002) ' 235.4',rM1Wt0235,'2238.4',rM1Wt0238,'  91  ',
     &                rM1Wt0091 ,'$'
      write(pNF,1002) '5001  ',rM1Wt5001,'3239.1',rM1Wt3239
      write(pNF,1001) 'MATERIAL 2 ',rDensityFe,rTempFe,2,' $'
      write(pNF,1002) '1056  ',rM2Wt1056
      write(pNF,1001) 'MATERIAL 3 ',rDensityLW,rTempLW,3,' $'
      write(pNF,1002) '2001  ',rM3Wt2001,'16    ',rM3Wt0016
      write(pNF,1001) 'MATERIAL 4 ',rDensityZr,rTempZr,2,' $'
      write(pNF,1002) '91    ',rM4Wt0091
         return
2        continue  
         write(pNF,1003) 'BEGINC'
         write(pNF,1003) 'BUCKLING 0.0  0.00464'
         write(pNF,1003) 'BEGINC'
         return
 1001    format(A,F6.4,1X,F7.2,I3,A)
 1002    format(4(1X,A6,1X,F10.7))
 1003    format(A,F6.2,F6.2,I5)
      end subroutine      