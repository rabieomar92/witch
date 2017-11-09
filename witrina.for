! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------
!
! This code contains the subroutines required to generate WIMS input files,
! WIMInnn.WIN. The subroutines are:
!
!              WimCom = The main subroutine that generates a WIMInnn.WIN file.
!                       WimCom calls W[CELL_TYPE]INP subroutine before writing
!                       WIMS input files. 
!
!              W[CELL_TYPE]INP = A subroutine that prepares WIMS cards for 
!                                CELL_TYPE element.
!  
! The cell types (pcCellType) are defined as follows:
!
! *Id:      FLIP - FLIP Fuel Element
!           LEU  - LEU Fule Element
!           ST8  - Standard 8% Fuel Element
!           ST12 - Standard 12% Fuel Element
!           IC1  - irradiation channel, type 1
!           IC2  - irradiation channel, type 2
!           IC3  - irradiation channel, type 3
!           IC4  - TRANZIENT ROD or TRCR
!           BE   - berillium element
!           GR   - graphite element
!           G    - graphite reflector
!           W    - water reflector
!           LW   - water cell  

! -----------------------------------------------------------------------------
!                          PLEASE READ THIS CAREFULLY
! -----------------------------------------------------------------------------
! ** This subroutine writes WIMS input cards into WIMS input file, WIMInnn.WIN.
!
! ** Parameter definitions:
!
!          pNF       - File unit number for WIMInnn.WIN file
!          pNFO      - File unit number for WiTrig.PRI output file.
!          pcCellType - Unit cell element type ST8? ST12? LEU?
!          prDensityUZrH - Density in g cm-3 of Uranium.
!          prWtU235    - Weight percent of U-235 in that element.
!          prWtEr166   - Weight percent of Er-166 in that element. For LEU and FLIP only.
!          prWtEr167   - Wieght percent of Er-167 in that element. For LEU and FLIP only.

      subroutine WimCom(pNF, pNFO, pcCellType, 
     &                  prDensityUZrH, prWtU235, prWtEr166,
     &                  prWtEr167, prPEl, prFuelTemp,
     &                  prTempLW, prDensityLW, prMassUZrH,
     &                  prBuMW, pnIsXe)
         implicit none
         character*(*) pcCellType
         integer, intent(in) :: pNF, pNFO, pnIsXe
         real   , intent(in) :: prDensityUZrH, prDensityLW
         real   , intent(in) :: prFuelTemp, prTempLW
         real   , intent(in) :: prPEl, prBuMW, prMassUZrH, prWtU235,
     &                          prWtEr166, prWtEr167
         real                :: rTempZr, rTempBe, 
     &                          rTempAl, rTempC, 
     &                          rTempFe
!     Declaring fuel burnup and mass of UZrH in ton unit (TU)
      real :: rBuMWTU, rMassUZrHTU
!     Specific power per element, in MW/TU Uranium.
      real :: rPsp, rPsp10
!     Time-steps for WIMS POWERC card in day(s).
      real :: rDay1, rDay2, rDay3
!     Time-step multipliers for WIMS POWERC card in days(s).
      integer :: nX1, nX2, nX3
!     WIMS POWERC burnup control check.
      real rBuMWTUControl
      
      rTempZr = prFuelTemp
      rTempBe = prTempLW
      rTempAl = prTempLW
      rTempC  = prTempLW
      rTempFe = (prFuelTemp + prTempLW) / 2.0

! -----------------------------------------------------------------------------
!               NOTES ON WIMS POWERC CARD (BURNUP CALCULATION)
! -----------------------------------------------------------------------------
!     The POWERC keyword determines how fuel depletion or other types of 
!     separate dependent WIMS cases will be calculated. The recommended 
!     fuel depletion parameter selection for POWERC is 
!        1) input the specific power in MW/Kg of initial heavy metal, 
!        2) specify the timestep interval in days, and 
!        3) request flux spectrum recalculation before each timestep. 
!     Other options for burnup are described in the RSIC WIMS-D4 input 
!     description. The POWERC must be used each time the user wishes to 
!     change the timestep characteristics, otherwise the fuel will continue 
!     depletion using the most recent POWERC specification. The user must have
!     two BEGINC cards for each burn step.   
! -----------------------------------------------------------------------------
!     WIMS POWERC USAGE
!
!     SYNTAX: POWERC I RQ RTAU INDNB RMAXD INDG list
!
!     I = 1 RQ is MW/ton initial heavy elements.
!     I = 2 RQ is fissions /cm3/ sec cell averaged.
!     I = 3 RQ is fissions /cm3/ sec averaged over burnup materials.
!     I = 4 RQ is total flux.
!     I = 5 RQ is flux in group INDG
!
!     RQ    = -1 the value reached by the previous burnup step is
!             assumed. e.g. I = 1, RQ = 20 on the first step and I = 4,
!     RQ    = -1 on the second gives a burnup at a constant flux
!             corresponding to an initial 20 MW/te.
!     RTAU  = the timestep between criticality calculations and
!             flux renormalization to the input power level.
!     INDNB = the number of such timesteps between lattice calculations.
! -----------------------------------------------------------------------------
!
!     Parameters:
!        
!        (1) rPsp = RQ = MW/TU (per ton unit) initial Uranium fuel elements.   
!        (2) rDay1
!     rPsp, Days, POWERC Card Steps (begin)
!     prBuMW: BU from ELEM.INP [MWd]
!     prMassUZrH: Uranium mass [g] from ELEM.INP -> conversion to ton(1000kg)

! -------------- REGION BEGIN: if THE UNIT CELL IS A FUEL CELL ----------------
      if(pcCellType .eq. 'ST12' .or. pcCellType .eq. 'ST8 '.or. 
     &   pcCellType .eq. 'LEU ' .or. pcCellType .eq. 'FLIP') then

         if(prMassUZrH .eq. 0.0) then
            write(pNFO,*)  pcCellType
            write(pNFO,*) 'Fatal Error: Mass of Uranium is 0.0 kg.'
            stop 'Fatal Error: Mass of Uranium is 0.0 kg.'
         endif
      
!        Convert mass of uranium in [g] into tons [TU]
         rMassUZrHTU = prMassUZrH * 1.0E-6
!        Calculate fuel burnup per unit ton of uranium.
         rBuMWTU  = prBuMW / rMassUZrHTU

!        -----------------------------------------------------------------------
!        Here we define the specific fuel element power (Psp).
!        This defines fuel element power per mass of UZrH in TU for TRIGA fresh
!        fuel. MW/TU
!        -----------------------------------------------------------------------

!        rPsp10 is the specific fuel element power of TRIGA fresh fuel (10kW/el)
         rPsp10 = (10.0 / 1000.0)  / rMassUZrHTU ! 10kW/mass U (TU)
         rPsp   = (prPEl / 1000.0) / rMassUZrHTU
!        rDay1, rDay2 and rDay3 are variables that stores the number of days that the fuel has burned up.  
         rDay1 = 100.0
         rDay2 = 1.0
         rDay3 = 0.01
!        Defining the multiplier of the timestep.         
         nX1 = 1
         nX2 = 1
         nX3 = 2
      
!        PE*0.01*2 > Burnup with input
         if (rPsp * rDay3 * nX3 .gt. rBuMWTU) then
            nX3 = 1
            rDay3 = rBuMWTU / rPsp
         endif
         
!        First POWERC Card (10kW*rDni1*nX1=rBuMWTU-PE*0.01*2)
         nX1    = (rBuMWTU - rPsp * rDay3 * nX3) / (rPsp10 * rDay1)
       
!        Remainder
         rDay2 = (rBuMWTU - rPsp * rDay3 * nX3 - rPsp10 * rDay1 *
     &           nX1) / (rPsp10 * nX2)
      
!        rPsp, Days, Steps for the POWERC card (end)
!        Control: See and monitor whether the total powerc card parameters sum to the total fuel burnup per 
!        unit ton, rBuMWTU. If rBuMWTUControl = rBuMWTU then the control check is passed.
         rBuMWTUControl = rPsp10 * rDay1 * nX1 + rPsp10 * rDay2 *
     &                    nX2 + rPsp * rDay3 * nX3
         write(pNFO,*) '  | '
         write(pNFO,*) '  |____  B U R N U P   C A L C U L A T I O N'
         write(pNFO,*) '         Mass of Uranium [g],[T]    =',
     & prMassUZrH, rMassUZrHTU
         write(pNFO,*) '         Fuel Burnup [MWd],[MWD/TU] =',
     &      prBuMW, rBuMWTU
         write(pNFO,*) '         Burnup [MWd/TU], Control   =', rBuMWTU, 
     &      rBuMWTUControl
         write(pNFO,*)

         if(pcCellType .eq. 'ST12') then
            call WST12INP(pNF, prDensityUZrH, prFuelTemp, prWtU235,
     &                    rTempFe, prDensityLW, prTempLW, 
     &                    rTempZr, 1)
         endif
         if(pcCellType .eq. 'ST8 ') then
            call WST8INP(pNF, prDensityUZrH, prFuelTemp, prWtU235,
     & rTempFe, prDensityLW, prTempLW, rTempZr, 1)
         endif
      
!     Printout POWERC ...
         if(rDay1 .eq. 0.0) nX1 = 0       
         if(rDay2 .eq. 0.0) nX2 = 0       
         if(rDay3 .eq. 0.0) nX3 = 0       

         if(rPsp10 .eq. 0.0) nX1 = 0       
         if(rPsp10 .eq. 0.0) nX2 = 0       
         if(rPsp   .eq. 0.0) nX3 = 0       

         if(nX1 .gt. 0) call fPowerC(pNF, rPsp10, rDay1, nX1 , 1, 0, 0)
         if(nX1 .gt. 0 .and. nX2 .gt. 0)
     &      write(pNF,'(A/A)') 'BEGINC', 'BEGINC'
         if(nX2 .gt. 0) call fPowerC(pNF, rPsp10, rDay2, nX2, 1, 0, 0)
         if(nX3 .gt. 0 .and. (nX1 .gt. 0 .or. nX3 .gt. 0))
     &                  write(pNF,'(A/A)') 'BEGINC', 'BEGINC'
         if(nX3 .gt. 0) call fPowerC(pNF, rPsp, rDay3, nX3,1,0,0)
 
!     no combustion
         if(nX1 .eq. 0 .and. nX2 .eq. 0 .and. nX3 .eq. 0)
     &      call fPowerC(pNF, 0.0, 0.0, 0, 1, 0, 0)
         write(pNF,'(A)') 'BEGINC'
         write(pNF,'(A)') 'LEAKAGE 5'
         write(pNF,'(A)') 'BUCKLING 0.0  0.00464'
         write(pNF,'(A)') 'BEGINC'
         if(pnIsXe .eq. 0) call fPowerC(pNF, 0.0, 0.0, 0, 1, 1, 0)
         call fPowerC(pNF, 0.0, 0.0, 0, 0, 0, 1)
         return
      endif
! -------------- REGION END: if THE UNIT CELL IS A FUEL CELL ------------------



! ------------ REGION BEGIN: if THE UNIT CELL IS NOT A FUEL CELL --------------
!     Generate WIMS input file WIMInnn.WIN according to cell type (G, GR, W, LW, ...)
!     But the cell type is not of a FUEL type.   

         write(pNFO,*) '  | '
         write(pNFO,*) '  |____  B U R N U P   C A L C U L A T I O N'
         write(pNFO,*) '         Not applicable. It is not a fuel cell.'
         write(pNFO,*)
         if(pcCellType .eq. 'G   ') then
            call WGINP(pNF,prFuelTemp,rTempAl,rTempC)
         endif
         if(pcCellType .eq. 'GR  ') then
            call WGRINP(pNF,prDensityLW,prTempLW,rTempC,rTempAl)
         endif
         if(pcCellType .eq. 'W   ') then
          call WWINP (pNF,prDensityLW,prTempLW,rTempAl,rTempC)
         endif
         if(pcCellType .eq. 'LW  ') then
          call WLWINP(pNF,prDensityLW,prTempLW,prDensityLW,prTempLW)
         endif
         if(pcCellType .eq. 'BE  ') then
          call WBEINP(pNF,prDensityLW,prTempLW,rTempBe,rTempAl)
         endif
         if(pcCellType .eq. 'IC1 ') then
          call WIC1INP(pNF,prDensityLW,prTempLW,rTempAl)
         endif
         if(pcCellType .eq. 'IC2 ') then
          call WIC2INP(pNF, prDensityLW, prTempLW, rTempAl,
     &                 prDensityLW, prTempLW)
         endif
         if(pcCellType .eq. 'IC3 ') then
          call WIC3INP(pNF, prDensityLW, prTempLW, prDensityLW,
     &                 prTempLW, rTempAl)
         endif
!        new IC4 (pulz rod)
         if((pcCellType .eq. 'IC4 ').or.(pcCellType .eq. 'TRCR')) THEN
          call WIC4INP(pNF,prDensityLW,prTempLW,rTempAl)
         endif
         call fPowerC(pNF,0.0,0.0,0,1,1,1)
C*
777      continue
         return
      end subroutine WimCom

C*
      subroutine fPowerC(pNF, prPsp, prDniBu, pnX, pnIsPo,
     &                   pnIsBe, pnIsEo)
         integer, intent(in) :: pNF, pnX, pnIsPo, pnIsBe, pnIsEo
         real   , intent(in) :: prPsp, prDniBu
         if(pnX .eq. 0 .and. pnIsPo .eq. 1) then
            write(pNF,1003) 'POWERC 0 0 '  
         endif
         if(pnX .ne. 0) then
            write(pNF,1003) 'POWERC 1 ', prPsp, prDniBu, pnX
         endif
         if(pnIsBe .eq. 1) then
            write(pNF,1003) 'BEGINC'
            write(pNF,1003) 'BEGINC'
         endif
         if(pnIsEo .eq. 1) then
            write(pNF,*) '*EOF'
         endif
         return
 1003    format(A,F11.6,F11.6,I5)
      end subroutine
