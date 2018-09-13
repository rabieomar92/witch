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
!          pNF       - File unit number for WIMInnn.WIN file.
!          pcCellType - Unit cell element type ST8? ST12? LEU?
!          prDensityUZrH - Density in g cm-3 of Uranium.
!          prWtU235    - Weight percent of U-235 in that element.
!          prWtEr166   - Weight percent of Er-166 in that element. For LEU and FLIP only.
!          prWtEr167   - Wieght percent of Er-167 in that element. For LEU and FLIP only.

      subroutine WimCom(pNF, pcCellType, 
     &                  prDensityUZrH, prWtU235, prWtEr166,
     &                  prWtEr167, prPEl, prFuelTemp,
     &                  prTempLW, prDensityLW, prMassUZrH,
     &                  prBuMW, pnIsXe, piCladType, prCellRadius,
     &                  prCoreRadius,prReflectorThickness,
     &                  prBuckling,pnCoreLayers,praPSpec, prPrevTau)
         implicit none
         character*(*) pcCellType
         integer, intent(in) :: pNF, pnIsXe, piCladType, pnCoreLayers
         real   , intent(in) :: prDensityUZrH, prDensityLW
         real   , intent(in) :: prFuelTemp, prTempLW, prCellRadius
         real   , intent(in) :: prPEl, prBuMW, prMassUZrH, prWtU235,
     &                          prWtEr166, prWtEr167, prBuckling,
     &                          prCoreRadius,prReflectorThickness
         real   , intent(in) :: praPSpec(5), prPrevTau
         real                :: rTempZr, rTempBe, 
     &                          rTempAl, rTempC, 
     &                          rTempFe
!     Declaring fuel burnup and mass of UZrH in ton unit (TU)
      real :: rBuMWTU, rMassUZrHTU
!     Specific power per element, in MW/TU Uranium.
      real :: rPsp, rPsp0
!     Time-steps for WIMS POWERC card in day(s).
      real :: rDay0, rDay2, rDay3
!     Time-step multipliers for WIMS POWERC card in days(s).
      integer :: nX0, nX2, nX3
!     WIMS POWERC burnup control check.
      real rBuMWTUControl
      
!     Here Rabie declare his own version of burnup steps.
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
!        (2) rDay0
!     rPsp, Days, POWERC Card Steps (begin)
!     prBuMW: BU from ELEM.INP [MWd]
!     prMassUZrH: Uranium mass [g] from ELEM.INP -> conversion to ton(1000kg)

! -------------- REGION BEGIN: if THE UNIT CELL IS A FUEL CELL ----------------
      if(pcCellType .eq. 'FE12' .or. pcCellType .eq. 'FE08' .or. 
     &   pcCellType .eq. 'LEUR' .or. pcCellType .eq. 'FLIP' .or.
     &   pcCellType .eq. 'FE20') then

         if(prMassUZrH .eq. 0.0) then
            print*, ' Fatal Error: Mass of Uranium is 0.0 kg.'
            stop
         endif
      
!        Convert mass of uranium in [g] into tons [TU]
         rMassUZrHTU = prMassUZrH * 1.0E-6 
!        Calculate fuel burnup per unit ton of uranium.
         rBuMWTU  = prBuMW / (rMassUZrHTU)

!        -----------------------------------------------------------------------
!        Here we define the specific fuel element power (Psp).
!        This defines fuel element power per mass of UZrH in TU for TRIGA fresh
!        fuel. MW/TU
!        -----------------------------------------------------------------------
!        HERE WE CALCULATE THE SPESIFIC POWER (rPsp0) IN MW/TU FOR BURNUP COR-
!        RECTION. ANY VALUE OF prPEl MAY NOT AFFECT NEUTRON CROSS SECTIONS.
!        IT IS IMPORTANT TO NOTE THAT prPEl IS IN kW. HERE WE ADJUST THE BURNUP
!        STEP TO 0.1MWd.
!        -----------------------------------------------------------------------
         if (pcCellType .eq. 'FE08') then 
            rPsp0 = (praPSpec(1) / (1000.0))
     &                / rMassUZrHTU
            rDay0 = 0.1 / (praPSpec(1) / (1000.0))
         elseif (pcCellType .eq. 'FE12') then
            rPsp0 = (praPSpec(2) / (1000.0)) 
     &               / rMassUZrHTU
            rDay0 = 0.1 / (praPSpec(2) / (1000.0))
         elseif (pcCellType .eq. 'FE20') then
            rPsp0 = (praPSpec(3) / (1000.0)) 
     &               / rMassUZrHTU
            rDay0 = 1.0 / (praPSpec(3) / (1000.0))
         elseif (pcCellType .eq. 'FLIP') then
            rPsp0 = (praPSpec(4) / (1000.0)) 
     &               / rMassUZrHTU
            rDay0 = 1.0 / (praPSpec(4) / (1000.0))
         elseif (pcCellType .eq. 'LEUR') then
            rPsp0 = (praPSpec(5) / (1000.0)) 
     &               / rMassUZrHTU
            rDay0 = 1.0 / (praPSpec(5) / ( 1000.0))
         endif
         
!        ONCE THE FUEL HAS BEEN CORRECTED TO THE CURRENT BURNUP LEVEL, WE CAL-
!        CULATE THE CURRENT OPERATING FUEL POWER IN kW/TU.
         rPsp   = (prPEl/(1000.0)) / (rMassUZrHTU)
  
         rDay2 = 1.0
         rDay3 = 0.01
!        Defining the multiplier of the timestep.         
         nX0 = 1
         nX2 = 1
         nX3 = 2
      
!        -----------------------------------------------------------------------
!        HERE WE CALCULATE THE NUMBER OF DAYS FOR THE BURNUP STEP THAT ACCOUNTS
!        THE BURNUP LEVEL AFTER CURRENT REACTOR OPERATION. 
!        -----------------------------------------------------------------------
!        RECALL THAT:
!        FIRST PLACE, WE BRING THE FUEL BURNUP LEVEL TO THE CURRENT BURNUP LEVEL
!        BEFORE CURRENT REACTOR OPERATION. NOW, WE HAVE TO ACCOMODATE CURRENT 
!        FUEL BURNUP DUE TO CURRENT OPERATION. HERE WE APPROXIMATE THE WORKING
!        TIME PERIOD OF CURRENT OPERATION TO 0.01DAY.
!        
         if (rPsp * rDay3 * nX3 .gt. rBuMWTU) then
            nX3 = 1
            rDay3 = rBuMWTU / rPsp
         endif

!        -----------------------------------------------------------------------
!                   B U R N U P    S C E N A R I O    E X A M P L E
!        -----------------------------------------------------------------------
!        IN THIS CODE, EACH BURNUP STEP IS SET TO 0.1MWd. SO, FOR A FUEL ELEMENT
!        WITH 2.41MWd BURNUP, THUS WE DIVIDE THE BURNUP STEPS AS FOLLOWING:
!
!           0.1MWd WITH 24 BURNUP STEPS WITH 0.01MWd REMAINDER.
!
!        FOR A 190g URANIUM FUEL OPERATING AT 10kW, THE SPECIFIC FUEL POWER IS
!        GIVEN BY 52.6315789 MW/TU. THUS FOR EACH BURNUP STEP, 
!
!           0.1MWd = 52.6315789 * NDAYS
!
!        THUS FOR EACH BURNUP STEP, THE NUMBER OF DAYS IS 0.0019 DAYS. WE CAN
!        TREAT THIS SIMILIARLY FOR THE BURNUP REMAINDER. THE PURPOSE OF DIVIDING
!        BURNUP LEVEL INTO SEVERAL STEPS IS FOR FLUX SPECTRUM RE-CALCULATION
!        IN CELL CALCULATION AFTER EACH BURNUP STEPS. THUS THE CALCULATION WILL
!        BE ACCURATE AS POSSIBLE.
!
!        HERE, WE CALCULATE THE NUMBER OF BURNUP STEPS TO ACCOMODATE THE TOTAL
!        BURNUP LEVEL OF THE FUEL. 
         nX0    = (rBuMWTU - rPsp * rDay3 * nX3) / (rPsp0 * rDay0)
       
!        HERE WE CALCULATE THE NUMBER OR DAYS FOR THE REMAINING BURNUP LEVEL.
         rDay2 = (rBuMWTU - rPsp * rDay3 * nX3 - rPsp0 * rDay0 *
     &           nX0) / (rPsp0 * nX2)
      
!        THIS IS FOR PROGRAMMER BURNUP CONTROL CHECK. PLEASE DISREGARD THIS.
         rBuMWTUControl = rPsp0 * rDay0 * nX0 + rPsp0 * rDay2 *
     &                    nX2 + rPsp * rDay3 * nX3
     
!        HERE WE WRITE THE APPROPRIATE WIMS INPUT. 
         if(pcCellType .eq. 'FE20') then
            call WST20INP(pNF, prDensityUZrH, prFuelTemp, prWtU235,
     &                    rTempFe, prDensityLW, prTempLW, 
     &                    rTempZr, 1, piCladType, prCellRadius,
     &                    prBuckling)
         endif
         if(pcCellType .eq. 'FE12') then
            call WST12INP(pNF, prDensityUZrH, prFuelTemp, prWtU235,
     &                    rTempFe, prDensityLW, prTempLW, 
     &                    rTempZr, 1, piCladType, prCellRadius,
     &                    prBuckling)
         endif
         if(pcCellType .eq. 'FE08') then
            call WST8INP(pNF, prDensityUZrH, prFuelTemp, prWtU235,
     &   rTempFe, prDensityLW, prTempLW, rTempZr, 1, 
     &   piCladType, prCellRadius, prBuckling)
         endif
      
!     Printout POWERC ...
         if(rDay0 .eq. 0.0) nX0 = 0       
         if(rDay2 .eq. 0.0) nX2 = 0       
         if(rDay3 .eq. 0.0) nX3 = 0       

         if(rPsp0 .eq. 0.0) nX0 = 0       
         if(rPsp0 .eq. 0.0) nX2 = 0       
         if(rPsp   .eq. 0.0) nX3 = 0       

         if(nX0 .gt. 0) call fPowerC(pNF, rPsp0, rDay0, nX0 , 1, 0, 0)
         if(nX0 .gt. 0 .and. nX2 .gt. 0)
     &      write(pNF,'(A/A)') 'BEGINC', 'BEGINC'
         if(nX2 .gt. 0) call fPowerC(pNF, rPsp0, rDay2, nX2, 1, 0, 0)
         if(nX3 .gt. 0 .and. (nX0 .gt. 0 .or. nX3 .gt. 0))
     &                  write(pNF,'(A/A)') 'BEGINC', 'BEGINC'
         if(nX3 .gt. 0) call fPowerC(pNF, rPsp, rDay3, nX3,1,0,0)
 
!     no combustion a.k.a if the fuel is still fresh..
         if(nX0 .eq. 0 .and. nX2 .eq. 0 .and. nX3 .eq. 0)
     &      call fPowerC(pNF, 0.0, 0.0, 0, 1, 0, 0)
         write(pNF,'(A)') 'BEGINC'
c         write(pNF,'(A)') 'LEAKAGE 5'
         write(pNF,'(A,F7.5)') 'BUCKLING 0.00000 ', prBuckling
         write(pNF,'(A)') 'BEGINC'
         if(pnIsXe .eq. 0) call fPowerC(pNF, 0.0, 0.0, 0, 1, 1, 0)
         call fPowerC(pNF, 0.0, 0.0, 0, 0, 0, 1)
         return
      endif
! -------------- REGION END: if THE UNIT CELL IS A FUEL CELL ------------------



! ------------ REGION BEGIN: if THE UNIT CELL IS NOT A FUEL CELL --------------
!     Generate WIMS input file WIMInnn.WIN according to cell type (G, GR, W, LW, ...)
!     But the cell type is not of a FUEL type.   

 
         if(pcCellType .eq. 'G   ') then
c            call WGINP(pNF,prFuelTemp,rTempAl,rTempC, prCoreRadius,
c     &                 prReflectorThickness)
            call WGR2INP(pNF,prDensityLW,prTempLW,rTempC,rTempAl,
     &                  prFuelTemp, prCellRadius, prBuckling)
         endif
         if(pcCellType .eq. 'GRAP') then
            call WGRINP(pNF,prDensityLW,prTempLW,rTempC,rTempAl,
     &                  prFuelTemp, prCellRadius, prBuckling)
         endif
         if(pcCellType .eq. 'W   ') then
          call WWINP (pNF,prDensityLW,prTempLW,rTempAl,rTempC,
     &                prCoreRadius,prReflectorThickness)
         endif
         if(pcCellType .eq. 'COOL') then
          call WLWINP(pNF,prDensityLW,prTempLW,prDensityLW,
     &                prTempLW, prFuelTemp, prCellRadius, prBuckling)
         endif
         if(pcCellType .eq. 'BERY') then
          call WBEINP(pNF,prDensityLW,prTempLW,rTempBe,
     &                rTempAl, prFuelTemp, prCellRadius, prBuckling)
         endif
         if(pcCellType .eq. 'CHN1') then
          call WIC1INP(pNF,prDensityLW,prTempLW,
     &                 rTempAl, prFuelTemp, prCellRadius, prBuckling)
         endif
         if(pcCellType .eq. 'CHN2') then
          call WIC2INP(pNF, prDensityLW, prTempLW, rTempAl,
     &                 prDensityLW, prTempLW, prFuelTemp, 
     &                 prCellRadius, prBuckling)
         endif
         if(pcCellType .eq. 'CHN3') then
          call WIC3INP(pNF, prDensityLW, prTempLW, prDensityLW,
     &                 prTempLW, rTempAl, prFuelTemp, 
     &                 prCellRadius, prBuckling)
         endif
!        new IC4 (pulz rod)
         if((pcCellType .eq. 'CHN4').or.(pcCellType .eq. 'TRCR')) THEN
          call WIC4INP(pNF,prDensityLW,prTempLW,rTempAl, prFuelTemp,
     &                 prCellRadius, prBuckling)
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
            write(pNF,1003) '*EOF'
         endif
         return
 1003    format(A,F11.6,F11.6,I5)
      end subroutine
