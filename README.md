# witch
WIMS Integrated TRIGA Core Homogenisation Code. This code prepares the homogenized neutron cross section data tape (.TXS) for the use of TRIMON code.
Please refer the basic code workflow this paper: doi yet to be prepared (paper is waiting to be published in a journal).

The code is divided into several sub-tasks. Each task has its own set of source files which has to be compiled into an object file (.o). The task listing is given as follows:

TRSTART (TRIGGER START)
This task performs necessary checks to the main input file (MAIN.INP) which includes input cards format validation.
If a format error is encountered, the code instructs the machine to terminate the program and request the user to
re-validate MAIN.INP. This task only require trstart.for source file. Compile procedure:

        if task == TRSTART (
            del "...\source"\trstart.o
            gfortran -static -c "...\source\trstart.for"  -fno-range-check -static-libgcc
            move trstart.o "...\source"
        )
        
WITRIG (WIMS INPUT FILES PREPARATION)
This task prepares all WIMS input file (Wimi###.win) for each core channel specified by the code user in the main
input file (MAIN.INP). This task requires all source files with the generic name witri*.for.

        if task == WITRIG (
            del "...\source"\witri*.o
            gfortran -static -c "...\source\witri*.for" -fno-range-check -static-libgcc
            move witri*.o "...\source"    
        )
        
LOOXSB (WIMS EXECUTION LOOP FOR THE PREPARATION OF NEUTRON CROSS-SECTION REVESION-B)
This task instructs WIMS-D code (WIMSD4.exe) to run cell calculation according to the given WIMS input file (Wimi###.win)
This task only require looxsb.for source file. Each WIMS execution loop will produce an WIMS output file of the form
(Wimi###.wou). Compile procedure:

        if task == LOOXSB (
            del "...\source"\looxsb.o
            gfortran -static -c "...\source\looxsb.for" -fno-range-check -static-libgcc
            move looxsb.o "...\source"    
        )
        
XSWOUT (CROSS SECTION GENERATION FROM WIMS OUTPUT)
This task reads the WIMS output files (Wimi###.wou), then performs EDH caculation and finally generates homogenized cross-
sections which are compiled into a single file with .TXS extension. A .TXS file is an input file for Homogenized Group Monte
Carlo (HGMC) code. This task require all source files with name xswout*.for. COmpile procedure:

        if task == XSWOUT (
            del "...\source"\xswout*.o
            gfortran -static -c "...\source\xswout*.for" -fno-range-check -static-libgcc
            move xswout*.o "...\source"    
        )

WITCH (MAIN WITCH EXECUTABLE FILE)
This is the main task that performs all four sub-tasks (TRSTART,WITRIG,LOOXSB,XSWOUT) serially. Please note that before
compiling WITCH, all of the necessary object files (.o) for all sub-tasks have to be ready. Compile procedure:

        if WITCH == WITCH (
            gfortran -g -static -o "...\bin"\WITCH "...\source"\MAIN.for "...\source"\*.o -fno-range-check -static-libgcc
            copy "...\bin"\WITCH.exe ...\Code
        )
