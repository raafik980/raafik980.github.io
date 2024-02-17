---
title: "Machine Learning Assisted QM/MM Free Energy Simulations: A Quick Way to Set up an Interface to Python in CHARMM"
author:
date: 2024-01-24  
categories: [Software, CHARMM]
tags: [machine learning potentials, deep learning, qm/mm simulations, python, fortran]
render_with_liquid: true
pin: false
toc: true
math: true
---

> *DISCLAIMER: The content in this article is for demonstration purposes only and may contain errors and technical inaccuracies*

<br>
<br>

## Interfaces to QM Programs in CHARMM

CHARMM houses code **(<path_to_charmm>/source/gukint/gukini.F90)** to interface with several quantum mechanical (QM) programs, including Gaussian, GAMESS, and Q-Chem. These interfaces allow CHARMM to perform QM/MM simulations, where the QM region is treated with a high-level QM method, and the MM region is treated as point charge (electrostatic embedding). These implementations can leverage the CHARMM capabilities, such as applying restraints, selection algebra, link-atom implementation, umbrella sampling, and various analysis modules. Depending on the atomic selection choices mentioned in the CHARMM running script, the simulation can represent the atoms with all QM, all MM, or a hybrid of QM and MM.
This article demonstrates a quick way to modify the existing implementation to interface with a Python package that could use machine learning (ML) potentials to calculate QM/MM energies and forces.

## CHARMM Q-Chem Interface for QM/MM Simulations
Interface to Q-Chem can be considered as one of the decently documented interfaces and provides several key options to control specifications of the QM/MM simulations. The interface is implemented in the file **gukini.F90** (*--with-qchem* key is required to include this piece of code to be included during compilation). The interface is called from the CHARMM script using the following command:

```console
!!CHARMM Script
!!Here the QMATOMS is the selection of atoms that will be treated with QM method

QCHEm REMO SELE QMATOMS SHOW END

!!CHARMM Script
```

## Add a New Option to Interface with Python in the Current Q-Chem Interface
* Add the new **QCHEm** option key to call in the CHARMM script to activate the interface with Python for QM/MM simulations (such as ML-assisted QM/MM simulations).The new option is added as **QCHARMMPY** in the **gukini.F90** file in the **(<path_to_charmm>/source/gukint/gukini.F90)**.

```fortran
!!source/ltm/gamess_ltm.F90
module gamess_fcm
  use chm_kinds
  use dimens_fcm
  use quantm,only:qmused_quantum
  implicit none

!
! rest of the code
!

! New Q-Chem Option CH2PY added with rest of options
LOGICAL QMP2,QLMP2,QCCSD,QCIS,QRIMP2,QSOSMP2,QMOSMP2,QSCSMP2, &
       QQCOORD,QRESTART,QSAVORB,QQCLJ,QMICRO,QFRSTMICIT,QRESET,  &
       QQCHARG,QQCPARA,QREADHESS,QSAVEHESS,QINIQC,QNRAP,QPRNTINP, & 
       QSAVEINP,QSAVEOUT,QPCM,QREADGRAD,QSAVEGRAD,QWRITEINP,QOPENMP, & 
       QMIXED,QQCSCRATCH,QQCHEM,QQEWALD,QMESS,QQRESD,QQCONS,QQNOLKATMS, QCHARMMPY ! added new option along with existing Q-Chem options

!
! rest of the code
!

end module gamess_fcm
```

## How to Initiate Custon Arrays to Extract Information from CHARMM Topology (.PSF)
In certain cases, the interface with Python may require the information from the CHARMM topology file (.psf) to calculate the QM/MM energies and forces which may not be readily available within the current gukin.F90 interface. So it would be beneficial to have copy of the information from the .psf file to be used to pass information to the Python script. The following code snippet demonstrates how to initiate a custom array to extract the information from the CHARMM topology file (.psf) by introducing custom variables in the **psf_ltm.F90** file in the **(<path_to_charmm>/source/psfgen/psf_ltm.F90)**.

The code below demonstrates how to introduce a new array variable (later to store a copy of the charge array from the .psf file to be passed to Python package).

```fortran
!!source/ltm/psf_ltm.F90
module psf
  use chm_kinds
  use dimens_fcm

  !
  ! rest of the code
  !

  ! New variable 'cg_dum' added to store a copy of charge array
  real(chm_real), allocatable, dimension(:) :: CG, AMASS, RSCLF, ALPHADP, THOLEI, cg_dum ! added new variable to store charge array

  !
  ! rest of the code
  !

  subroutine allocate_psf_ltm()
    use memory

    !
    ! rest of the code
    !

    ! Allocate memory for the new array variable 'cg_dum'
    call chmalloc(file_name, routine_name, 'iacc  ', maxpad, intg=iacc)
    call chmalloc(file_name, routine_name, 'iac1  ', maxpad, intg=iac1)
    call chmalloc(file_name, routine_name, 'inb   ', maxnb, intg=inb)
    call chmalloc(file_name, routine_name, 'iblo  ', maxaim, intg=iblo)
    call chmalloc(file_name, routine_name, 'cg    ', maxaim, crl=cg)
    call chmalloc(file_name, routine_name, 'cg_dum', maxaim, crl=cg_dum) ! added a new line allocate memory for the new variable 'cg_dum'
    call chmalloc(file_name, routine_name, 'amass ', maxaim, crl=amass)
    call chmalloc(file_name, routine_name, 'rsclf ', maxaim, crl=rsclf)
    call chmalloc(file_name, routine_name, 'alphadp', maxaim, crl=alphadp)
    call chmalloc(file_name, routine_name, 'tholei', maxaim, crl=tholei)

    !
    ! rest of the code
    !

  end subroutine allocate_psf_ltm

  !
  ! rest of the code
  !

end module psf
```

### How Interface Passes Information to and from Python Package
1) <u>Consolidate the information required from CHARMM into a file (e.g., **charmm2py.inp**).</u>
 This file will contain the information required to calculate/predict the QM/MM energies and forces using the Python script (like an ML potential). In this demonstration example, the following information is included in the **charmm2py.inp** file:

    - Simulation box dimensions (for periodic boundary conditions, long-range electrostatics, equivariance transformations required for ML potentials, etc.)
    - MM region atomic coordinates
    - MM region atomic charges
    - QM region atomic coordinates
    - QM region atomic charges
    - QM region atom types
<br>
2) <u>Define environment variables to be used in the CHARMM script to control arguments to be passed to Python.</u>
 In this demonstration example, the following environment variables are added in the **gukini.F90** code, assuming our python package (PY_PACKAGE) takes the arguments QM_CHARGE, QM_MULTIPLICITY, CHARMM2PY_FILE, and PY2CHARMM_FILE:

    - **PY_PACKAGE**: Path to the Python package (e.g., mlpotential) *[It will be convenient if the package is installed in a conda environment]*
    - **QM_CHARGE**: Total charge of the QM region
    - **QM_MULTIPLICITY**: Multiplicity of the QM region
    - **CHARMM2PY_FILE**: Path to the input file for the Python package (e.g., charmm2py.inp)
    - **PY2CHARMM_FILE**: Path to the output file from the Python package (e.g., py2charmm.out)
<br>
3) <u>Call the Python package with **system** call in fortran90.</u>
An example of the system call:
    
    ```bash
    python -m $PY_PACKAGE --qm_charge $QM_CHARGE --qm_multiplicity $QM_MULTIPLICITY --inp_file $CHARMM2PY --out_file $PY2CHARMM_FILE
    ```
    Instead of a plain text file, one can pass the information as binary, fifo, or any other suitable format considering performance and security. In this example, we are passing the information as a plain text file.
<br>
4) <u>Read the output from the Python package and update the QM/MM energies and forces in CHARMM.</u>
In this demonstration example, the **py2charmm.out** file will contain the QM/MM energies and forces calculated/ML-predicted by the Python package. The **gukini.F90** code will read the **py2charmm.out** file and update the QM/MM energies and forces in CHARMM.

## gukini.F90 Code Modification to Interface with Python
The following code snippets demonstrates how to modify the **gukini.F90** file to interface with a Python package (e.g., mlpotential) to calculate the QM/MM energies and forces. We will go section by section to understand the modifications.

1) <u>Initialization of the new QCHARMMPY option along with other Q-Chem options in the **gukini.F90** file.</u>

```fortran
!!source/gukint/gukini.F90
    !
    ! rest of the code
    !

SUBROUTINE GUKINI(COMLYN,COMLEN)

    !
    ! rest of the code
    !

    #if KEY_QCHEM==1
    QRESET=(INDXA(COMLYN,COMLEN,'RESE').GT.0)
    IF(QRESET) THEN 
       IF(PRNLEV.GE.2) THEN 
          WRITE(OUTU,22) &
               'RESET: Resetting Q-Chem options' 
       ENDIF
       QQCOORD=.FALSE.      ! Pass Coordinates from Q-Chem back to CHARMM
       QRESTART=.FALSE.     ! Determine if Q-Chem restarts first step from saved orbitals
       QSAVORB=.FALSE.      ! Save orbitals from Q-Chem calculation
       QQCLJ=.FALSE.        ! Use Lennard-Jones parameters in Q-Chem calculation
       QMICRO=.FALSE.       ! Controls Micro-iteration procedure
       QFRSTMICIT=.false.   ! First micro-iteration step?
       OPTCNT=0             ! Counts QM/MM optimization steps
       QCPARA=-1            ! Reset the Parallel options 
       QSAVEHESS=.false.    ! Reset Hessian saving option
       QREADHESS=.false.    ! Reset Hessian reading option 
       QSAVEGRAD=.false.    ! Reset Gradient saving option
       QREADGRAD=.false.    ! Reset Gradient reading option
       QSAVEINP=.false.     ! Save Q-Chem input files
       QSAVEOUT=.false.     ! Save Q-Chem output files
       QPCM=.false.         ! Turn off QM/MM/PCMa
       QWRITeinp=.false.    ! Force Q-Chem to write input file and then quit
       NREStart=0           ! Number of times to restart Q-Chem if a job fails 
       QQEWALD=.false.      ! Not using John Herbert's QM/MM Ewald
       QMESS=.false.        ! Not using MESS QM/MM energy estimation
       NRoots=0             ! Number of roots for MESS-H
       QMALpha=ZERO         ! alpha / kappa for QM part of QM/MM ewald 
       MMALpha=ZERO         ! alpha / kappa for MM part of QM/MM ewald
       QQNOLKATMS=.false.   ! Check if we explicitly exclude link atoms from Q-Chem


       !!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

       QCHARMMPY=.false.    ! Previously defined QCHARMMPY option is initialized to false here

       !!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


    ENDIF
#endif 

    !
    ! rest of the code
    !

#if KEY_QCHEM==1
    ! Test to see if Hessian is gonna be read in 
    QREADHESS=(INDXA(COMLYN,COMLEN,'RHES').GT.0)
    QSAVEHESS=(INDXA(COMLYN,COMLEN,'SHES').GT.0)
    ! Test to see if Gradient is going to be saved
    QSAVEGRAD=(INDXA(COMLYN,COMLEN,'SGRA').GT.0)
    QREADGRAD=(INDXA(COMLYN,COMLEN,'RGRA').GT.0)
    ! Test to see if input/output files will be saved
    QSAVEINP=(INDXA(COMLYN,COMLEN,'SINP').GT.0)
    QSAVEOUT=(INDXA(COMLYN,COMLEN,'SOUT').GT.0)
    ! Test to see if PCM will be used with QM/MM
    QPCM=(INDXA(COMLYN,COMLEN,'PCM').GT.0)
    QWRITeinp=(INDXA(COMLYN,COMLEN,'WQIN').GT.0)
    QOPENMP=(INDXA(COMLYN,COMLEN,'OMP').GT.0)
    QQEWALD=(INDXA(COMLYN,COMLEN,'EWAL').GT.0)
    QMESS=(INDXA(COMLYN,COMLEN,'MESS').GT.0)
    ! Test to check for link atom exclusions
    QQNOLKATMS=(INDXA(COMLYN,COMLEN,'NOLI').GT.0)


    !!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

    QCHARMMPY=(INDXA(COMLYN,COMLEN,'CHPY').GT.0)  !! Q-Chem keyword to interface with Python
   IF(QCHARMMPY) THEN
      WRITE(OUTU,22) &
               'CHARMM Python Interfacer Selected'  !! Print message of interfacing with Python
    ENDIF

    !!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


    !
    ! rest of the code
    !

END SUBROUTINE GUKINI
    
    !
    ! rest of the code
    !
```
2. <u>Pass the box dimensions to the fortran90 unit opened for the Python package input file.</u>
This will introduce a section for box dimensions in the file, like the example below, where the 10.0 represents the side of a cubic box.
```console
$box
10.0  0.0  0.0
0.0  10.0  0.0
0.0  0.0  10.0
$end
```
Corresponding modifications in the **gukini.F90** file are as follows:
```fortran
!!source/gukint/gukini.F90

!
! rest of the code
!

SUBROUTINE QCHEM(E,DX,DY,DZ,CGX,AMASSX,IACX,NDD1,DD1,QSECD,IUPT,JUPT)

!
! rest of the code
!

1150     CONTINUE
!
       WRITE(OMO,'(A)') '   '
       WRITE(OMO,'(A)') '@@@'
       WRITE(OMO,'(A)') '   '
       REWIND(IMO)

    ENDIF


    !!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

    IF(QCHARMMPY) THEN
      write(omo,'(A)')'$box'
      write(omo,'(F12.7,A,F12.7,A,F12.7)') XUCELL(1), '  ', 0.0, '  ', 0.0
      write(omo,'(F12.7,A,F12.7,A,F12.7)') 0.0, '  ', XUCELL(2), '  ', 0.0
      write(omo,'(F12.7,A,F12.7,A,F12.7)') 0.0, '  ', 0.0, '  ', XUCELL(3)
      write(omo,'(A)')'$end'
    ENDIF

    !!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

!
! rest of the code
!

END SUBROUTINE QCHEM
```
3. <u>Passing the MM region coordinates and charges to the file opened for the Python package input.</u>
For this purpose we are not going to modify the **gukini.F90** file. The current code will introduce a section to the file, as shown below, where the columns represent the x coordinate, y coordinate, z coordinate, and charge of the MM region atoms. 
```console
$external_charges
0.0  0.0  0.0  0.0
1.0  1.0  1.0  1.0
$end
```
4. Make a copy of the charge array (cg_dum) from the .psf file to be passed to the file opened for the Python package input.
*(This because the QM/MM algorithm zero out the charges of the atoms in the QM region in the original array, so we need to make a copy of the charges from the .psf file to be passed to the Python package)*

```fortran
!!source/gukint/gukini.F90

!
! rest of the code
!

SUBROUTINE COPSEL(ISLCT,QQINP)

!
! rest of the code
!

999    CONTINUE
       ENDIF
       ENDDO


    !!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

    IF(QGMREM) THEN
      DO I=1,NATOM
        cg_dum(I)=CG(I)  ! Make a copy of the charge array to be passed to the Python package
      ENDDO
    ENDIF

    !!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


!
! rest of the code
!

END SUBROUTINE COPSEL

```
5. <u>Passing the QM region coordinates and charges to the file opened for the Python package input.</u>
This will introduce a section to the file, as shown below, where the columns represent the x coordinate, y coordinate, z coordinate, and charge of the QM region atoms. 
```console
$molecule
0.0  0.0  0.0  0.0 C
1.0  1.0  1.0  1.0 H
$end
```
The corresponding modifications in the **gukini.F90** file are as follows:
```fortran
!!source/gukint/gukini.F90

!
! rest of the code
!

SUBROUTINE QCHEM(E,DX,DY,DZ,CGX,AMASSX,IACX,NDD1,DD1,QSECD,IUPT,JUPT)

!
! rest of the code
!

!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

    IF(QCHARMMPY) THEN
      WRITE(OMO,'(A)')'$molecule'
      DO I=1, NATOM
        IF((IGMSEL(I).EQ.1).OR.(IGMSEL(I).EQ.2))THEN
          WRITE(OMO,'(3F20.10,A)')X(I),Y(I),Z(I),cg_dum(I), ELE
        ENDIF
      ENDDO
      WRITE(OMO,'(A)')'$end'
    ELSE

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


    IF(STRFIN(LINE,'$molecule').GT.0)THEN
      READ(IMO,'(A)',END=100)LINE
      !WRITE(OMO,'(A)')LINE(1:ILEN)
!     WRITE(*,*)'testing molecule - ',QRPMINP,LINE
      IF(QRPMINP) THEN 
        IF(SGROUP(1:M).EQ."1") THEN 
          READ(70,'(A)',END=100)LINE
        ENDIF 
        IF(SGROUP(1:M).EQ."2") THEN 
          READ(71,'(A)',END=100)LINE
        ENDIF 
        WRITE(OMO,'(A)')LINE(1:ILEN)
      ELSE 
        WRITE(OMO,'(A)')LINE(1:ILEN)
      ENDIF 
      !WRITE(*,'(A)')LINE(1:ILEN)
      QMATOMS=0
      IPT=MMATOMS
      DO I=1, NATOM
        IF((IGMSEL(I).EQ.1).OR.(IGMSEL(I).EQ.2))THEN
          ELE='      '
          LQINIGM=QINIGM
          CALL FINDEL(ATCT(IACX(I)),AMASSX(I),I,ELE,T,LQINIGM)
          IF(ELE(1:3).EQ.'QQH')ELE(1:6)=' H    '
          IF(ELE(1:3).EQ.'qqh')ELE(1:6)=' H    '
          IF (QQEWALD) THEN
          numqcatmtyp=size(qcffatmtyp)
!         write(*,*)'size = ',loc 
!         qcffatmtyp(0)=4
!         qcffatmtyp(1)=75
!         qcffatmtyp(2)=79
!         qcffatmtyp(3)=163
            do j=1,nbond
              qcbonds(j)=0
              tmpqcb(j)=0
            enddo 
            do j=1,nbond
              if(I .eq. IB(J)) then 
                qcbonds(j)=JB(J)
              elseif(I .eq. JB(J)) then
                qcbonds(j)=IB(J)
              else
                qcbonds(j)=0
              endif
            enddo
            l=0
            do k=1,nbond
              if(qcbonds(k).gt.0) then
                l=l+1
                tmpqcb(l)=qcbonds(k)
                qcbonds(k)=0
              endif
            enddo
            locloop1: do l=0,numqcatmtyp-1
              loc=abs(qcffatmtyp(l)-IAC(I)) 
              if(loc .eq. 0) then
                loc=l
                exit locloop1
              endif 
            enddo locloop1
            if(ELE(2:2) .eq. 'H') then 
              WRITE(OMO,'(A6,3F20.10,5I6)')ELE,X(I),Y(I),Z(I),-loc-1,tmpqcb(1),0,0,0
            else 
              WRITE(OMO,'(A6,3F20.10,5I6)')ELE,X(I),Y(I),Z(I),-loc-1,tmpqcb(1),tmpqcb(2),tmpqcb(3),tmpqcb(4)
            endif 


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

          ELSE IF (QCHARMMPY) THEN
            WRITE(OMO,'(3F20.10,F16.8,A6)') X(I),Y(I),Z(I),cg_dum(I), ELE 
            ! /* RAAFIK 2023 */
          ELSE
            WRITE(OMO,'(A6,3F20.10)')ELE,X(I),Y(I),Z(I)
          ENDIF

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


          IPT=IPT+1
          MAPQM(IPT)=I
          QMATOMS=QMATOMS+1
        ELSE
          IF(IGMSEL(I).EQ.0 .OR. IGMSEL(I).EQ.5)THEN
            IF (QQEWALD) THEN
              ELE='      '
              LQINIGM=QINIGM
              CALL FINDEL(ATCT(IACX(I)),AMASSX(I),I,ELE,T,LQINIGM)
              numqcatmtyp=size(qcffatmtyp)
              IF (QQEWALD) THEN
                do j=1,nbond
                  qcbonds(j)=0
                  tmpqcb(j)=0
                enddo
                do j=1,nbond
                  if(I .eq. IB(J)) then
                    qcbonds(j)=JB(J)
                    elseif(I .eq. JB(J)) then
                    qcbonds(j)=IB(J)
                  else
                    qcbonds(j)=0
                  endif
                enddo
                l=0
                do k=1,nbond
                  if(qcbonds(k).gt.0) then
                    l=l+1
                    tmpqcb(l)=qcbonds(k)
                    qcbonds(k)=0
                  endif
                enddo
              endif
              locloop2: do l=0,numqcatmtyp-1
                loc=abs(qcffatmtyp(l)-IAC(I)) 
                if(loc .eq. 0) then
                  loc=l
                  exit locloop2
                endif 
              enddo locloop2
              if(ELE(2:2) .eq. 'H') then 
                WRITE(OMO,'(A6,3F20.10,5I6)')ELE,X(I),Y(I),Z(I),-loc-1,tmpqcb(1),0,0,0
              else 
                WRITE(OMO,'(A6,3F20.10,5I6)')ELE,X(I),Y(I),Z(I),-loc-1,tmpqcb(1),tmpqcb(2),tmpqcb(3),tmpqcb(4)
              endif 
            ENDIF
            MAPMM(JPT)=I
            JPT=JPT+1
          ENDIF
        ENDIF
      ENDDO
    QINIGM=.FALSE.
    ENDIF
    ENDIF

ENDIF

!
! rest of the code
!

END SUBROUTINE QCHEM
```
6. <u>Set Environment Variables to Control Arguments to be Passed to Python.</u>
This will introduce a section to the **gukini.F90** file to set the environment variables to control arguments to be passed to Python. The corresponding modifications in the **gukini.F90** file are as follows:
```fortran
!!source/gukint/gukini.F90


6. <u>Call the Python package with system call in fortran90.</u>
This will introduce a section to the **gukini.F90** file to call the Python package with the system call. The corresponding modifications in the **gukini.F90** file are as follows:
```fortran
!!source/gukint/gukini.F90

!
! rest of the code
!

SUBROUTINE QCHEM(E,DX,DY,DZ,CGX,AMASSX,IACX,NDD1,DD1,QSECD,IUPT,JUPT)

!
! rest of the code
!

          IF(QQCSCRATCH) THEN                ! QSCRATCH TEST 
          IF(QNOGU) THEN
             CALL SYSTEM('$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' ' &
                  //FILOUT(1:LO)//' '//QCSCRATCH)
          ELSE         
             IF(QRESTART) THEN 
                CALL SYSTEM('$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' ' &
                     //FILOUT(1:LO)//' '//QCSCRATCH)
                QRESTART=.FALSE.
             ELSEIF (QSAVORB) THEN 
                CALL SYSTEM('$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' ' &
                     //FILOUT(1:LO)//' '//QCSCRATCH)
                QSAVORB=.FALSE.
             ELSE 
                CALL SYSTEM('$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' ' &
                     //FILOUT(1:LO))
             ENDIF
          ENDIF
          ELSE                             ! QSCRATCH TEST 
          IF(QNOGU) THEN
             CALL SYSTEM('$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' ' &
                  //FILOUT(1:LO)//' '//'SAVE')
          ELSE         
             IF(QRESTART) THEN 
                CALL SYSTEM('$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' ' & 
                     //FILOUT(1:LO)//' '//'SAVE')
                QRESTART=.FALSE.
             ELSEIF (QSAVORB) THEN 
                CALL SYSTEM('$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' ' & 
                     //FILOUT(1:LO)//' '//'SAVE')
                QSAVORB=.FALSE.


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

                ELSEIF (QCCHARMMPY) THEN
                  CALL SYSTEM('python -m $PY_PACKAGE --qm_charge $QM_CHARGE --qm_multiplicity $QM_MULTIPLICITY --inp_file $CHARMM2PY --out_file $PY2CHARMM_FILE')


             ELSE 
                CALL SYSTEM( &
                     '$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' '//FILOUT(1:LO))
             ENDIF

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


          ENDIF
          ENDIF                            ! QSCRATCH TEST 
       ENDIF
#elif KEY_PARALLEL==0
       ! HLW - Dec. 2007
       ! Fix the serial CHARMM version to work with QCPARA and allow 
       ! parallel execution of Q-Chem
       IF(QQCSCRATCH) THEN                ! QSCRATCH TEST 
       IF(QNOGU) THEN
          CALL SYSTEM('$QCHEMEXE '//FILIN(1:LI)//' '//FILOUT(1:LO)// &
               ' '//QCSCRATCH)
       ELSE
          IF(QRESTART) THEN 
             CALL SYSTEM('$QCHEMEXE '//FILIN(1:LI)//' '//FILOUT(1:LO)// &
                  ' '//QCSCRATCH)
             QRESTART=.FALSE.
          ELSEIF (QSAVORB) THEN 
             CALL SYSTEM('$QCHEMEXE '//FILIN(1:LI)//' '//FILOUT(1:LO)// &
                  ' '//QCSCRATCH)
             QSAVORB=.FALSE.
          ELSE 
             CALL SYSTEM('$QCHEMEXE '//FILIN(1:LI)//' '//FILOUT(1:LO))
          ENDIF
       ENDIF
       ELSE                             ! QSCRATCH TEST 
       IF(QNOGU) THEN
          CALL SYSTEM('$QCHEMEXE '//FILIN(1:LI)//' '//FILOUT(1:LO)// & 
               ' '//'SAVE')
       ELSE
          IF(QRESTART) THEN 
             CALL SYSTEM('$QCHEMEXE '//FILIN(1:LI)//' '//FILOUT(1:LO)// &
                  ' '//'SAVE')
             QRESTART=.FALSE.
          ELSEIF (QSAVORB) THEN 
             CALL SYSTEM('$QCHEMEXE '//FILIN(1:LI)//' '//FILOUT(1:LO)// &
                  ' '//'SAVE')
             QSAVORB=.FALSE.


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

                ELSEIF (QCCHARMMPY) THEN
                  CALL SYSTEM('python -m $PY_PACKAGE --qm_charge $QM_CHARGE --qm_multiplicity $QM_MULTIPLICITY --inp_file '//FILI(1:LI)//' --out_file '//FILOUT(1:LO))


             ELSE 
                CALL SYSTEM( &
                     '$QCHEMEXE '//NPOPT//' '//NP//FILIN(1:LI)//' '//FILOUT(1:LO))
             ENDIF

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


       ENDIF
       ENDIF                            ! QSCRATCH TEST 
#endif 

!
! rest of the code
!

END SUBROUTINE QCHEM
```
