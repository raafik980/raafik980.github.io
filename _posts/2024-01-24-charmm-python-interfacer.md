---
title: "Machine Learning Assisted QM/MM Free Energy Simulations: A Quick Way to Set up an Interface to Python in CHARMM"
author:
date: 2024-01-24  
categories: [Software, CHARMM]
tags: [machine learning, deep learning, qm/mm simulations, python, fortran]
render_with_liquid: true
pin: false
toc: true
math: true
---

> *DISCLAIMER: The content in this article is for demonstration purposes only and may contain errors and technical inaccuracies.*

> *DISCLAIMER: The code modifications might not be looking clean as I have kept the original code structure as it is and added the new code snippets to demonstrate the modifications. Look for 'Added Section for Q-Chem-CHARMM interfacing with Python' coment in the code to see the new code snippets.*

<br>
<br>

## References

- We use the CHARMM MD simulation program which is available [here](https://academiccharmm.org/program)
  -  Brooks, B. R.; Brooks, C. L., III; Mackerell, A. D., Jr; Nilsson, L.; Petrella, R. J.; Roux, B.; Won, Y.; Archontis, G.; Bartels, C.; Boresch, S.; Caflisch, A.; Caves, L.; Cui, Q.; Dinner, A. R.; Feig, M.; Fischer, S.; Gao, J.; Hodoscek, M.; Im, W.; Kuczera, K.; Lazaridis, T.; Ma, J.; Ovchinnikov, V.; Paci, E.; Pastor, R. W.; Post, C. B.; Pu, J. Z.; Schaefer, M.; Tidor, B.; Venable, R. M.; Woodcock, H. L.; Wu, X.; Yang, W.; York, D. M.; Karplus, M. [CHARMM: The Biomolecular Simulation Program.](https://doi.org/10.1002/jcc.21287) J. Comput. Chem. 2009, 30 (10), 1545–1614.

- We use QM/MM Interfacer module to generate QM/MM simulation template scripts for our system of interest. The module is currently available with [CHARMM-GUI](https://www.charmm-gui.org/) platform.
  - *[Manuscript under preperation, 2024]*


## I. Introduction: Interfaces to QM Programs in CHARMM

CHARMM houses code (**path_to_charmm/source/gukint/gukini.F90**) to interface with several quantum mechanical (QM) programs, including Gaussian, GAMESS, and Q-Chem. These interfaces allow CHARMM to perform QM/MM simulations, where the QM region is treated with a high-level QM method, and the MM region is treated as point charge (electrostatic embedding). These implementations can leverage the CHARMM capabilities, such as applying restraints, selection algebra, link-atom implementation, umbrella sampling, and various analysis modules. Depending on the atomic selection choices mentioned in the CHARMM running script, the simulation can represent the atoms with all QM, all MM, or a hybrid of QM and MM.
This article demonstrates a quick way to modify the existing implementation to interface with a Python package that could use machine learning (ML) potentials to calculate QM/MM energies and forces.

## II. CHARMM Q-Chem Interface for QM/MM Simulations
Interface to Q-Chem can be considered as one of the decently documented interfaces and provides several key options to control specifications of the QM/MM simulations. The interface is implemented in the file **gukini.F90** (*--with-qchem* key is required to include this piece of code to be included during compilation). The interface is called from the CHARMM script using the following command:

```console
!!CHARMM Script
!!Here the QMATOMS is the selection of atoms that will be treated with QM method

QCHEm REMO SELE QMATOMS SHOW END

!!CHARMM Script
```

## III. Add a New Option to Interface with Python in the Current Q-Chem Interface
* Add the new **QCHEm** option key to call in the CHARMM script to activate the interface with Python for QM/MM simulations (such as ML-assisted QM/MM simulations).The new option is added as **CHPYthon** with a new variable **QCHARMMPY** in the **gamess_ltm.F90** file in the **(<path_to_charmm>/source/ltm/gamess_ltm.F90)**.

```fortran
!!source/ltm/gamess_ltm.F90

  !
  ! rest of the code
  !
  
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

!
! rest of the code
!
```

## IV. How to Initiate Custom Arrays to Extract Information from CHARMM Topology (.PSF)
In certain cases, the interface with Python may require the information from the CHARMM topology file (.psf) to calculate the QM/MM energies and forces which may not be readily available within the current gukin.F90 interface. So it would be beneficial to have copy of the information from the .psf file to be used to pass information to the Python script. The following code snippet demonstrates how to initiate a custom array to extract the information from the CHARMM topology file (.psf) by introducing custom variables in the **psf_ltm.F90** file in the **(<path_to_charmm>/source/ltm/psf_ltm.F90)**.

The code below demonstrates how to introduce a new array variable (later to store a copy of the charge array from the .psf file to be passed to the Python package).

```fortran
!!source/ltm/psf_ltm.F90

  !
  ! rest of the code
  !

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

  !
  ! rest of the code
  !
```

## V. How Interface Passes Information to and from Python Package

#### 1. <u>Consolidate the information required from CHARMM into a file (e.g., **charmm2py.inp**).</u>
This file will contain the information required to calculate/predict the QM/MM energies and forces using the Python script (like an ML potential). In this demonstration example, the following information is included in the **charmm2py.inp** file:

- Simulation box dimensions (for periodic boundary conditions, long-range electrostatics, equivariance transformations required for ML potentials, etc.)
- MM region atomic coordinates
- MM region atomic charges
- QM region atomic coordinates
- QM region atomic charges
-  QM region atom types
<br>

#### 2. <u>Define environment variables to be used in the CHARMM script to control arguments to be passed to Python.</u>

In this demonstration example, the following environment variables are added in the **gukini.F90** code, assuming our python package (PY_PACKAGE) CHARMM2PYINP, and PY2CHARMMOUT:

- **PY_PACKAGE**: Path to the Python package (e.g., mlpotential) *[It will be convenient if the package is installed in a conda environment]*
- **CHARMM2PYINP**: Path to the input file for the Python package (e.g., charmm2py.inp)
- **PY2CHARMMOUT**: Path to the output file from the Python package (e.g., py2charmm.out)
<br>

#### 3. <u>The QCHEMCNT environment file to introduce total charge and multiplicity (also any general additional information) of the QM region.</u>

The Qchem-CHARMM expects a control file associated with the QM region where we add the charge, multiplicity, and any additional specifications associated with the calculation. This file is introduced in the CHARMM script via an environment variable **QCHEMCNT**. The file mentioned in the **QCHEMCNT** environment variable will contain the following information:

```console
$comment
any comments or additional information.
In the below $molecule section the first value is total charge of QM region and the second value is multiplicity of the QM region.
$end
$molecule
1 1
$end
```

It is possible to further streamline the workflow by introducing a new environment variable within the gukini.F90 code to control the arguments to be passed to the Python package. However, in this demonstration example, we are using the existing environment variable **QCHEMCNT** associated with a file (e.g., qchemcnt.inp) to control the arguments to be passed to the Python package.
<br>

#### 4. <u>Call the Python package with **system** call in fortran90.</u>
An example of the system call:

```bash
python -m $PY_PACKAGE --inp_file $CHARMM2PYINP --out_file $PY2CHARMMOUT
```

Instead of a plain text file, one can pass the information as binary, fifo, or any other suitable format considering performance and security. In this example, we are passing the information as a plain text file.
<br>

#### 5. <u>Read the output from the Python package and update the QM/MM energies and forces in CHARMM.</u>
In this demonstration example, the **py2charmm.out** file will contain the QM/MM energies and forces calculated/ML-predicted by the Python package. The **gukini.F90** code will read the **py2charmm.out** file and update the QM/MM energies and forces in CHARMM.

## VI. gukini.F90 Code Modification to Interface with Python
The following code snippets demonstrates how to modify the **gukini.F90** file to interface with a Python package (e.g., mlpotential) to calculate the QM/MM energies and forces. We will go section by section to understand the modifications.

#### 1. <u>Initialization of the new QCHARMMPY option along with other Q-Chem options in the **gukini.F90** file.</u>

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

#### 2. <u>Pass the box dimensions to the fortran90 unit opened for the Python package input file.</u>
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

!
! rest of the code
!
```

#### 3. <u>Passing the MM region coordinates and charges to the file opened for the Python package input.</u>

For this purpose we are not going to modify the **gukini.F90** file. The current code will introduce a section to the file, as shown below, where the columns represent the x coordinate, y coordinate, z coordinate, and charge of the MM region atoms. 

```console
$external_charges
0.0  0.0  0.0  0.0
1.0  1.0  1.0  1.0
$end
```
#### 4. <u>Make a copy of the charge array (cg_dum) from the .psf file to be passed to the file opened for the Python package input.</u>
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

!
! rest of the code
!


```

#### 5. <u>Passing the QM region coordinates and charges to the file opened for the Python package input.</u>

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

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


          ELSE
            WRITE(OMO,'(A6,3F20.10)')ELE,X(I),Y(I),Z(I)
          ENDIF


!
! rest of the code
!

END SUBROUTINE QCHEM

!
! rest of the code
!

```

#### 6. <u>Set Environment Variables to Control Arguments to be Passed to Python.</u>

This will introduce a section to the **gukini.F90** file to set the environment variables to control arguments to be passed to Python. The corresponding modifications in the **gukini.F90** file are as follows:

```fortran
!!source/gukint/gukini.F90

!
! rest of the code
!

SUBROUTINE QCHEM(E,DX,DY,DZ,CGX,AMASSX,IACX,NDD1,DD1,QSECD,IUPT,JUPT)

!
! rest of the code
!

#if KEY_PERT == 1
    IF(QPERT) THEN
       IF (QMSTATE .EQ. 0) THEN
          call get_environment_variable('SAINP', filcnt, lc)
          IF(LC.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No input specified.')
       ELSE IF (QMSTATE .EQ. 1) THEN
          CALL get_environment_variable('SBINP', FILCNT, LC)
          IF(LC.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No input specified.')
       ENDIF
    ELSE
#endif /* KEY_PERT */
       
       CALL get_environment_variable("QCHEMCNT", FILCNT, LC)
       IF(LC.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No input specified.')

#if KEY_PERT == 1
    ENDIF

    IF(QPERT) THEN
       IF(QMSTATE .EQ. 0) THEN
          CALL GET_ENVIRONMENT_VARIABLE("STATEAINP", FILIN, LI)
          IF(LI.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No input specified.')
       ELSE IF(QMSTATE .EQ. 1) THEN
          CALL GET_ENVIRONMENT_VARIABLE("STATEBINP", FILIN, LI)
          IF(LI.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No input specified.')
       ENDIF
    ELSE
#endif /* KEY_PERT */

       FILIN=''


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

IF(QCHARMMPY) THEN
        CALL GET_ENVIRONMENT_VARIABLE("CHARMM2PYINP", FILIN, LI)
        IF(LI.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No input specified.')

        ELSE

       CALL GET_ENVIRONMENT_VARIABLE("QCHEMINP", FILIN, LI)
       IF(LI.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No input specified.')

       ENDIF

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


#if KEY_PERT == 1
    ENDIF


    IF (QPERT) THEN
       IF (QMSTATE .EQ. 0) THEN
          CALL GET_ENVIRONMENT_VARIABLE("STATEAOUT", FILOUT, LO)
          IF(LO.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No output specified.')
       ELSE IF (QMSTATE .EQ. 1) THEN
          CALL GET_ENVIRONMENT_VARIABLE("STATEBOUT", FILOUT, LO)
          IF(LO.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No output specified.')
       ENDIF
    ELSE
#endif /* KEY_PERT */


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!! 

    IF(QCHARMMPY) THEN
        CALL GET_ENVIRONMENT_VARIABLE("PY2CHARMMOUT", FILOUT, LO)
        IF(LO.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No output specified.')

        ELSE
       
       CALL GET_ENVIRONMENT_VARIABLE("QCHEMOUT", FILOUT, LO)
       IF(LO.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No output specified.')

       ENDIF

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


#if KEY_PERT == 1
    ENDIF
#endif


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

IF(QCHARMMPY) THEN
    CALL GET_ENVIRONMENT_VARIABLE("PY_PACKAGE", FILEXE, LX)
    IF(LX.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No Python package specified.')

    ELSE

    CALL GET_ENVIRONMENT_VARIABLE("QCHEMEXE", FILEXE, LX)
    IF(LX.EQ.0)CALL WRNDIE(-5,'<QCHEM>','No Q-chem specified.')

    ENDIF

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


    !

    IMO=90  ! Input file (from outside program) reading fortran unit
    OMO=91  ! Output file (from outside program) reading fortran unit

!
! rest of the code
!

END SUBROUTINE QCHEM

!
! rest of the code
!

```

With this, we will have our finalized input file containing all the information needed for the Python program (for ML potential) to calculate the QM/MM energies and forces. The input file will be passed to the Python package using the system call in fortran90. The output file from the Python package will be read, and the QM/MM energies and forces will be updated in CHARMM.
A sample input file for the Python package (e.g., mlpotential) is as follows:

```console
$box
10.0  0.0  0.0
0.0  10.0  0.0
0.0  0.0  10.0
$end
$external_charges
0.0  0.0  0.0  0.0
1.0  1.0  1.0  1.0
$end
$comment
any comments or additional information.
In the below $molecule section the first value is total charge of QM region and the second value is multiplicity of the QM region.
$end
$molecule
1 1
0.0  0.0  0.0  0.0 C
1.0  1.0  1.0  1.0 H
$end
$rem
$end
$ewald
0 
0
$end
```

#### 7. <u>Call the Python package with system call in fortran90.</u>

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

            ELSEIF (QCHARMMPY) THEN
              CALL SYSTEM('python -m $PY_PACKAGE --inp_file '//FILI(1:LI)//' --out_file '//FILOUT(1:LO))


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

                ELSEIF (QCHARMMPY) THEN
                  CALL SYSTEM('python -m $PY_PACKAGE --inp_file '//FILI(1:LI)//' --out_file '//FILOUT(1:LO))

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

!
! rest of the code
!
```

<br>

#### 8. <u>Read the output from the Python package and update the QM/MM energy in CHARMM.</u>

Here we assume the fist line of the output file from the Python package contains the QM/MM energy in hartrees. The corresponding modifications in the **gukini.F90** file are as follows:

```fortran
!!source/gukint/gukini.F90

!
! rest of the code
!

SUBROUTINE QCHEM(E,DX,DY,DZ,CGX,AMASSX,IACX,NDD1,DD1,QSECD,IUPT,JUPT)

!
! rest of the code
!

       !------------------------------------------------------
       !---- Get the energy and forces from Q-chem output ----
       !------------------------------------------------------

       E=ZERO
       T=ZERO
       CC=ZERO

       open(omo,FILE=filout(1:lo),status='old')


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

20   IF (QCHARMMPY) THEN
                  read(omo, *) e
     ELSE  

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!! 


     read(omo,'(a)',end=200) line

       IF(QMP2.or.QCCSD.or.QSOSMP2.or.QMOSMP2.or.QSCSMP2) THEN 
          i=index(line,'The QM part of the Energy is')
          if (i.gt.0) then
             read(line,*)tmp1,tmp2,tmp2,tmp4,tmp5,tmp6,tmp7,e
          endif
       ELSE If(QLMP2) THEN
          i=index(line,'TRIM MP2           total energy')
          if (i.gt.0) then
             read(line,*)tmp1,tmp2,tmp3,tmp4,tmp5,e
          endif
       ELSE IF(QRIMP2) THEN 
          i=index(line,'RI-MP2 TOTAL ENERGY')
          if (i.gt.0) then
             read(line,*)tmp1,tmp2,tmp3,tmp4,e
          endif
       ELSE IF(QCIS) THEN
          i=index(line,'Total energy for state   ')
          if (i.gt.0) then
             tmp1=line(29:30)
             if (tmp1(2:3) .eq. ncis) then
                read(line,*)tmp1,tmp2,tmp3,tmp4,tmp5,e
             endif
          endif
       ELSE
          i=index(line,'criterion')
          if (i.gt.0) then
             read(line,*)ix, e
          endif
       ENDIF


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

      endif

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

!
! rest of the code
!

END SUBROUTINE QCHEM

!
! rest of the code
!
```

<br>

#### 9. <u>Read the output from the Python package and update the MM and QM atomic forces in CHARMM.</u>

Here, we assume the next lines of the output file (the first line is energy) from the Python package contain rows of the atomic gradients in hartrees/bohr (dE/dx dE/dy dE/dz). The initial atomic gradients correspond to the MM region, and the following atomic gradients correspond to the QM region. The corresponding modifications in the **gukini.F90** file are as follows:

```fortran
!!source/gukint/gukini.F90

!
! rest of the code
!

SUBROUTINE QCHEM(E,DX,DY,DZ,CGX,AMASSX,IACX,NDD1,DD1,QSECD,IUPT,JUPT)

!
! rest of the code
!

    !-----------------------------------------------------------
    !    GET QM AND EXTERNAL FIELD DERIVATIVES and CHARGES
    !-----------------------------------------------------------
    TMPVAR=MMATOMS
    IF(QQCRP) THEN
       open(imo,FILE=PWDQC(1:LPWD)//'/rpath'//SGROUP(1:M)// &
            '/efield.dat',status='unknown')


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

      ELSEIF(.not.QCHARMMPY) THEN
       open(imo,FILE='efield.dat',status='unknown')
    ENDIF

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


    !------------------------------------------------------

    !     CONVERT FROM A.U. TO KCAL/MOL


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

    IF(QCHARMMMPY) THEN
      close(omo)
      open(omo,FILE=FILOUT(1:LO),status='unknown')
      read(omo, *)  ! Skip the first line, which contains the QM/MM energy
      DO I=1,MMATOMS

         read(omo,'(a)',end=300)line
         read(line,*)DUMX,DUMY,DUMZ
         !read(line,*,iostat=iostat)DUMX,DUMY,DUMZ
         IPT=MAPQM(I)
         IF (IGMSEL(IPT).EQ.0) THEN   ! need this condition if file format is different
         DX(IPT) = DX(IPT) +  (DUMX*TOKCAL/BOHRR) !*(-CGX(IPT))
         DY(IPT) = DY(IPT) +  (DUMY*TOKCAL/BOHRR) !*(-CGX(IPT))
         DZ(IPT) = DZ(IPT) +  (DUMZ*TOKCAL/BOHRR) !*(-CGX(IPT))
         !print *, "MM Force updated.", IPT, DUMX,DUMY,DUMZ  ! to test
         ENDIF
      ENDDO
          ! Print a message for the true case
    WRITE(outu, 22) 'Python MM Forces Updated'
    
      DO I=MMATOMS+1,MMATOMS+QMATOMS
         read(omo,'(a)',end=300)line
         read(line,*)DUMX,DUMY,DUMZ
         IPT=MAPQM(I)
         IF ((IGMSEL(IPT).EQ.1) .OR. (IGMSEL(IPT).EQ.2) ) THEN
          DX(IPT) = DX(IPT) + DUMX*TOKCAL/BOHRR
          DY(IPT) = DY(IPT) + DUMY*TOKCAL/BOHRR
          DZ(IPT) = DZ(IPT) + DUMZ*TOKCAL/BOHRR
          !print *, "QM Force updated.", IPT, DUMX,DUMY,DUMZ  ! to test
         ENDIF
      ENDDO
      WRITE(outu, 22) 'Python QM Forces Updated'
   ELSE

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!



    DO I=1,MMATOMS
       read(imo,'(a)',end=300)line
       read(line,*)DUMX,DUMY,DUMZ
       IPT=MAPQM(I)
       !hlw_080705
       IF (IGMSEL(IPT).EQ.0) THEN
! for DIV - guanhua_puja_QC_UW1212
        IF (NDIV(IPT).EQ.0) THEN
          DX(IPT) = DX(IPT) + (DUMX*TOKCAL/BOHRR)*(-CGX(IPT))
          DY(IPT) = DY(IPT) + (DUMY*TOKCAL/BOHRR)*(-CGX(IPT))
          DZ(IPT) = DZ(IPT) + (DUMZ*TOKCAL/BOHRR)*(-CGX(IPT))
        ELSE IF (NDIV(IPT).EQ.1) THEN
          DX(IPT) = DX(IPT) + (DUMX*TOKCAL/BOHRR)*(-ZLHOST(IPT))
          DY(IPT) = DY(IPT) + (DUMY*TOKCAL/BOHRR)*(-ZLHOST(IPT))
          DZ(IPT) = DZ(IPT) + (DUMZ*TOKCAL/BOHRR)*(-ZLHOST(IPT))
        ENDIF
!
       ENDIF
       !hlw_080705
    ENDDO
    DO I=MMATOMS+1,MMATOMS+QMATOMS
       read(imo,'(a)',end=300)line
       read(line,*,iostat=iostat)DUMX,DUMY,DUMZ
! LNI format fix, in an attempt to cope with lines such as this:
!  13.97408441879924545503-116.04812193725601332517 -31.62032204259966761128
       if(iostat > 0) read(line,'(3F25.0)')DUMX,DUMY,DUMZ
       IPT=MAPQM(I)
       DX(IPT) = DX(IPT) + DUMX*TOKCAL/BOHRR
       DY(IPT) = DY(IPT) + DUMY*TOKCAL/BOHRR
       DZ(IPT) = DZ(IPT) + DUMZ*TOKCAL/BOHRR
    ENDDO


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

   ENDIF 

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


300 continue

!
! rest of the code
!

END SUBROUTINE QCHEM

!
! rest of the code
!
```

With these steps, we have  interfaced CHARMM0(--with-qchem) and Python. CHARMM generates a input file for the Python package, which calculates the QM/MM energies and forces. The output file from the Python package is read and the QM/MM energies and forces are updated in CHARMM.

## VII. How to run the interfaced CHARMM and Python package

#### 1. <u>Compile CHARMM with the interfaced code.</u>
Download the CHARMM source code from the CHARMM website. Replace the **gukini.F90** file in the **source/gukint** directory with the modified **gukini.F90** file. Compile CHARMM with the following command:

```bash
cd <path_to_charmm>
mkdir charmm_mod_build
cd charmm_mod_build
../configure --prefix=<path_to_charmm>/charmm_mod_install --with-qchem
make install

#Now the CHARMM executable with the interfaced code is in the charmm_mod_install/bin directory
```

#### 2. <U>Generate CHARMM script to run QM/MM simulations with QM/MM Interfacer in CHARMM-GUI</u>

QM/MM Interfacer in CHARMM-GUI provides a GUI based platform to generate CHARMM input files for QM/MM simulations. Including system preparation, reparametrization, QM  region selection, etc.

#### 3. <u>Modify the CHARMM script to introduce new options for interfacing with Python.</u>

The CHARMM script generated by CHARMM-GUI can be modified to include new options for interfacing with Python. The following is an example of a CHARMM script to run the modified CHARMM with the interfaced code:

```console
!!<path_to_charmm-gui-generated_directory/charmm/step5.production.inp
!!
!!Part of CHARMM script to show the modification made to the QM/MM Interfacer generated CHARMM script

* GENERATED BY CHARMM-GUI (http://www.charmm-gui.org) v3.7 on Dec, 19. 2023. JOBID=0302931610
* INPUT FILE FOR NVT DYNAMICS OF SOLVATED GLOBULAR PROTEIN
*

DIMENS CHSIZE 5000000 MAXRES 3000000

!
! rest of the script
!


!!!!Start: Added Section for Q-Chem-CHARMM interfacing with Python !!!!

if @qmpackage .eq. python then
    if ?python .eq. 1 then

        set QCHEMCNT qchemcnt.inp         ! Name of the control file for total charge and multiplicity
        set PY_PACKAGE mlpotential        ! Name of the Python package
        set CHARMM2PYINP charmm2py.inp    ! Name of the input file for the Python package
        set PY2CHARMMOUT py2charmm.out    ! Name of the output file from the Python package

        QCHEm CHPYthon REMO SELE ( QMS .or. type QQH* ) END

!!!!End: Added Section for Q-Chem-CHARMM interfacing with Python !!!!


energy

!
! rest of the script
!

stop

```

#### 4. <u>Run the modified CHARMM script.</u>

A safe way will be to run the script in a conda environment with the required Python package and its dependencies. Edit the README file in the <path_to_charmm-gui-generated_directory/charmm> directory to include the following line at the beginning of the file:

```bash

!/bin/bash
conda activate <conda_env_name>

#
# Rest of the script to set of specification and run CHARMM
#


```

## Conclusion

This article demonstrates a quick and dirty way to introduce an interface to custom external programs such as Python packages from CHARMM. Although it is possible to introduce new modules to CHARMM without major restructuring of the program, being a single executable, it would be challenging for beginners to understand the CHARMM source code and make the necessary modifications. This article provides a step-by-step guide to introduce a new interface to CHARMM, which can be used as a starting point for more complex interfaces. This article also helps to understand certain subroutines in the CHARMM source code, which can be used as a reference for future modifications.




