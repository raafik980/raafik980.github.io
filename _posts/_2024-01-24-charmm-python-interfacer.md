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
This article demonstrates a quick way to modify the existing implementation to interface with a Python script that could use machine learning (ML) potentials to calculate QM/MM energies and forces.

## CHARMM Q-Chem Interface for QM/MM Simulations
Interface to Q-Chem can be considered as one of the decently documented interfaces and provides several key options to control specifications of the QM/MM simulations. The interface is implemented in the file **gukini.F90** (*--with-qchem* key is required to include this piece of code to be included during compilation). The interface is called from the CHARMM script using the following command:

```console
!!CHARMM Script
!!Here the QMATOMS is the selection of atoms that will be treated with QM method

QCHEm REMO SELE QMATOMS SHOW END

!!CHARMM Script
```

## Add a New Option to Interface with Python in the Current Q-Chem Interface
* Add the new **QCHEm** option key to call in the CHARMM script to activate the interface with Python for QM/MM simulations (such as ML-assisted QM/MM simulations).The new option is added as **QCHARMM2PY** in the **gukini.F90** file in the **(<path_to_charmm>/source/gukint/gukini.F90)**.

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
       QMIXED,QQCSCRATCH,QQCHEM,QQEWALD,QMESS,QQRESD,QQCONS,QQNOLKATMS, QCHARMM2PY ! added new option

!
! rest of the code
!

end module gamess_fcm
```

## How to Extract Custom Information from CHARMM Topology (.PSF)
In certain cases, the interface with Python may require the information from the CHARMM topology file (.psf) to calculate the QM/MM energies and forces which may not be readily available within the current gukin.F90 interface. So it would be beneficial to have copy of the information from the .psf file to be used to pass information to the Python script. The following code snippet demonstrates how to extract the information from the CHARMM topology file (.psf) by introducing custom variables in the **psf_ltm.F90** file in the **(<path_to_charmm>/source/psfgen/psf_ltm.F90)**.

The code below demonstrates how to introduce a new variable to store a copy of the charge array from the .psf file. Various other information from PSF file (CHARMM topology) can be introduced similarly. (In the next section, we will see how to pass this saved array to the Python script.)

```fortran
!!source/ltm/psf_ltm.F90
module psf
  use chm_kinds
  use dimens_fcm

  !
  ! rest of the code
  !

  ! New variable 'cg_dum' added to store charge array
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
