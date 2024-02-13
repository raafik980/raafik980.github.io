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

CHARMM houses code **(<path_to_charmm>/source/gukint/gukini.F90)** to interface with a number of quantum mechanical (QM) programs, including Gaussian, GAMESS, and Q-Chem. These interfaces allow CHARMM to perform QM/MM simulations, where the QM region is treated with a high-level QM method, and the MM region is treated as point charge (electrostatic embedding). These implementations can leverage the CHARMM capabilities, such as applying restraints, umbrella sampling, and various analysis modules. Depending on the atomic selection choices mentioned in the CHARMM running script, the simulation can represent the atoms with all QM, all MM, or a hybrid of QM and MM.
This article demonstrates a quick way to modify the existing implementation to interface with a Python script that could use machine learning potentials to calculate QM/MM energies and forces.