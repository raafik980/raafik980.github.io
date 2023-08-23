---
title: Lipari-Szabo $S^2$ Order Parameters from Molecular Dynamics Simulation Trajectories
author:
date: 2023-08-14  
categories: [Software, pyCHARMM]
tags: [mdanalysis, charmm, molecular dynamics, nmr, pycharmm, python]
render_with_liquid: true
pin: false
toc: true
math: true
---

> *DISCLAIMER: The content in this article is for demonstration purposes only and may contain errors and technical inaccuracies.*


This article discusses how molecular dynamics (MD) simulation trajectory results can be used to calculate the Lipari-Szabo $S^2$ order parameters for a protein backbone and briefly goes through the theory behind the calculation. The Python notebook associated with this article can be found in this [GitHub repository](https://github.com/raafik980/charmm-md-analysis-with-pycharmm.lingo/tree/main/nmr-order-parameter). Also, this article is a continuation of the previous article on the [time autocorrelation function](https://raafik980.github.io/posts/correlation-time-from-md/) and mainly refers to the following sources:

- [CHARMM NMR module documentation](https://academiccharmm.org/documentation/version/c47b2/nmr)

- Levy, R. M.; Karplus, M. [Trajectory Studies of NMR Relaxation in Flexible Molecules. In Advances in Chemistry](https://pubs.acs.org/doi/10.1021/ba-1983-0204.ch018); AMERICAN CHEMICAL SOCIETY: WASHINGTON, D.C., 1983; pp 445–468

- Gu, Y.; Li, D.-W.; Brüschweiler, R. [NMR Order Parameter Determination from Long Molecular Dynamics Trajectories for Objective Comparison with Experiment](https://pubs.acs.org/doi/10.1021/ct500181v). J. Chem. Theory Comput. 2014, 10 (6), 2599–2607




## Spectral Density Function

The structural and dynamics results of computer simulations most often be transformed between time and frequency domains. To be compared with the experiment, a [time autocorrelation function](https://raafik980.github.io/posts/correlation-time-from-md/) is usually transformed into a frequency spectrum (spectral density function) $J(\omega)$ by a Fourier transform. Specifically, in the case of NMR relaxation, the $J(\omega)$ can be used to calculate the relaxation rates $R_1$ and $R_2$ and the heteronuclear NOE. Giovanni Lipari and Attila Szabo developed the Model-Free Approach, which utilizes the spectral density function $J(\omega)$ to least-square fit relaxation data by treating $S^2$ and $\tau_e$ as the only adjustable parameters. The $S^2$ order parameters can be used as an indicator of the amplitude of a particular bond vector's motion, hence enabling structural interpretation of the dynamics of a protein. In the NMR analysis module in CHARMM, the spectral density function $J(\omega)$ is calculated from the time autocorrelation function $C(t)$ by the following equation:

$$
J(\omega) = \int_{0}^{\infty} C(t) cos(\omega t) dt
$$


The NMR module in CHARMM is a powerful tool for calculating NMR parameters from MD trajectories. For more information about the available commands, please refer to the [CHARMM NMR module documentation](https://academiccharmm.org/documentation/version/c47b2/nmr). 


## NMR Analysis in pyCHARMM 

Recently, Joshua Buckner et al. from the Brooks group developed a Python interface to CHARMM called [pyCHARMM](https://doi.org/10.1021/acs.jctc.3c00364). This allows users to run CHARMM functionality from Python scripts. Currently, pyCHARMM is available along with the [CHARMM program](https://academiccharmm.org/program) installation and can be easily imported into Python scripts. The installation instructions can be found [here](https://github.com/BrooksResearchGroup-UM/MSLD-Workshop/tree/main/0Install_Tools).


```python


#CHARMM Stream File Generation for NMR Module, Set options for NMR module

nmr_stream = '''* NMR Order Parameter Calculation       !!Stream File Title, Syntax Requirement
*                                                       !! Syntax Requirement
                                                        !! Blank line, Syntax Requirement    
NMR                                                     !! NMR module Start    
RESEt                                                   !! Reset NMR module
RTIMes sele atom * * N end sele atom * * HN end         !! Select the atoms to calculate the order parameter
DYNA firstu 62 -                                        !! unit number to read from (same as dcd unit)
     nunit 1 -                                          !! number of units to read
     begin 1 -                                          !! first frame to read
     stop 1124 -                                        !! last frame to read
     skip 1 -                                           !! read every skip frames
     tmax 3.0 -                                         !! maximum time for correlation function
     cut 2.0 -                                          !! cut-off for correlation function
     ilist 21 -                                         !! unit number to write to (same as outdat unit)
     dsigma -160.0 C(t) !                               !! correlation function
END                                                     !! End of NMR module ! More option can be seen from the documentation
'''
with open('nmr.str', 'w') as f:
    f.write(nmr_stream)

# Running NMR Module via read option in pyCHARMM
read.stream('nmr.str')

```

In this particular example, the **RTIMes** option defines the atoms for which the tumbling correlation function will be calculated. The **DYNA** option defines the parameters for the calculation of the tumbling correlation function. The **ilist** option defines the unit number to write the correlation function to. The **dsigma** option defines the correlation function to be calculated. The **END** option ends the NMR module. More information about the NMR module can be found in the [CHARMM documentation](https://academiccharmm.org/documentation/version/c47b2/nmr). 



## Case Study: $S^2$ Order Parameters of the Protein Backbone
The Python notebook (nmr.ipynb) example given in this [GitHub repository](https://github.com/raafik980/charmm-md-analysis-with-pycharmm.lingo/tree/main/nmr-order-parameter) shows how to calculate the Lipari-Szabo $S^2$ order parameters from an MD trajectory utilizing the pyCHARMM library and plot the data using Matplotlib. 

The dynamics associated with each residue can be seen by plotting the $S^2$ order parameters as a function of residue number. In this particular case, a clear description of the lid dynamics can be obtained.