---
title: PyMOL-Python External Functions for CHARMM MD Trajectory Analysis 
author:
date: 2023-07-19 
categories: [Software, PyMOL]
tags: [pymol, mdanalysis, charmm, python, molecular dynamics]
render_with_liquid: true
pin: false
toc: true
---

> *DISCLAIMER: The content in this article is for demonstration purposes only and may contain errors and technical inaccuracies*
<br>
<br>

This post is about how to do some quick analyses directly from pre-rendered CHARMM molecular dynamics trajectories in PyMOL using custom external Python functions and PyMOL selection algebra. For more extended information about PyMOL's capabilities, [pymolwiki.org](https://pymolwiki.org/index.php/Launching_From_a_Script) is a great resource.

## Introduction
PyMOL is a powerful molecular visualization tool that is widely used in the computational chemistry community. It is also a great tool for creating high-quality figures for publications. PyMOL's capability to read CHARMM dcd trajectories and render them in a variety of ways makes it a great tool for visualizing MD simulations. PyMOL being a Python library and having a Python interpreter built-in, it is also possible to write custom functions to perform specific tasks as well as readily test possible analyses based on visual inspection of MD trajectories. In this post, I will be discussing how to read and render CHARMM trajectories in PyMOL and some custom functions to perform specific tasks directly in PyMOL and plot the results. The example files are available in this [GitHub repository](https://github.com/raafik980/charmm-md-analysis-in-pymol.git)

## Reading CHARMM Trajectories in PyMOL

This section assumes one has already Open-Source PyMOL installed on their system. If not, please refer to [pymolwiki.org](https://pymolwiki.org/index.php/Main_Page) for installation instructions. Also, assumes one has already preprocessed the trajectories which include removing solvent molecules and retaining relevant segments, recentering, aligning, and applying stride for a descent number of frames. One can refer the [CHARMM documentation](https://academiccharmm.org/documentation/latest/dynamc#Merge) for more information on how to perform these tasks or refer to the [script archive section](https://www.charmm.org/ubbthreads/ubbthreads.php?ubb=showflat&Number=324#Post324) in charmm forum for ready-to-use scripts.

- In PyMOL, drag the .psf file into the workspace. This will load the topology file into PyMOL. And then, drag the .dcd file into the workspace. This will load the trajectory into PyMOL.

OR

- To render the trajectory, use the following command:

```console
PyMOL> cd /your/path/to/cloned/repo
PyMOL> load traj.psf
PyMOL> load traj.dcd
```
- The slider and the buttons on the bottom of the PyMOL window can be used to navigate through the trajectory. 

- Configuring Mouse > Selection Mode options lets you select atoms, residues, segments, etc. The particular atom selected will get displayed in the PyMOL console. An example is:
    
```console
 Selector: selection "sele" defined with 7 atoms.
 You clicked /traj/PROA/P/THR`155/CA
```

## Essential External Python Libraries

A convenient way to deal with external libraries which will be used in Python functions would be working in a Conda environment. Apart from pymol, the following libraries are required for the functions discussed in this post:

- matplotlib
- numpy
- multiprocessing
- tqdm
- scipy.stats



## Custom Analysis Functions

The example files for these analyses can be found in this [GitHub repository](https://github.com/raafik980/charmm-md-analysis-in-pymol.git). The functions are written as .py files which can be called through the in-built Python interpreter in PyMOL. The following command can be used to call the functions after loading the trajectory as mentioned in the previous section:

```console
PyMOL> cd /your/path/to/cloned/repo
PyMOL> run your_func.py
```

### 1. Distance vs. Frame

A simple quick analysis to check the distance between two atoms in a trajectory. The function takes the following arguments and usage with respect to our example is as follows (**num_process** option is to parallelize within available CPUs):

```console
##USAGE: dist_vs_frame('object_name', 'atom1', 'atom2', num_process=1)

PyMOL> dist_vs_frame('traj', '/traj/PROA/P/PRO`128/CA', '/traj/PROA/P/LYS`40/CA', num_process=1)
```

The selection format can be easily accessed from PyMOL console section as mentioned in the previous section. The function will plot the distance between the two atoms as a function of frame number. The script can be modified readily to suit the needs of the user. For example, calculating angles, dihedrals, etc.

pymol.cmd command for angle calculation:

```python
angle = pymol.cmd.get_angle('selection1', 'selection2', 'selection3')
```

### 2. RMSD vs. Frame 

The function takes the following arguments and usage with respect to our example as follows (the **num_process** option is to parallelize within available CPUs and the **stride** option is to skip frames):


```console
##USAGE: RMSD_vs_frame('protein_selection', stride=5, num_process=4)

PyMOL> RMSD_vs_frame('/traj/PROA/P  and ( name  N+CA+C+O ) ', stride=5, num_process=1)
```
### 3. Angle vs. Distance Distribution

The function takes the following arguments and usage with respect to our example is as follows (**num_process** option is to parallelize within available CPUs):

```console
##USAGE: angle_vs_dist_distribution('object_name','atom1_angle', 'atom2_angle', 'atom3_angle', 'atom1_distance', 'atom2_distance', num_processes=2)

PyMOL> angle_vs_dist_distribution('traj','/traj/PROA/P/VAL`148/CA', '/traj/PROA/P/LEU`82/CA', '/traj/PROA/P/SER`43/CA', '/traj/PROA/P/PRO`128/CA', '/traj/PROA/P/ARG`36/CA', num_processes=4)
```

### 4. Center of Mass Distance Distribution

PyMOL has an in-built function to calculate the center of mass of a selection. With the center of mass feature along with selection tools, we will be able to make distribution plots with respect to certain domain motions of the structure. The function takes the following arguments and usage with respect to our example is as follows (**num_process** option is to parallelize within available CPUs):

```console
##USAGE: cmass_dist_distribution('object_name', 'core_selection', 'domain1_selection', 'domain2_selection', num_processes=2 )

PyMOL> cmass_dist_distribution( ' traj ', '  ( /traj/PROA/P ) and ( name  N+CA+C+O ) and  ( resi 3-23 or resi 81-85 or resi 109-115  ) ',  ' (  /traj/PROA/P ) and ( name N+CA+C+O )  and  ( resi 129-159  ) ', ' ( /traj/PROA/P ) and ( name  N+CA+C+O )  and  ( resi 33-55  )' , num_processes=4)
```

More on [selection algebra](https://pymolwiki.org/index.php/Selection_Algebra) in PyMOL can be found at pymolwiki.org.

## Case Study: MD Simulations of an Enzyme

In the [GitHub repository](https://github.com/raafik980/charmm-md-analysis-in-pymol.git), I have included a CHARMM trajectory demonstrating the opening of a protein with two lid domains. The trajectory is preprocessed with CHARMM scripts and the example functions are used to perform some quick analyses demonstrating the opening of the protein structure. 