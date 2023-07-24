---
title: Lipari-Szabo S2 Order Parameters from CHARMM MD Trajectories
author:
date: 2023-07-24 
categories: [Reading Notes, NMR]
tags: [mdanalysis, charmm, molecular dynamics, nmr]
render_with_liquid: true
pin: false
toc: true
---

This article covers the relation of the Lipari-Szabo S^2^ order parameter for N-H bond vectors with the eigenvalues of the gyration tensor obtained from CHARMM molecular dynamics trajectories. This articles also provide an example based on CHARMM scripts to calculate Lipari-Szabo S^2^ order parameter for N-H bond vectors.

## Introduction

NMR relaxation experiments are a powerful tool to study the dynamics of biomolecules. The Lipari-Szabo model-free analysis is a widely used method to extract the dynamics parameters from NMR relaxation experiments. The Lipari-Szabo model-free analysis is based on the following equation:

$$
\frac{R_1}{R_2} = \frac{1}{2} + \frac{1}{2} \frac{\tau_e}{\tau_e + \tau_s} + \frac{1}{2} \frac{\tau_e}{\tau_e + \tau_f} \left( 1 - \frac{S^2}{5} \right)
$$

where $R_1$ and $R_2$ are the longitudinal and transverse relaxation rates, respectively. $\tau_e$, $\tau_s$, and $\tau_f$ are the correlation times for the overall, fast, and slow motions, respectively. $S^2$ is the generalized order parameter for the N-H bond vector. The generalized order parameter is defined as:

$$
S^2 = \frac{1}{2} \left( 3 \cos^2 \theta - 1 \right)
$$

where $\theta$ is the angle between the N-H bond vector and the external magnetic field. The generalized order parameter is related to the eigenvalues of the gyration tensor, $G_{ij}$, as:

$$
S^2 = \frac{1}{2} \left( \frac{G_{xx} + G_{yy}}{G_{xx} + G_{yy} + G_{zz}} \right)
$$

The gyration tensor is defined as:

$$
G_{ij} = \frac{1}{M} \sum_{k=1}^{N} m_k \left( r_{ik} - r_{i} \right) \left( r_{jk} - r_{j} \right)
$$

where $M$ is the total mass of the system, $N$ is the total number of atoms, $m_k$ is the mass of the $k^{th}$ atom, $r_{ik}$ is the $i^{th}$ component of the position vector of the $k^{th}$ atom, and $r_{i}$ is the $i^{th}$ component of the center of mass vector.

## Example

The following example is based on the CHARMM scripts available in the [CHARMM forum](https://www.charmm.org/ubbthreads/ubbthreads.php?ubb=showflat&Number=324#Post324). The example files are available in this [GitHub repository](