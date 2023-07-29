---
title: Lipari-Szabo S2 Order Parameters from Molecular Dynamics Simulation Trajectories
author:
date: 2023-07-28 
categories: [Reading Notes, NMR]
tags: [mdanalysis, charmm, molecular dynamics, nmr]
render_with_liquid: true
pin: false
toc: true
---

This article discusses how molecular dynamics (MD) simulation trajectory results can be used to calculate the Lipari-Szabo S^2^ order parameters for a protein backbone and briefly go through the theory behind the calculation. This article is a continuation of the previous article on [**time aoutocorrelation function**](/_posts/2023-07-26-correlation-time-from-md.md) and mainly refers to the following sources:


## Introduction

The structural and dynamics results of computer simulations must often be transformed between time and frequency domains. To be compared with experiment, a [time autocorrelation function](/_posts/2023-07-26-correlation-time-from-md.md) is usually transformed into a frequency spectrum (spectral density function) $J(\omega)$ by a Fourier transform. Specifically, in the case of NMR relaxation, the $J(\omega)$ can be used to calculate the relaxation rates $R_1$ and $R_2$ and the heteronuclear NOE. Giovanni Lipari and Attila Szabo developed the Model-Free Approach, which utilize the spectral density function $J(\omega)$ to least-square fit relaxation data by treating $S^2$ and $\tau_e$ as the only adjustable parameters.