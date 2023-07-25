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

This article discusses how molecular dynamics (MD) simulation trajectory results can be used to calculate the Lipari-Szabo S^2^ order parameters for a protein backbone and briefly go through the theory behind the calculation. The NMR module in CHARMM provides a convenient way to calculate the S^2^ order parameters from MD trajectories. 

## Introduction

The Lipari-Szabo S^2^ order parameters are a measure of the amplitude of the motion of a particular bond vector (in a protein backbone) indicating the degree of flexibility within a molecular structure (of protein). While it is challenging to measure the S^2^ order parameters experimentally, they can be calculated from MD trajectories. 