---
title: "Reactive Molecular Dynamics with Machine Learning Potential in CHARMM: Building an Interface to Python "
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

This topic demonstrates an example of incorporating a Python package into CHARMM. The aim of calling the Python package is to perform set of tasks to perform during each frame generation within a QM/MM MD simulation, such as calculating energy and forces based on a pretrained machine learning potential (MLP).

