---
title: "Poster Presentation at 2024 CHARMM Developers Meeting and Gordon Research Conference"
image:
    path: "/assets/2024-07-30/MA.png"
    alt: "Portland Head Light, Cape Elizabeth, ME, USA"
author:
date: 2024-07-30  
categories: [Presentations]
tags: [protein dynamics, machine learning potentials, free energy simulations]
render_with_liquid: true
pin: false
toc: true
math: true

---
<br>

> ***DISCLAIMER**: The content in this article is for demonstration purposes only and may contain errors and technical inaccuracies.* 


<br>

## Exploring Transferability of Machine Learning Potentials for Free Energy Simulations of Enzymatic Reaction: A Case Study on Dihydrofolate Reductase Catalyzed Reaction
Machine learning potentials (MLPs) have shown success in predicting potential energy surfaces (PES) based on atomic coordinates. However, applying MLPs to predict PES along reaction pathways for large, heterogeneous systems like enzymes remains challenging due to the high cost of generating sufficiently large training data through all-atom quantum mechanical calculations along the reaction coordinate. To address this challenge, a viable strategy is to employ a multiscale quantum mechanical/molecular mechanical (QM/MM) description for training data generation in MLP-based PES models, especially for condensed phase systems. In this study, we undertake a comprehensive re-examination of the MLP architecture introduced by Pan et al., to rigorously evaluate the transferability of a pre-trained MLP and ΔMLP model along the space of enzyme mutations. Enzyme mutations studied range from a single-point substitution to a homologous enzyme from different species and even water. This work focuses on evaluating the ability of MLP and ΔMLP models to accurately capture and represent the diverse effects of mutations in the molecular mechanics environment, especially in the context of enzymatic reactions. The results demonstrate the ability of the MLP models to effectively capture and predict the effects of enzyme mutations on electrostatic interactions, producing accurate free energy landscapes of enzyme-catalyzed reactions. Nevertheless, some notable limitations in the transferability of MLP models are observed, requiring further evaluation and development of advanced MLP models and their training strategy.
![Meeting Poster](/assets/2024-07-30/poster.png)

<br>

#### Pictures from the Meetings

![CHARMM Meeting](/assets/2024-07-30/CHARMM.jpg)
![Gordon Research Conference](/assets/2024-07-30/GRC.png)

