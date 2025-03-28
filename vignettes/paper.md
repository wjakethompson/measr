---
title: "measr: Bayesian psychometric measurement using Stan"
output:
  rmarkdown::html_vignette:
    keep_md: TRUE
tags:
  - R
  - Stan
  - bayesian-modeling
  - educational-assessment
  - psychometrics
  - diagnostic-modeling
authors:
  - name: W. Jake Thompson
    orcid: 0000-0001-7339-0300
    affiliation: "1, 2"
affiliations:
  - name: Accessible, Teaching, Learning, and Assessment Systems
    index: 1
  - name: University of Kansas
    index: 2
date: 6 November 2023
bibliography: paper.bib
vignette: >
  %\VignetteEncoding{UTF-8}
  %\VignetteIndexEntry{measr: Bayesian psychometric measurement using Stan}
  %\VignetteEngine{knitr::rmarkdown}
---

# Summary

In educational and psychological research, we are often interested in discrete latent states of individuals responding to an assessment (e.g., proficiency or non-proficiency on educational standards, the presence or absence of a psychological disorder).
Diagnostic classification models (DCMs; also called cognitive diagnostic models [CDMs]) are a type of psychometric model that facilitates these inferences [@rupp-dcm; @dcm-handbook].
DCMs are multi-dimensional, meaning that we can classify respondents on multiple latent attributes within a profile of skills.
A Q-matrix is then used to define which items on the assessment measure each attribute.
Using the pre-defined latent profiles and the Q-matrix, DCMs then estimate the probability that respondents are in profile, or have the corresponding pattern of proficiency, or presence, of the attributes.
This means that DCMs are able to provide fine-grained feedback on specific skills that may need additional instruction in an educational context, or particular symptoms that may be contributing to a diagnosis in a psychological context.
Finally, because DCMs are classifying respondents rather than placing them along a performance continuum, these models are able to achieve more reliable results with shorter test lengths [@templin2013a], reducing the burden on respondents.

Given these benefits, the goal of measr is to make DCMs more accessible to applied researchers and practitioners by providing a simple interface for estimating and evaluating DCMs.

# Statement of need

measr is an R package developed to easily estimate and evaluate DCMs in applied settings.
Despite the ability of DCMs to provide reliable, fine-grained feedback on specific skills, these models have not been widely used for research or operational programs.
This is due in large part to limitations in existing software for estimating and evaluating DCMs [@sessoms2018; @ravand2020].
Typically, DCMs are estimated with a maximum likelihood estimator and then evaluated using limited-information fit indices [e.g., @liu2016].
This is the approach taken when using Mplus [e.g., @templin2013b] and popular R packages GDINA [@R-gdina] and CDM [@R-cdm].
However, as the name "limited-information" implies, these methods only look at limited relationships between the items, such as univariate or bivariate relationships.
This means that higher-level relationships between the items cannot be evaluated (e.g., relationships between triplets of items).

Bayesian estimation methods offer more robust methods for evaluating model fit through posterior predictive checks [@park2015; @thompson2019].
To date, there are three R packages that offer Bayesian estimation of DCMs: dina [@R-dina], hmcdm [@R-hmcdm], and blatent [@R-blatent].
However, all of these packages only estimate a single type of DCM, severely limiting their generalizability to a wide range of applications.

The measr package seeks to overcome the limitations of existing software options by serving as an interface to the Stan probabilistic programming language [@stan].
With Stan as a backend, measr can estimate a wide variety of DCMs.
Primarily, measr supports the estimation of the loglinear cognitive diagnostic model (LCDM).
However, because the LCDM is a general DCM that subsumes many subtypes [@lcdm], measr also supports other DCMs such as the deterministic inputs, noisy "and" gate (DINA) model [@dina] and the deterministic inputs, noisy "or" gate (DINO) model [@dino].
After estimation, measr provides model evaluations using both limited-information indices and posterior predictive checks.
By providing straightforward estimation and evaluation of DCMs, measr makes these models more accessible to practitioners and applied researchers.
Thus, with measr, users get the power of Bayesian methods for model evaluation, compatibility with other packages in the larger Stan ecosystem, and a user-friendly interface so that knowledge of the Stan language is not required.
However, models estimated with measr also include the fitted Stan object, so users can access it if they are familiar with Stan and prefer to work with that object.
Additionally, the Stan code used to estimate the model is also returned so that users familiar with the Stan language can use that code as a starting point for writing their own customized models.

# Acknowledgments

The research reported here was supported by the Institute of Education Sciences, U.S. Department of Education, through Grant [R305D210045](https://ies.ed.gov/funding/grantsearch/details.asp?ID=4546) to the University of Kansas. The opinions expressed are those of the authors and do not represent the views of the Institute or the U.S. Department of Education.

We are grateful to the project advisory committee members who provided feedback on the development of the R package: Russell Almond, Claudia Flowers, Robert Henson, and Matthew Madison.

# References
