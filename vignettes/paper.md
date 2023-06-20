---
title: |
  measr: Bayesian psychometric measurement using 'Stan'
output:
  rmarkdown::html_vignette:
    keep_md: TRUE
tags:
  - R
authors:
  - name: W. Jake Thompson
    orcid: 0000-0001-7339-0300
    affiliation: "1, 2"
affiliations:
  - name: Accessible, Teaching, Learning, and Assessment Systems
    index: 1
  - name: University of Kansas
    index: 2
date: 20 June 2023
bibliography: paper.bib
---

# Summary

![](measr-logo.png){ width=120px }

In educational and psychological research, we are often interested discrete latent states of individuals responding to an assessment (e.g., proficiency or non-proficiency on educational standards, the presence or absence of a psychological disorder).
Diagnostic classification models (DCMs; also called cognitive diagnostic models [CDMs]) are a type of psychometric model that facilitates these inferences [@rupp-dcm; @dcm-handbook].
DCMs are multi-dimensional, meaning that we can classify respondents over a profile of assessed skills, or attributes.
A Q-matrix is then used to define which items on the assessment measure each attribute.
Using the pre-defined latent profiles and the Q-matrix, DCMs then estimate the probability that respondents are in profile, or have the corresponding pattern of proficiency, or presence, of the attributes.
Finally, because DCMs are classifying respondents rather than placing them along a performance continuum, these models are able to achieve more reliable results with shorter test lengths [@templin2013], reducing the burden on respondents.

# Statement of need

Something...

# Acknowledgements

The research reported here was supported by the Institute of Education Sciences, U.S. Department of Education, through Grant [R305D210045](https://ies.ed.gov/funding/grantsearch/details.asp?ID=4546) to the University of Kansas. The opinions expressed are those of the authors and do not represent the views of the the Institute or the U.S. Department of Education.

# References
