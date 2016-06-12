# Export backend for Org-Mode to Jupyter Notebook, specifaically geared towards slides #

## Goal 

This project provides a backend for org-mode that specifcally concentrates on producing useable slidesets. It tires to leverage the RISE project and support pythontutor. 

* [RISE at Github ](https://github.com/damianavila/RISE)
* [Pythontutor ](http://www.pythontutor.com)

Mid-term goal is to write a slide dekc and script for Computer Science 101 Intorudciton to Programming class with it. (Taught at Paderborn University, winter term 2016/2017).

Specifically, Org-mode annotations should enable animations on slides, play nice with source code, etc. 

## Alternatives 

- A similar backend exists and inspired this one: https://bitbucket.org/sinayoko/org-mode-jupyter-backend . I tried to base on that one, but couldn't quite figure it out. The present one tries to stay closer to org-mode backend conventions, to the best of my knowlegde: 
- EIN: https://github.com/millejoh/emacs-ipython-notebook - great, but different goal. 
- ob-python: similar. 

## Installation 

Load ox-juslides.el into emacs and evaluate it. This provides a new backend to org-mode, via which you can produce 

## Usage 

- First-level headings define Sections in the document; do not use them with content
- Second-level headings define slides 
- Third- and further down headings can be used on slides 
   - If they have the property animate, they will be turned into a fragment 
- Source code blocks are supported 
- Can have an animate attribute as well 

