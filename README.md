# Export backend for Org-Mode to Jupyter Notebook, specifically geared towards slides #

## Goal 

This project provides a backend for org-mode that specifcally concentrates on producing useable slidesets. It tires to leverage the RISE project and support pythontutor. 

* [RISE at Github ](https://github.com/damianavila/RISE)
* [Pythontutor ](http://www.pythontutor.com)

Mid-term goal is to write a slide deck and script for Computer Science 101 Intorudciton to Programming class with it. (Taught at Paderborn University, winter term 2016/2017).

Specifically, Org-mode annotations should enable animations on slides, play nice with source code, etc. 

## Alternatives 

- A similar backend exists and inspired this one: https://bitbucket.org/sinayoko/org-mode-jupyter-backend . I tried to base on that one, but couldn't quite figure it out. The present one tries to stay closer to org-mode backend conventions, to the best of my knowlegde: 
- EIN: https://github.com/millejoh/emacs-ipython-notebook - great, but different goal. 
- ob-python: similar. 

## Installation 

Load ox-juslides.el into emacs and evaluate it. Or set it up for
auto-loading. 

## Usage 

- Evaluating ox-juslides provides a new
backend to org-mode
- This backend allows you to produce  iPython / Jupyter
~.ipynb~ Notebook files
- Load these notebook files using ~jupyter notebook bla.ipynb~ 
- To display as slides, you need to have RISE installed. Follow
  instructions there to display the slide set (easy once set up
  correctly) 
  
  

## Writing useful org files for exporting 

Basically, look at file:test.org ; it has extensive examples with
comments. 

- First-level headings define Sections in the document; do not use them with content
- Second-level headings define slides 
- Third- and further down headings can be used on slides 
   - If they have the property animate, they will be turned into a fragment 
- Source code blocks are supported 
- Can have an animate attribute as well 

