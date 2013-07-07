#!/bin/bash

pdflatex functorlazyvector1.tex
pdflatex functorlazyvector2.tex
pdflatex functorlazyvector3.tex
pdflatex functorlazyvector4.tex
pdflatex functorlazyvector5.tex

pdflatex boxedvector1.tex
pdflatex boxedvector2.tex
pdflatex boxedvector3.tex
pdflatex boxedvector4.tex
pdflatex boxedvector5.tex

pdflatex fig1.tex
pdflatex fig2.tex
pdflatex fig3.tex
pdflatex fig4.tex
pdflatex fig5.tex

convert -density 70 fig1.pdf fig1.png
convert -density 70 fig2.pdf fig2.png
convert -density 70 fig3.pdf fig3.png
convert -density 70 fig4.pdf fig4.png
convert -density 70 fig5.pdf fig5.png

