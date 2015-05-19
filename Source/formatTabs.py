# formatTabs.py v0.00             SB/DC/JG/AV              yyyy-mm-dd:2015-05-19
#---|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
#
# This file formats tables to produce output for the paper "Maternal Mortality 
# and Female Life Expectancy: The Importance of Gender Inequality" (Bhalotra, 
# Clarke, Gomes, Venkataramani).  It produces individual tex files, and one fin-
# al file called tables.tex which should be called by the main paper.tex file.  
# The only thing that needs to be changed is the definition of RES and TAB in 
# section (1).

import re
import os

print('\n\n Producing tex files for output tables.\n\n')

#==============================================================================
#== (1a) File names (comes from Stata do files)
#==============================================================================
RES   = "/home/damiancclarke/investigacion/2013/WorldMMR/Results/"
TAB   = "/home/damiancclarke/investigacion/2013/WorldMMR/tables/"

polRight = RES + 'rights/MMR-wopol_5.tex' 
ecoRight = RES + 'rights/MMR-wecon_5.tex' 
socRight = RES + 'rights/MMR-wosoc_5.tex' 


#==============================================================================
#== (1b) shortcuts
#==============================================================================
foot = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"
ls   = "\\\\"

mr   = '\\midrule'
tr   = '\\toprule'
br   = '\\bottomrule'
mc1  = '\\multicolumn{'
mc2  = '}}'
twid = ['7','5']
tcm  = ['}{p{15.8cm}}','}{p{12.0cm}}']
mc3  = '{\\begin{footnotesize}\\textsc{Notes:} '
lname = "Fertility$\\times$desire"
tname = "Twin$\\times$desire"
tsc  = '\\textsc{' 
ebr  = '}'
R2   = 'R$^2$'

#==============================================================================
#== (2) Generate rights table
#==============================================================================
rightT = open(TAB + 'rightsMMR.tex', 'w')
rightT.write('\\begin{landscape}')

pR = open(polRight, 'r').readlines()
for i, line in enumerate(pR):
    if i==8:
        rightT.write('\n \\textsc{Panel A: Political Rights}&&&&&\\\\ \n')
    if i==10:
        line = line.replace('\\midrule', '')
    if i<13:
        rightT.write(line)
rightT.write('\n &&&&&& \\\\\n \\textsc{Panel B: Economic Rights}&&&&&\\\\ \n')

eR = open(ecoRight, 'r').readlines()
for i, line in enumerate(eR):
    line = line.replace('\\midrule', '')
    if i>6 and i<13:
        rightT.write(line)
rightT.write('\n &&&&&& \\\\\n \\textsc{Panel C: Social Rights}&&&&&\\\\ \n')

sR = open(socRight, 'r').readlines()
for i, line in enumerate(sR):
    line = line.replace('\\midrule', '')
    line = line.replace('Year', '\\midrule Year')
    if i>6 and i<15:
        rightT.write(line)

rightT.write('\n'+mr+mc1+twid[0]+tcm[0]+mc3+
"Country fixed effects are included in all cases.  MMR is from WDI, and is "
"defined as deaths per 100,000 live births.  The rights data comes from the "
"Cingranelli, Richards, and Clay data set. All the are quinquennial averages." 
" The estimation sample consists of 160 countries for the years 1990, 1995, "
"2000, 2005 and 2010 (panel C does not contain the year 2010)."
"\\end{footnotesize}} \\\\ \\bottomrule \n \\end{tabular}\\end{table}")

rightT.write('\\end{landscape}')
rightT.close()
