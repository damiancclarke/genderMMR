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
twid = ['7','7','7','7','7','6']
tcm  = ['}{p{16.2cm}}','}{p{13.8cm}}','}{p{16.8cm}}','}{p{18.8cm}}',
        '}{p{18.8cm}}','}{p{15.2cm}}']
mc3  = '{\\begin{footnotesize}\\textsc{Notes:} '
lname = "Fertility$\\times$desire"
tname = "Twin$\\times$desire"
tsc  = '\\textsc{' 
ebr  = '}'
R2   = 'R$^2$'
n1=['MMR is from WDI, and is defined as deaths per 100,000 live births',
    'TB is from WDI and is the incidence of tuberculosis per 100,000 people.'
    ,'LE ratio is the log of the ratio of female to male LE times 100,000',
    'Abortion comes from womenrights.org, and refers to unrestricted access',
    'Abortion legislation takes +1 if legalising abortion, and -1 if criminalising.']
n2=['All values are quinquennial averages for 160 countries.',
    'TB data is available yearly from 1990-2012 for almost all countries',
    'The estimation sample consists of 150 countries for years 1981-2012.',
    'Estimation sample includes countries with existing abortion legislation',
    'Estimation sample includes countries with existing abortion legislation']

#==============================================================================
#== (2) Generate rights table
#==============================================================================
cc = 0
for v in ['MMR','tb','ln_LE_ratio','abortion','abortionLeg']:

    if v=='MMR':
        polRight = RES + 'rights/'+v+'-wopol_5.tex' 
        ecoRight = RES + 'rights/'+v+'-wecon_5.tex' 
        socRight = RES + 'rights/'+v+'-wosoc_5.tex' 
    else:
        polRight = RES + 'rights/'+v+'-wopol.tex' 
        ecoRight = RES + 'rights/'+v+'-wecon.tex' 
        socRight = RES + 'rights/'+v+'-wosoc.tex' 

    rightT = open(TAB + 'rights'+v+'.tex', 'w')
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

    rightT.write('\n'+mr+mc1+twid[cc]+tcm[cc]+mc3+
    "Country fixed effects are included in all cases."  
    +n1[cc]+ 
    "The rights data comes from the Cingranelli, Richards, and Clay data set. "
    +n2[cc]+ "\\label{TAB:"+v+"rights}\n"
    "\\end{footnotesize}} \\\\ \\bottomrule \n \\end{tabular}\\end{table}")

    rightT.write('\\end{landscape}')
    rightT.close()
    cc = cc+1

#==============================================================================
#== (3) Edit GII table
#==============================================================================
for v in ['MMR','tb','ln_LE_ratio','abortion','abortionLeg']:
    GII = RES + 'gii/'+v+'GII.tex' 

    giiT = open(TAB +v+ 'GII.tex', 'w')
    mG   = open(GII, 'r').readlines()

    for i,line in enumerate(mG):
        if i<8:
            giiT.write(line)
        if i==8:
            giiT.write('\\multicolumn{9}{l}{'
                       '\\textsc{Panel A: No Interaction}}\\\\\n')
        if i>7 and i<12:
            giiT.write(line)
        if i>12 and i<15:
            giiT.write(line)
        if i==15:
            giiT.write('\\\\ \\multicolumn{9}{l}{'
                       '\\textsc{Panel B: GDP Interaction}}\\\\\n')
        if i>25:
            giiT.write(line)

giiT.close()
print 'end'


#==============================================================================
#== (4) Summary
#==============================================================================
Health = RES + 'summary/health.tex'
Gender = RES + 'summary/gender.tex'

sumT = open(TAB + 'sumStats.tex', 'w')
sumT.write('\\begin{table}[htpb!] \n \\begin{center} \n' 
'\\caption{Descriptive Statistics}\n \\label{TAB:sumstats} '
'\\begin{tabular}{lccccc} '
'\n \\toprule\\toprule \\vspace{5mm} \n'
'& N & Mean & Std. Dev. & Min. & Max. \\\\ \\midrule \n'
'\multicolumn{6}{l}{\\textbf{Panel A: Health Measures}} \\\\ \n')

He  = open(Health,  'r').readlines()
Ge  = open(Gender, 'r').readlines()

for i,line in enumerate(He):
    if i>8 and i<20:
        line = line.replace('\\hline','\\midrule')
        line = line.replace('(%)','')
        sumT.write(line)

sumT.write(' \n \\multicolumn{6}{l}{\\textbf{Panel B: Gender Measures}}\\\\ \n ')
for i,line in enumerate(Ge):
    if i>8 and i<19:
        line = line.replace('(%)','')
        line = line.replace('\\hline','\\midrule')
        sumT.write(line)

sumT.write('\n'+mr+mc1+twid[5]+tcm[5]+mc3+
           "Sample consists of country$\\times$year measures between 1960 and "
           "2013 (with gaps).  Further discussion of coverage is discussed in "
           "appendix \\ref{app:Data}. Mortality measures come from the UN and "
           "are quinquennial from 1960-2010. Maternal mortality comes from the"
           " WHO and are quinquennial from 1990 to 2010.                      "
           "\\end{footnotesize}} \\\\ \\bottomrule \n \\end{tabular}\\end{center}"
           "\\end{table}")
sumT.close()

#==============================================================================
#== (5) Mortality table
#==============================================================================
Low  = open(RES + 'mortality/Mortalitylow.tex').readlines()
Mid  = open(RES + 'mortality/Mortalitymid.tex').readlines()
High = open(RES + 'mortality/Mortalityhigh.tex').readlines()

MortT = open(TAB + 'mortality.tex', 'w')
for i1,line1 in enumerate(Low):
    print str(i1) + line1
    if i1<15 or i1==16 or i1==17:
        line1 = line1.replace('\\toprule','\\toprule\\toprule')
        line1 = line1.replace('\\midrule',
                              '\\midrule \\multicolumn{7}{l}{\\textsc{Panel A: Low Income}}\\\\')
        MortT.write(line1)

for i2,line2 in enumerate(Mid):
    if i2>7 and i2<15 or i2==16 or i2==17:
        line2 = line2.replace('\\midrule',
                              '\\midrule \\multicolumn{7}{l}{\\textsc{Panel A: Middle Income}}\\\\')
        MortT.write(line2)

for i3,line3 in enumerate(High):
    if i3>7 and i3<15 or i3==16 or i3==17:
        line3 = line3.replace('\\midrule',
                              '\\midrule \\multicolumn{7}{l}{\\textsc{Panel A: High Income}}\\\\')
        MortT.write(line3)
MortT.write('\\midrule'
            '\\multicolumn{7}{p{16.2cm}}{\\begin{footnotesize} \\textsc{Notes:} '
            'All regressions include country and year fixed effects. Standard   '
            'errors clustered by country are included in parentheses. Mortality '
            'rates come from the United Nations Population Division (2013) and  '
            'are available 5 yearly from 1960 to 2005. MMR and IMR data come    '
            'from the World Bank WDI (based on WHO data). MMR is available for 5'
            'year time periods from 1990 to 2010 while IMR data is available for'
            ' 1990, 2000, 2010 and 2012. Odd columns are based on the years 1990'
            ', 1995, 2000 and 2005, and even columns: the years 1990 and 2000.  '
            '\\label{TAB:mortality}'
            '\\end{footnotesize}}\\\\ \\bottomrule\\end{tabular}\\end{table}')


 
MortT.close()
