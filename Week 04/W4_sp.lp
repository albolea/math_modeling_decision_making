/* Objective function */
min: +2 OA +5 OB +4 OC +2 AB +7 AD +BC +4 BD +3 BE +4 CE +DE +5 DT +7 ET;

/* Constraints */
O: +OA +OB +OC = 1;
A: -OA +AB +AD = 0;
B: -OB -AB +BC +BD +BE = 0;
C: -OC -BC +CE = 0;
D: -AD -BD +DE +DT = 0;
E: -BE -CE -DE +ET = 0;
T: -DT -ET = -1;
