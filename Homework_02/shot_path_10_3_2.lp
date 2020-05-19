/* Objective function */
min: +40 OA +60 OB +50 OC +10 AB +10 BA +70 AD +70 DA +20 BC +20 CB +55 BD +55 DB +40 BE +40 EB +50 CE +50 EC
 +10 DE +10 ED +60 DT +80 ET;

/* Constraints */
O: +OA +OB +OC = 1;
A: -OA +AB -BA +AD -DA = 0;
B: -OB -AB +BA +BC -CB +BD -DB +BE -EB = 0;
C: -OC -BC +CB +CE -EC = 0;
D: -AD +DA -BD +DB -DE +ED +DT = 0;
E: -BE +EB -CE +EC +DE -ED +ET = 0;
T: -DT -ET = -1;
