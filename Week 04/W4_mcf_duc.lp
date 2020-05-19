/* Objective function */
min: +2 AB +4 AC +9 AD +3 BC +CE +3 DE +2 ED;

/* Constraints */
A: +AB +AC +AD = 50;
B: -AB +BC = 40;
C: -AC -BC +CE = 0;
D: -AD +DE -ED = -30;
E: -CE -DE +ED = -60;
Uab: +AB <= 10;
Uce: +CE <= 80;
