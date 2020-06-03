/* Objective function */
min: +9 AB +7 AC +20 AF +7 BD +2 BE +4 CD +6 CE +3 DE +6 DF +9 EF;

/* Constraints */
A: +AB +AC +AF = 1;
B: -AB +BD +BE = 0;
C: -AC +CD +CE = 0;
D: -BD -CD +DE +DF = 0;
E: -DE -DF -EF = 0;
F: -AF -DF -EF = -1;
