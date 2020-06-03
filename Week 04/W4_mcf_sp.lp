/* Objective function */
min: +100 OT;

/* Constraints */
O: +OA +OB +OC +OT = 20;
A: -OA +AB +AD = 0;
B: -OB -AB +BC +BD +BE = 0;
C: -OC -BC +CE = 0;
D: -AD -BD -DE +DT = 0;
E: -BE -CE +DE +ET = 0;
T: -DT -ET -OT = -20;
Uoa: +OA <= 5;
Uob: +OB <= 7;
Uoc: +OC <= 4;
Uab: +AB <= 1;
Uad: +AD <= 3;
Ubc: +BC <= 2;
Ubd: +BD <= 4;
Ube: +BE <= 5;
Uce: +CE <= 4;
Ude: +DE <= 1;
Udt: +DT <= 9;
Uet: +ET <= 6;
Uot: +OT <= 20;
