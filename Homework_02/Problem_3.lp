/* Objective function */
min: +0.754 Back_Carl +0.658 Back_Chris +0.676 Back_David +0.74 Back_Tony +0.708 Back_Ken +0.866 Breast_Carl
 +0.662 Breast_Chris +0.844 Breast_David +0.694 Breast_Tony +0.836 Breast_Ken +0.666 Butterfly_Carl +0.57 Butterfly_Chris
 +0.778 Butterfly_David +0.608 Butterfly_Tony +0.672 Butterfly_Ken +0.584 Freestyle_Carl +0.528 Freestyle_Chris
 +0.592 Freestyle_David +0.57 Freestyle_Tony +0.622 Freestyle_Ken;

/* Constraints */
Back: +Back_Carl +Back_Chris +Back_David +Back_Tony +Back_Ken = 50;
Breast: +Breast_Carl +Breast_Chris +Breast_David +Breast_Tony +Breast_Ken = 50;
Butterfly: +Butterfly_Carl +Butterfly_Chris +Butterfly_David +Butterfly_Tony +Butterfly_Ken = 50;
Freestyle: +Freestyle_Carl +Freestyle_Chris +Freestyle_David +Freestyle_Tony +Freestyle_Ken = 50;
Bench: +Bench_Carl +Bench_Chris +Bench_David +Bench_Tony +Bench_Ken = 50;
Carl: +Back_Carl +Breast_Carl +Butterfly_Carl +Freestyle_Carl +Bench_Carl = 50;
Chris: +Back_Chris +Breast_Chris +Butterfly_Chris +Freestyle_Chris +Bench_Chris = 50;
David: +Back_David +Breast_David +Butterfly_David +Freestyle_David +Bench_David = 50;
Tony: +Back_Tony +Breast_Tony +Butterfly_Tony +Freestyle_Tony +Bench_Tony = 50;
Ken: +Back_Ken +Breast_Ken +Butterfly_Ken +Freestyle_Ken +Bench_Ken = 50;

/* Integer definitions */
int Back_Carl,Back_Chris,Back_David,Back_Tony,Back_Ken,Breast_Carl,Breast_Chris,Breast_David,Breast_Tony,Breast_Ken,Butterfly_Carl,Butterfly_Chris,Butterfly_David,Butterfly_Tony,Butterfly_Ken,Freestyle_Carl,Freestyle_Chris,Freestyle_David,Freestyle_Tony,Freestyle_Ken,Bench_Carl,Bench_Chris,Bench_David,Bench_Tony,Bench_Ken;
