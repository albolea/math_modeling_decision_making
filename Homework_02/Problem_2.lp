/* Objective function */
min: +425 P1_W1 +560 P1_W2 +510 P2_W1 +600 P2_W2 +470 W1_RO1 +505 W1_RO2 +490 W1_RO3 +390 W2_RO1 +410 W2_RO2
 +440 W2_RO3;

/* Constraints */
P1: +P1_W1 +P1_W2 = 200;
P2: +P2_W1 +P2_W2 = 300;
P1_W1: +P1_W1 <= 125;
P1_W2: +P1_W2 <= 150;
P2_W1: +P2_W1 <= 175;
P2_W2: +P2_W2 <= 200;
W1: +P1_W1 +P2_W1 <= 350;
W2: +P1_W2 +P2_W2 <= 350;
W1_in_out: +P1_W1 +P2_W1 -W1_RO1 -W1_RO2 -W1_RO3 = 0;
W2_in_out: +P1_W1 +P2_W1 -W2_RO1 -W2_RO2 -W2_RO3 = 0;
W1_RO1: +W1_RO1 <= 100;
W1_RO2: +W1_RO2 <= 150;
W1_RO3: +W1_RO3 <= 100;
W2_RO1: +W2_RO1 <= 125;
W2_RO2: +W2_RO2 <= 150;
W2_RO3: +W2_RO3 <= 75;
RO1: +W1_RO1 +W2_RO1 = 150;
RO2: +W1_RO2 +W2_RO2 = 200;
RO3: +W1_RO3 +W2_RO3 = 150;

/* Integer definitions */
int P1_W1,P1_W2,P2_W1,P2_W2,W1_RO1,W1_RO2,W1_RO3,W2_RO1,W2_RO2,W2_RO3;
