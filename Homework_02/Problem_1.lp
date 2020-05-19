/* Objective function */
min: +622 PA_W1 +614 PA_W2 +630 PA_W3 +641 PB_W1 +645 PB_W2 +649 PB_W3;

/* Constraints */
PA: +PA_W1 +PA_W2 +PA_W3 +PA_W4D = 100;
PB: +PB_W1 +PB_W2 +PB_W3 +PB_W4D = 120;
W1: +PA_W1 +PB_W1 = 80;
W2: +PA_W2 +PB_W2 = 60;
W3: +PA_W3 +PB_W3 = 70;
W4D: +PA_W4D +PB_W4D = 10;
