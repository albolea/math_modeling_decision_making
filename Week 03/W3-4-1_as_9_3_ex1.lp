/* Objective function */
min: +13 C1 +16 C2 +12 C3 +11 C4 +15 C5 +1000 C6 +13 C7 +20 C8 +5 C9 +7 C10 +10 C11 +6 C12;

/* Constraints */
+C1 +C2 +C3 +C4 = 1;
+C5 +C6 +C7 +C8 = 1;
+C9 +C10 +C11 +C12 = 1;
+C13 +C14 +C15 +C16 = 1;
+C1 +C5 +C9 +C13 = 1;
+C2 +C6 +C10 +C14 = 1;
+C3 +C7 +C11 +C15 = 1;
+C4 +C8 +C12 +C16 = 1;
