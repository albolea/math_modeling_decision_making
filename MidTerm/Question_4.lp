/* Objective function */
max: +150 NY_ZA_Airplane +240 NY_GA_Ship +150 NY_KE_Airplane +150 NY_SU_Airplane +240 NY_AN_Ship +240 NY_SE_Ship
 +150 FL_ZA_Airplane +240 FL_GA_Ship +150 FL_KE_Airplane +150 FL_SU_Airplane +240 FL_AN_Ship +240 FL_SE_Ship;

/* Constraints */
Max_NY: +150 NY_ZA_Airplane +240 NY_GA_Ship +150 NY_KE_Airplane +150 NY_SU_Airplane +240 NY_AN_Ship +240 NY_SE_Ship <= 500000;
Max_FL: +150 FL_ZA_Airplane +240 FL_GA_Ship +150 FL_KE_Airplane +150 FL_SU_Airplane +240 FL_AN_Ship +240 FL_SE_Ship <= 500000;
Air_NY_ZA: +NY_ZA_Airplane <= 300;
Air_NY_KE: +NY_KE_Airplane <= 500;
Air_NY_SU: +NY_SU_Airplane <= 500;
Air_FL_ZA: +FL_ZA_Airplane <= 500;
Air_FL_KE: +FL_KE_Airplane <= 700;
Air_FL_SU: +FL_SU_Airplane <= 600;
Air_ZA_NI: +ZA_NI_Airplane <= 200;
Air_KE_NI: +KE_NI_Airplane <= 0;
Air_SU_NI: +SU_NI_Airplane <= 300;
Air_ZA_DRC: +ZA_DRC_Airplane <= 140;
Air_KE_DRC: +KE_DRC_Airplane <= 40;
Air_SU_DRC: +SU_DRC_Airplane <= 80;
Air_ZA_CH: +ZA_CH_Airplane <= 0;
Air_KE_CH: +KE_CH_Airplane <= 300;
Air_SU_CH: +SU_CH_Airplane <= 40;
Truck_AN_DRC: +AN_DRC_Truck <= 250;
Truck_AN_CH: +AN_CH_Truck <= 240;
Truck_GA_DRC: +GA_DRC_Truck <= 300;
Truck_GA_CH: +GA_CH_Truck <= 160;
Truck_SE_DRC: +SE_DRC_Truck <= 700;
Truck_SE_CH: +SE_CH_Truck <= 450;
Max_Aid_SE: -240 NY_SE_Ship -240 FL_SE_Ship +17.7 SE_NI_Truck +17.7 SE_DRC_Truck +17.7 SE_CH_Truck >= -50000;
Max_Aid_GA: -240 NY_GA_Ship -240 FL_GA_Ship +17.7 GA_NI_Truck +17.7 GA_DRC_Truck +17.7 GA_CH_Truck >= -100000;
Max_Aid_AN: -240 NY_AN_Ship -240 FL_AN_Ship +17.7 AN_NI_Truck +17.7 AN_DRC_Truck +17.7 AN_CH_Truck >= -130000;
Max_Aid_SU: -150 NY_SU_Airplane -150 FL_SU_Airplane +150 SU_NI_Airplane +150 SU_DRC_Airplane +150 SU_CH_Airplane >= -90000;
Max_Aid_ZA: -150 NY_ZA_Airplane -150 FL_ZA_Airplane +150 ZA_NI_Airplane +150 ZA_DRC_Airplane +150 ZA_CH_Airplane >= -150000;
Max_Aid_KE: -150 NY_KE_Airplane -150 FL_KE_Airplane +150 KE_NI_Airplane +150 KE_DRC_Airplane +150 KE_CH_Airplane >= -120000;
Max_Aid_NI: -150 ZA_NI_Airplane -17.7 GA_NI_Truck -150 KE_NI_Airplane -150 SU_NI_Airplane -17.7 AN_NI_Truck -17.7 SE_NI_Truck >= -100000;
Max_Aid_DRC: -150 ZA_DRC_Airplane -17.7 GA_DRC_Truck -150 KE_DRC_Airplane -150 SU_DRC_Airplane -17.7 AN_DRC_Truck
 -17.7 SE_DRC_Truck >= -180000;
Max_Aid_CH: -150 ZA_CH_Airplane -17.7 GA_CH_Truck -150 KE_CH_Airplane -150 SU_CH_Airplane -17.7 AN_CH_Truck -17.7 SE_CH_Truck >= -80000;
