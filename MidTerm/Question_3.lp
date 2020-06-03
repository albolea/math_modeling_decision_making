/* Objective function */
min: +50 NY_ZA_Airplane +30 NY_GA_Ship +55 NY_KE_Airplane +45 NY_SU_Airplane +30 NY_AN_Ship +32 NY_SE_Ship
 +57 FL_ZA_Airplane +48 FL_GA_Ship +61 FL_KE_Airplane +49 FL_SU_Airplane +44 FL_AN_Ship +56 FL_SE_Ship
 +24 ZA_NI_Airplane +3 GA_NI_Truck +28 KE_NI_Airplane +22 SU_NI_Airplane +3 AN_NI_Truck +5 SE_NI_Truck
 +22 ZA_DRC_Airplane +4 GA_DRC_Truck +25 KE_DRC_Airplane +19 SU_DRC_Airplane +5 AN_DRC_Truck +5 SE_DRC_Truck
 +23 ZA_CH_Airplane +7 GA_CH_Truck +2 KE_CH_Airplane +4 SU_CH_Airplane +8 AN_CH_Truck +9 SE_CH_Truck;

/* Constraints */
Max_NY: +150 NY_ZA_Airplane +240 NY_GA_Ship +150 NY_KE_Airplane +150 NY_SU_Airplane +240 NY_AN_Ship +240 NY_SE_Ship = 500000;
Max_FL: +150 FL_ZA_Airplane +240 FL_GA_Ship +150 FL_KE_Airplane +150 FL_SU_Airplane +240 FL_AN_Ship +240 FL_SE_Ship = 500000;
Only_Air_NI: +GA_NI_Truck +AN_NI_Truck +SE_NI_Truck = 0;
Truck_Limit_GA_CH: +GA_CH_Truck <= 840;
Truck_Limit_AN_CH: +AN_CH_Truck <= 840;
Truck_Limit_SE_CH: +SE_CH_Truck <= 840;
Air_Limit_ZA_CH: +ZA_CH_Airplane <= 200;
Air_Limit_SU_CH: +SU_CH_Airplane <= 200;
SE: -240 NY_SE_Ship -240 FL_SE_Ship +17.7 SE_NI_Truck +17.7 SE_DRC_Truck +17.7 SE_CH_Truck = -50000;
GA: -240 NY_GA_Ship -240 FL_GA_Ship +17.7 GA_NI_Truck +17.7 GA_DRC_Truck +17.7 GA_CH_Truck = -100000;
AN: -240 NY_AN_Ship -240 FL_AN_Ship +17.7 AN_NI_Truck +17.7 AN_DRC_Truck +17.7 AN_CH_Truck = -130000;
SU: -150 NY_SU_Airplane -150 FL_SU_Airplane +150 SU_NI_Airplane +150 SU_DRC_Airplane +150 SU_CH_Airplane = -90000;
ZA: -150 NY_ZA_Airplane -150 FL_ZA_Airplane +150 ZA_NI_Airplane +150 ZA_DRC_Airplane +150 ZA_CH_Airplane = -150000;
KE: -150 NY_KE_Airplane -150 FL_KE_Airplane +150 KE_NI_Airplane +150 KE_DRC_Airplane +150 KE_CH_Airplane = -120000;
NI: -150 ZA_NI_Airplane -17.7 GA_NI_Truck -150 KE_NI_Airplane -150 SU_NI_Airplane -17.7 AN_NI_Truck -17.7 SE_NI_Truck = -100000;
DRC: -150 ZA_DRC_Airplane -17.7 GA_DRC_Truck -150 KE_DRC_Airplane -150 SU_DRC_Airplane -17.7 AN_DRC_Truck
 -17.7 SE_DRC_Truck = -180000;
CH: -150 ZA_CH_Airplane -17.7 GA_CH_Truck -150 KE_CH_Airplane -150 SU_CH_Airplane -17.7 AN_CH_Truck -17.7 SE_CH_Truck = -80000;
