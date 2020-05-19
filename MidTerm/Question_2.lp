/* Objective function */
min: +20.245 NY_ZA_Airplane +172.114285714 NY_GA_Ship +20.125 NY_KE_Airplane +17.6025 NY_SU_Airplane +186.457142857 NY_AN_Ship
 +119.2 NY_SE_Ship +19.86 FL_ZA_Airplane +180.828571429 FL_GA_Ship +19.9025 FL_KE_Airplane +17.71 FL_SU_Airplane
 +195.085714286 FL_AN_Ship +112.114285714 FL_SE_Ship +10.175 ZA_NI_Airplane +20.64 GA_NI_Truck +6.31 KE_NI_Airplane
 +5.1625 SU_NI_Airplane +58.32 AN_NI_Truck +33.12 SE_NI_Truck +1.865 ZA_DRC_Airplane +36.84 GA_DRC_Truck
 +2.725 KE_DRC_Airplane +3.4425 SU_DRC_Airplane +27.78 AN_DRC_Truck +92.46 SE_DRC_Truck +5.275 ZA_CH_Airplane
 +28.3 GA_CH_Truck +4.42 KE_CH_Airplane +3 SU_CH_Airplane +53.04 AN_CH_Truck +54.5 SE_CH_Truck;

/* Constraints */
NY: +NY_ZA_Airplane +NY_GA_Ship +NY_KE_Airplane +NY_SU_Airplane +NY_AN_Ship +NY_SE_Ship -US_NY_Airplane = 0;
FL: +FL_ZA_Airplane +FL_GA_Ship +FL_KE_Airplane +FL_SU_Airplane +FL_AN_Ship +FL_SE_Ship -US_FL_Airplane = 0;
ZA: -NY_ZA_Airplane -FL_ZA_Airplane +ZA_NI_Airplane +ZA_DRC_Airplane +ZA_CH_Airplane = 0;
GA: -NY_GA_Ship -FL_GA_Ship +GA_NI_Truck +GA_DRC_Truck +GA_CH_Truck = 0;
KE: -NY_KE_Airplane -FL_KE_Airplane +KE_NI_Airplane +KE_DRC_Airplane +KE_CH_Airplane = 0;
SU: -NY_SU_Airplane -FL_SU_Airplane +SU_NI_Airplane +SU_DRC_Airplane +SU_CH_Airplane = 0;
AN: -NY_AN_Ship -FL_AN_Ship +AN_NI_Truck +AN_DRC_Truck +AN_CH_Truck = 0;
SE: -NY_SE_Ship -FL_SE_Ship +SE_NI_Truck +SE_DRC_Truck +SE_CH_Truck = 0;
US: +US_NY_Airplane +US_FL_Airplane = 1;
NI: -ZA_NI_Airplane -GA_NI_Truck -KE_NI_Airplane -SU_NI_Airplane -AN_NI_Truck -SE_NI_Truck = 0;
DRC: -ZA_DRC_Airplane -GA_DRC_Truck -KE_DRC_Airplane -SU_DRC_Airplane -AN_DRC_Truck -SE_DRC_Truck = 0;
CH: -ZA_CH_Airplane -GA_CH_Truck -KE_CH_Airplane -SU_CH_Airplane -AN_CH_Truck -SE_CH_Truck = -1;
