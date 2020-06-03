/* Objective function */
min: +464 Bell_Sac +513 Bell_SLC +654 Bell_RC +867 Bell_Alb +352 Eue_Sac +416 Eue_SLC +690 Eue_RC +791 Eue_Alb
 +995 AL_Sac +682 AL_SLC +388 AL_RC +685 AL_Alb;

/* Constraints */
Bell: +Bell_Sac +Bell_SLC +Bell_RC +Bell_Alb = 75;
Eue: +Eue_Sac +Eue_SLC +Eue_RC +Eue_Alb = 125;
AL: +AL_Sac +AL_SLC +AL_RC +AL_Alb = 100;
Sac: +Bell_Sac +Eue_Sac +AL_Sac = 80;
SLC: +Bell_SLC +Eue_SLC +AL_SLC = 65;
RC: +Bell_RC +Eue_RC +AL_RC = 70;
Alb: +Bell_Alb +Eue_Alb +AL_Alb = 85;
