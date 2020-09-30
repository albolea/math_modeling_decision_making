/* Objective function */
min: +300000 Nicholas_Per_Wir_M_Scu +250000 Nicholas_Bur_Wir_M_Scu +125000 Nicholas_Eme_Wir_M_Scu +400000 Rita_Bey_Com_F_Dra
 +500000 Rita_Who_Com_F_Dra +400000 Rita_Dom_Ink_F_Dra +550000 Rita_Inn_Ink_F_Dra +700000 Norm_Agi_Scu_M_Scu
 +575000 Norm_Was_Col_M_Col +125000 Helen_Ser_Wat_F_Pai +300000 Helen_Cal_Wat_F_Pai +150000 Robert_Voi_Oil_M_Pai
 +150000 Robert_Sun_Oil_M_Pai +850000 Enrique_Sto_Pho_M_Pai +750000 Enrique_Har_Pho_M_Pai +400000 Angie_Con_Col_F_Col
 +175000 Angie_Ref_Scu_F_Scu +450000 Angie_Tro_Scu_F_Scu +500000 David_Zig_Pho_M_Pai +500000 David_Zig_Cub_M_Pai
 +500000 David_Zig_Exp_M_Pai +650000 Bill_Whe_Oil_M_Pai +650000 Bill_Pio_Oil_M_Pai +250000 Stuart_Wis_Ink_M_Dra
 +350000 Stuart_Sup_Ink_M_Dra +450000 Stuart_Liv_Oil_M_Pai +400000 Candy_Stu_Cub_F_Pai +400000 C28 +300000 Rick_My _Col_M_Col
 +300000 Rick_Nar_Col_M_Col +50000 Valentino_All_Wat_M_Pai +50000 Valentino_The_Wat_M_Pai +50000 Valentino_Win_Wat_M_Pai
 +50000 Valentino_107_Wat_M_Pai;

/* Constraints */
1_coll: +Norm_Was_Col_M_Col +Angie_Con_Col_F_Col +Rick_My _Col_M_Col +Rick_Nar_Col_M_Col = 1;
at_l_1_w_if_c_d: -0.333333333334 Nicholas_Per_Wir_M_Scu -0.333333333334 Nicholas_Bur_Wir_M_Scu -0.333333333334 Nicholas_Eme_Wir_M_Scu
 +Rita_Bey_Com_F_Dra +Rita_Who_Com_F_Dra >= 0;
at_l_1_d_if_c_w: +Nicholas_Per_Wir_M_Scu +Nicholas_Bur_Wir_M_Scu +Nicholas_Eme_Wir_M_Scu -0.5 Rita_Bey_Com_F_Dra -0.5 Rita_Who_Com_F_Dra >= 0;
at_l_1_pht: +Enrique_Sto_Pho_M_Pai +Enrique_Har_Pho_M_Pai +David_Zig_Pho_M_Pai >= 1;
at_l_1_cub: +David_Zig_Cub_M_Pai +Candy_Stu_Cub_F_Pai +C28 >= 1;
at_l_1_expt: +David_Zig_Exp_M_Pai = 1;
at_l_1_wcol: +Helen_Ser_Wat_F_Pai +Helen_Cal_Wat_F_Pai +Valentino_All_Wat_M_Pai +Valentino_The_Wat_M_Pai +Valentino_Win_Wat_M_Pai
 +Valentino_107_Wat_M_Pai >= 1;
at_l_1_oil: +Robert_Voi_Oil_M_Pai +Robert_Sun_Oil_M_Pai +Bill_Whe_Oil_M_Pai +Bill_Pio_Oil_M_Pai +Stuart_Liv_Oil_M_Pai >= 1;
paint<=3non: -3 Nicholas_Per_Wir_M_Scu -3 Nicholas_Bur_Wir_M_Scu -3 Nicholas_Eme_Wir_M_Scu -3 Rita_Bey_Com_F_Dra -3 Rita_Who_Com_F_Dra
 -3 Rita_Dom_Ink_F_Dra -3 Rita_Inn_Ink_F_Dra -3 Norm_Agi_Scu_M_Scu -3 Norm_Was_Col_M_Col +Helen_Ser_Wat_F_Pai
 +Helen_Cal_Wat_F_Pai +Robert_Voi_Oil_M_Pai +Robert_Sun_Oil_M_Pai +Enrique_Sto_Pho_M_Pai +Enrique_Har_Pho_M_Pai
 -3 Angie_Con_Col_F_Col -3 Angie_Ref_Scu_F_Scu -3 Angie_Tro_Scu_F_Scu +David_Zig_Pho_M_Pai +David_Zig_Cub_M_Pai
 +David_Zig_Exp_M_Pai +Bill_Whe_Oil_M_Pai +Bill_Pio_Oil_M_Pai -3 Stuart_Wis_Ink_M_Dra -3 Stuart_Sup_Ink_M_Dra
 +Stuart_Liv_Oil_M_Pai +Candy_Stu_Cub_F_Pai +C28 -3 Rick_My _Col_M_Col -3 Rick_Nar_Col_M_Col +Valentino_All_Wat_M_Pai
 +Valentino_The_Wat_M_Pai +Valentino_Win_Wat_M_Pai +Valentino_107_Wat_M_Pai <= 0;
valentino: +Valentino_All_Wat_M_Pai +Valentino_The_Wat_M_Pai +Valentino_Win_Wat_M_Pai +Valentino_107_Wat_M_Pai = 4;
helen: +Helen_Ser_Wat_F_Pai +Helen_Cal_Wat_F_Pai = 2;
David=Enrique: -Enrique_Sto_Pho_M_Pai -Enrique_Har_Pho_M_Pai +David_Zig_Pho_M_Pai +David_Zig_Cub_M_Pai +David_Zig_Exp_M_Pai = 0;
at_l_1_Darcy: +David_Zig_Pho_M_Pai +David_Zig_Cub_M_Pai +David_Zig_Exp_M_Pai >= 1;
Rick<=1: +Rick_My _Col_M_Col +Rick_Nar_Col_M_Col <= 1;
3Fem=Male: +Nicholas_Per_Wir_M_Scu +Nicholas_Bur_Wir_M_Scu +Nicholas_Eme_Wir_M_Scu -3 Rita_Bey_Com_F_Dra -3 Rita_Who_Com_F_Dra
 -3 Rita_Dom_Ink_F_Dra -3 Rita_Inn_Ink_F_Dra +Norm_Agi_Scu_M_Scu +Norm_Was_Col_M_Col -3 Helen_Ser_Wat_F_Pai
 -3 Helen_Cal_Wat_F_Pai +Robert_Voi_Oil_M_Pai +Robert_Sun_Oil_M_Pai +Enrique_Sto_Pho_M_Pai +Enrique_Har_Pho_M_Pai
 -3 Angie_Con_Col_F_Col -3 Angie_Ref_Scu_F_Scu -3 Angie_Tro_Scu_F_Scu +David_Zig_Pho_M_Pai +David_Zig_Cub_M_Pai
 +David_Zig_Exp_M_Pai +Bill_Whe_Oil_M_Pai +Bill_Pio_Oil_M_Pai +Stuart_Wis_Ink_M_Dra +Stuart_Sup_Ink_M_Dra
 +Stuart_Liv_Oil_M_Pai -3 Candy_Stu_Cub_F_Pai -3 C28 +Rick_My _Col_M_Col +Rick_Nar_Col_M_Col +Valentino_All_Wat_M_Pai
 +Valentino_The_Wat_M_Pai +Valentino_Win_Wat_M_Pai +Valentino_107_Wat_M_Pai <= 0;
AErth_a_o_WR: +Norm_Agi_Scu_M_Scu +Norm_Was_Col_M_Col >= 1;
at_l_1_Stuart: +Stuart_Wis_Ink_M_Dra +Stuart_Sup_Ink_M_Dra +Stuart_Liv_Oil_M_Pai >= 1;
at_l_1_4_pieces: +Rita_Bey_Com_F_Dra +Rita_Who_Com_F_Dra +Bill_Whe_Oil_M_Pai +Bill_Pio_Oil_M_Pai >= 1;
max_4_scul: +Nicholas_Per_Wir_M_Scu +Nicholas_Bur_Wir_M_Scu +Nicholas_Eme_Wir_M_Scu +Norm_Agi_Scu_M_Scu +Angie_Ref_Scu_F_Scu
 +Angie_Tro_Scu_F_Scu <= 4;
max_20_n_scul: +Rita_Bey_Com_F_Dra +Rita_Who_Com_F_Dra +Rita_Dom_Ink_F_Dra +Rita_Inn_Ink_F_Dra +Norm_Was_Col_M_Col +Helen_Ser_Wat_F_Pai
 +Helen_Cal_Wat_F_Pai +Robert_Voi_Oil_M_Pai +Robert_Sun_Oil_M_Pai +Enrique_Sto_Pho_M_Pai +Enrique_Har_Pho_M_Pai
 +Angie_Con_Col_F_Col +David_Zig_Pho_M_Pai +David_Zig_Cub_M_Pai +David_Zig_Exp_M_Pai +Bill_Whe_Oil_M_Pai
 +Bill_Pio_Oil_M_Pai +Stuart_Wis_Ink_M_Dra +Stuart_Sup_Ink_M_Dra +Stuart_Liv_Oil_M_Pai +Candy_Stu_Cub_F_Pai
 +C28 +Rick_My _Col_M_Col +Rick_Nar_Col_M_Col +Valentino_All_Wat_M_Pai +Valentino_The_Wat_M_Pai +Valentino_Win_Wat_M_Pai
 +Valentino_107_Wat_M_Pai <= 20;
Narc_with_Refl: -Angie_Ref_Scu_F_Scu +Rick_Nar_Col_M_Col <= 0;
At_l_20_Pieces: +Nicholas_Per_Wir_M_Scu +Nicholas_Bur_Wir_M_Scu +Nicholas_Eme_Wir_M_Scu +Rita_Bey_Com_F_Dra +Rita_Who_Com_F_Dra
 +Rita_Dom_Ink_F_Dra +Rita_Inn_Ink_F_Dra +Norm_Agi_Scu_M_Scu +Norm_Was_Col_M_Col +Helen_Ser_Wat_F_Pai
 +Helen_Cal_Wat_F_Pai +Robert_Voi_Oil_M_Pai +Robert_Sun_Oil_M_Pai +Enrique_Sto_Pho_M_Pai +Enrique_Har_Pho_M_Pai
 +Angie_Con_Col_F_Col +Angie_Ref_Scu_F_Scu +Angie_Tro_Scu_F_Scu +David_Zig_Pho_M_Pai +David_Zig_Cub_M_Pai
 +David_Zig_Exp_M_Pai +Bill_Whe_Oil_M_Pai +Bill_Pio_Oil_M_Pai +Stuart_Wis_Ink_M_Dra +Stuart_Sup_Ink_M_Dra
 +Stuart_Liv_Oil_M_Pai +Candy_Stu_Cub_F_Pai +C28 +Rick_My _Col_M_Col +Rick_Nar_Col_M_Col +Valentino_All_Wat_M_Pai
 +Valentino_The_Wat_M_Pai +Valentino_Win_Wat_M_Pai +Valentino_107_Wat_M_Pai >= 20;

/* Variable bounds */
Nicholas_Per_Wir_M_Scu <= 1;
Nicholas_Bur_Wir_M_Scu <= 1;
Nicholas_Eme_Wir_M_Scu <= 1;
Rita_Bey_Com_F_Dra <= 1;
Rita_Who_Com_F_Dra <= 1;
Rita_Dom_Ink_F_Dra <= 1;
Rita_Inn_Ink_F_Dra <= 1;
Norm_Agi_Scu_M_Scu <= 1;
Norm_Was_Col_M_Col <= 1;
Helen_Ser_Wat_F_Pai <= 1;
Helen_Cal_Wat_F_Pai <= 1;
Robert_Voi_Oil_M_Pai <= 1;
Robert_Sun_Oil_M_Pai <= 1;
Enrique_Sto_Pho_M_Pai <= 1;
Enrique_Har_Pho_M_Pai <= 1;
Angie_Con_Col_F_Col <= 1;
Angie_Ref_Scu_F_Scu <= 1;
Angie_Tro_Scu_F_Scu <= 1;
David_Zig_Pho_M_Pai <= 1;
David_Zig_Cub_M_Pai <= 1;
David_Zig_Exp_M_Pai <= 1;
Bill_Whe_Oil_M_Pai <= 1;
Bill_Pio_Oil_M_Pai <= 1;
Stuart_Wis_Ink_M_Dra <= 1;
Stuart_Sup_Ink_M_Dra <= 1;
Stuart_Liv_Oil_M_Pai <= 1;
Candy_Stu_Cub_F_Pai <= 1;
C28 <= 1;
Rick_My _Col_M_Col <= 1;
Rick_Nar_Col_M_Col <= 1;
Valentino_All_Wat_M_Pai <= 1;
Valentino_The_Wat_M_Pai <= 1;
Valentino_Win_Wat_M_Pai <= 1;
Valentino_107_Wat_M_Pai <= 1;

/* Integer definitions */
int Nicholas_Per_Wir_M_Scu,Nicholas_Bur_Wir_M_Scu,Nicholas_Eme_Wir_M_Scu,Rita_Bey_Com_F_Dra,Rita_Who_Com_F_Dra,Rita_Dom_Ink_F_Dra,Rita_Inn_Ink_F_Dra,Norm_Agi_Scu_M_Scu,Norm_Was_Col_M_Col,Helen_Ser_Wat_F_Pai,Helen_Cal_Wat_F_Pai,Robert_Voi_Oil_M_Pai,Robert_Sun_Oil_M_Pai,Enrique_Sto_Pho_M_Pai,Enrique_Har_Pho_M_Pai,Angie_Con_Col_F_Col,Angie_Ref_Scu_F_Scu,Angie_Tro_Scu_F_Scu,David_Zig_Pho_M_Pai,David_Zig_Cub_M_Pai,David_Zig_Exp_M_Pai,Bill_Whe_Oil_M_Pai,Bill_Pio_Oil_M_Pai,Stuart_Wis_Ink_M_Dra,Stuart_Sup_Ink_M_Dra,Stuart_Liv_Oil_M_Pai,Candy_Stu_Cub_F_Pai,C28,Rick_My _Col_M_Col,Rick_Nar_Col_M_Col,Valentino_All_Wat_M_Pai,Valentino_The_Wat_M_Pai,Valentino_Win_Wat_M_Pai,Valentino_107_Wat_M_Pai;
