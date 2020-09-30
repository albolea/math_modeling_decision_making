/* Objective function */
min: +Perfection +Burden +Emergence +Beyond +Who Has Control? +Domestication +Innocence +Aging Earth +Wasted Resources
 +Serenity +Calm Before the Storm +Void +Sun +Storefront Window +Harley +Consumerism +Reflection +Trojan Victory
 +Ziggy +Ziggy II +Ziggy III +Where are we now? +Pioneers +Wisdom +Superior Powers +Living Land +Study of a Violin
 +Study of a Fruit Bowl +My Namesake +Narcissism +All That Glitters +The Rock +Winding Road +1071 Fifth Ave;

/* Constraints */
1_coll: +Wasted Resources +Consumerism +My Namesake +Narcissism = 1;
at_l_1_wire_if_comp: +Perfection +Burden +Emergence >= 0;
at_l_1_comp_if_wire: +Beyond +Who Has Control? >= 0;
at_l_1_pht: +Storefront Window +Harley +Ziggy >= 1;
at_l_1_cub: +Ziggy II +Study of a Violin +Study of a Fruit Bowl >= 1;
at_l_1_expt: +Ziggy III >= 1;
at_l_1_wcol: +Serenity +Calm Before the Storm +All That Glitters +The Rock +Winding Road +1071 Fifth Ave >= 1;
at_l_1_oil: +Void +Sun +Where are we now? +Pioneers +Living Land >= 1;
paint<=2non: -2 Perfection -2 Burden -2 Emergence -2 Beyond -2 Who Has Control? -2 Domestication -2 Innocence -2 Aging Earth
 -2 Wasted Resources +Serenity +Calm Before the Storm +Void +Sun +Storefront Window +Harley -2 Consumerism
 -2 Reflection -2 Trojan Victory +Ziggy +Ziggy II +Ziggy III +Where are we now? +Pioneers -2 Wisdom -2 Superior Powers
 +Living Land +Study of a Violin +Study of a Fruit Bowl -2 My Namesake -2 Narcissism +All That Glitters
 +The Rock +Winding Road +1071 Fifth Ave <= 0;
valentina: +All That Glitters +The Rock +Winding Road +1071 Fifth Ave = 4;
hector: +Serenity +Calm Before the Storm = 2;
Darcy=Emma: -Perfection -Burden -Emergence -Beyond -Who Has Control? -Domestication -Innocence -Aging Earth -Wasted Resources
 -Serenity -Calm Before the Storm -Void -Sun -Consumerism -Reflection -Trojan Victory -Where are we now?
 -Pioneers -Wisdom -Superior Powers -Living Land -Study of a Violin -Study of a Fruit Bowl -My Namesake
 -Narcissism -All That Glitters -The Rock -Winding Road -1071 Fifth Ave = 0;
at_l_1_Darcy: +Ziggy +Ziggy II +Ziggy III >= 1;
Rick<=1: +My Namesake +Narcissism <= 1;
2Fem=Male: +Perfection +Burden +Emergence -2 Beyond -2 Who Has Control? -2 Domestication -2 Innocence -2 Aging Earth
 -2 Wasted Resources +Serenity +Calm Before the Storm +Void +Sun +Storefront Window +Harley -2 Consumerism
 -2 Reflection -2 Trojan Victory -2 Ziggy -2 Ziggy II -2 Ziggy III +Where are we now? +Pioneers +Wisdom
 +Superior Powers +Living Land -2 Study of a Violin -2 Study of a Fruit Bowl +My Namesake +Narcissism
 -2 All That Glitters -2 The Rock -2 Winding Road -2 1071 Fifth Ave <= 0;
AErth_a_o_WR: +Aging Earth +Wasted Resources >= 1;
at_l_1_Stuart: +Wisdom +Superior Powers +Living Land >= 1;
at_l_1_4_pieces: +Beyond +Where are we now? +Pioneers >= 1;
max_4_scul: +Perfection +Burden +Emergence +Aging Earth +Reflection +Trojan Victory <= 4;
max_20_n_scul: +Beyond +Who Has Control? +Domestication +Innocence +Wasted Resources +Serenity +Calm Before the Storm
 +Void +Sun +Storefront Window +Harley +Consumerism +Ziggy +Ziggy II +Ziggy III +Where are we now? +Pioneers
 +Wisdom +Superior Powers +Living Land +Study of a Violin +Study of a Fruit Bowl +My Namesake +Narcissism
 +All That Glitters +The Rock +Winding Road +1071 Fifth Ave <= 20;
Narc_with_Refl: +Reflection +Narcissism = 0;

/* Variable bounds */
Perfection <= 1;
Burden <= 1;
Emergence <= 1;
Beyond <= 1;
Who Has Control? <= 1;
Domestication <= 1;
Innocence <= 1;
Aging Earth <= 1;
Wasted Resources <= 1;
Serenity <= 1;
Calm Before the Storm <= 1;
Void <= 1;
Sun <= 1;
Storefront Window <= 1;
Harley <= 1;
Consumerism <= 1;
Reflection <= 1;
Trojan Victory <= 1;
Ziggy <= 1;
Ziggy II <= 1;
Ziggy III <= 1;
Where are we now? <= 1;
Pioneers <= 1;
Wisdom <= 1;
Superior Powers <= 1;
Living Land <= 1;
Study of a Violin <= 1;
Study of a Fruit Bowl <= 1;
My Namesake <= 1;
Narcissism <= 1;
All That Glitters <= 1;
The Rock <= 1;
Winding Road <= 1;
1071 Fifth Ave <= 1;
0 <= 1;

/* Integer definitions */
int Perfection,Burden,Emergence,Beyond,Who Has Control?,Domestication,Innocence,Aging Earth,Wasted Resources,Serenity,Calm Before the Storm,Void,Sun,Storefront Window,Harley,Consumerism,Reflection,Trojan Victory,Ziggy,Ziggy II,Ziggy III,Where are we now?,Pioneers,Wisdom,Superior Powers,Living Land,Study of a Violin,Study of a Fruit Bowl,My Namesake,Narcissism,All That Glitters,The Rock,Winding Road,1071 Fifth Ave,0;
