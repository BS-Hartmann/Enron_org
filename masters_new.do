use "/Users/hartmabs/Box Sync/NAAS/NAAS/naas_2202019.dta", replace

keep if race == 1 | race == 6

	rename q2_2d blm_final
	recode blm (0 1 = 0) (2 3 = 1), gen(blm2)
    recode cat_effectall_aapi (0 1 = 0) (2 3 = 1), gen(race_linked)
    recode cat_effectall_ethnic (0 1 = 0) (2 3 = 1), gen(eth_linked)
	recode race_important (0 1 = 0) (2 3 = 1)
	recode ethnic_important (0 1 = 0) (2 3 = 1)

	gen dream = q2_2f1
		replace dream = q2_2f2
		recode dream (1 2 = 0) (3 4 = 1) (5 6 = .)
	recode partyid (0 = 0 "Democrat") (1=1 "Republican") (2 3 = 2 "Other"), gen(pid)

drop if missing(money_worries, region, usborn, sec_gen, pid, educ, female, ///
age, race_important, ethnic_important, effectall_aapi, effectall_ethnic)

desctable i.race_important i.ethnic_important i.effectall_a ///
	i.effectall_e i.age i.female i.educ i.pid ///
	i.money_worries i.region i.usborn i.sec_gen, group(race) ///
	filename(model_var) ///
	stats(mean freq sd min max iqr median)

desctable i.blm_final, group(race) filename(outcome_var) 

local dep blm_final
local dep2 blm2
local inv i.race_important i.ethnic_important i.race_linked i.eth_linked 
local control i.usborn i.sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region

	logit `dep2' (`inv')##i.race `control' 
		capture gen samp = 1 if e(sample)==1
		est store blm_interaction
	logit `dep2' `inv' i.race `control' if samp == 1
		est store blm_add
	logit `dep2' `inv' `control' if samp == 1
		est store blm_base	
	logit `dep2' `control' if samp == 1
		est store blm_control
	logit `dep2' `inv' if samp==1
		est store blm_inv 

esttab blm_inv blm_control blm_base blm_add blm_interaction using Table1.csv, ///
replace n pr2 aic bic eform nobaselevels compress ///
mtitles("Model 1" "Model 2" "Model 3") ///
addnote("Source: NAAS 2016") b(%4.3f) se(%4.3f) ///
coeflabels(1.race_important "Racial Identity" 1.ethnic_important "Ethnic Identity" ///
1.race_linked "Racial Linked fate" 1.eth_linked "Ethnic Linked fate" ///
usborn "US Born" sec_gen "2nd Gen Immigrant" 1.age "Age: over 35" 1.female "Female" ///
educ "Education" 1.pid "Republican" 2.pid "Other Party" money_worries "Finacial Concerns" ///
1.region "Northeast" 2.region "Midwest" 3.region "South")

capture drop blm_*
foreach num in 1 6 {
	clonevar blm_`num' = blm2
		replace blm_`num' = . if race != `num'
	}
	
local dep blm_final
local dep2 blm2
local inv i.race_important i.ethnic_important i.race_linked i.eth_linked 
local control i.usborn i.sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region
	
gsem (blm_1 <- `inv' `control', logit) /// 
     (blm_6 <- `inv' `control', logit), vce(robust) 
	 est store gsemmodel	
	 
esttab gsemmodel using tabl2.csv, unstack aic bic ///
replace n pr2 eform nobaselevels compress ///
addnote("Source: NAAS 2016") b(%4.3f) se(%4.3f) ///
coeflabels(1.race_important "Racial Identity" 1.ethnic_important "Ethnic Identity" ///
1.race_linked "Racial Linked fate" 1.eth_linked "Ethnic Linked fate" ///
1.usborn "US Born" 1.sec_gen "2nd Gen Immigrant" 1.age "Age: over 35" 1.female "Female" ///
educ "Education" 1.pid "Republican" 2.pid "Other Party" money_worries "Finacial Concerns" ///
1.region "Northeast" 2.region "Midwest" 3.region "South")
	
drop blm_1 blm_6
	 
foreach num in 1 2 3 4 5 6 7 8 10 11 {
	clonevar blm_`num' = blm2
		replace blm_`num' = . if ethnicity != `num'
	}

gsem (blm_1 <-  `inv' `control', logit) /// 
	 (blm_2 <-  `inv' `control', logit) /// 
	 (blm_3 <-  `inv' `control', logit) /// 
	 (blm_4 <-  `inv' `control', logit) /// 
	 (blm_5 <-  `inv' `control', logit) /// 
	 (blm_6 <-  `inv' `control', logit) /// 
	 (blm_7 <-  `inv' `control', logit) /// 
	 (blm_8  <- `inv' `control', logit) /// 
	 (blm_10 <- `inv' `control', logit) /// 
     (blm_11 <- `inv' `control', logit), vce(robust) 
	 est store gsemmodel2	
	 
esttab gsemmodel2 using tabl3.csv, unstack aic bic ///
replace n pr2 eform nobaselevels compress ///
addnote("Source: NAAS 2016" "Model also controls for immigration, age, gender, political party, region, and economic situtation") b(%4.3f) se(%4.3f) ///
keep(1.race_important 1.ethnic_important 1.race_linked 1.eth_linked) ///
coeflabels(race_important "Racial Identity" ethnic_important "Ethnic Identity" ///
race_linked "Racial Linked fate" eth_linked "Ethnic Linked fate" ///
1.usborn "US Born" 1.sec_gen "2nd Gen Immigrant" 1.age "Age: over 35" 1.female "Female" /// 
educ "Education" 1.pid "Republican" 2.pid "Other Party" money_worries "Finacial Concerns" ///
1.region "Northeast" 2.region "Midwest" 3.region "South")

est restore gsemmodel
	qui margins, dydx(race_important ethnic_important) post
	est store ind
		 
mlincom, clear
	qui mlincom 1,            rowname("Asian: Racial") stat(est se p) clear 
	qui mlincom 3,            rowname("Asian: Ethnicity") stat(est se p) add 
	qui mlincom 1 - 3,        rowname("Asian: Diff") stat(est se p) add 
	qui mlincom 2,            rowname("Latino: Racial") stat(est se p) add 
	qui mlincom 4,            rowname("Latino: Ethnic") stat(est se p) add 
	qui mlincom 2 - 4,        rowname("Latino: Diff") stat(est se p) add
	qui mlincom (1-3)-(2-4),  rowname("Second Diff: Racial-Ethnic") stat(est se p) add	
mlincom, stat(est se p) dec(3) twidth(15) ///
        title("Race - Ethnic: Identity")		

est restore gsemmodel
	margins race_important ethnic_important, post
	marginsplot, recast(scatter)
	est store ind
		 
mlincom, clear
	qui mlincom 1,            rowname("Asian: Racial") stat(est se p) clear 
	qui mlincom 3,            rowname("Asian: Ethnicity") stat(est se p) add 
	qui mlincom 1 - 3,        rowname("Asian: Diff") stat(est se p) add 
	qui mlincom 2,            rowname("Latino: Racial") stat(est se p) add 
	qui mlincom 4,            rowname("Latino: Ethnic") stat(est se p) add 
	qui mlincom 2 - 4,        rowname("Latino: Diff") stat(est se p) add
	qui mlincom (1-3)-(2-4),  rowname("Second Diff: Racial-Ethnic") stat(est se p) add	
mlincom, stat(est se p) dec(3) twidth(15) ///
        title("Race - Ethnic: Identity")		


est restore gsemmodel
	qui margins, dydx(race_important ethnic_important) predict(outcome(blm_1)) post
	est store ind_asian
est restore gsemmodel
	qui margins, dydx(race_important ethnic_important) predict(outcome(blm_6)) post
	est store ind_latino		
est restore gsemmodel
	qui margins, dydx(race_linked eth_linked) predict(outcome(blm_1)) post
	est store link_asian
est restore gsemmodel
	qui margins, dydx(race_linked eth_linked) predict(outcome(blm_6)) post
	est store link_latino
	
coefplot ind_asian ind_latino link_asian link_latino, vertical yline(0) ylabel(-0.1(.1).2) ///
	plotlabels("Racial Identity" "Ethnic Identity" "Race Linked" "Ethnicity Linked", wrap(13)) ///
	xlabel(.8 "AA: Identity" 1.8 "LA: Identity" 3.23 "AA: Linked" 4.23 "LA: Linked") ////
	msize(large) title("ADC on Probability of Supporting Black Lives Matters")

est restore gsemmodel
	qui margins, dydx(race_linked eth_linked) post
	est store ind
	
mlincom, clear
	qui mlincom 1,            rowname("Asian: Racial") stat(est se p) clear 
	qui mlincom 3,            rowname("Asian: Ethnicity") stat(est se p) add 
	qui mlincom 1 - 3,        rowname("Asian: Diff") stat(est se p) add 
	qui mlincom 2,            rowname("Latino: Racial") stat(est se p) add 
	qui mlincom 4,            rowname("Latino: Ethnic") stat(est se p) add 
	qui mlincom 2 - 4,        rowname("Latino: Diff") stat(est se p) add
	qui mlincom (1-3)-(2-4),  rowname("Second Diff: Racial-Ethnic") stat(est se p) add	
mlincom, stat(est se p) dec(3) twidth(15) ///
        title("Race - Ethnic: Linked Fate")
		
est restore gsemmodel2
	qui margins, dydx(race_important ethnic_important) post
mlincom, clear
	qui mlincom 1,            rowname("Bangladeshi: Racial") stat(est se p) clear 
	qui mlincom 11,           rowname("Bangladeshi: Ethnicity") stat(est se p) add 
	qui mlincom 1 - 11,       rowname("Bangladeshi: Diff") stat(est se p) add 
	qui mlincom 2,            rowname("Cambodian: Racial") stat(est se p) add 
	qui mlincom 12,           rowname("Cambodian: Ethnic") stat(est se p) add 
	qui mlincom 2 - 12,       rowname("Cambodian: Diff") stat(est se p) add
	qui mlincom 3,            rowname("Chinese: Racial") stat(est se p) add 
	qui mlincom 13,           rowname("Chinese: Ethnic") stat(est se p) add 
	qui mlincom 3-13,         rowname("Chinese: Diff") stat(est se p) add
	qui mlincom 4,            rowname("Filipino: Racial") stat(est se p) add 
	qui mlincom 14,           rowname("Filipino: Ethnic") stat(est se p) add 
	qui mlincom 4-14,         rowname("Filipino: Diff") stat(est se p) add
	qui mlincom 5,            rowname("Hmong: Racial") stat(est se p) add 
	qui mlincom 15,           rowname("Hmong: Ethnic") stat(est se p) add 
	qui mlincom 5-15,         rowname("Hmong: Diff") stat(est se p) add
	qui mlincom 6,            rowname("Indian: Racial") stat(est se p) add 
	qui mlincom 16,           rowname("Indian: Ethnic") stat(est se p) add 
	qui mlincom 6-16,         rowname("Indian: Diff") stat(est se p) add
	qui mlincom 7,            rowname("Japanese: Racial") stat(est se p) add 
	qui mlincom 17,           rowname("Japanese: Ethnic") stat(est se p) add 
	qui mlincom 7-17,         rowname("Japanese: Diff") stat(est se p) add
	qui mlincom 8,            rowname("Korean: Racial") stat(est se p) add 
	qui mlincom 18,           rowname("Korean: Ethnic") stat(est se p) add 
	qui mlincom 8-18,         rowname("Korean: Diff") stat(est se p) add
	qui mlincom 9,            rowname("Pakistani: Racial") stat(est se p) add 
	qui mlincom 19,           rowname("Pakistani: Ethnic") stat(est se p) add 
	qui mlincom 9-19,         rowname("Pakistani: Diff") stat(est se p) add
	qui mlincom 10,           rowname("Vietnamese: Racial") stat(est se p) add 
	qui mlincom 20,           rowname("Vietnamese: Ethnic") stat(est se p) add 
	qui mlincom 10-20,        rowname("Vietnamese: Diff") stat(est se p) add
mlincom, stat(est se p) dec(3) twidth(15) ///
        title("Race - Ethnic: Identity")
	 
est restore gsemmodel2
	qui margins, dydx(race_linked eth_linked) post
mlincom, clear
	qui mlincom 1,            rowname("Bangladeshi: Racial") stat(est se p) clear 
	qui mlincom 11,           rowname("Bangladeshi: Ethnicity") stat(est se p) add 
	qui mlincom 1 - 11,       rowname("Bangladeshi: Diff") stat(est se p) add 
	qui mlincom 2,            rowname("Cambodian: Racial") stat(est se p) add 
	qui mlincom 12,           rowname("Cambodian: Ethnic") stat(est se p) add 
	qui mlincom 2 - 12,       rowname("Cambodian: Diff") stat(est se p) add
	qui mlincom 3,            rowname("Chinese: Racial") stat(est se p) add 
	qui mlincom 13,           rowname("Chinese: Ethnic") stat(est se p) add 
	qui mlincom 3-13,         rowname("Chinese: Diff") stat(est se p) add
	qui mlincom 4,            rowname("Filipino: Racial") stat(est se p) add 
	qui mlincom 14,           rowname("Filipino: Ethnic") stat(est se p) add 
	qui mlincom 4-14,         rowname("Filipino: Diff") stat(est se p) add
	qui mlincom 5,            rowname("Hmong: Racial") stat(est se p) add 
	qui mlincom 15,           rowname("Hmong: Ethnic") stat(est se p) add 
	qui mlincom 5-15,         rowname("Hmong: Diff") stat(est se p) add
	qui mlincom 6,            rowname("Indian: Racial") stat(est se p) add 
	qui mlincom 16,           rowname("Indian: Ethnic") stat(est se p) add 
	qui mlincom 6-16,         rowname("Indian: Diff") stat(est se p) add
	qui mlincom 7,            rowname("Japanese: Racial") stat(est se p) add 
	qui mlincom 17,           rowname("Japanese: Ethnic") stat(est se p) add 
	qui mlincom 7-17,         rowname("Japanese: Diff") stat(est se p) add
	qui mlincom 8,            rowname("Korean: Racial") stat(est se p) add 
	qui mlincom 18,           rowname("Korean: Ethnic") stat(est se p) add 
	qui mlincom 8-18,         rowname("Korean: Diff") stat(est se p) add
	qui mlincom 9,            rowname("Pakistani: Racial") stat(est se p) add 
	qui mlincom 19,           rowname("Pakistani: Ethnic") stat(est se p) add 
	qui mlincom 9-19,         rowname("Pakistani: Diff") stat(est se p) add
	qui mlincom 10,           rowname("Vietnamese: Racial") stat(est se p) add 
	qui mlincom 20,           rowname("Vietnamese: Ethnic") stat(est se p) add 
	qui mlincom 10-20,        rowname("Vietnamese: Diff") stat(est se p) add
mlincom, stat(est se p) dec(3) twidth(15) ///
        title("Race - Ethnic: Linked Fate")
	 
local dep blm_final
local dep2 blm2
local inv i.race_important i.ethnic_important i.race_linked i.eth_linked 
local control i.usborn i.sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region
*gen total2 = 7 - total	
poisson total2 `inv' `control' if race == 1

local dep blm_final
local dep2 blm2
local inv c.race_important c.ethnic_important c.race_linked c.eth_linked 
local control i.usborn i.sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region
	
gsem (blm2 <- `inv' `control', logit), vce(robust)
	 est store gsemmodel3
	 
est restore gsemmodel3
	 margins, dydx(race_important ethnic_important race_linked eth_linked) post
	 
mlincom, clear
	qui mlincom 1,            rowname("Identity: Racial") stat(est se p) clear 
	qui mlincom 2,            rowname("Identity: Ethnicity") stat(est se p) add 
	qui mlincom 1 - 2,        rowname("Identity: Diff") stat(est se p) add 
	qui mlincom 3,            rowname("Linked: Racial") stat(est se p) add 
	qui mlincom 4,            rowname("Linked: Ethnic") stat(est se p) add 
	qui mlincom 3 - 4,        rowname("Linked: Diff") stat(est se p) add
mlincom, stat(est se p) dec(3) twidth(15) ///
        title("Race - Ethnic: Linked Fate")
	 
esttab gsemmodel3 using table4.csv, unstack aic bic ///
replace n pr2 eform nobaselevels compress ///
addnote("Source: NAAS 2016") b(%4.3f) se(%4.3f) ///
coeflabels(1.race_important "Racial Identity" 1.ethnic_important "Ethnic Identity" ///
1.race_linked "Racial Linked fate" 1.eth_linked "Ethnic Linked fate" ///
1.usborn "US Born" 1.sec_gen "2nd Gen Immigrant" 1.age "Age: over 35" 1.female "Female" ///
educ "Education" 1.pid "Republican" 2.pid "Other Party" money_worries "Finacial Concerns" ///
1.region "Northeast" 2.region "Midwest" 3.region "South")
