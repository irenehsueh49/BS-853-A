libname project "C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 853 - Generalized Linear Models with Applications/Project";
proc format;
	value sex_format 		1="Male"			2="Female";
	value bp_meds_format	1="BP Meds"			0="No BP Meds";
	value chol_format		1="<200"			2="200 - 219"		3="220 - 259"		4="260+";
	value smoking_format	1="Smoker"			0="Not Smoker";
	value cigarette_format	1="0 Cigarettes"	2="1-10 Cigarettes"	3="11-20 Cigarettes" 4="21+ Cigarettes";
	value diabetes_format	1="Diabetes"		0="No Diabetes";
	value cvd_format		1="CVD"				0="No CVD";
	value chd_format		1="CHD"				0="No CHD";
	value stroke_format		1="Stroke"			0="No Stroke";
run;

data framingham;
	set project.framingham
		(keep = randid sex age period sysbp diabp bpmeds heartrte totchol 
				cursmoke cigpday bmi glucose diabetes 
				cvd anychd stroke timechd timecvd timestrk
		rename=(randid=id sex=sex age=age period=exam 
				sysbp=sbp diabp=dbp bpmeds=bp_meds heartrte=bpm totchol=total_chol 
				cursmoke=smoking_status cigpday=cigarettes bmi=bmi glucose=glucose diabetes=diabetes
				cvd=cvd anychd=chd stroke=stroke 
				timecvd=time_cvd timechd=time_chd timestrk=time_stroke));
	attrib 
		id 				label="ID"
		sex 			label="Sex"										format=sex_format.
		age 			label="Age"
		exam			label="Exam #"
		sbp				label="Systolic Blood Pressure"
		dbp 			label="Diastolic Blood Pressure"
		bp_meds			label="Use of BP Meds"							format=bp_meds_format.
		bpm				label="Heart Rate (bpm)"
		total_chol		label="Serum Total Cholesterol"
		chol_cat		label="Cholesterol Category"					format=chol_format.
		smoking_status	label="Smoking Status"							format=smoking_format.
		cigarettes		label="Number of Cigarettes Smoked per Day"
		cigarette_cat	label="Cigarettes Per Day Category"				format=cigarette_format.
		bmi 			label="BMI"			
		glucose			label="Serum Glucose"
		diabetes		label="Diabetes"								format=diabetes_format.
		cvd 			label="Cardiovascular Disease"					format=cvd_format.
		chd 			label="Coronary Heart Disease"					format=chd_format.
		stroke 			label="Stroke"									format=stroke_format.
		time_cvd 		label="Time to CVD (Days)"
		time_chd 		label="Time to CHD (Days)"
		time_stroke 	label="Time to Stroke (Days)"
	;
	if 				   total_chol < 200 	then chol_cat=1;
	else if 	200 <= total_chol < 220 	then chol_cat=2;
	else if 	220 <= total_chol < 260 	then chol_cat=3;
	else if 		   total_chol >= 260 	then chol_cat=4;

	if 				  cigarettes = 0 		then cigarette_cat=1;	
	else if 	1 <=  cigarettes <= 10 		then cigarette_cat=2;
	else if 	11 <= cigarettes <= 20 		then cigarette_cat=3;
	else if 		  cigarettes >= 21 		then cigarette_cat=4;

	log_age = log(age);
	log_cigarettes = log(cigarettes + 1);
	log_sbp = log(sbp);
	log_chol = log(total_chol);
	log_bmi = log(bmi);
run;

proc print data=framingham (obs=20) label;
run;



/* Descriptive Statistics */
title "Overall Descriptive Statistics";
proc freq data=framingham;
	where exam=1;
	tables bp_meds chol_cat smoking_status cigarette_cat diabetes cvd chd stroke / nocum;
run;
title;

title "Overall Descriptive Statistics";
proc means data=framingham maxdec=2;
	where exam=1;
	var age sbp dbp bpm total_chol cigarettes bmi glucose time_cvd time_chd time_stroke;
run;
title;



proc sort data=framingham;
	by sex;
run;

title "Descriptive Statistics by Sex";
proc freq data=framingham;
	where exam=1;
	tables bp_meds*sex chol_cat*sex smoking_status*sex cigarette_cat*sex 
		   diabetes*sex cvd*sex chd*sex stroke*sex / nopercent norow nocum chisq;
run;
title;

title "Descriptive Statistics by Sex";
proc means data=framingham maxdec=2;
	where exam=1;
	by sex;
	var age sbp dbp bpm total_chol cigarettes bmi glucose time_cvd time_chd time_stroke;
run;
title;

title "Two-Sample T-Test by Sex";
proc ttest data=framingham;
	where exam=1;
	class sex;
	var age sbp dbp bpm total_chol cigarettes bmi glucose time_cvd time_chd time_stroke;
run;
title;




/* Data Exploration */
title "Checking Distribution of Continuous Variables and Identifying Outliers"; 
proc univariate data=framingham plots;
	where exam=1;
	var age log_age cigarettes log_cigarettes sbp log_sbp total_chol log_chol bmi log_bmi;
	histogram / normal;
run;
title;

title "Correlation Between Variables";
proc corr data=framingham pearson spearman;
	where exam=1;
	var total_chol cigarette_cat sex age sbp bp_meds diabetes bmi;
run;
title;




/* Final Cox Proportional Hazards Models */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham zph;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham zph;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham zph;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;


ODS HTML close;
ODS HTML;



/* Model A1: Cholesterol Category, Smoking Status, Sex, Age, SBP, BP_Meds, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") smoking_status(ref="Not Smoker") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = chol_cat smoking_status sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") smoking_status(ref="Not Smoker") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = chol_cat smoking_status sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") smoking_status(ref="Not Smoker") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = chol_cat smoking_status sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;



/* Model A2: Total Cholesterol, Smoking Status, Sex, Age, SBP, BP_Meds, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class smoking_status(ref="Not Smoker") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol smoking_status sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class smoking_status(ref="Not Smoker") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol smoking_status sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class smoking_status(ref="Not Smoker") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol smoking_status sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;



/* Model A3: Cholesterol Category, Cigarettes, Sex, Age, SBP, BP_Meds, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = chol_cat cigarettes sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") sex(ref="Female") bp_meds(ref="No BP Meds") smoking_status(ref="Not Smoker") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = chol_cat cigarettes sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") sex(ref="Female") bp_meds(ref="No BP Meds") smoking_status(ref="Not Smoker") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = chol_cat cigarettes sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;



/* Model A4: Total Cholesterol, Cigarettes, Sex, Age, SBP, BP_Meds, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarettes sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarettes sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarettes sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;



/* Model A5: Cholesterol Category, Cigarette Category, Sex, Age, SBP, BP_Meds, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = chol_cat cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = chol_cat cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class chol_cat(ref="<200") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = chol_cat cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;



/* Model A6: Total Cholesterol, Cigarette Category, Sex, Age, SBP, BP_Meds, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;



ODS HTML close;
ODS HTML;



/* Model B1: Total Cholesterol, Cigarette Category, Sex, Age, SBP, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp diabetes bmi / risklimits;
run;
title;



/* Model B2: Total Cholesterol, Cigarette Category, Sex, Age, BP_Meds, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age bp_meds diabetes bmi / risklimits;
run;
title;



/* Model B3: Total Cholesterol, Cigarette Category, Sex, Age, SBP, BP_Meds BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds bmi / risklimits;
run;
title;



/* Model B4: Total Cholesterol, Cigarette Category, Sex, Age, SBP, BP_Meds, Diabetes */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes / risklimits;
run;
title;



/* Model B5: Total Cholesterol, Cigarette Category, Sex, Age, SBP, BP_Meds */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds / risklimits;
run;
title;



/* Model B6: Total Cholesterol, Cigarette Category, Sex, Age, SBP */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp / risklimits;
run;
title;



/* Model B7: Total Cholesterol, Cigarette Category, Sex, Age, BP_Meds */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age bp_meds / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age bp_meds / risklimits;
run;
title;

title "Cox Proportion Hazards Model for stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age bp_meds / risklimits;
run;
title;



/* Model B8: Total Cholesterol, Cigarette Category, Sex, Age, Diabetes, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age diabetes bmi / risklimits;
run;
title;



/* Model B9: Total Cholesterol, Cigarette Category, Sex, Age, Diabetes */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age diabetes / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age diabetes / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age diabetes / risklimits;
run;
title;



/* Model B10: Total Cholesterol, Cigarette Category, Sex, Age, BMI */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age bmi / risklimits;
run;
title;



/* Model B11: Total Cholesterol, Cigarette Category, Sex, Age */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age / risklimits;
run;
title;



ODS HTML close;
ODS HTML;



/* Model C: Logarithmically Transformed Continuous Variables */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = log_chol cigarette_cat sex log_age log_sbp bp_meds diabetes log_bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = log_chol cigarette_cat sex log_age log_sbp bp_meds diabetes log_bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = log_chol cigarette_cat sex log_age log_sbp bp_meds diabetes log_bmi / risklimits;
run;
title;



ODS HTML close;
ODS HTML;



/* Model D1: Interaction Between Total Cholesterol and Age */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi total_chol*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi total_chol*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi total_chol*age / risklimits;
run;
title;



/* Model D2: Interaction Between Total Cholesterol and Sex */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp diabetes bmi total_chol*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi total_chol*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi total_chol*sex / risklimits;
run;
title;



/* Model D3: Interaction Between Cigarette Category and Age */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi cigarette_cat*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi cigarette_cat*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi cigarette_cat*age / risklimits;
run;
title;



/* Model D4: Interaction Between Cigarette Category and Sex */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi cigarette_cat*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi cigarette_cat*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi cigarette_cat*sex / risklimits;
run;
title;



/* Model D5: Interaction Between SBP and Age */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi sbp*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi sbp*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi sbp*age / risklimits;
run;
title;



/* Model D6: Interaction Between SBP and Sex */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi sbp*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi sbp*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi sbp*sex / risklimits;
run;
title;



/* Model D7: Interaction Between Diabetes and Age */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi diabetes*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi diabetes*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi diabetes*age / risklimits;
run;
title;



/* Model D8: Interaction Between Diabetes and Sex */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi diabetes*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi diabetes*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi diabetes*sex / risklimits;
run;
title;



/* Model D9: Interaction Between BMI and Age */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi bmi*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi bmi*age / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi bmi*age / risklimits;
run;
title;



/* Model D10: Interaction Between Diabetes and Sex */
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi bmi*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi bmi*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat sex age sbp bp_meds diabetes bmi bmi*sex / risklimits;
run;
title;



/* Model D11: Interaction Between Total Cholesterol and Cigarette Category*/
title "Cox Proportion Hazards Model for CVD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol | cigarette_cat sex age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol | cigarette_cat sex age sbp bp_meds diabetes bmi bmi*sex / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke";
proc phreg data=framingham;
	where exam=1;
	class cigarette_cat(ref="0 Cigarettes") sex(ref="Female") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol | cigarette_cat sex age sbp bp_meds diabetes bmi bmi*sex / risklimits;
run;
title;



ODS HTML close;
ODS HTML;



/* Model Cox Proportional Hazards for Males */
title "Cox Proportion Hazards Model for CVD among Males";
proc phreg data=framingham;
	where exam=1 and sex=1;
	class cigarette_cat(ref="0 Cigarettes") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD among Males";
proc phreg data=framingham;
	where exam=1 and sex=1;
	class cigarette_cat(ref="0 Cigarettes") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke among Males";
proc phreg data=framingham;
	where exam=1 and sex=1;
	class cigarette_cat(ref="0 Cigarettes") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat age sbp bp_meds diabetes bmi / risklimits;
run;
title;



/* Cox Proportional Hazards for Females */
title "Cox Proportion Hazards Model for CVD among Females";
proc phreg data=framingham;
	where exam=1 and sex=2;
	class cigarette_cat(ref="0 Cigarettes") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_cvd*cvd(0) = total_chol cigarette_cat age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for CHD among Females";
proc phreg data=framingham;
	where exam=1 and sex=2;
	class cigarette_cat(ref="0 Cigarettes") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_chd*chd(0) = total_chol cigarette_cat age sbp bp_meds diabetes bmi / risklimits;
run;
title;

title "Cox Proportion Hazards Model for Stroke among Females";
proc phreg data=framingham;
	where exam=1 and sex=2;
	class cigarette_cat(ref="0 Cigarettes") bp_meds(ref="No BP Meds") diabetes(ref="No Diabetes") / param=ref;
	model time_stroke*stroke(0) = total_chol cigarette_cat age sbp bp_meds diabetes bmi / risklimits;
run;
title;
