Conflicts of interest: I declare I have no conflicts of interest.

Contributions: As sole team member I conducted all stages of the research process. Thanks to Antonio Schettino for giving me feedback on my planned analytic strategy.

**ELIGIBILITY CHECKLIST** 

Please tick the following statements to confirm them. Please note that if your report does not satisfy all these statements it will be considered incomplete and **therefore not eligible for inclusion in the study.** 

O We have provided a written description of our analysis strategy. 

O We have described our model’s results in terms of AIC/DIC, odds ratios, 95% confidence intervals and p-values. 

**Description of analysis.** 

Please provide a short description of your analysis. This will be used to describe your analysis in the final manuscript. 

I employed Bayesian ordinal regression models to answer the question of whether computer use on weekdays and weekends at 16 was associated with depression at 18. In the data provided to use in this project, both computer use and depression were ordered categorical responses (e.g., less than an hour per day, less than two hours per day, three hours or more per day). "Ordinal models" refer to an appropriate statistical model to assess such associations between independent and dependent variables that are both ordinal. Bayesian ordinal models were employed on the basis that I was interested in the probability of the hypothesis given my data and my (weak) existing beliefs.

**Analytical choices.** 

Please explain the analytical choices you made during your analysis. You are asked to provide a rationale for each choice. We should be able to replicate your analysis from this description. 

1. Outcomes: 
   1. How did you define depression? The dep_scores variable was used to define depression.
   2. Why? [Free text description] 
2. Exposures: 
   1. How did you define computer use? The comp_use_1 and comp_use_2 variables were used to define computer use on the weekdays and weekends (respectively), in two separate models. 
   2. Why? [Free text description] 
3. Transformations 
   1. What transformations were applied to the variables? None.
   2. Why? Ordinal modelling allowed for the analysis of the data in its native format.
4. Outliers 
   1. How did you deal with outliers? Univariate outliers were assessed by counting the data present at each ordinal level. While skew was present, no variable contained obvious outliers.  
   2. Did you exclude any cases? No.
   3. What rules did you use for exclusion? Inspection of the count data for each ordinal level suggested no outliers were present. No follow-up quantitative metrics were applied.
   4. Why? 
5. Missing data 

a. How did you deal with missing data? [free text] b. Why? 

1. Covariates and controls 

   1. What variables were included as covariates or control variables in your 

      model? None.

   2. Why? Including additional variables such as depression at age 16 would serve to answer a different research question than that which was asked (e.g., is computer use at 16 associated with change in depression at 18).

2. Statistical models used 

   1. Name: Two Bayesian ordinal regression models using cumulative logistic link functions
   
   2. Please describe the model (include the model formula) 
   

Model 1: DV = depression, IV = computer use on weekdays.
```{r}
brm(formula      = depression ~ comp_use_1,
    data         = data,
    family       = cumulative("logit"),
    sample_prior = TRUE,
    iter         = 2000,
    chains       = 4,
    control      = list(adapt_delta = 0.95))
```

Model 2: DV = depression, IV = computer use on weekends.
```{r}
brm(formula      = depression ~ comp_use_2,
    data         = data,
    family       = cumulative("logit"),
    sample_prior = TRUE,
    iter         = 2000,
    chains       = 4,
    control      = list(adapt_delta = 0.95))
```



c. What are some references for your model (even if you think it is ‘standard’ please give a reference to a similar use to yours)? Burkner & Vuorre. (2018) Ordinal Regression Models in Psychology: A Tutorial, doi: 10.1177/2515245918823199. FIND AMPPS REFERENCE

d. Why did you use this technique? Both the IVs and DV were natively ordinal, therefore cumulative ordinal regression seemed suitable to the research question. In addition, the results report required us to submit our results as an Odds Ratio (e.g., rather than a Spearman correlation).

a. Please describe any other parameters, or techniques used in your analysis e.g. regularization techniques, justifications for priors (in Bayesian analysis). Please provide references. Given my genuinely weak prior beliefs about this effect, combined with the use of the brms default prior in the recent tutorial paper on these models (referenced above), I elected to employ the default brms prior (i.e., a t distribution on all parameters with mean = 0 and df = 3). This is also broadly in line with recommendations made by other Bayesian analysts for models where one desires a weak prior (e.g., Gelman MCMC web page). Inspection of the posteriors demonstrated that the prior easily overwhelmned the prior, allowing me to label it as a weak prior.

**Results.** 

Please state the results of your analyses using AICs/DICs, standardised odds ratios, 95% confidence (or credible) intervals and, if appropriate, p-values below. Note: 

- ●  If you have more than one exposure variable, or your exposure variable has more than two levels then you will have multiple odds ratios. Please make sure the definitions of these are included in your *Final Analysis Report*. 

- ●  The definitions of AIC and DIC are: 

  AIC: 2*k - 2*Ln(Likelihood) DIC: 2*p - 2*Ln(Deviance) 

  k(p) = number of (effective) parameters. For the deviance please use the version implemented here: https://www.rdocumentation.org/packages/AICcmodavg/versions/2.2 -1/topics/DIC 

I report odds ratios for linear trend across the levels of the IVs. 95% CIs refer to 95% HDIs.

| **Variable definition**              | **Value** | **95% CI** | **p-Value** |
| ------------------------------------ | --------- | ---------- | ----------- |
| Odds ratio: Outcome/Exposure level 1 |           |            |             |
| Odds ratio: Outcome/Exposure level 1 |           |            |             |
| AIC/DIC                              |           |            |             |

**Feedback** 

As a team do have any feedback about the project e.g. it’s coordination, how it could be improved, etc? [Free text description] 

*Final Analysis Report (Preview) V1.0* 9 April 2019 3 

*Note. This is a preview the Final Analysis Report. This preview may differ from the final version of the Final Analysis Report which will be sent* *to teams via email. Do not use this preview to submit your team’s final* *analysis.* 

**Demographics** 

\1. Name 

Ian Hussey

**SURVEY TO BE COMPLETED BY EACH TEAM MEMBER** 

1. Age in years 
2. What is the highest qualification you have attained? [drop down - bachelors, masters, PhD, other] 

a. If other please specify [free text] 

1. What title best describes your current position? [drop down - doctoral student, postdoctoral student, assistant professor, associate professor, full professor, outside of academia] 
2. How many years have you spent in research (including the years you spent doing a PhD)? [Free numerical text] 
3. Have you read the paper by Khouja et al (2019)? [No/Yes-abstract/Yes - results/ Yes - methods/ Yes - methods & results/entire paper] 
4. Did you access the original ALSPAC dataset during this study? [Yes/No] 
5. Did you see or discuss the Synthetic Dataset before you submitted your Team Registration Form? [Yes/No] 
6. Did you or your other team members communicate with other MAPS teams about the Synthetic Dataset or your analysis strategies? 
7. To what extent do you agree that your team’s analysis is independent of analyses planned by other MAPS teams? [strongly disagree 1 - strongly agree 7] 

**Statistical Expertise** 

\1. Do you have any of the following qualifications in statistics or a statistics-related field? [bachelors, masters, PhD, other] 

a. If other please specify [free text]
 \2. Have you taught an undergraduate level statistics course? 

*Final Analysis Report (Preview) V1.0* 9 April 2019 4 

*Note. This is a preview the Final Analysis Report. This preview may differ from the final version of the Final Analysis Report which will be sent* *to teams via email. Do not use this preview to submit your team’s final* *analysis.* 

a. 

1. Have a. 

2. Have 

   strongly agree 7] 

**Research Question Expertise** 

\1. Have you published a paper on screen-time and mental health research? a. Yes - How many papers in total? 

\2. To what extent do you agree that you are an expert on screen-time and mental health research? (strongly disagree 1- strongly agree 7) 

**Subjective Beliefs** 

This section will ask you about your current opinions regarding the research question studied in this project: *is computer use during weekdays and weekends at 16 years old associated with depression at 18 years old?* 

A positive association indicates that computer use at 16 is linked with increased depression at 18. 

A negative association indicates that computer use at 16 is linked with decreased depression at 18. 

No association indicates that computer use at 16 and depression at 18 are not related. 

1. What do you think the association is between computer use at 16 **during the week** and depression at 18? (strong negative association [1] - strong positive association [7]) 
2. How sure are you that this estimate for computer use **during the week** is true? (very unsure [1] - very sure [7]) 

Yes - How many times in total?
 you taught a graduate level statistics course? 

Yes - How many times in total?
 you published a paper on statistical methods? 

Yes - How many papers in total?
 you published a paper that used statistical models? 

Yes - How many papers in total?
 \6. To what extent do you agree that you are an expert in statistics? [strongly disagree 1- 

a. 5. Have a. 

*Final Analysis Report (Preview) V1.0* 9 April 2019 5 

*Note. This is a preview the Final Analysis Report. This preview may differ from the final version of the Final Analysis Report which will be sent* *to teams via email. Do not use this preview to submit your team’s final* *analysis.* 

1. What do you think the association is between computer use at 16 **during the weekend** and depression at 18? (strong negative association [1] - strong positive association [7]) 
2. How sure are you that this estimate for computer use **during the weekend** is true? (strong negative association [1] - strong positive association [7]) 

**Authorship** 

\1. Do you wish to be included as an author on reports of this project e.g. papers, conference abstracts, etc. 

a. Yes/No
 i. If yes what is your publication name? 

**Feedback** 

\1. Do you have any feedback about the project e.g. it’s coordination, how it could be improved, etc? 