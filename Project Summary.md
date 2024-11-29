# 958-final-project

Assignment Guidelines: Create different classes of models
	traditional lm/repeated measures approach
	mixed effects lmer
	etc.

Analysis Plan:
	- Need a common approach like median split or linear
	- Comparing the approaches that violate assumptions in some way like using linear model to assess DDk
	- Bayesian analysis to estimate the distributions?
	- Cluster analysis to identify certain types of respondents?

Outcome Variables:
	DDk (delay discounting) - This is the one we're looking at right now

	SOI (sociosexual orientation)
	KSF (general life history)
	RiskTaking (likelihood to take risks)


Predictor Variables:
	Income
		Average in location (averagein)
		Household income while in location (HHin)
	
	Density of people
		Average in location (dens)
		Household density while in location (roommates)
	
	Mate Competition
		Sex Ratio in location (SR)
	
	Mortality Risk
		Average life expectancy in location

Comments:

	I think it best to limit our predictors to one from each of the categories described above (i.e., choose either population or household average for income and density).
	Since SR and LE were measured on a population-level, I think using population averages across the board makes the most sense. So the final model would be:

	Delay Discounting ~ Average_income + Population_density + SR + LE
or
	DDk ~ averagein + dens + SR + LE
