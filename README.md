_____________________________________________________________________________________________________________________________________
#CELRAK10 GITHUB PROJECT PORTFOLIO ON CREDIT METRICS
______________________________________________________________________________________________________________________________________________

	FOLLOW ME AT 
		https://github.com/celrak10/CreditMetrics
		https://www.linkedin.com/in/ceballosalfonsoeng/


SIMULATION OF A REGIONAL BANK CREDIT PORTFOLIO:

	The data set that contains the simulation of the portfolio is called IFRS_Simulation
	Feel free to use it, modify and change it
	
		Characteristics of the portfolio:

		1. Currently the portfolio is simulated as a healthy 96% performing loans, 
		2. 20% simple, 80% revolving lines
		3. Around 90% of the clients has a credit on the local currency, 10% in USD
		4. The NPL performance is measure on Buckets of 0,1,2,3,4 where 0 current and 4 default
		5. Activity and Sector is based on NAICS 6 digit and 2-3 digit respectively
		6. Some credits have funding program 

What columns does the dataset contains?
	Dictionary of the data set:
	
		Date	  		      Date in yyyy/mm/dd format. Dataset spans Jan-2022 to Dec-2024.
		DATE_F	  	      Numeric year–month yyyymm (month is zero-padded, e.g., 202201, 202212).
		Id_client		      Unique client ID (total of 5,199 clients).
		Activity		      Client’s economic activity, based on 6-digit NAICS code.
		Sector			      Client’s economic sector, based on 2- or 3-digit NAICS code.
		SIZE			        Company size: 1 = Micro, 2 = Small, 3 = Medium, 4 = Large.
		Program			      Whether the client is in a financing program (details to be explained).
		NPL			          Days-past-due bucket: 0 = Current, 1 = 1–30 days, 2 = 31–60, 3 = 61–90, 4 = Default (>90 DPD).
		Internal Score		Regional bank’s internal score. Only 59–100 appear because lower scores are not credit-	approvable.
		Currency		      Currency of the loan.
		Loan_Amount_USD		Loan amount granted in USD.
		Loan_Amount_Local	Loan amount granted in local currency (MXN).
		PD_reg			      Regulatory Probability of Default (Based on Anex 22 of Mexican regulatory credit risk framework)
		LGD			          Regulatory Loss Given Default (Based on Anex 22 of Mexican regulatory credit risk framework)
		I_rate			      Annual interest rate of the loan.
		Capital			      Loan principal for the period.
		Interest		      Loan interest for the period.
		EAI			          Total amount for the period = Principal (Capital) + Interest.
		Credit_B		      Total amount of the client’s credit lines at origination.
		F_Debt			      Client’s total financial debt at origination.
		Total_Debt		    Client’s total debt from financial statements (or estimated if statements are unavailable).
		Assets			      Company assets at origination.
		Equity			      Equity = Assets − Liabilities.
		RATING			      Empty; in a future project I will use this column as credit that has external rating.
		Cred_Type		      Credit type: Rev = Revolving, Simple = Term loan.
		Seed			        Loan origination date. 

Where the code to run the analysis and what it runs?

	The code that runs all the modeling and analysis is FullCode.R
	
	The topics that cover:
	
		1. Modeling
			PD by Logit, Random Forest, Neural Networks (Future Update)
			AUC-ROC, KS, Brier Score, calibration curve
			LGD estimation by beta and Gradient Boosting
			EAD linear estimation
			Expected Losses for portfolio and client

		2. Dependencies (Non expected losses)
			Transitional Matrix
			Initial Rating
			Thresholds for Rating 
			Simulations for loss distributions UL, Var, ECAP
			Gaussian Copula with PD/LGD
		
		Future updates (Probably August-September):
		3. Stress Testing
			Shocks to interest rates, FX depreciation, Sectorial deterioration
			Natural Shocks in Vulnerable Sectors
			Recalculation of the EL by shock scenarios

		4. Metrics and Dashboarding (Probably BI)
			EL, UL, VAR/Es, Ecap
			RAROC
			Top exposures
			Sectorial concentration
			Maturity, currency etc		
			
		5. Sensitivity Analysis
			Internal credit sensitivity analysis

______________________________________________________________________________________________________________________________________________


























#CELRAK10 GITHUB PROJECT PORTFOLIO ON CREDIT METRICS

	FOLLOW ME AT 
		https://github.com/celrak10/CreditMetrics
		https://www.linkedin.com/in/ceballosalfonsoeng/

