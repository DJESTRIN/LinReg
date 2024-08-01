<h1> <b> LinReg: A unifying linear regression library </b> </h1>

In R, there is a wide range of libaries used for linear regression. This often makes R scripts difficult to read 
given repeating code blocks. LinReg is a tool box I am continously developing to allow for easy analysis of various datasets. 

<h2> <b> A unifying framework  </b> </h2>
The backbone of LinReg contains the most common regression libaries (lmer, glmer, glmmTMB,nlme, etc) as well as helper packages
(emmeans, mass, car, statmod, multcomp, ggplot,effsize, etc) in a single spot. 

Statistical pipelines should be a common set of questions that make up a common protocol. These question might include:
- Is the data normally distributed? 
- Does this dataset violate any assumptions for linear regression?
- Should my model contain random effects?
- How should we handle missing data?
- How well does the model fit the data?
- Should I use a hold-out dataset to validate model performance?

LinReg attempts to answer these questions sequentially and allow users to easily develop models from multiple packages utalizing a 
single library. 