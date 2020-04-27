## Capstone


### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

1. Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**The process of childbirth in humans has undergone dramatic changes throughout history. A few hundred years ago, most women gave birth in their homes with little-to-no medical intervention in the process. Now, most births in the US happen in hospitals, and mothers are given many different types of medical intervention options. Today, the vast majority of women who give birth in the US recieve an epidural - a procedure where analgesic medication is administered into the epidural space. Although there are not any known long-term harmful effects of epidurals on the child, the pain medication administeded in an epidural is circulated through the child in addition to the mother. Because of this, babies born to mothers who have recieved epidurals may be influenced by the pain medication at the time of their birth.**

2. Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**Although many studies have been done to investigate the relationship between breast feeding outcomes and the administration of epidurals during childbirth, whether a relationship exists has remained controversial.**

3. Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If epidurals affect a newborn's ability to breastfeed at the time of birth, then mothers (who want to breastfeed) who recieve epidurals should resort to bottles during their hospital stay more than mothers who do not recieve epidurals.**

4. What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**The predictor variable will be a sorted variable: whether a mother recieves an epidural or has no pain medication during childbirth (note all other types of pain medication will be excluded from this study).**

**The dependent variable will be how many times the newborn baby recieved a bottle during the postnatal stay at the hospital. This is an ordered variable.**

5. Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

$H_a:$ **The group of women who recieve epidurals during childbirth will give newborns more bottles than the group of women who do not recieve any pain medication.**

6. What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**We will use a Wilcoxon Mann Whitney Rank Sum Test for 2 independent groups to test our statistical hypothesis. This test is preferable because we are dealing with an ordered dependent variable. Ordered variables are inherently non-parametric because they are discrete. As an alternatve, we could consider using a transformation to make the dependent variable appear to be parametric and use a t-test. However, using a non-parametric test allows us to use the data without imposing a transformation.**

7. List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**We will invite women who will give birth at Emory Midtown to participate in our study. We will limit the study to pregnant women between the ages of 20-35. We will exclude all women whose pregnancies fall into the high-risk category. We will exclusively focus on women whose plan prior to giving birth is to exclusively breastfeed postpartum. We will include only the data for women who recieved an epidural and no other pain medication during the birth in the epidural group. We will  include only the data for women who did not recieve any pain madication during the birth in the no-medication group. We will exclude all women who stayed in the hospital post-birth for < 24 hours or >48 hours. Each mother+child combination is considered to be an independent replicate.**

8) Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.

```{r message=FALSE, warning=FALSE}
#Load relevant packages
library(tidyverse)
```

```{r message=FALSE, warning=FALSE}
feedingDataMaker <- function(n, means) {

  params <- data.frame(n, means)

  data <- as.tibble(apply(params, 1, function (x) rpois(x[1], x[2])))

  oldnames <- c("V1","V2")
  newnames <- c("control", "epidural")
  data <- data %>% rename_at(vars(oldnames), ~ newnames)

  data <- pivot_longer(data, everything(), 
                         names_to = "treatment",
                         values_to="nBottles") %>% 
           mutate(id= as.factor(1:(n[1] + n[2])),
             treatment=as.factor(treatment))
  
data 
}
data <- feedingDataMaker(c(20, 20), c(1, 2))

ggplot(data) +
    aes(treatment, nBottles, color = treatment) +
    geom_boxplot(width=0.3) +
    geom_jitter(height=0,
    width=0.15) + 
    xlab("Pain Medication during Labor") + 
    ylab("Number of Bottles during Hospital Stay")

```

9. Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.

```{r message=FALSE, warning=FALSE}
sims = 1000 #number of Monte Carlo simulations to run. 
n = c(25, 25)
means = c(1, 2)

pval <- replicate(
  sims, {
 
    sample.df <- feedingDataMaker(n, means)
    
    w <- wilcox.test(nBottles ~ treatment, 
            data = sample.df, 
            alternative ="less", 
            conf.level=0.95, 
            conf.int=T)
  
  pval <- w$p.value
    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power. Change 'n' in your initializer for higher or lower power.")
```
