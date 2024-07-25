**Authors and affiliations**

<div style="font-size: larger;">
Marina Heniková<sup>1,2</sup>, Anna Ouřadová<sup>1</sup>, Eliška Selinger<sup>1,3</sup>, Filip Tichanek<sup>4</sup>, Petra Polakovičová<sup>4</sup>, Dana Hrnčířová<sup>2</sup>, Pavel Dlouhý<sup>2</sup>, Martin Světnička<sup>5</sup>, Eva El-Lababidi<sup>5</sup>, Jana Potočková<sup>1</sup>, Tilman Kühn<sup>6</sup>, Monika Cahová<sup>4</sup>, Jan Gojda<sup>1</sup>
</div>

<br>

<sup>1</sup> Department of Internal Medicine, Kralovske Vinohrady University Hospital and Third Faculty of Medicine, Charles University, Prague, Czech Republic.  
<sup>2</sup> Department of Hygiene, Third Faculty of Medicine, Charles University, Prague, Czech Republic.  
<sup>3</sup> National Health Institute, Prague, Czech Republic.  
<sup>4</sup> Institute for Clinical and Experimental Medicine, Prague, Czech Republic.  
<sup>5</sup> Department of Pediatrics, Kralovske Vinohrady University Hospital and Third Faculty of Medicine, Charles University, Prague, Czech Republic.  
<sup>6</sup> Department of Epidemiology, MedUni, Vienna, Austria.

---------------------------------------------------------------------------------------------------

This is a statistical report of the study that has been submitted to [TO BE ADDED]

When using this code or data, cite the original publication:

> TO BE ADDED

BibTex citation for the original publication:

> TO BE ADDED

---------------------------------------------------------------------------------------------------

Original [GitHub repository](https://github.com/filip-tichanek/kompas_clinical): https://github.com/filip-tichanek/kompas_clinical

Statistical **reports** can be found on the [reports hub](https://filip-tichanek.github.io/kompas_clinical/).

Data analysis is described in detail in the [statistical methods](https://filip-tichanek.github.io/kompas_clinical/368_G_Methods.html) report.

----------------------------------------------------------------------------------------------------

# Introduction

This project is designed to evaluate and compare clinical outcomes across three distinct dietary strategy groups:

-   Vegans
-   Vegetarians
-   Omnivores

The dataset is structured with data clustered within families, encompassing both adults and children. Children and adults are analyzed separately. The analysis for children is twofold: firstly, all children are considered as a single group with age (log2-transformed) serving as a covariate; secondly, a more detailed examination divides children into two age groups—those younger than 3 years and those 3 years and older.

## Main Questions

The study aims to address the following key questions:

Q1. Do clinical outcomes vary significantly across different diet strategies?

Q2. Are there differences in the broader variability of clinical outcomes across diet groups, beyond just the average/median values?

Q3. Beyond diet group, what factors (such sex, age, breastfeeding status in children) are the most important drivers of clinical outcomes? How much are clinical charactersitics clustered within families?

Q4. Could the clinical characteristics effectively discriminate between different diet groups?

## Statistical methods

Statistical modelling is described in detail in [this report](https://filip-tichanek.github.io/kompas_clinical/368_G_Methods.html). Briefly, statistical methods used included: 

The **Kruskal-Wallis** test and **Mann-Whitney U test** to compare unadjusted medians, **Fisher test** was used to compare unadjusted proportions (Q1). 

**Quantile regression** (QR) was used to obtain adjusted differences in medians (Q1), 20th and 80th percentiles of the clinical characteristics (Q2), and to assess the relative importance of other covariates (Q3). **Linear mixed-effects models** (LME) provided information about adjusted differences between diet groups (Q1) and the importance of other variables, including clustering within families (Q3).  
Both QR and LME **were adjusted for confounders including age, sex, breast-feeding status (children), and relevant supplementation**.

**Elastic net logistic regression** was employed to address Q4, offering a predictive perspective on the role of diet in determining clinical profiles.

Separate analyses were conducted for adults and children. For children, the analysis included a joint examination of all children, incorporating log2(age) as a covariate, and subgroup-specific analyses of children *under* and *over* 3 years of age separately.

