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

This is a statistical report of the study currenlty *under review* in the [*Communications Medicine*](https://www.nature.com/commsmed) journal.

When using this code or data, cite the original publication:

> TO BE ADDED

BibTex citation for the original publication:

> TO BE ADDED

---------------------------------------------------------------------------------------------------

Original [GitHub repository](https://github.com/filip-tichanek/kompas_clinical): https://github.com/filip-tichanek/kompas_clinical

Statistical **reports** can be found on the [reports hub](https://filip-tichanek.github.io/kompas_clinical/).

Data analysis is described in detail in the [statistical methods](https://filip-tichanek.github.io/kompas_clinical/html/368_G_Methods.html) report.

----------------------------------------------------------------------------------------------------

# Introduction

This project is designed to evaluate and compare clinical outcomes across three distinct dietary strategy groups:

-   Vegans
-   Vegetarians
-   Omnivores

The dataset includes both adults and children, with data clustered within families.

## Main Questions

The study addresses the following key questions:

Q1. Do clinical outcomes vary significantly across different diet strategies?

Q2. Beyond diet group, which factors (e.g., sex, age, breastfeeding status for children, or supplementation when applicable) most strongly influence clinical outcomes? How correlated (“clustered”) are these characteristics within the same family?

Q3. Could the clinical characteristics effectively discriminate between different diet groups?


## Statistical Methods

For full methodological details, see [this report](https://filip-tichanek.github.io/kompas_clinical/html/368_G_Methods.html). In brief:

- **Robust linear mixed-effects models (rLME)** were used to estimate adjusted differences between diet groups (Q1) and assess the importance of other variables (Q2), including how much clinical characteristics tend to cluster within families. Covariates included age, sex, breastfeeding status for children, and relevant supplementation factors where applicable.   

- **Elastic net logistic regression** was employed to answer Q3, evaluating whether clinical characteristics provide a strong overall signal distinguishing between diet groups, incorporating a predictive perspective.

All analyses were conducted separately for adults and children.
