# Calculation of breast cancer risk

## Gail Model
Breast cancer prevention, among other factors, depends on the ability to identify at-risk individuals who should be screened. One of the more common risk model for breast cancer screening is the Gail model. The original 1989 model is a logistic regression model and includes five factors:

1. age category
2. age of menarche
3. age at first live birth
4. number of biopsies
5. number of first degree relatives with breast cancer.


## Risk
Calculation of relative and absolute risks were provided in the paper and implemented in this repository. Briefly, relative risks are obtained from directly inserting patient risk factors into the model, while absolute risks accounts for other hazards not modeled. Competing competing hazards consisted of non-breast cancer hazards and breast cancer composite incidence rates; these values were taken from the [National Cancer Institute's Breast Cancer Risk Assessment Tool](https://bcrisktool.cancer.gov/).

For full model details, see:

Gail, Mitchell H., Louise A. Brinton, David P. Byar, Donald K. Corle, Sylvan B. Green, Catherine Schairer, and John J. Mulvihill. "Projecting individualized probabilities of developing breast cancer for white females who are being examined annually." JNCI: Journal of the National Cancer Institute 81, no. 24 (1989): 1879-1886.

This calculator was created for:

Smedley, Nova F., Ngan Chau, Antonia Petruse, Alex AT Bui, Arash Naeim, and William Hsu. "A Platform for Generating and Validating Breast Risk Models from Clinical Data: Towards Patient-Centered Risk Stratified Screening." In AMIA. 2015.
