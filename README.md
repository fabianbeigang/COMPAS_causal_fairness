# A fairness analysis of the COMPAS algorithm
An analysis of the fairness of the COMPAS risk recidivism predictions using a new causal fairness metric. The central idea behind this project is to define fairness as the notion that a sensitive attribute (e.g. ethnicity) should not influence a prediction more than is justified by its causal relevance. This is made precise using causal inference methods. This fairness criterion is then applied to ProPublica's COMPAS data set (obtained from https://github.com/propublica/compas-analysis)

This project consists of two files:
* [The preprint of the article]()
* [The R file with the code of the analysis]

This project showcases the following skills:
* Causal inference (via *Mahalanobis distance matching*)
* Using basic R libraries (*ggplot2*, *dplyr*)
