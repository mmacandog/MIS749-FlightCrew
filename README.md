# BA749-FlightCrew

<b> Executive Summary </b>

The application of data science in the domains of consumer behavior has potential of great value for customer facing companies. Airlines, which already have vast data collection protocols, would benefit greatly in applying these practices to gain valuable insights on customer behavior. For this reason, we hypothesize that there may be valuable variables, such as on-board service, amenities, and airport features, that may greatly influence a customer’s satisfaction or dissatisfaction in their flight experience.

We retrieved our dataset titled “Airline Passenger Satisfaction” from Kaggle. We first conducted data cleaning by removing null values, missing observations, or rows with 0. During preprocessing, we began by creating dummy variables, identifying and removing predictors with near zero variance (departure delay and arrival delay), scaled and centered the dataset to the same unit, and aimed to remove highly correlated variables over .7, but found none. One of the classes for our response variable, which was originally labeled as “neutral or dissatisfied”, was changed to dissatisfied to simplify interpretation and analysis.
PCA was initialized for dimension reduction prior to analysis but no loadings above .3 were discovered and was therefore ultimately not considered. 

The following classification analyses were applied: Logistic Regression, Linear Discriminant Analysis, Quadratic Discriminant Analysis, and K-Nearest Neighbors. Tree-based models, such as Decision Trees, Random Forests, and Bagging, were also applied. A 10-fold cross-validation parameter was included in all classification models to optimize the training dataset, which was partitioned beforehand into 70% of the data. For our interest in understanding accurate customer dissatisfaction, sensitivity and accuracy were the performance metrics considered. Model performance indicates that among the classification models, LDA performed the best due to its high sprecificity rate of .90 and ROC curve of .96. On the other hand, bagging achieved the best performance with a .99 score for accuracy, sensitivity and specificity. All of our models gave online boarding, in–flight wifi, and type of travel a high coefficient value which would suggest that airlines should concentrate on improving their mobile app services and wifi reliability. Type of travel would be a difficult predictor to improve on barring a questionnaire on the mobile app to identify business or personal travel prior to the flight.

For the full report, please message Joseline: https://github.com/joseliine or Marrion: https://github.com/mmacandog.

Tableau Dashboard: https://public.tableau.com/views/AirlineCustomerSatisfaction_16456890973030/Dashboard1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link
