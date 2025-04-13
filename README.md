# The Power of Proximity -  Using Network Science to Analyze the Link Between Coaching Communities and Team Success in the National Football League
First of its kind coaching dataset, presented at 2025 Connecticut Sports Analytics Conference and 2025 Cornell Sports Research Symposium.

## Data
The data was manually entered from [Pro-Football-History.com](https://pro-football-history.com/), which gave the coaching staff for every team in a given year. Head coaches, coordinators, positional coaches, and quality control coaches were all used. The data was chosen from 2010 to 2024, amounting to a over 6600 coaches.

## Methods
Each coach was given a score based on their authority. Head coaches were given a score of 10, coordinators were given a score of 7.5, positional coaches a score of 5, and quality control coaches a score of 2.5. 

Connections were made between two coaches if they were on the same coaching staff. The connection strength would be the sum of their respective scores. A head coach-coordinator connection would have a strength of 7.5.

Aggregated closeness score is the sum of the connection strength across all years where two coaches coached together.

With the connection strength created, the result is a weighted network with the nodes as coaches and the teams as edges.

## Analysis
### Coaching communities
Pruned the network to only have connection strength greater than 50. Used a fast-greedy modularity maximizing algorithm to cluster the network into different communities. Found 19 different communities, 14 of which were deemed to be significant.

### Shanahan score - Separation theory
Created a distance metric (inverse of the sum of aggregated closeness score and years coached together) to figure out how close every coach was to Kyle Shanahan. Conducted a shortest distance algorithm to determine distance and path from Shanahan to all other coaches.

### Effect of coaching connectiveness on team success
Created a team closeness score (average closeness across the entire coaching staff) and conducted three different models to predict three different forms of team success.
- Model 1: Predictor - Team closeness score, Controls - Strength of Schedule and lag win percentage, Outcome - Win percentage. Method: Linear regression
- Model 2: Predictor - Team closeness score, Controls - Strength of Schedule and lag win percentage, Outcome - Made playoffs. Method: Logistic regression
- Model 3: Predictor - Team closeness score, Controls - Strength of Schedule and lag win percentage, Outcome - Won division. Method: Logistic regression

Found positive, statistically significant associations between team closeness and all three measures of team success.
