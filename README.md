# Analysis-Of-The-Performance-Of-Bus-Depots-Using-Unsupervised-Learning
Our Objectives:

• To find similarity among depots based on various parameters e.g. 
utilization variables , performance variables , earning variables and do the 
outcomes remain similar across all quarters.

• To rank the depots based on different parameters in various quarters.

• Are there any outlier present in our data as presence of extreme or 
unusual values can affect our statistical analysis.

Methodology Used:

• Cluster Analysis- Here Hierarchical Clustering using Euclidean Distance and 
Ward Linkage Method have been used to make groups of depots such that within group memebers will be similar in some sense.

• Principal Component Analysis ( PCA )- Here it 
is studied to see the contribution of various parameters in 
performance of depots and to rank depots using their scores.

Data Pre-processing:

• Initially dataset had 39 features but 11 features are selected to do further analysis. Some data cleaning is done in excel.

• Since some of the variables have extreme values and multicollinearity is there we will opt for Random Forest method for missing value imputation instead of MICE or KNN.

• Here the variables are of different units and the 
range of one variable varies than that of some other variables to a great 
extent. So by normalizing, we brought them down to the same scale resulting 
the comparison of variables would be fair.

Using the same ideas we can analyse any data with similar kind of query.
