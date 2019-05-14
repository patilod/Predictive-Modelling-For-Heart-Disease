
########### Bagging 
rm(list=ls())

data=read.csv("C:/Users/stats2/Desktop/heart.csv",header=T)
n=nrow(data)
s=sample(1:n,round(0.7*n)) # training indices
ts=seq(1,n)[-s]    # testing indices
y1=rep(0,91)
for(i in 1:10)
{
train=sample(s,length(s),replace=T)
data1=data[train,]
model=glm(target~.,family="binomial",data=data1)
y=predict(model,data[ts,-14],type="response")
y.pred=rep(0,91)
y.pred[y>.5]=1

print(summary(model))
accuracy=mean(y.pred==data[ts,14])
print(accuracy)
y1=y1+y.pred
}
#creating y*
y_star=rep(0,91)
y_star[y1>5]=1
#creatingconfusion matrix
table(y_star,data[ts,14])
#storingaccuracy
accuracy1=mean(y_star==data[ts,14])
accuracy1

################## output ####################
Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.32408  -0.33706   0.02982   0.44480   2.62380  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  6.582479   3.732707   1.763  0.07782 .  
ï..age      -0.064336   0.035754  -1.799  0.07195 .  
sex         -4.274488   0.901966  -4.739 2.15e-06 ***
cp           1.278542   0.262769   4.866 1.14e-06 ***
trestbps    -0.037008   0.012388  -2.987  0.00281 ** 
chol        -0.008085   0.006449  -1.254  0.20996    
fbs         -0.493522   0.721314  -0.684  0.49385    
restecg      0.072506   0.487145   0.149  0.88168    
thalach      0.049841   0.018305   2.723  0.00647 ** 
exang       -1.499422   0.583551  -2.569  0.01019 *  
oldpeak     -0.247114   0.274474  -0.900  0.36795    
slope        0.976874   0.451236   2.165  0.03040 *  
ca          -0.471144   0.291119  -1.618  0.10558    
thal        -1.034320   0.354432  -2.918  0.00352 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 292.97  on 211  degrees of freedom
Residual deviance: 134.85  on 198  degrees of freedom
AIC: 162.85

Number of Fisher Scoring iterations: 7

[1] 0.8241758

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3023  -0.2973   0.1284   0.6076   2.9501  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.887180   3.652858  -0.243  0.80810    
ï..age       0.011157   0.029358   0.380  0.70392    
sex         -2.627938   0.667521  -3.937 8.26e-05 ***
cp           0.880773   0.243015   3.624  0.00029 ***
trestbps    -0.019602   0.012421  -1.578  0.11454    
chol        -0.006564   0.005760  -1.140  0.25448    
fbs         -0.091832   0.685933  -0.134  0.89350    
restecg      0.338404   0.467252   0.724  0.46892    
thalach      0.047014   0.016613   2.830  0.00466 ** 
exang       -0.509144   0.555091  -0.917  0.35902    
oldpeak     -0.546514   0.261108  -2.093  0.03634 *  
slope        0.620424   0.475871   1.304  0.19231    
ca          -0.768447   0.231895  -3.314  0.00092 ***
thal        -0.603509   0.355782  -1.696  0.08983 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 292.36  on 211  degrees of freedom
Residual deviance: 143.74  on 198  degrees of freedom
AIC: 171.74

Number of Fisher Scoring iterations: 6

[1] 0.8901099

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.5738  -0.3361   0.1631   0.6285   2.6642  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  4.8328072  3.3934024   1.424 0.154395    
ï..age       0.0001554  0.0286395   0.005 0.995671    
sex         -0.6994227  0.5049512  -1.385 0.166013    
cp           0.8051770  0.2471019   3.258 0.001120 ** 
trestbps    -0.0028990  0.0123713  -0.234 0.814728    
chol        -0.0072800  0.0055478  -1.312 0.189436    
fbs         -0.3043128  0.5978408  -0.509 0.610738    
restecg      0.9249710  0.4359994   2.121 0.033880 *  
thalach     -0.0008484  0.0149434  -0.057 0.954727    
exang       -1.2304346  0.5077504  -2.423 0.015380 *  
oldpeak     -0.9457000  0.2870615  -3.294 0.000986 ***
slope        0.9546422  0.4437690   2.151 0.031459 *  
ca          -0.9810439  0.2340633  -4.191 2.77e-05 ***
thal        -1.1671049  0.3860956  -3.023 0.002504 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 293.21  on 211  degrees of freedom
Residual deviance: 154.51  on 198  degrees of freedom
AIC: 182.51

Number of Fisher Scoring iterations: 6

[1] 0.8241758

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.1580  -0.3085   0.1210   0.4511   2.4737  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  4.327551   4.059874   1.066  0.28645    
ï..age      -0.022951   0.034362  -0.668  0.50418    
sex         -1.126389   0.549813  -2.049  0.04049 *  
cp           1.684479   0.319024   5.280 1.29e-07 ***
trestbps    -0.028339   0.015625  -1.814  0.06972 .  
chol        -0.006375   0.005983  -1.065  0.28667    
fbs          0.008341   0.664493   0.013  0.98998    
restecg      1.654339   0.513931   3.219  0.00129 ** 
thalach      0.015352   0.016231   0.946  0.34423    
exang       -1.284852   0.580062  -2.215  0.02676 *  
oldpeak     -0.470954   0.310887  -1.515  0.12980    
slope        0.640052   0.502246   1.274  0.20253    
ca          -0.416992   0.222370  -1.875  0.06076 .  
thal        -0.516741   0.362035  -1.427  0.15349    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 290.70  on 211  degrees of freedom
Residual deviance: 133.44  on 198  degrees of freedom
AIC: 161.44

Number of Fisher Scoring iterations: 6

[1] 0.8241758

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7651  -0.3468   0.1261   0.5043   1.7976  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  7.034091   3.729894   1.886 0.059313 .  
ï..age      -0.040508   0.035128  -1.153 0.248846    
sex         -2.449312   0.656490  -3.731 0.000191 ***
cp           0.626871   0.237475   2.640 0.008297 ** 
trestbps    -0.023094   0.014661  -1.575 0.115208    
chol        -0.011839   0.005495  -2.154 0.031208 *  
fbs          0.331173   0.686544   0.482 0.629539    
restecg      0.495468   0.429371   1.154 0.248525    
thalach      0.038231   0.014405   2.654 0.007955 ** 
exang       -1.982385   0.651799  -3.041 0.002355 ** 
oldpeak     -0.642664   0.267145  -2.406 0.016143 *  
slope        0.458838   0.426635   1.075 0.282159    
ca          -0.042669   0.244453  -0.175 0.861435    
thal        -1.411753   0.393610  -3.587 0.000335 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 290.70  on 211  degrees of freedom
Residual deviance: 139.23  on 198  degrees of freedom
AIC: 167.23

Number of Fisher Scoring iterations: 6

[1] 0.8351648

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3062  -0.5057   0.1191   0.5316   2.3593  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.695416   3.401484   1.086 0.277296    
ï..age      -0.006066   0.031611  -0.192 0.847825    
sex         -1.686510   0.511510  -3.297 0.000977 ***
cp           0.615086   0.219874   2.797 0.005151 ** 
trestbps    -0.015050   0.011958  -1.259 0.208184    
chol        -0.002902   0.005726  -0.507 0.612322    
fbs         -0.418368   0.598814  -0.699 0.484763    
restecg      0.444597   0.430098   1.034 0.301271    
thalach      0.017077   0.013622   1.254 0.209969    
exang       -1.851336   0.536282  -3.452 0.000556 ***
oldpeak     -0.586270   0.253040  -2.317 0.020509 *  
slope        0.383920   0.363377   1.057 0.290725    
ca          -0.382857   0.217469  -1.761 0.078321 .  
thal        -0.922351   0.344100  -2.680 0.007352 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 293.88  on 211  degrees of freedom
Residual deviance: 152.13  on 198  degrees of freedom
AIC: 180.13

Number of Fisher Scoring iterations: 6

[1] 0.8571429

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3942  -0.1050   0.1238   0.4741   3.1777  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.729220   4.132885  -0.176  0.85995    
ï..age       0.002954   0.031439   0.094  0.92514    
sex         -1.458113   0.699587  -2.084  0.03714 *  
cp           0.931154   0.309415   3.009  0.00262 ** 
trestbps    -0.004333   0.016323  -0.265  0.79068    
chol        -0.009474   0.006621  -1.431  0.15246    
fbs         -1.314580   0.748869  -1.755  0.07919 .  
restecg      0.984569   0.517220   1.904  0.05697 .  
thalach      0.044324   0.017825   2.487  0.01290 *  
exang       -1.144010   0.674495  -1.696  0.08987 .  
oldpeak     -0.413132   0.337818  -1.223  0.22135    
slope        1.001533   0.453825   2.207  0.02732 *  
ca          -1.267293   0.278085  -4.557 5.18e-06 ***
thal        -1.381351   0.432228  -3.196  0.00139 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 281.99  on 211  degrees of freedom
Residual deviance: 122.05  on 198  degrees of freedom
AIC: 150.05

Number of Fisher Scoring iterations: 7

[1] 0.8571429

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3627  -0.3026   0.2427   0.5668   2.9566  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.7911951  3.4786431  -0.802 0.422333    
ï..age      -0.0004966  0.0278137  -0.018 0.985754    
sex         -1.4169101  0.5388080  -2.630 0.008546 ** 
cp           0.7638213  0.2266411   3.370 0.000751 ***
trestbps    -0.0144779  0.0122354  -1.183 0.236700    
chol        -0.0070220  0.0056633  -1.240 0.215010    
fbs          0.9381625  0.7079214   1.325 0.185093    
restecg      0.5101576  0.4444162   1.148 0.250998    
thalach      0.0447845  0.0146914   3.048 0.002301 ** 
exang       -0.3655110  0.5152481  -0.709 0.478083    
oldpeak     -0.3268291  0.2654135  -1.231 0.218175    
slope        1.1989802  0.4436902   2.702 0.006886 ** 
ca          -0.8782676  0.2555764  -3.436 0.000589 ***
thal        -0.5068418  0.3544764  -1.430 0.152765    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 287.75  on 211  degrees of freedom
Residual deviance: 153.87  on 198  degrees of freedom
AIC: 181.87

Number of Fisher Scoring iterations: 6

[1] 0.8681319

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0938  -0.2700   0.1832   0.5749   2.2338  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)   
(Intercept)  4.457772   3.422573   1.302  0.19276   
ï..age      -0.001279   0.030877  -0.041  0.96697   
sex         -1.537988   0.548380  -2.805  0.00504 **
cp           0.578928   0.212109   2.729  0.00635 **
trestbps    -0.042490   0.013730  -3.095  0.00197 **
chol        -0.005678   0.005539  -1.025  0.30537   
fbs          0.547923   0.705302   0.777  0.43724   
restecg      0.679168   0.440004   1.544  0.12270   
thalach      0.033438   0.014298   2.339  0.01936 * 
exang       -1.594034   0.549850  -2.899  0.00374 **
oldpeak     -0.396294   0.260668  -1.520  0.12844   
slope        0.782593   0.441204   1.774  0.07610 . 
ca          -0.840749   0.296382  -2.837  0.00456 **
thal        -0.893380   0.360767  -2.476  0.01327 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 291.61  on 211  degrees of freedom
Residual deviance: 146.72  on 198  degrees of freedom
AIC: 174.72

Number of Fisher Scoring iterations: 6

[1] 0.8571429

Call:
glm(formula = target ~ ., family = "binomial", data = data1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.6775  -0.1650   0.1116   0.3543   2.6241  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)   
(Intercept) -0.860679   4.233468  -0.203  0.83890   
ï..age       0.027210   0.033722   0.807  0.41973   
sex         -2.178378   0.793632  -2.745  0.00605 **
cp           0.690605   0.261338   2.643  0.00823 **
trestbps    -0.034380   0.015394  -2.233  0.02553 * 
chol        -0.003934   0.007524  -0.523  0.60105   
fbs         -0.329560   0.808129  -0.408  0.68342   
restecg      0.232705   0.513874   0.453  0.65066   
thalach      0.053070   0.017968   2.954  0.00314 **
exang       -1.494122   0.644955  -2.317  0.02052 * 
oldpeak     -0.520168   0.339665  -1.531  0.12567   
slope        1.313905   0.492827   2.666  0.00767 **
ca          -0.798297   0.278139  -2.870  0.00410 **
thal        -1.088222   0.397504  -2.738  0.00619 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 290.70  on 211  degrees of freedom
Residual deviance: 111.31  on 198  degrees of freedom
AIC: 139.31

Number of Fisher Scoring iterations: 7

[1] 0.8791209
> #creating y*
> y_star=rep(0,91)
> y_star[y1>5]=1
> #creatingconfusion matrix
> table(y_star,data[ts,14])
      
y_star  0  1
     0 38  6
     1  4 43
> #storingaccuracy
> accuracy1=mean(y_star==data[ts,14])
> accuracy1
[1] 0.8901099
