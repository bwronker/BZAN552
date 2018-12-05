---
title: "Analyzing Grocery Store Purchases using Association Rules"
author: "Blog Post 2: Brandon Wronker"
date: "November 29, 2018"
output: 
  html_document:
    keep_md: true
always_allow_html: yes
---





```r
# Set Working Directory
setwd("C:/Users/bwronker/OneDrive/Documents/MSBA UTenn/BZAN 552 Multivariate/Posts")

# Read in our Product & Transactional datasets
prod <- read.csv("product.csv", header = TRUE, colClasses = c("factor",
                                                              "factor",
                                                              "factor",
                                                              "factor",
                                                              "factor"))

transactions <- read.csv("retail.csv", header = TRUE, colClasses = c("factor",
                                                              "factor",
                                                              "factor",
                                                              "factor",
                                                              "integer",
                                                              "numeric",
                                                              "numeric",
                                                              "numeric",
                                                              "integer"))
```



```r
## Preprocessing

# Join Tables on Product ID so that we have full product descriptions in our baskets
transfull <- merge(transactions, prod, by = "PRODUCT_ID")

# Let's narrow down our dataset by selecting the most relevant variables
transfull <- transfull[, c("household_key", "BASKET_ID", 
                           "PRODUCT_ID", "MANUFACTURER", 
                           "DEPARTMENT", "BRAND", "COMMODITY_DESC", 
                           "SUB_COMMODITY_DESC", 
                           "QUANTITY", "SALES_VALUE")]

# Let's take a closer look at our quantitative variables
summary(transfull[,c(9,10)])
```

```
##     QUANTITY       SALES_VALUE    
##  Min.   : 1.000   Min.   : 0.150  
##  1st Qu.: 1.000   1st Qu.: 1.250  
##  Median : 1.000   Median : 2.000  
##  Mean   : 1.258   Mean   : 2.679  
##  3rd Qu.: 1.000   3rd Qu.: 3.330  
##  Max.   :20.000   Max.   :20.000
```

```r
#install.packages("outliers")
library(outliers)

#### Identify outliers and oddities in Quantity

trans_quant <- transfull[,9]

plot(density(trans_quant), col = "red", main = "Item Quantity Distribution", xlab = "Quantity")
```

![](BlogPost2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
grubbs.result <- grubbs.test(trans_quant)
grubbs.result$p.value
```

```
## [1] 0
```

```r
grubbs.result$alternative
```

```
## [1] "highest value 20 is an outlier"
```

```r
#### Identify outliers and oddities in Sales Value

trans_sales <- transfull[,10]

plot(density(trans_sales), col = "red", main = "Item Sales Distribution", xlab = "Sales Value")
```

![](BlogPost2_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
grubbs.result <- grubbs.test(trans_sales)
grubbs.result$p.value
```

```
## [1] 1.8332e-12
```

```r
grubbs.result$alternative
```

```
## [1] "highest value 20 is an outlier"
```

**After seeing these results,** let's go ahead and remove some of the baskets with unusually high quantities in their baskets and check to see if our plot looks more normal!


```r
#### Remove outliers
transfull <- transfull[-which(transfull$QUANTITY >= 3),]
plot(density(transfull$QUANTITY), col =  "springgreen3", main = "Item Quantity Distribution", xlab = "Quantity")
```

![](BlogPost2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

**Now that** we've got our complete dataset, let's transform the data into a "transactions" table and look at some trends we find in the data.


```r
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'arules'
```

```
## The following objects are masked from 'package:base':
## 
##     abbreviate, write
```

```r
library(arulesViz)
```

```
## Loading required package: grid
```

```r
# Format our complete dataset into a "transactions" table
trans <- as(split(transfull[,"COMMODITY_DESC"], transfull[,"BASKET_ID"]), "transactions")
```

```
## Warning in asMethod(object): removing duplicated items in transactions
```

```r
# Let's look at the frequency with which our products were purchased before we do our Market Basket Analysis
itemFrequencyPlot(trans, topN = 20, col = "light blue", type = "absolute", horiz = TRUE, xlab = "Item Frequency (counts)", main = "Frequency of Commodity Purchased")
```

![](BlogPost2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

**As you can see** from our plot, the most popular product at this store (for this time period) is Soft Drinks! There's also quite a bit of lunch & dinner type foods. Sauces, gravy, beef, lunch meat, and bread are some of the most common foods in this dataset. We also see some snack food and desserts. To me, this points to a majority of the customers at this store being parents shopping for the whole family!


```r
# Creating basket rules of length 2 & length 3 with the same parameters for Confidence and Support
basket_rules2 <- apriori(trans, parameter = list(sup = 0.05, conf = 0.5, target="rules", minlen = 2, maxlen = 2))
```

```
## Warning in apriori(trans, parameter = list(sup = 0.05, conf = 0.5, target =
## "rules", : Mining stopped (maxlen reached). Only patterns up to a length of
## 2 returned!
```

```r
basket_rules3 <- apriori(trans, parameter = list(sup = 0.05, conf = 0.5, target="rules", minlen = 3, maxlen = 3))
```

```
## Warning in apriori(trans, parameter = list(sup = 0.05, conf = 0.5, target =
## "rules", : Mining stopped (maxlen reached). Only patterns up to a length of
## 3 returned!
```

**FOR REFERENCE:**

**SUPPORT:** The first measure called the **support** is the number of transactions that include items in the {LHS} and {RHS} parts of the rule as a percentage of the total number of transactions. It is a measure of how frequently the collection of items occur together as a percentage of all transactions. 
Interpreted as: Fraction of transactions that contain both LHS & RHS.

**CONFIDENCE:** The second measure called the **confidence** of the rule is the ratio of the number of transactions that include all items in {RHS} as well as the number of transactions that include all items in {LHS} to the number of transactions that include all items in {LHS}.
Interpreted as: How often items in RHS appear in transactions that contain LHS only.

**LIFT:** The **Lift** tells us how much better a rule is at predicting the result than just assuming the result in the first place (aka random chance). Greater lift values indicate stronger associations.
Interpreted as: How much our confidence has increased that RHS will be purchased given that LHS was in the basket.

*_______________________________________________*

***PLAY WITH OUR INTERACTIVE MAPS BELOW!***

***Let's start with "rules of length 2" -- meaning one product on the LHS and one product on the RHS***


```r
inspect(sort(basket_rules2, by="lift", decreasing = TRUE)[1:30])
```

```
##      lhs                                 rhs                            support confidence      lift count
## [1]  {SHORTENING/OIL}                 => {EGGS}                      0.05333333  1.0000000 12.500000     4
## [2]  {EGGS}                           => {SHORTENING/OIL}            0.05333333  0.6666667 12.500000     4
## [3]  {DELI MEATS}                     => {BAKED BREAD/BUNS/ROLLS}    0.10666667  1.0000000  6.250000     8
## [4]  {BAKED BREAD/BUNS/ROLLS}         => {DELI MEATS}                0.10666667  0.6666667  6.250000     8
## [5]  {COOKIES/CONES}                  => {ICE CREAM/MILK/SHERBTS}    0.09333333  0.8750000  5.468750     7
## [6]  {ICE CREAM/MILK/SHERBTS}         => {COOKIES/CONES}             0.09333333  0.5833333  5.468750     7
## [7]  {VEGETABLES - SHELF STABLE}      => {BEEF}                      0.14666667  0.9166667  4.583333    11
## [8]  {BEEF}                           => {VEGETABLES - SHELF STABLE} 0.14666667  0.7333333  4.583333    11
## [9]  {CRACKERS/MISC BKD FD}           => {VEGETABLES - SHELF STABLE} 0.06666667  0.7142857  4.464286     5
## [10] {VEGETABLES - SHELF STABLE}      => {DRY SAUCES/GRAVY}          0.16000000  1.0000000  4.166667    12
## [11] {DRY SAUCES/GRAVY}               => {VEGETABLES - SHELF STABLE} 0.16000000  0.6666667  4.166667    12
## [12] {CRACKERS/MISC BKD FD}           => {BEEF}                      0.06666667  0.7142857  3.571429     5
## [13] {LUNCHMEAT}                      => {BAKED BREAD/BUNS/ROLLS}    0.05333333  0.5714286  3.571429     4
## [14] {CRACKERS/MISC BKD FD}           => {CHEESE}                    0.06666667  0.7142857  3.151261     5
## [15] {REFRGRATD DOUGH PRODUCTS}       => {REFRGRATD JUICES/DRNKS}    0.05333333  0.6666667  3.125000     4
## [16] {DRY SAUCES/GRAVY}               => {BEEF}                      0.14666667  0.6111111  3.055556    11
## [17] {BEEF}                           => {DRY SAUCES/GRAVY}          0.14666667  0.7333333  3.055556    11
## [18] {CRACKERS/MISC BKD FD}           => {DRY SAUCES/GRAVY}          0.06666667  0.7142857  2.976190     5
## [19] {COOKIES/CONES}                  => {FLUID MILK PRODUCTS}       0.05333333  0.5000000  2.678571     4
## [20] {CONVENIENT BRKFST/WHLSM SNACKS} => {FLUID MILK PRODUCTS}       0.05333333  0.5000000  2.678571     4
## [21] {BEEF}                           => {CHEESE}                    0.12000000  0.6000000  2.647059     9
## [22] {CHEESE}                         => {BEEF}                      0.12000000  0.5294118  2.647059     9
## [23] {VEGETABLES - SHELF STABLE}      => {CHEESE}                    0.09333333  0.5833333  2.573529     7
## [24] {DINNER MXS:DRY}                 => {SOFT DRINKS}               0.05333333  1.0000000  2.272727     4
## [25] {DELI MEATS}                     => {CHEESE}                    0.05333333  0.5000000  2.205882     4
## [26] {BAKED BREAD/BUNS/ROLLS}         => {CHEESE}                    0.08000000  0.5000000  2.205882     6
## [27] {SALAD BAR}                      => {SOFT DRINKS}               0.05333333  0.8000000  1.818182     4
## [28] {FROZEN PIZZA}                   => {SOFT DRINKS}               0.05333333  0.8000000  1.818182     4
## [29] {DELI MEATS}                     => {SOFT DRINKS}               0.08000000  0.7500000  1.704545     6
## [30] {VEGETABLES - SHELF STABLE}      => {SOFT DRINKS}               0.12000000  0.7500000  1.704545     9
```

**BELOW** you will find a fun interactive tool where you can see which two products are purchased together most often! But before we look at that, let's highlight a few interesting numbers. 

It is no surprise that Oil/Shortening & Eggs are purchased together quite often. This is the basis for many great meals such as breakfasts, fried food, as well as desserts. We also see that Lunchmeat, Breads, and & Cheeses are often ourchased together. And finally, on a less healthy note, we can see from Rule 28 that Frozen Pizzas and Soft Drinks are a pretty dynamic duo!


```r
subrules2 <- head(basket_rules2, n = 20, by = "lift")
plot(subrules2, method = "graph", engine = "htmlwidget")
```

<!--html_preserve--><div id="htmlwidget-8b587729bdf66383f28d" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-8b587729bdf66383f28d">{"x":{"nodes":{"id":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36],"label":["SHORTENING/OIL","EGGS","DELI MEATS","BAKED BREAD/BUNS/ROLLS","COOKIES/CONES","ICE CREAM/MILK/SHERBTS","VEGETABLES - SHELF STABLE","BEEF","CRACKERS/MISC BKD FD","DRY SAUCES/GRAVY","LUNCHMEAT","REFRGRATD DOUGH PRODUCTS","CONVENIENT BRKFST/WHLSM SNACKS","CHEESE","REFRGRATD JUICES/DRNKS","FLUID MILK PRODUCTS","rule 1","rule 2","rule 3","rule 4","rule 5","rule 6","rule 7","rule 8","rule 9","rule 10","rule 11","rule 12","rule 13","rule 14","rule 15","rule 16","rule 17","rule 18","rule 19","rule 20"],"group":["item","item","item","item","item","item","item","item","item","item","item","item","item","item","item","item","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule"],"value":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,50.5,50.5,38.125,38.125,87.625,87.625,13.375,100,100,13.375,1,13.375,1,87.625,87.625,13.375,1,1],"color":["#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#EE1B1B","#EE1B1B","#EEABAB","#EEABAB","#EEB5B5","#EEB5B5","#EEC1C1","#EEC1C1","#EEC3C3","#EEC8C8","#EEC8C8","#EECFCF","#EECFCF","#EED5D5","#EED5D5","#EED7D7","#EED7D7","#EED7D7","#EEDCDC","#EEDCDC"],"title":["SHORTENING/OIL","EGGS","DELI MEATS","BAKED BREAD/BUNS/ROLLS","COOKIES/CONES","ICE CREAM/MILK/SHERBTS","VEGETABLES - SHELF STABLE","BEEF","CRACKERS/MISC BKD FD","DRY SAUCES/GRAVY","LUNCHMEAT","REFRGRATD DOUGH PRODUCTS","CONVENIENT BRKFST/WHLSM SNACKS","CHEESE","REFRGRATD JUICES/DRNKS","FLUID MILK PRODUCTS","<B>[1]<\/B><BR><B>{SHORTENING/OIL}<\/B><BR>&nbsp;&nbsp; => <B>{EGGS}<\/B><BR><BR>support = 0.0533<BR>confidence = 1<BR>lift = 12.5<BR>count = 4<BR>order = 2","<B>[2]<\/B><BR><B>{EGGS}<\/B><BR>&nbsp;&nbsp; => <B>{SHORTENING/OIL}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.667<BR>lift = 12.5<BR>count = 4<BR>order = 2","<B>[3]<\/B><BR><B>{DELI MEATS}<\/B><BR>&nbsp;&nbsp; => <B>{BAKED BREAD/BUNS/ROLLS}<\/B><BR><BR>support = 0.107<BR>confidence = 1<BR>lift = 6.25<BR>count = 8<BR>order = 2","<B>[4]<\/B><BR><B>{BAKED BREAD/BUNS/ROLLS}<\/B><BR>&nbsp;&nbsp; => <B>{DELI MEATS}<\/B><BR><BR>support = 0.107<BR>confidence = 0.667<BR>lift = 6.25<BR>count = 8<BR>order = 2","<B>[5]<\/B><BR><B>{COOKIES/CONES}<\/B><BR>&nbsp;&nbsp; => <B>{ICE CREAM/MILK/SHERBTS}<\/B><BR><BR>support = 0.0933<BR>confidence = 0.875<BR>lift = 5.47<BR>count = 7<BR>order = 2","<B>[6]<\/B><BR><B>{ICE CREAM/MILK/SHERBTS}<\/B><BR>&nbsp;&nbsp; => <B>{COOKIES/CONES}<\/B><BR><BR>support = 0.0933<BR>confidence = 0.583<BR>lift = 5.47<BR>count = 7<BR>order = 2","<B>[7]<\/B><BR><B>{VEGETABLES - SHELF STABLE}<\/B><BR>&nbsp;&nbsp; => <B>{BEEF}<\/B><BR><BR>support = 0.147<BR>confidence = 0.917<BR>lift = 4.58<BR>count = 11<BR>order = 2","<B>[8]<\/B><BR><B>{BEEF}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.147<BR>confidence = 0.733<BR>lift = 4.58<BR>count = 11<BR>order = 2","<B>[9]<\/B><BR><B>{CRACKERS/MISC BKD FD}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.0667<BR>confidence = 0.714<BR>lift = 4.46<BR>count = 5<BR>order = 2","<B>[10]<\/B><BR><B>{VEGETABLES - SHELF STABLE}<\/B><BR>&nbsp;&nbsp; => <B>{DRY SAUCES/GRAVY}<\/B><BR><BR>support = 0.16<BR>confidence = 1<BR>lift = 4.17<BR>count = 12<BR>order = 2","<B>[11]<\/B><BR><B>{DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.16<BR>confidence = 0.667<BR>lift = 4.17<BR>count = 12<BR>order = 2","<B>[12]<\/B><BR><B>{CRACKERS/MISC BKD FD}<\/B><BR>&nbsp;&nbsp; => <B>{BEEF}<\/B><BR><BR>support = 0.0667<BR>confidence = 0.714<BR>lift = 3.57<BR>count = 5<BR>order = 2","<B>[13]<\/B><BR><B>{LUNCHMEAT}<\/B><BR>&nbsp;&nbsp; => <B>{BAKED BREAD/BUNS/ROLLS}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.571<BR>lift = 3.57<BR>count = 4<BR>order = 2","<B>[14]<\/B><BR><B>{CRACKERS/MISC BKD FD}<\/B><BR>&nbsp;&nbsp; => <B>{CHEESE}<\/B><BR><BR>support = 0.0667<BR>confidence = 0.714<BR>lift = 3.15<BR>count = 5<BR>order = 2","<B>[15]<\/B><BR><B>{REFRGRATD DOUGH PRODUCTS}<\/B><BR>&nbsp;&nbsp; => <B>{REFRGRATD JUICES/DRNKS}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.667<BR>lift = 3.12<BR>count = 4<BR>order = 2","<B>[16]<\/B><BR><B>{DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{BEEF}<\/B><BR><BR>support = 0.147<BR>confidence = 0.611<BR>lift = 3.06<BR>count = 11<BR>order = 2","<B>[17]<\/B><BR><B>{BEEF}<\/B><BR>&nbsp;&nbsp; => <B>{DRY SAUCES/GRAVY}<\/B><BR><BR>support = 0.147<BR>confidence = 0.733<BR>lift = 3.06<BR>count = 11<BR>order = 2","<B>[18]<\/B><BR><B>{CRACKERS/MISC BKD FD}<\/B><BR>&nbsp;&nbsp; => <B>{DRY SAUCES/GRAVY}<\/B><BR><BR>support = 0.0667<BR>confidence = 0.714<BR>lift = 2.98<BR>count = 5<BR>order = 2","<B>[19]<\/B><BR><B>{COOKIES/CONES}<\/B><BR>&nbsp;&nbsp; => <B>{FLUID MILK PRODUCTS}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.5<BR>lift = 2.68<BR>count = 4<BR>order = 2","<B>[20]<\/B><BR><B>{CONVENIENT BRKFST/WHLSM SNACKS}<\/B><BR>&nbsp;&nbsp; => <B>{FLUID MILK PRODUCTS}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.5<BR>lift = 2.68<BR>count = 4<BR>order = 2"],"shape":["box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle"],"x":[0.28974885649553,0.611762148745311,0.452521072281252,0.727794014289391,-0.729645409155507,-1,-0.324395636999208,-0.71614747895517,-0.104819931631239,-0.598792290950268,1,0.978634979187628,0.0108178573684607,0.367764134702667,0.950737440061607,-0.289713941696959,0.453816102572094,0.44869121767986,0.679826118383186,0.495240779617389,-0.96711919649446,-0.769730974012492,-0.592987106803225,-0.469560294065265,-0.017013646396594,-0.307068348609053,-0.508781275254843,-0.432258958365487,0.866164641634997,0.138119837542118,0.939046598576569,-0.898752440590371,-0.866580739270147,-0.266207347586951,-0.512916901534411,-0.179118409079116],"y":[0.648083583517214,0.661624564291817,-1,-0.705753707567803,0.569583202055869,0.275953774029146,-0.745624163057036,-0.352687434180544,-0.269063605442408,-0.76859226875107,-0.340108432043778,0.00174370761611931,1,0.016861243693485,0.401548582972546,0.719687210176372,0.494435384562217,0.814358893647882,-0.945295577925723,-0.770680142358998,0.505909281395585,0.337965896502928,-0.577651176489658,-0.426282459321352,-0.575102368306668,-0.980955471244554,-0.997348093730828,-0.130322878783711,-0.504823589905351,-0.0724829079490144,0.196440114859409,-0.497900306878642,-0.66215568667875,-0.531752759408872,0.683042302808898,0.905998857251457]},"edges":{"from":[1,2,3,4,5,6,7,8,9,7,10,9,11,9,12,10,8,9,5,13,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36],"to":[17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,2,1,4,3,6,5,8,7,7,10,7,8,4,14,15,8,10,10,16,16],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","scaling":{"label":{"enabled":true}},"physics":false},"manipulation":{"enabled":false},"edges":{"smooth":false},"physics":{"stabilization":false},"interaction":{"hover":true}},"groups":["item","rule"],"width":null,"height":null,"idselection":{"enabled":true,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)"},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"square"},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);","highlight":{"enabled":true,"hoverNearest":true,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

*_____________*

***And Finally we have our "rules of length 3" -- meaning two products on the LHS and one product on the RHS***


```r
inspect(sort(basket_rules3, by="lift", decreasing = TRUE)[1:30])
```

```
##      lhs                            rhs                            support confidence     lift count
## [1]  {CRACKERS/MISC BKD FD,                                                                         
##       DRY SAUCES/GRAVY}          => {VEGETABLES - SHELF STABLE} 0.06666667  1.0000000 6.250000     5
## [2]  {BEEF,                                                                                         
##       CRACKERS/MISC BKD FD}      => {VEGETABLES - SHELF STABLE} 0.06666667  1.0000000 6.250000     5
## [3]  {COOKIES/CONES,                                                                                
##       SOFT DRINKS}               => {ICE CREAM/MILK/SHERBTS}    0.05333333  1.0000000 6.250000     4
## [4]  {CHEESE,                                                                                       
##       DELI MEATS}                => {BAKED BREAD/BUNS/ROLLS}    0.05333333  1.0000000 6.250000     4
## [5]  {BAKED BREAD/BUNS/ROLLS,                                                                       
##       CHEESE}                    => {DELI MEATS}                0.05333333  0.6666667 6.250000     4
## [6]  {DELI MEATS,                                                                                   
##       SOFT DRINKS}               => {BAKED BREAD/BUNS/ROLLS}    0.08000000  1.0000000 6.250000     6
## [7]  {BAKED BREAD/BUNS/ROLLS,                                                                       
##       SOFT DRINKS}               => {DELI MEATS}                0.08000000  0.6666667 6.250000     6
## [8]  {LUNCHMEAT,                                                                                    
##       SOFT DRINKS}               => {BAKED BREAD/BUNS/ROLLS}    0.05333333  1.0000000 6.250000     4
## [9]  {BEEF,                                                                                         
##       DRY SAUCES/GRAVY}          => {VEGETABLES - SHELF STABLE} 0.14666667  1.0000000 6.250000    11
## [10] {CHEESE,                                                                                       
##       DRY SAUCES/GRAVY}          => {VEGETABLES - SHELF STABLE} 0.09333333  1.0000000 6.250000     7
## [11] {CHEESE,                                                                                       
##       VEGETABLES - SHELF STABLE} => {CRACKERS/MISC BKD FD}      0.05333333  0.5714286 6.122449     4
## [12] {CHEESE,                                                                                       
##       DRY SAUCES/GRAVY}          => {CRACKERS/MISC BKD FD}      0.05333333  0.5714286 6.122449     4
## [13] {DRY SAUCES/GRAVY,                                                                             
##       SOFT DRINKS}               => {VEGETABLES - SHELF STABLE} 0.12000000  0.9000000 5.625000     9
## [14] {ICE CREAM/MILK/SHERBTS,                                                                       
##       SOFT DRINKS}               => {COOKIES/CONES}             0.05333333  0.5714286 5.357143     4
## [15] {CRACKERS/MISC BKD FD,                                                                         
##       VEGETABLES - SHELF STABLE} => {BEEF}                      0.06666667  1.0000000 5.000000     5
## [16] {CHEESE,                                                                                       
##       CRACKERS/MISC BKD FD}      => {VEGETABLES - SHELF STABLE} 0.05333333  0.8000000 5.000000     4
## [17] {CRACKERS/MISC BKD FD,                                                                         
##       DRY SAUCES/GRAVY}          => {BEEF}                      0.06666667  1.0000000 5.000000     5
## [18] {CHEESE,                                                                                       
##       VEGETABLES - SHELF STABLE} => {BEEF}                      0.09333333  1.0000000 5.000000     7
## [19] {BEEF,                                                                                         
##       SOFT DRINKS}               => {VEGETABLES - SHELF STABLE} 0.10666667  0.8000000 5.000000     8
## [20] {CHEESE,                                                                                       
##       DRY SAUCES/GRAVY}          => {BEEF}                      0.09333333  1.0000000 5.000000     7
## [21] {BEEF,                                                                                         
##       CHEESE}                    => {VEGETABLES - SHELF STABLE} 0.09333333  0.7777778 4.861111     7
## [22] {DRY SAUCES/GRAVY,                                                                             
##       VEGETABLES - SHELF STABLE} => {BEEF}                      0.14666667  0.9166667 4.583333    11
## [23] {SOFT DRINKS,                                                                                  
##       VEGETABLES - SHELF STABLE} => {BEEF}                      0.10666667  0.8888889 4.444444     8
## [24] {CRACKERS/MISC BKD FD,                                                                         
##       VEGETABLES - SHELF STABLE} => {DRY SAUCES/GRAVY}          0.06666667  1.0000000 4.166667     5
## [25] {BEEF,                                                                                         
##       CRACKERS/MISC BKD FD}      => {DRY SAUCES/GRAVY}          0.06666667  1.0000000 4.166667     5
## [26] {BEEF,                                                                                         
##       VEGETABLES - SHELF STABLE} => {DRY SAUCES/GRAVY}          0.14666667  1.0000000 4.166667    11
## [27] {CHEESE,                                                                                       
##       VEGETABLES - SHELF STABLE} => {DRY SAUCES/GRAVY}          0.09333333  1.0000000 4.166667     7
## [28] {SOFT DRINKS,                                                                                  
##       VEGETABLES - SHELF STABLE} => {DRY SAUCES/GRAVY}          0.12000000  1.0000000 4.166667     9
## [29] {CHEESE,                                                                                       
##       CRACKERS/MISC BKD FD}      => {BEEF}                      0.05333333  0.8000000 4.000000     4
## [30] {DRY SAUCES/GRAVY,                                                                             
##       SOFT DRINKS}               => {BEEF}                      0.10666667  0.8000000 4.000000     8
```

**BELOW** you will find a fun interactive tool where you can see which three products are purchased together most often! But before we look at that, let's highlight a few interesting numbers. 

Looking at our basket rules in groups of three can shed some light on what people are using in homemade meals! For example, Beef,Gravy & Vegetables in Rule 9. My favorite from this set of rules, however, is Rule 14 -- Ice Cream, Soft Drinks & Cookies/Cones!


```r
subrules3 <- head(basket_rules3, n = 20, by = "lift")
plot(subrules3, method = "graph", engine = "htmlwidget")
```

<!--html_preserve--><div id="htmlwidget-486e0e159e82e71b0b5f" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-486e0e159e82e71b0b5f">{"x":{"nodes":{"id":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31],"label":["CRACKERS/MISC BKD FD","DRY SAUCES/GRAVY","BEEF","COOKIES/CONES","SOFT DRINKS","CHEESE","DELI MEATS","BAKED BREAD/BUNS/ROLLS","LUNCHMEAT","VEGETABLES - SHELF STABLE","ICE CREAM/MILK/SHERBTS","rule 1","rule 2","rule 3","rule 4","rule 5","rule 6","rule 7","rule 8","rule 9","rule 10","rule 11","rule 12","rule 13","rule 14","rule 15","rule 16","rule 17","rule 18","rule 19","rule 20"],"group":["item","item","item","item","item","item","item","item","item","item","item","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule","rule"],"value":[1,1,1,1,1,1,1,1,1,1,1,15.1428571428571,15.1428571428571,1,1,1,29.2857142857143,29.2857142857143,1,100,43.4285714285714,1,1,71.7142857142857,1,15.1428571428571,1,15.1428571428571,43.4285714285714,57.5714285714286,43.4285714285714],"color":["#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#EE1B1B","#EE1B1B","#EE1B1B","#EE1B1B","#EE1B1B","#EE1B1B","#EE1B1B","#EE1B1B","#EE1B1B","#EE1B1B","#EE3434","#EE3434","#EE9797","#EEB5B5","#EEDCDC","#EEDCDC","#EEDCDC","#EEDCDC","#EEDCDC","#EEDCDC"],"title":["CRACKERS/MISC BKD FD","DRY SAUCES/GRAVY","BEEF","COOKIES/CONES","SOFT DRINKS","CHEESE","DELI MEATS","BAKED BREAD/BUNS/ROLLS","LUNCHMEAT","VEGETABLES - SHELF STABLE","ICE CREAM/MILK/SHERBTS","<B>[1]<\/B><BR><B>{CRACKERS/MISC BKD FD,<BR>&nbsp;&nbsp;DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.0667<BR>confidence = 1<BR>lift = 6.25<BR>count = 5<BR>order = 3","<B>[2]<\/B><BR><B>{BEEF,<BR>&nbsp;&nbsp;CRACKERS/MISC BKD FD}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.0667<BR>confidence = 1<BR>lift = 6.25<BR>count = 5<BR>order = 3","<B>[3]<\/B><BR><B>{COOKIES/CONES,<BR>&nbsp;&nbsp;SOFT DRINKS}<\/B><BR>&nbsp;&nbsp; => <B>{ICE CREAM/MILK/SHERBTS}<\/B><BR><BR>support = 0.0533<BR>confidence = 1<BR>lift = 6.25<BR>count = 4<BR>order = 3","<B>[4]<\/B><BR><B>{CHEESE,<BR>&nbsp;&nbsp;DELI MEATS}<\/B><BR>&nbsp;&nbsp; => <B>{BAKED BREAD/BUNS/ROLLS}<\/B><BR><BR>support = 0.0533<BR>confidence = 1<BR>lift = 6.25<BR>count = 4<BR>order = 3","<B>[5]<\/B><BR><B>{BAKED BREAD/BUNS/ROLLS,<BR>&nbsp;&nbsp;CHEESE}<\/B><BR>&nbsp;&nbsp; => <B>{DELI MEATS}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.667<BR>lift = 6.25<BR>count = 4<BR>order = 3","<B>[6]<\/B><BR><B>{DELI MEATS,<BR>&nbsp;&nbsp;SOFT DRINKS}<\/B><BR>&nbsp;&nbsp; => <B>{BAKED BREAD/BUNS/ROLLS}<\/B><BR><BR>support = 0.08<BR>confidence = 1<BR>lift = 6.25<BR>count = 6<BR>order = 3","<B>[7]<\/B><BR><B>{BAKED BREAD/BUNS/ROLLS,<BR>&nbsp;&nbsp;SOFT DRINKS}<\/B><BR>&nbsp;&nbsp; => <B>{DELI MEATS}<\/B><BR><BR>support = 0.08<BR>confidence = 0.667<BR>lift = 6.25<BR>count = 6<BR>order = 3","<B>[8]<\/B><BR><B>{LUNCHMEAT,<BR>&nbsp;&nbsp;SOFT DRINKS}<\/B><BR>&nbsp;&nbsp; => <B>{BAKED BREAD/BUNS/ROLLS}<\/B><BR><BR>support = 0.0533<BR>confidence = 1<BR>lift = 6.25<BR>count = 4<BR>order = 3","<B>[9]<\/B><BR><B>{BEEF,<BR>&nbsp;&nbsp;DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.147<BR>confidence = 1<BR>lift = 6.25<BR>count = 11<BR>order = 3","<B>[10]<\/B><BR><B>{CHEESE,<BR>&nbsp;&nbsp;DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.0933<BR>confidence = 1<BR>lift = 6.25<BR>count = 7<BR>order = 3","<B>[11]<\/B><BR><B>{CHEESE,<BR>&nbsp;&nbsp;VEGETABLES - SHELF STABLE}<\/B><BR>&nbsp;&nbsp; => <B>{CRACKERS/MISC BKD FD}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.571<BR>lift = 6.12<BR>count = 4<BR>order = 3","<B>[12]<\/B><BR><B>{CHEESE,<BR>&nbsp;&nbsp;DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{CRACKERS/MISC BKD FD}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.571<BR>lift = 6.12<BR>count = 4<BR>order = 3","<B>[13]<\/B><BR><B>{DRY SAUCES/GRAVY,<BR>&nbsp;&nbsp;SOFT DRINKS}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.12<BR>confidence = 0.9<BR>lift = 5.62<BR>count = 9<BR>order = 3","<B>[14]<\/B><BR><B>{ICE CREAM/MILK/SHERBTS,<BR>&nbsp;&nbsp;SOFT DRINKS}<\/B><BR>&nbsp;&nbsp; => <B>{COOKIES/CONES}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.571<BR>lift = 5.36<BR>count = 4<BR>order = 3","<B>[15]<\/B><BR><B>{CRACKERS/MISC BKD FD,<BR>&nbsp;&nbsp;VEGETABLES - SHELF STABLE}<\/B><BR>&nbsp;&nbsp; => <B>{BEEF}<\/B><BR><BR>support = 0.0667<BR>confidence = 1<BR>lift = 5<BR>count = 5<BR>order = 3","<B>[16]<\/B><BR><B>{CHEESE,<BR>&nbsp;&nbsp;CRACKERS/MISC BKD FD}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.8<BR>lift = 5<BR>count = 4<BR>order = 3","<B>[17]<\/B><BR><B>{CRACKERS/MISC BKD FD,<BR>&nbsp;&nbsp;DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{BEEF}<\/B><BR><BR>support = 0.0667<BR>confidence = 1<BR>lift = 5<BR>count = 5<BR>order = 3","<B>[18]<\/B><BR><B>{CHEESE,<BR>&nbsp;&nbsp;VEGETABLES - SHELF STABLE}<\/B><BR>&nbsp;&nbsp; => <B>{BEEF}<\/B><BR><BR>support = 0.0933<BR>confidence = 1<BR>lift = 5<BR>count = 7<BR>order = 3","<B>[19]<\/B><BR><B>{BEEF,<BR>&nbsp;&nbsp;SOFT DRINKS}<\/B><BR>&nbsp;&nbsp; => <B>{VEGETABLES - SHELF STABLE}<\/B><BR><BR>support = 0.107<BR>confidence = 0.8<BR>lift = 5<BR>count = 8<BR>order = 3","<B>[20]<\/B><BR><B>{CHEESE,<BR>&nbsp;&nbsp;DRY SAUCES/GRAVY}<\/B><BR>&nbsp;&nbsp; => <B>{BEEF}<\/B><BR><BR>support = 0.0933<BR>confidence = 1<BR>lift = 5<BR>count = 7<BR>order = 3"],"shape":["box","box","box","box","box","box","box","box","box","box","box","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle"],"x":[-0.39284586294158,-0.435703572429134,-0.72269056983479,-0.786622828350435,-0.0872592684552004,0.389781888346041,0.952363780212411,0.864309648302378,1,-0.443649757437351,-0.488588416089084,-0.564453089016483,-0.946474178638392,-0.542099857378203,0.780712801767575,0.970152252847368,0.500613756420826,0.629258776421364,0.631827601652117,-1,0.0798918361624847,-0.0417307931839176,0.090112568280496,-0.310771672489717,-0.35320302518465,-0.76595818050843,0.149088091386791,-0.866329898543143,-0.187758240240674,-0.544321611402348,-0.146422113402207],"y":[-1,-0.709740841238876,-0.602037412433959,0.927907869635705,0.308857634282629,-0.564705664306096,0.0708687105973258,0.175386756203947,0.852357962990537,-0.602897215883662,1,-0.939644798153799,-0.807392066027256,0.706171328906942,-0.166957787187726,-0.198297621608702,0.237867703943124,0.32950215098719,0.5411110588663,-0.590807291723637,-0.621900221613941,-0.832224263919959,-0.947876031369905,-0.226483296669774,0.755854849915106,-0.850863017735964,-0.829645271383951,-0.963176601124707,-0.476283664799254,-0.179020341920213,-0.649577207846874]},"edges":{"from":[1,2,3,1,4,5,6,7,8,6,7,5,8,5,9,5,3,2,6,2,6,10,6,2,2,5,11,5,1,10,6,1,1,2,6,10,3,5,6,2,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31],"to":[12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,22,23,23,24,24,25,25,26,26,27,27,28,28,29,29,30,30,31,31,10,10,11,8,7,8,7,8,10,10,1,1,10,4,3,10,3,3,10,3],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","scaling":{"label":{"enabled":true}},"physics":false},"manipulation":{"enabled":false},"edges":{"smooth":false},"physics":{"stabilization":false},"interaction":{"hover":true}},"groups":["item","rule"],"width":null,"height":null,"idselection":{"enabled":true,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)"},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"square"},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);","highlight":{"enabled":true,"hoverNearest":true,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**THANK YOU FOR READING!**

