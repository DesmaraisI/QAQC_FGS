---
title: "FGS QAQC update March 2020"
author: "Isabelle"
date: "3/23/2020"
output: 
  html_document: 
    keep_md: yes
---
# INTRODUCTION

The purpose of this document is to review the state of QAQC for the FGS surveys.




# State of DO13C data

## Results flags
All of the "NA" flags for end of 2019 and beginning of 2020 are related to results that have not yet been QC'd, therefore, they had no flags attached before being exported to R.


## Quality levels
"Raw" quality levels are from recent FGS surveys that have not been QC'd by the oceanography team yet.

2015 missing values were given a "Error" as Quality level. They have been changed to PI or technician, depending on the situation.


```r
DO13C_qualitylevel<-ggplot(DO13C_FGS_data, aes(x=date, y=ppm_c_doc)) + geom_point(aes(shape=quality_level, color=ppm_c_doc_flag)) + theme_linedraw()

plot(DO13C_qualitylevel)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
DO13C_range_survey<-ggplot(DO13C_FGS_data, aes(x=date, y=ppm_c_doc)) + theme_linedraw() + geom_point(aes(color=ppm_c_doc_flag)) + facet_grid(survey~.)

plot(DO13C_range_survey)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
DO13C_range_rivers<-ggplot(subset(DO13C_FGS_data, survey == "FGS"), aes(x=date, y=ppm_c_doc)) + geom_point(aes(colour=site_id)) + theme_linedraw()
plot(DO13C_range_rivers)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

# State of DOC data

Double surveys (FGS and FGS_R) are appropriate as the sample collected was for both surveys instead of collecting 2 samples on the same day. I will fix the order of the surveys so they plot in the same grid.

All "NA" flags are actually results that don't have any flags associated to them yet.


```r
DOC_qualitylevel<-ggplot(DOC_FGS_data, aes(x=date, y=doc)) + geom_point(aes(shape=quality_level, color=doc_flag)) + theme_linedraw()

plot(DOC_qualitylevel)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
DOC_range_survey<-ggplot(DOC_FGS_data, aes(x=date, y=doc)) + theme_linedraw() + geom_point(aes(color=doc_flag)) + facet_grid(survey~.)

plot(DOC_range_survey)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
DOC_range_rivers<-ggplot(subset(DOC_FGS_data, survey == "FGS"), aes(x=date, y=doc)) + geom_point(aes(colour=site_id)) + theme_linedraw()

plot(DOC_range_rivers)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

# State of O18 data

"Raw" quality levels are from recent FGS surveys that have not been QC'd by the oceanography team yet.They do not show up in the figures because we haven't sent them to the lab for analysis yet (n=18).

All "NA" flags are from results that hasn't been flagged yet.


```r
O18_qualitylevel<-ggplot(O18_FGS_data, aes(x=date, y=delta_o18)) + geom_point(aes(shape=quality_level, color=delta_o18_flag)) + theme_linedraw()

plot(O18_qualitylevel)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
h2_quality_level<-ggplot(O18_FGS_data, aes(x=date, y=delta_h2)) + geom_point(aes(shape=quality_level, color=delta_h2_flag)) + theme_linedraw()

plot(h2_quality_level)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
O18vsH2_GMWL<-ggplot(O18_FGS_data,aes(delta_o18,delta_h2,colour=survey))+geom_point()+theme_linedraw()+stat_function(fun=function(x)8*x+10)

plot(O18vsH2_GMWL)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

# State of nutrients data

The "raw" quality levels (n=2) are from 2016 Wannock samples. The oceanographers have been made aware of it and will work to resolve it.


```r
#Overall review of the counts of Quality level of all nutrient samples
Nut_qualitylevel<-ggplot(Nut_FGS_data,aes(x=quality_level,fill=survey))+geom_bar()
Nut_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#NH4 
NH4_qualitylevel<-ggplot(Nut_FGS_data, aes(x=date, y=nh4_)) + geom_point(aes(shape=quality_level, color=nh4__flag)) + theme_linedraw()+ facet_grid(survey~.)
NH4_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
#TP
TP_qualitylevel<-ggplot(Nut_FGS_data, aes(x=date, y=tp)) + geom_point(aes(shape=quality_level, color=tp_flag)) + theme_linedraw()+ facet_grid(survey~.)
TP_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
#TN
TN_qualitylevel<-ggplot(Nut_FGS_data, aes(x=date, y=tn)) + geom_point(aes(shape=quality_level, color=tn_flag)) + theme_linedraw()+ facet_grid(survey~.)
TN_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

```r
#TDP
TDP_qualitylevel<-ggplot(Nut_FGS_data, aes(x=date, y=tdp)) + geom_point(aes(shape=quality_level, color=tdp_flag)) + theme_linedraw()+ facet_grid(survey~.)
TDP_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

```r
#TDN
TDN_qualitylevel<-ggplot(Nut_FGS_data, aes(x=date, y=tdn)) + geom_point(aes(shape=quality_level, color=tdn_flag)) + theme_linedraw()+ facet_grid(survey~.)
TDN_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-5-6.png)<!-- -->

# State of TSS data

All "NA" flags are from results that hasn't been flagged yet.

The "raw" quality levels are all from recent (October 2019 to February 2020) FGS samples that have not been QC'd by the oceanographers yet.


```r
TSS_qualitylevel<-ggplot(TSS_FGS_data, aes(x=date, y=tss_mg_l)) + geom_point(aes(shape=quality_level, color=tss_flag)) + theme_linedraw()

TSS_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
TSS_range_survey<-ggplot(TSS_FGS_data, aes(x=date, y=tss_mg_l)) + theme_linedraw() + geom_point(aes(color=tss_flag)) + facet_grid(survey~.)

TSS_range_survey
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
TSS_range_rivers<-ggplot(subset(TSS_FGS_data, survey == "FGS"), aes(x=date, y=tss_mg_l)) + geom_point(aes(colour=site_id)) + theme_linedraw()

plot(TSS_range_rivers)
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

# State of POM data

All of the "Raw" Quality level are for FGS samples collected between July 2019 and February 2020. The oceanographers are working through those surveys.

For C and N flags, NAs seen in the graphs means that there is no currently flags assigned. 

It should be noted that there are many POM samples that have not yet been sent for analysis. 


```r
POM_n_qualitylevel<-ggplot(POM_FGS_data, aes(x=date, y=ug_n)) + geom_point(aes(shape=quality_level, color=n_flag)) + theme_linedraw()

POM_n_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
POM_c_qualitylevel<-ggplot(POM_FGS_data, aes(x=date, y=ug_c)) + geom_point(aes(shape=quality_level, color=c_flag)) + theme_linedraw()

POM_c_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

#State of Akan and Cations data

Some "Raw" samples were from a 2016 FG survey. The oceanographers have been informed and will work through it. The other "Raw" samples are from recent FGS surveys not yet QC'd.




```r
#OVerall review of the Quality level of all nutrient samples
Akan_Cat_qualitylevel<-ggplot(Akan_Cat_data,aes(x=quality_level,fill=survey))+geom_bar()
Akan_Cat_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#Fe
Fe_qualitylevel<-ggplot(Akan_Cat_data, aes(x=date, y=fe)) + geom_point(aes(shape=quality_level, color=fe_flag)) + theme_linedraw()
Fe_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
#Ca
Ca_qualitylevel<-ggplot(Akan_Cat_data, aes(x=date, y=ca)) + geom_point(aes(shape=quality_level, color=ca_flag)) + theme_linedraw()
Ca_qualitylevel
```

![](FGS_QAQC_March2020_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

