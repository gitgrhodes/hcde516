
# assigning csv to var d for data, b for ... titles?
d <-ratingsCleaned # values only/no text
b <-ratingsCleaned_names # same data but with re-coded age, gender, edu from values back to text; I used for some nominal data since I needed their names/categories

#slicing into TREATMENT groups 
d.ctl <- subset(d,d$tmt.group=="ctl")
d.algo <- subset(d,d$tmt.group=="algo")
d.hum <- subset(d,d$tmt.group=="hum")

#slicing again for NAMES;used as an alt for when I need to show nomnial lables 
b.ctl <- subset(b,b$tmt.group=="ctl")
b.algo <- subset(b,b$tmt.group=="algo")
b.hum <- subset(b,b$tmt.group=="hum")

#Slicing into PRODUCT groups 
d.raz <-subset(d,d$prod.grp=="raz") #99 cases
d.tooth <-subset(d,d$prod.grp=="tooth") #103 cases

#Slicing again for NAMES; used as an alt for when I need to show nomnial lables 
b.raz <-subset(b,b$prod.grp=="raz") 
b.tooth <-subset(b,b$prod.grp=="tooth")

b.tooth.gender <-subset(b,b$prod.grp=="tooth" & (b$gender=="Male" | b$gender=="Female"))


#Slicing into PRODUCT+TREATMENT groups
d.ctl_raz <-subset(d, d$group=="Raz.Ctl")
d.ctl_tooth <-subset(d, d$group=="Tooth.Ctl")
d.algo_tooth <-subset(d, d$group=="Tooth.Algo")
d.algo_raz <-subset(d, d$group=="Raz.Algo")
d.hum_raz <-subset(d, d$group=="Raz.Hum")
d.hum_tooth <-subset(d, d$group=="Tooth.Hum")



#DESCRIPTIVE STATISITCS

    #CONTROL comparsion:  means, SDs for Product Rating Trust X Control groups
    d.mean_ctl_tooth <-mean(d.ctl_tooth$r.prodrat.trust) #the mean trust rating for the ctl-tooth group
    sd(d.ctl_tooth$r.prodrat.trust) #SD = 1.211172

    d.mean_ctl_raz <-mean(d.ctl_raz$r.prodrat.trust) #the mean trust rating for the ctl-razor group
    sd(d.ctl_raz$r.prodrat.trust) # SD = 1.78915



# PRODUCT comparison
# Going down the product path since our T tests (below) show that there's a significant difference between the two

#Gender x PROD group                
table(b$gender, b$prod.grp)
                   
#                    raz tooth
#  Female             53    63
#  Male               46    38
#  Prefer not to say   0     2

table(b$gender, b$group)
                   
#                    Raz.Algo Raz.Ctl Raz.Hum Tooth.Algo Tooth.Ctl Tooth.Hum
#  Female                  22      17      14         19        24        20
#  Male                    18      11      17         13        15        10
#  Prefer not to say        0       0       0          1         0         1


#Gender x TREATMENT groups
table(b$gender, b$tmt.group)
#                   
#                    algo ctl hum
#  Female              41  41  34
#  Male                31  26  27
#  Prefer not to say    1   0   1



#Edu x PRODUCT GROUP
table(b$edu, b$prod.grp)
                                      
#                                       raz tooth
#  Completed a Master's degree or Ph.D.  20    20
#  Completed an Associate degree         12     9
#  Completed Bachelor's degree           30    36
#  Completed some college                24    27
#  Completed some high school             1     1
#  Completed some postgraduate            5     4
#  High school graduate                   7     6


table(b$edu, b$group)
                                      
#                                       Raz.Algo Raz.Ctl Raz.Hum Tooth.Algo Tooth.Ctl
#  Completed a Master's degree or Ph.D.       10       4       6          7         8
#  Completed an Associate degree               6       3       3          3         4
#  Completed Bachelor's degree                11      11       8         11        17
#  Completed some college                      9       6       9          7         6
#  Completed some high school                  1       0       0          0         1
#  Completed some postgraduate                 1       2       2          2         1
#  High school graduate                        2       2       3          3         2
#                                      
#                                       Tooth.Hum
#  Completed a Master's degree or Ph.D.         5
#  Completed an Associate degree                2
#  Completed Bachelor's degree                  8
#  Completed some college                      14
#  Completed some high school                   0
#  Completed some postgraduate                  1
#  High school graduate                         1


#Edu x TREATMENT group
table(b$edu, b$tmt.group)
#                                      
#                                       algo ctl hum
#  Completed a Master's degree or Ph.D.   17  12  11
#  Completed an Associate degree           9   7   5
#  Completed Bachelor's degree            22  28  16
#  Completed some college                 16  12  23
#  Completed some high school              1   1   0
#  Completed some postgraduate             3   3   3
#  High school graduate                    5   4   4




#PRODUCT RATING TRUST

table(b$prod.grp, b$r.prodrat.trust) # Trust by product groups only
#       
#         0  2  3  4  5  6  7  8  9 10
#  raz    1  1  4  4 18  9 20 36  5  1
#  tooth  0  0  0  1 10  5 26 38 13 10

mean(b.raz$r.prodrat.trust,na.rm = TRUE) # Means for respondents in the  RAZ grp = 6.636364
mean(b.tooth$r.prodrat.trust,na.rm = TRUE) # Means for respondents in the  RAZ grp = 7.640777


table(b$group, b$r.prodrat.trust) # Trust by treament groups
#            
#              0  2  3  4  5  6  7  8  9 10
#  Raz.Algo    1  0  3  0  8  2  9 16  1  0
#  Raz.Ctl     0  1  1  1  3  6  5  9  1  1
#  Raz.Hum     0  0  0  3  7  1  6 11  3  0
#  Tooth.Algo  0  0  0  1  6  2  4 14  3  3
#  Tooth.Ctl   0  0  0  0  2  1 13 13  6  4
#  Tooth.Hum   0  0  0  0  2  2  9 11  4  3

#INFO SOURCE TRUST
table(b$prod.grp, b$r.site.trust)
#       
#         0  1  3  4  5  6  7  8  9 10
#  raz    1  1  2  3 34  7 18 29  3  1
#  tooth  0  0  0  2 15  7 24 36 14  5

table(b$group, b$r.site.trust)
#            
#              0  1  3  4  5  6  7  8  9 10
#  Raz.Algo    1  0  1  1 10  2  8 17  0  0
#  Raz.Ctl     0  0  1  1 11  4  4  5  1  1
#  Raz.Hum     0  1  0  1 13  1  6  7  2  0
#  Tooth.Algo  0  0  0  2  8  0  8  9  4  2
#  Tooth.Ctl   0  0  0  0  4  4  9 13  6  3
#  Tooth.Hum   0  0  0  0  3  3  7 14  4  0


#INFERENTIAL STATISITICS


#Power Tests - CONTROL GROUPS

#templates from class
#pwr.t.test  (d=(6.79-7.31)/1.56, power=0.8, sig.level=0.05, type="two.sample", alternative = "two.sided")
#pwr.t.test (n=148, d=(7.30-6.86)/1.56, sig.level=.05, type="two.sample", alternative = "two.sided")


            #Power test - using 1.65 as the SD for the entire sample, using the means from the control subsets / not sure if this is correct.
            pwr.t.test  (d=(d.mean_ctl_tooth - d.mean_ctl_raz)/1.65, power=0.8, sig.level=0.05, type="two.sample", alternative = "two.sided")
            pwr.t.test (n=202, d=(d.mean_ctl_tooth - d.mean_ctl_raz)/1.65, sig.level=.05, type="two.sample", alternative = "two.sided")
            #Resuts:  required n = 31 / power with 202 = 0.9999999


            #Power test - using 1.78915 as the SD for the ctl-raz subset, 67 observations for the combined ctl grps
            pwr.t.test  (d=(d.mean_ctl_tooth - d.mean_ctl_raz)/1.78915, power=0.8, sig.level=0.05, type="two.sample", alternative = "two.sided")
            pwr.t.test (n=67, d=(d.mean_ctl_tooth - d.mean_ctl_raz)/1.78915, sig.level=.05, type="two.sample", alternative = "two.sided")
            #Results:  required n = 37.21 / power with 67 = 0.9657652


            #Power test - using 1.211172 as the SD for the ct-tooth subset, 67 observations for the combined ctl grps
            pwr.t.test  (d=(d.mean_ctl_tooth - d.mean_ctl_raz)/1.211172, power=0.8, sig.level=0.05, type="two.sample", alternative = "two.sided")
            pwr.t.test (n=67, d=(d.mean_ctl_tooth - d.mean_ctl_raz)/1.211172, sig.level=.05, type="two.sample", alternative = "two.sided")
            #Results:  required n = 17.61 / power with 67 = 0.9998563


# T Tests - CONTROL GROUPS

#T Tests to see if there's any effect on ratings trust based on prodcut (Yes, in all treatment conditions respondents scored the ratings for Oral B Toothbrushes higher.)
#t.test(d$`Will-recode`~d$Group, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

            #T Test for CONTROL groups:  Is there a sig difference in product rating trust based on product group when method isn't explained?  Yes! :-/
            t.test(d.ctl$r.prodrat.trust ~d.ctl$prod.grp, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
            #Results:  t = -3.0212, df = 44.283, p-value = 0.004172
            #Means:  Raz = 6.642857, Tooth = 7.820513 

            #T Test for ALGO groups:  Is there a sig difference in product rating trust based on product group when method is algo?  Yes! :-/
            t.test(d.algo$r.prodrat.trust ~d.algo$prod.grp, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
            #Results:  t = -2.0327, df = 70.939, p-value = 0.04582
            #Means:  Raz = 6.525000, Tooth = 7.363636

            #T Test for HUM groups:  Is there a sig difference in product rating trust based on product group when method is human?  Yes! :-/
            t.test(d.hum$r.prodrat.trust ~d.hum$prod.grp, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
            #Results:  t = -2.5435, df = 56.966, p-value = 0.01371
            #Means:  Raz = 6.774194, Tooth = 7.709677 

            #T test for TOOTH sample; star rating trust by gender groups
            t.test(b.tooth.gender$r.prodrat.trust ~b.tooth.gender$gender, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
            # p-value = 0.07501





# T Tests - TOOTHBRUSH 

            t.test(d.tooth$r.prodrat.trust ~d.tooth$group=="Tooth.Hum" | d.tooth$group=="Tooth.Algo") # t = 1.0869, df = 91.644, p-value = 0.2799
            t.test(d.tooth$r.prodrat.trust ~d.tooth$group=="Tooth.Hum" | d.tooth$group=="Tooth.ctl") # t = -0.34843, df = 63.234, p-value = 0.7287
            t.test(d.tooth$r.prodrat.trust ~d.tooth$group=="Tooth.Algo" | d.tooth$group=="Tooth.ctl") # t = 1.2844, df = 50.109, p-value = 0.2049


#ANOVA 
#aov(d$`Gender-recode` ~d$Group + d$gender + d$Group:d$gender)
#ANOVA2 <- aov(d$`Gender-recode` ~d$Group + d$gender + d$Group:d$gender)

#One Way ANOVA for the entire sample

            aov (d$r.prodrat.trust ~d$tmt.group)
            ANOVA <-aov (d$r.prodrat.trust ~d$tmt.group)
            summary(ANOVA)  #Results:  F = 1.297  P = 0.276

#One way ANOVA by prod groups:

            aov (d.raz$r.prodrat.trust ~d.raz$tmt.group)
            RazANOVA <-aov (d.raz$r.prodrat.trust ~d.raz$tmt.group)
            summary(RazANOVA)  #results:  F = 0.17, P=0.84

            aov (d.tooth$r.prodrat.trust ~d.tooth$tmt.group)
            ToothANOVA <-aov (d.tooth$r.prodrat.trust ~d.tooth$tmt.group)
            summary(ToothANOVA)  #Results:  F = 1.05, P=0.354


#Two way ANOVA by prodcut, treatment 
            aov (d$r.prodrat.trust ~d$tmt.group + d$prod.grp + d$tmt.group:d$prod.grp)
            TP2ANOVA <-aov (d$r.prodrat.trust ~d$tmt.group + d$prod.grp + d$tmt.group:d$prod.grp) 
               #                              Df Sum Sq Mean Sq F value  Pr(>F)    
               #     d$tmt.group              2    7.1    3.53   1.403   0.248    
               #     d$prod.grp               1   47.9   47.86  19.004 2.1e-05 ***
               #     d$tmt.group:d$prod.grp   2    1.0    0.51   0.204   0.815    
               #     Residuals              196  493.6    2.52   

            TukeyHSD(TP2ANOVA)
                #  Tukey multiple comparisons of means
                #    95% family-wise confidence level
                #
                # Fit: aov(formula = d$r.prodrat.trust ~ d$tmt.group + d$prod.grp + d$tmt.group:d$prod.grp)
                #
                # $`d$tmt.group`
                #                 diff        lwr       upr     p adj
                # ctl-algo  0.42424862 -0.2098212 1.0583185 0.2566299
                # hum-algo  0.33782589 -0.3094382 0.9850900 0.4354048
                # hum-ctl  -0.08642273 -0.7468630 0.5740175 0.9487316

                # $`d$prod.grp`
                #                diff       lwr      upr    p adj
                # tooth-raz 0.9678729 0.5273863 1.408359 2.34e-05
                # 
                # $`d$tmt.group:d$prod.grp`
                #                            diff         lwr       upr     p adj
                # ctl:raz-algo:raz      0.1178571 -1.00755180 1.2432661 0.9996628
                # hum:raz-algo:raz      0.2491935 -0.84371356 1.3421007 0.9863546
                # algo:tooth-algo:raz   0.8386364 -0.23545045 1.9127232 0.2212418
                # ctl:tooth-algo:raz    1.2955128  0.26769495 2.3233307 0.0048437
                # hum:tooth-algo:raz    1.1846774  0.09177031 2.2775845 0.0250766
                # hum:raz-ctl:raz       0.1313364 -1.05944231 1.3221151 0.9995656
                # algo:tooth-ctl:raz    0.7207792 -0.45274984 1.8943083 0.4890183
                # ctl:tooth-ctl:raz     1.1776557  0.04632126 2.3089901 0.0359941
                # hum:tooth-ctl:raz     1.0668203 -0.12395843 2.2575990 0.1075005
                # algo:tooth-hum:raz    0.5894428 -0.55295428 1.7318399 0.6742451
                # ctl:tooth-hum:raz     1.0463193 -0.05268856 2.1453271 0.0720481
                # hum:tooth-hum:raz     0.9354839 -0.22462586 2.0955936 0.1906916
                # ctl:tooth-algo:tooth  0.4568765 -0.62341737 1.5371703 0.8280551
                # hum:tooth-algo:tooth  0.3460411 -0.79635604 1.4884381 0.9527945
                # hum:tooth-ctl:tooth  -0.1108354 -1.20984323 0.9881724 0.9997197

            #Interactions in order of lowest -> highest P values
                # ctl:tooth-algo:raz    1.2955128  0.26769495 2.3233307 0.0048437
                # hum:tooth-algo:raz    1.1846774  0.09177031 2.2775845 0.0250766
                # ctl:tooth-ctl:raz     1.1776557  0.04632126 2.3089901 0.0359941


#Two way ANOVA by prodcut, treatment, site trust
            aov (d$r.site.trust ~d$tmt.group + d$prod.grp + d$tmt.group:d$prod.grp)
            # 
            # Terms:
            #                 d$tmt.group d$prod.grp d$tmt.group:d$prod.grp Residuals
            # Sum of Squares       2.0184    51.5715                 7.6239  493.0287
            # Deg. of Freedom           2          1                      2       196
            # 
            # Residual standard error: 1.586018
            # Estimated effects may be unbalanced

            TP2ANOVA2 <-aov (d$r.site.trust ~d$tmt.group + d$prod.grp + d$tmt.group:d$prod.grp) 
            summary(TP2ANOVA2)
            #                         Df Sum Sq Mean Sq F value   Pr(>F)    
            # d$tmt.group              2    2.0    1.01   0.401    0.670    
            # d$prod.grp               1   51.6   51.57  20.502 1.03e-05 ***
            # d$tmt.group:d$prod.grp   2    7.6    3.81   1.515    0.222    
            # Residuals              196  493.0    2.52                     
            # ---
            # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


            TukeyHSD(TP2ANOVA2)
            # Tukey multiple comparisons of means
            # 95% family-wise confidence level
            # 
            # Fit: aov(formula = d$r.site.trust ~ d$tmt.group + d$prod.grp + d$tmt.group:d$prod.grp)
            # 
            # $`d$tmt.group`
            #                 diff        lwr       upr     p adj
            # ctl-algo  0.23164997 -0.4020593 0.8653592 0.6640540
            # hum-algo  0.05302696 -0.5938690 0.6999229 0.9795539
            # hum-ctl  -0.17862301 -0.8386876 0.4814416 0.7987273
            #  
            # $`d$prod.grp`
            #               diff       lwr      upr    p adj
            # tooth-raz 1.004732 0.5644961 1.444968 1.16e-05
            # 
            # $`d$tmt.group:d$prod.grp`
            #                             diff         lwr       upr     p adj
            # ctl:raz-algo:raz     -0.34642857 -1.47119745 0.7783403 0.9493950
            # hum:raz-algo:raz     -0.33145161 -1.42373713 0.7608339 0.9524415
            # algo:tooth-algo:raz   0.50530303 -0.56817289 1.5787790 0.7538060
            # ctl:tooth-algo:raz    1.03910256  0.01186926 2.0663359 0.0456409
            # hum:tooth-algo:raz    0.89435484 -0.19793068 1.9866404 0.1769848
            # hum:raz-ctl:raz       0.01497696 -1.17512450 1.2050784 1.0000000
            # algo:tooth-ctl:raz    0.85173160 -0.32113002 2.0245932 0.2967586
            # ctl:tooth-ctl:raz     1.38553114  0.25484016 2.5162221 0.0068498
            # hum:tooth-ctl:raz     1.24078341  0.05068195 2.4308849 0.0355115
            # algo:tooth-hum:raz    0.83675464 -0.30499271 1.9785020 0.2867925
            # ctl:tooth-hum:raz     1.37055418  0.27217140 2.4689370 0.0054951
            # hum:tooth-hum:raz     1.22580645  0.06635653 2.3852564 0.0314515
            # ctl:tooth-algo:tooth  0.53379953 -0.54587988 1.6134789 0.7130521
            # hum:tooth-algo:tooth  0.38905181 -0.75269555 1.5307992 0.9236190
            # hum:tooth-ctl:tooth  -0.14474773 -1.24313050 0.9536351 0.9989675



# Data Visuals & PLOTS

    #Plots for age
                barplot(as.matrix(table(b$age,b$tmt.group)),
                beside = TRUE, 
                ylim = c(1,30),
                names=c("Algo", "Control", "Human"), 
                ylab ="Respondent's Age In Years",
                main="Respondent's Age In Years By Treatment Group: \n (18-24), (25-34), (35-44), (45-54), (55-64), (65+)")


    #Plots for entire sample by GENDER
                boxplot(b$r.prodrat.trust ~b$gender, ylab="Respondent's rating", main="Trust in product rating description by respondent's reported gender")

                #Tooth by gender
                boxplot(b.tooth$r.prodrat.trust ~b.tooth$gender, ylab="Respondent's rating", main="main")
                
                #Razor by gender
                boxplot(b.raz$r.prodrat.trust ~b.raz$gender, ylab="Respondent's rating", main="main")
                

    #Plots for entire sample by EDU level
                boxplot(b$r.prodrat.trust ~b$edu, ylab="Respondent's rating", main="Trust in product rating description by respondent's reported education level")

                barplot(table(d$edu), 
                names=c("Completed some HS", "HS Grad", "Some College", "Completed AD","Completed Undergrad", "Some Post Grad", "Completed Post Grad"),
                main="Respondent's education", 
                ylim =c(1,100),                                             
                ylab="Frequency")

    #Plots for entire sample (simple plot followed by same plot with lables for report)
                boxplot(d$r.prodrat.trust ~d$tmt.group) 
                boxplot(d$r.prodrat.trust ~d$tmt.group, names=c("Machine Learned System", "Control Group", "Human Average") ,ylab="Respondent's Rating", xlab = "Sample Groups", main="Whole Sample:  On a scale of 0 to 10, how would you rate the trustworthiness of the product rating based on the product rating description?")


    #Plots for Toothbrush group (simple plot followed by same plot with lables for report)
                boxplot(d.tooth$r.prodrat.trust ~d.tooth$tmt.group) 
                boxplot(d.tooth$r.prodrat.trust ~d.tooth$tmt.group,names =c("Machine Learned System", "Control Group", "Human Average") ,ylab="Respondent's Rating", xlab = "Sample Groups", main="Oral B Toothbrush Group:  On a scale of 0 to 10, how would you rate the trustworthiness of the product rating based on the product rating description?")    
                boxplot(d.tooth$r.prodrat.trust ~d.tooth$tmt.group,
                names =c("Machine Learned System", "Control Group", "Human Average") ,
                ylab ="Respondent's Rating", 
                ylim = c(1,11),
                xlab = "Sample Groups", 
                main="Oral B Toothbrush Group:  On a scale of 0 to 10, how would you rate \n the trustworthiness of the product rating based on the product rating description?")


                

    #Plots for Razor group (simple plot followed by same plot with lables for report)
                boxplot(d.raz$r.prodrat.trust ~d.raz$tmt.group)
                boxplot(d.raz$r.prodrat.trust ~d.raz$tmt.group, names=c("Machine Learned System", "Control Group", "Human Average") ,ylab="Respondent's Rating", xlab = "Sample Groups", main="Bic Razor Group:  On a scale of 0 to 10, how would you rate the trustworthiness of the product rating based on the product rating description?")

                boxplot(d.raz$r.prodrat.trust ~d.raz$tmt.group, 
                names=c("Machine Learned System", "Control Group", "Human Average") ,
                ylab="Respondent's Rating", 
                ylim = c(1,10),
                xlab = "Sample Groups", 
                main="Bic Razor Group:  On a scale of 0 to 10, how would you rate the trustworthiness of \n the product rating based on the product rating description?")



    #BOXPLOT FOR REPORT - STAR RATING - Details the entire sample's means for raz and tooth
                boxplot(b$r.prodrat.trust ~b$group, 
                ylab = "Means for trust in star rating",
                ylim = c(1,10),
                xlab = "Product-Treatment Groups",
                main = "Figure 1:  Trust in product star rating description by group assignment",
                )

    #BOXPLOT FOR REPORT - SITE TRUST - Details the entire sample's means for raz and tooth
                boxplot(b$r.site.trust ~b$group, 
                ylab = "Means for trust in information source",
                ylim = c(1,10),
                xlab = "Product-Treatment Groups",
                main = "Figure 2:  Trust in information source by group assignments",
                )


#Histograms

            # Toothbrush group
            hist(b.tooth$r.prodrat.trust)

            #Razor group
            hist(b.raz$r.prodrat.trust)


 # Faults with our study:
            # made a mistake in choosing the items.  We(I) should have caught it, but (we)I fucked up.  The orab-b makes sense that it is seen as a higher trust in the product
            # They are not seen on equal terms.  The Bics are LOW quality Razors were the oral-b is seen as a higher end manual tooth brush.
            # People also probably have more good experiences with the tooth brush.
            # Lession to be learned, we really need to be picky and vet the products that we choose.  I don't think they were BAD products per say, but I think we could have picked better.
            
            # Limitations:
            # If this was a real experiment in the real world, we would have had more time to vet the products.  
            # For our pilot we were more worried about who was seeing what than checking if there was a problem with our products.
            # We let bias come in because of time constraints and focus.
            # What we could do differently next time, is run small samples to vet the products until we find products that are similar to one another.
            
            # Next steps/Design implications
            # If we asked a different question, this could have turned out differently based on the numbers we got.
            # We can see that the variance of the human group definitely has lowest range which is interesting in itself, especially from a industry perspective.

