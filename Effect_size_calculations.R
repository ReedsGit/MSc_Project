library(dmetar)
library(esc)

#Calculate d and SE_d for effect sizes given in D
D=-23.9
n_A=23
n_B=27
s_pooled=9.22

SE_D=s_pooled*(sqrt((1/n_A)+(1/n_B))); SE_D
d=D/s_pooled; d
SE_d=sqrt(((n_A+n_B)/(n_A*n_B))+(d^2/(2*(n_A+n_B)))); SE_d

#######################################################
#Calculate SE's from p-value
#se.from.p(effect size, p=p-value, N=total study sample)

se.from.p(-0.17, p=0.02, N=24, effect.size.type="difference")

########################################################
#Calculate mean difference from control effect sizes and treatment effect sizes
x_A = 1.09   #treatment group mean effect
x_B = 0.19  #control group mean effect
D=x_A-x_B  #raw mean difference
n_A=18    #treatment sample
n_B=21     #no treatment sample (control) - choosing this rather than sham treatment

#t-statistic from p-value
#Need the inverse CDF function of 1-p
p_A=0.21/2 #divide by 2 for 2-tailed test
t_A=qt(1-p_A, n_A-1); t_A #function to give t-statistic

p_B=0.91/2 #divide by 2 for 2-tailed test
t_B=qt(1-p_B, n_B-1); t_B #function to give t-statistic

#standard deviation of differences for group A and B
s_A=abs((x_A*sqrt(n_A))/t_A); s_A
s_B=abs((x_B*sqrt(n_B)))/t_B; s_B

################################################
#Standard deviations from Confidence intervals 
x_A = 3.7   #treatment group mean effect
x_B = -1.1  #control group mean effect
D=x_A-x_B  #raw mean difference
n_A=24
n_B=23
t_A=qt(0.975, n_A-1) # For 2-tailed 95% CI
t_B=qt(0.975, n_B-1)

upper_CI_A=11.2  #upper 95% CI of treatment group
upper_CI_B=5.2   #upper 95% CI of control group
lower_CI_A=-3.7
lower_CI_B=-7.3

s_A=sqrt(n_A)*(upper_CI_A - lower_CI_A)/3.92; s_A
s_B=sqrt(n_B)*(upper_CI_B - lower_CI_B)/3.92; s_B
# OR
s_A=sqrt(n_A)*(upper_CI_A - lower_CI_A)/(2*t_A); s_A
s_B=sqrt(n_B)*(upper_CI_B - lower_CI_B)/(2*t_B); s_B

# OR if given standard deviations 
s_A=
s_B=
#################################################
#Finding s_pooled, d and SE_d

s_pooled=sqrt(((n_A-1)*s_A^2+(n_B-1)*s_B^2)/((n_A-1)+(n_B-1))); s_pooled
SE_D=s_pooled*(sqrt((1/n_A)+(1/n_B))); SE_D
d=D/s_pooled; d
SE_d=sqrt(((n_A+n_B)/(n_A*n_B))+(d^2/(2*(n_A+n_B)))); SE_d


